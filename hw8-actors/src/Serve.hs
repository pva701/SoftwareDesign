{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serve
    ( runMasterActor
    ) where

import           Control.Concurrent.Actor (Actor, ActorM, Address, Flag (..),
                                           Handler (..), RemoteException (..), monitor,
                                           receive, self, send, setFlag, spawn)
import           Control.Monad.State      (get)
import           Data.Aeson               (decode)
import qualified Data.Set                 as S
import qualified Network.Wreq             as Wr (get, responseBody, responseStatus,
                                                 statusCode)
import           System.FilePath          ((</>))
import           Universum

import           Types                    (Response)

--import           Types                    (SearchEntry (..))

data Start = Start !String !String
    deriving (Typeable)

data SlaveException = SlaveSearchFailed
    deriving (Show, Typeable)
deriving instance Exception SlaveException

type MasterState = (Set Address, Response)

runMasterActor :: Int -> String -> [String] -> (Response -> IO ()) -> IO ()
runMasterActor tl query urls = void . spawn . masterActor tl query urls

masterActor :: Int -> String -> [String] -> (Response -> IO ()) -> Actor
masterActor ((*1000) -> respTL) query urls onFinish = do
    setFlag TrapRemoteExceptions
    logInfo $ "Master Actor started, timeout (mcs): " ++ show respTL
    me <- self
    let childrenLen = length urls
    children <- lift $ replicateM childrenLen $ spawn $ slaveActor me
    logInfo $ show children
    mapM_ monitor children
    taddress <- lift $ spawn $ timeoutActor me respTL
    monitor taddress

    logInfo "Sending Start..."
    mapM_ (uncurry $ flip send . Start query) $ zip urls children
    masterActorDo (S.fromList children, []) $ \responses -> do
        lift $ onFinish responses
        logInfo "Master Actor finished"

masterActorDo :: MasterState -> (Response -> Actor) -> Actor
masterActorDo st@(expSet, ls) onFinish
    | null expSet = onFinish ls
    | otherwise = do
        receive [ Case onResponse
                , Case onTimeout
                , Case onException
                , Default defaultHandler
                ]
  where
    defaultHandler = do
        logInfo "Received malformed message"
        masterActorDo st onFinish

    onResponse :: (Response, Address) -> Actor
    onResponse (r, from) = do
        logInfo $ "Response from: " ++ show from ++ " msg: " ++ show r
        masterActorDo (S.delete from expSet, ls `mappend` r) onFinish

    onTimeout :: () -> Actor
    onTimeout x = do
        logInfo "Timeout message"
        masterActorDo (mempty, ls) onFinish

    onException :: RemoteException -> Actor
    onException (RemoteException fr ex) = do
        logInfo $ "Remote Exception: " <> show fr <> " " <> show ex
        masterActorDo (S.delete fr expSet, ls) onFinish

slaveActor :: Address -> Actor
slaveActor parent = receive $
    [ Case onStart
    , Default $ logInfo "Received malformed message"
    ]
  where
    onStart :: Start -> Actor
    onStart (Start query url) = do
        logInfo $ "Start"
        resMB <- lift $ requestSearch query url
        case resMB of
            Nothing  -> lift $ throwM $ SlaveSearchFailed
            Just res -> self >>= send parent . (res, )

timeoutActor :: Address -> Int -> Actor
timeoutActor parent tl = do
    lift $ threadDelay tl
    send parent ()

requestSearch :: String -> String -> IO (Maybe Response)
requestSearch query url = requestDo `catch` (\(_::SomeException) -> pure Nothing)
  where
    requestDo = do
        resp <- Wr.get $ url </> query
        pure $
            if | resp ^. Wr.responseStatus . Wr.statusCode == 200 ->
                  decode $ resp ^. Wr.responseBody
                | otherwise -> Nothing

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn
