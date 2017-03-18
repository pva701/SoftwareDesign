{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serve
    ( runMasterActor
    ) where

import           Control.Concurrent.Actor (Actor, ActorM, Address, Handler (..),
                                           RemoteException (..), monitor, receive, self,
                                           send, spawn)
import           Control.Monad.State      (get)
import           Data.Aeson               (decode)
import qualified Data.Set                 as S
import qualified Network.Wreq             as Wr (get, responseBody, responseStatus,
                                                 statusCode)
import           System.FilePath          ((</>))
import           Universum

import           Types                    (Response (..))

data Start = Start !String !String
    deriving (Typeable)

data SlaveException = SlaveSearchFailed
    deriving (Show)
deriving instance Exception SlaveException

type MasterState = (Set Address, [Response])

runMasterActor :: MonadIO m => Int -> String -> [String] -> ([Response] -> IO ()) -> m ()
runMasterActor tl query urls = void . liftIO . spawn . masterActor tl query urls

masterActor :: Int -> String -> [String] -> ([Response] -> IO ()) -> Actor
masterActor ((*1000) -> respTL) query urls onFinish = do
    lift $ print "Master Actor started"
    me <- self
    let childrenLen = length urls
    children <- lift $ replicateM childrenLen $ spawn $ slaveActor me
    mapM_ monitor children
    taddress <- lift $ spawn (timeoutActor me respTL)
    monitor taddress

    mapM_ (uncurry $ flip send . Start query) $ zip urls children
    masterActorDo (S.fromList children, []) $ \responses -> do
        lift $ onFinish responses
        lift $ print "Master Actor finished"

masterActorDo :: MasterState -> ([Response] -> Actor) -> Actor
masterActorDo st@(expSet, ls) onFinish
    | null expSet = onFinish ls
    | otherwise = receive $
        [ Case onResponse
        , Case onTimeout
        , Case onException
        ]
  where
    onResponse :: (Response, Address) -> Actor
    onResponse (r, from) =
        masterActorDo (S.delete from expSet, r:ls) onFinish

    onTimeout :: () -> Actor
    onTimeout _ = masterActorDo (mempty, ls) onFinish

    onException :: RemoteException -> Actor
    onException (RemoteException fr _) =
        masterActorDo (S.delete fr expSet, ls) onFinish

slaveActor :: Address -> Actor
slaveActor parent = receive $
    [ Case onStart
    , Default $ error "Unexpected msg"
    ]
  where
    onStart :: Start -> Actor
    onStart (Start query url) = do
        resMB <- lift $ requestSearch query url
        me <- self
        case resMB of
            Nothing  -> lift $ throwM $ SlaveSearchFailed
            Just res -> send parent (res, me)

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
