module Lib
    ( runMasterActor
    ) where

import           Control.Concurrent.Actor (Actor, ActorM, Address, Handler (..),
                                           RemoteException, monitor, receive, self, send,
                                           spawn)
import           Control.Concurrent.MVar  (MVar)
import           Control.Monad.State      (get)
import           Universum


data Response
data Start = Start !Text !String
    deriving (Typeable)

type ActorState = StateT (Int, [Response]) ActorM ()

masterActor :: Int -> Text -> [String] -> Actor
masterActor ((*1000) -> respTL) query urls = do
    lift $ print "Master Actor started"
    me <- self
    let childrenLen = length urls
    children <- lift $ replicateM childrenLen $ spawn $ slaveActor me
    mapM_ monitor children
    void $ lift $ spawn (timeoutActor me respTL)
    mapM_ (uncurry $ flip send . Start query) $ zip urls children
    masterActorDo (childrenLen, []) $ \(_, responses) -> do
        notImplemented
        lift $ print "Master Actor finished"

data ResponseRes = Js Response | No | TL

masterActorDo :: (Int, [Response]) -> ((Int, [Response]) -> Actor) -> Actor
masterActorDo st@(ex, ls) onFinish
    | ex == 0 = onFinish st
    | otherwise = receive $
        [ Case onResponse
        , Case onTimeout
        , Case onException
        ]
  where
    onResponse :: Response -> Actor
    onResponse r = masterActorDo (ex - 1, r:ls) onFinish

    onTimeout :: () -> Actor
    onTimeout _ = masterActorDo (0, ls) onFinish

    onException :: RemoteException -> Actor
    onException _ = masterActorDo (ex - 1, ls) onFinish

slaveActor :: Address -> Actor
slaveActor parent = receive $
    [ Case onStart
    , Default $ error "Unexpected msg"
    ]
  where
    onStart :: Start -> Actor
    onStart (Start query url) = do
        -- TODO request here
        notImplemented
        send parent (""::Text)

timeoutActor :: Address -> Int -> Actor
timeoutActor parent tl = do
    lift $ threadDelay tl
    send parent ()

runMasterActor :: MonadIO m => Int -> Text -> [String] -> m ()
runMasterActor tl query = void . liftIO . spawn . masterActor tl query
