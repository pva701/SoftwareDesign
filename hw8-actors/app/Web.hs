{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Web App

module Web where

import           Control.Concurrent.MVar              (putMVar)
import           Data.Aeson                           (encode)
import           Data.ByteString.Lazy                 (ByteString)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Universum                            hiding (ByteString)
import qualified Web.Scotty                           as Sc

import           Serve                                (runMasterActor)

runWebApp :: Word16 -> Int -> [String] -> IO ()
runWebApp (fromIntegral -> port) tl urls = do
    logInfo $ "Running web app on port: " <> show port
    application <- Sc.scottyApp $ webApp tl urls
    Warp.run port $ logStdoutDev  application

webApp :: Int -> [String] -> Sc.ScottyM ()
webApp tl urls = do
    Sc.get (Sc.capture "/api/query/:q") $ do
        (q::String) <- Sc.param "q"
        r <- lift $ handleQuery tl q urls
        Sc.raw r

handleQuery :: Int -> String -> [String] -> IO ByteString
handleQuery tl query urls = do
    mvar <- newEmptyMVar
    runMasterActor tl query urls $ putMVar mvar
    encode <$> takeMVar mvar

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn
