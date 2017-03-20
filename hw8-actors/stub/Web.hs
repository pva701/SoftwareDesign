{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Web App

module Web
       ( runStubApp
       ) where

import           Control.Concurrent.MVar              (putMVar)
import           Data.Aeson                           (encode)
import           Data.ByteString.Lazy                 (ByteString)
import qualified Data.HashMap.Strict                  as HM
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Universum                            hiding (ByteString)
import qualified Web.Scotty                           as Sc

import           Types                                (Response)

runStubApp :: Word16 -> Int -> HashMap String Response -> IO ()
runStubApp (fromIntegral -> port) ((*1000) -> tlResp) resp = do
    logInfo $ "Running stub server on port: " <> show port
    application <- Sc.scottyApp $ stubApp resp tlResp
    Warp.run port $ logStdoutDev  application

stubApp :: HashMap String Response -> Int -> Sc.ScottyM ()
stubApp resp tlResp = do
    Sc.get (Sc.capture "/:searchsystem/:q") $ do
        (searchsystem::String) <- Sc.param "searchsystem"
        (q::String) <- Sc.param "q"
        let query = searchsystem ++ "/" ++ q
        logInfo $ "Query: " ++ query
        let res = HM.lookupDefault [] query resp
        lift $ threadDelay tlResp
        Sc.raw $ encode res

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn
