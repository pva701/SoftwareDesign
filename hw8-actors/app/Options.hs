{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module Options
       ( getOptions
       , Args (..)
       ) where

import           Options.Applicative.Simple (Parser, auto, command, help, info, long,
                                             metavar, option, progDesc, simpleOptions,
                                             value)
import           Universum

data Args = Args
    { reqTL :: !Int -- in ms
    , port   :: !Word16
    } deriving (Show)

argsParser :: Parser Args
argsParser = do
    reqTL <- option auto (
        long "req-tl" <>
        metavar "INT" <>
        help "Request timeout" <>
        value 2000
        )
    port <- option auto (
        long "port" <>
        metavar "INT" <>
        help "Web app port" <>
        value 3000
        )
    pure Args{..}

getOptions :: IO Args
getOptions = do
    (res, ()) <-
        simpleOptions
            "hw8-app"
            "App searcher server"
            "App searcher server"
            argsParser
            empty
    pure res
