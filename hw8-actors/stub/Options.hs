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
    { respTL :: !Int -- in ms
    , port   :: !Word16
    } deriving (Show)

argsParser :: Parser Args
argsParser = do
    respTL <- option auto (
        long "resp-tl" <>
        metavar "INT" <>
        help "Response timeout" <>
        value 0
        )
    port <- option auto (
        long "port" <>
        metavar "INT" <>
        help "Web app port" <>
        value 3333
        )
    pure Args{..}

getOptions :: IO Args
getOptions = do
    (res, ()) <-
        simpleOptions
            "hw8-stub"
            "Stub server"
            "Stub server"
            argsParser
            empty
    pure res
