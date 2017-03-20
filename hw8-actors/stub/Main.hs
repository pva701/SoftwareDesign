{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.HashMap.Strict as HM
import           Universum

import           Options             (Args (..), getOptions)
import           Types               (Response, SearchEntry (..))
import           Web                 (runStubApp)

resp :: HashMap String Response
resp = HM.fromList
    [ ("yandex/title", [SearchEntry "url" "title" "yandex"])
    ]

main :: IO ()
main = do
    Args {..} <- getOptions
    runStubApp port respTL resp
