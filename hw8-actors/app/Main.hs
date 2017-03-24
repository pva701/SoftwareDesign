{-# LANGUAGE RecordWildCards #-}

module Main where

import           Universum

import           Options   (Args (..), getOptions)
import           Web       (runWebApp)

main :: IO ()
main = do
    Args {..} <- getOptions
    runWebApp port reqTL [ "http://localhost:3333/yandex/"
                         , "http://localhost:3334/google/"
                         ]
