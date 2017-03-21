module Main where

import           Universum
import           Web       (runWebApp)

main :: IO ()
main = do
   runWebApp 3000 2000 [ "http://localhost:3333/yandex/"
                       , "http://localhost:3333/google/"
                       , "http://localhost:3333/yahoo/"
                       ]
