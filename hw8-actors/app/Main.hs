module Main where

import           Universum
import           Web       (runWebApp)

main :: IO ()
main = do
   runWebApp 3000 2000 ["http://localhost:9000/login"]
