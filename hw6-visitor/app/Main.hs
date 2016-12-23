module Main where

import           Control.Monad
import           Expr
import           Visitor

main :: IO ()
main = do
    s <- getLine
    let tMb = tokenize s
    case tMb of
        Nothing -> putStrLn "Invalid string"
        Just t  -> do
            forM_ t visit
            putStrLn ""
            polish <- runConverter (forM_ t visit)
            forM_ polish visit
            result <- runCalculator (forM_ polish visit)
            putStrLn ""
            print result
