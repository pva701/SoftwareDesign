module Expr
    ( tokenize
    , TokenVisitor (..)
    , Token (..)
    , Op (..)
    ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Char           (digitToInt, isDigit)

data Token = LeftB
           | RightB
           | Operation Op
           | Num Int
           deriving (Show, Eq)

data Op = Plus | Minus | Div | Mul
    deriving (Show, Eq)

priority :: Token -> Int
priority (Operation Plus)  = 1
priority (Operation Minus) = 1
priority (Operation Mul)   = 2
priority (Operation Div)   = 2
priority LeftB = 0
priority RightB = 2

instance Ord Token where
    compare x y = compare (priority x) (priority y)

tokenize :: String -> Maybe [Token]
tokenize s =
    let res = runState (doIt s) ([], True) in
    case fst res of
        Nothing -> Nothing
        Just _  -> Just (reverse . fst . snd $ res)
  where
    doIt :: String -> State ([Token], Bool) (Maybe ())
    doIt [] = pure $ Just ()
    doIt (' ':xs) = _2 .= True >> doIt xs
    doIt ('(':xs) = _1 %= (LeftB:)  >> _2 .= True >> doIt xs
    doIt (')':xs) = _1 %= (RightB:) >> _2 .= True >> doIt xs
    doIt ('+':xs) = _1 %= (Operation Plus:) >> _2 .= True >> doIt xs
    doIt ('-':xs) = _1 %= (Operation Minus:) >> _2 .= True >> doIt xs
    doIt ('*':xs) = _1 %= (Operation Mul:) >> _2 .= True >> doIt xs
    doIt ('/':xs) = _1 %= (Operation Div:) >> _2 .= True >> doIt xs
    doIt (c:xs) =
        if isDigit c then do
            finish <- use _2
            if not finish then
                _1 %= (\t-> mult10 (digitToInt c) (head t) : tail t)
            else do
                _1 %= (Num (digitToInt c):)
                _2 .= False
            doIt xs
        else
            pure Nothing
    mult10 d (Num x) = Num (x * 10 + d)

class TokenVisitor m where
    visit :: Token -> m ()
