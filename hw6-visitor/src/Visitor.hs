{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Visitor
       (
         runConverter
       , runCalculator
       ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.State

import           Expr                (Op (..), Token (..), TokenVisitor (..))

data PrinterException = PrinterException String
    deriving Show
instance Exception PrinterException

instance TokenVisitor IO where
    visit op@(Operation _) = putStr $ "'" ++ show op ++ "' "
    visit n@(Num _)        = putStr $ "'" ++ show n ++ "' "
    visit LeftB            = putStr $ "'" ++ show LeftB ++ "'"
    visit RightB           = putStr $ "'" ++ show RightB ++ "'"

newtype PolishConverter m a = PolishConverter
    { converterHolder :: StateT ([Token], [Token]) m a
    } deriving (Functor, Applicative, Monad, MonadThrow, MonadState ([Token], [Token]))

data ConverterException = ConverterException String
    deriving Show
instance Exception ConverterException

instance MonadThrow m => TokenVisitor (PolishConverter m) where
    visit n@(Num _) = _1 %= (++ [n])
    visit l@LeftB   = _2 %= (l:)
    visit RightB    = do
        nl <- uses _2 null
        if nl then throwM $ ConverterException "Invalid brackets balance"
        else do
            h <- uses _2 head
            _2 %= tail
            if h == LeftB then pure ()
            else _1 %= (++[h]) >> visit RightB
    visit t1 = do
        nl <- uses _2 null
        if nl then _2 %= (t1:)
        else do
            t2 <- uses _2 head
            if t1 <= t2 then do
                _2 %= tail
                _1 %= (++ [t2])
                visit t1
            else
                _2 %= (t1:)

runConverter :: MonadThrow m => PolishConverter m a -> m [Token]
runConverter conv = do
    (res, ops) <- execStateT (converterHolder conv) ([], [])
    if all notBr ops then
        return $ res ++ ops
    else
        throwM $ ConverterException "Invalid brackets balance"
  where
    notBr LeftB = False
    notBr _ = True

newtype PolishCalculator m a = PolishCalculator
    { calculatorHolder :: StateT [Int] m a
    } deriving (Functor, Applicative, Monad, MonadThrow, MonadState [Int])

runCalculator :: MonadThrow m => PolishCalculator m a -> m Int
runCalculator polish = do
    st <- execStateT (calculatorHolder polish) []
    if length st == 1 then return $ head st
    else throwM $ CalculatorException "Invalid expression"

data CalculatorException = CalculatorException String
    deriving Show
instance Exception CalculatorException

instance MonadThrow m => TokenVisitor (PolishCalculator m) where
    visit (Operation op) = do
        stack <- get
        when (length stack <= 1) $ throwM $ CalculatorException "Invalid stack"
        let x1 = stack !! 0
            x2 = stack !! 1
            ns = drop 2 stack
        case op of
             Plus  -> put (x1 + x2 : ns)
             Mul   -> put (x1 * x2 : ns)
             Minus -> put (x2 - x1 : ns)
             Div   -> put (x2 `div` x1 : ns)
    visit (Num x) = modify (x:)
    visit LeftB = throwM $ CalculatorException "Unexpected left brace"
    visit RightB = throwM $ CalculatorException "Unexpected right brace"
