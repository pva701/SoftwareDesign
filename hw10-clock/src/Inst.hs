module Inst
       ( setTime
       ) where

import           Control.Monad.State   (get, put)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units       (Microsecond (..), fromMicroseconds)
import           Universum

import           Class                 (ClockClass (..))

instance ClockClass (State Int) where
    now = fromMicroseconds . fromIntegral <$> get

setTime :: Int -> State Int ()
setTime = put

instance ClockClass IO where
    now = notImplemented
