module Class
       ( EvStatClass (..)
       , ClockClass (..)
       ) where

import           Data.Time.Units (Microsecond)
import           Universum

class EvStatClass m where
    incEvent :: String -> m ()
    getEventStatisticByName :: String -> m Double
    getAllEventStatistic :: m [(String, Double)]
    getStatistic :: m [(String, Double)]

class ClockClass m where
    now :: m Microsecond

instance (Monad m, ClockClass m) => ClockClass (StateT e m) where
    now = lift now
