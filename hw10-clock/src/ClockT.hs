module ClockT
       ( ClockT (..)
       , runClockT
       , addHours
       , addMinutes
       , addHM
       ) where

import           Control.Monad.State   (get, put)
import           Control.Monad.Trans   (MonadTrans (..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units       (Hour, Microsecond (..), Minute, addTime)
import           Universum

import           Class                 (ClockClass (..))

newtype ClockT m a = ClockT
    { getClockT :: StateT Microsecond m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               )

instance Monad m => ClockClass (ClockT m) where
    now = ClockT get

runClockT :: Monad m => ClockT m a -> m a
runClockT = flip evalStateT (fromIntegral 0) . getClockT

addHours :: Monad m => Int -> ClockT m ()
addHours h = ClockT $ modify $ addTime $ fromIntegral @_ @Hour h

addMinutes :: Monad m => Int -> ClockT m ()
addMinutes mn = ClockT $ modify $ addTime $ fromIntegral @_ @Minute mn

addHM :: Monad m => Int -> Int -> ClockT m ()
addHM hr mn = addHours hr >> addMinutes mn

