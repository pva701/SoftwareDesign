-- |

module EvStatT
       ( EvStatT (..)
       ) where

import           Control.Monad.State (get, put)
import           Data.List           (groupBy)
import           Data.Time.Units     (Hour, Microsecond, subTime)
import           Universum

import           Class               (ClockClass (..), EvStatClass (..))

type Events = [(String, Microsecond)]

newtype EvStatT m a = EvStatT
    { runEvStatT :: StateT Events m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState Events
               )

hour :: Hour
hour = fromIntegral 1

takeLastHour :: Microsecond -> Events -> Events
takeLastHour _ [] = []
takeLastHour nw (x:xs)
    | snd x >= subTime nw hour = (x:takeLastHour nw xs)
    | otherwise = []

calcRPM :: Events -> (String, Double)
calcRPM xs@(x:_) = (fst x, (/60) . fromIntegral . length $ xs)
calcRPM []       = ("", 0)

instance (Monad m, ClockClass m) => EvStatClass (EvStatT m) where
    incEvent name = EvStatT $ do
        nw <- now
        modify ((name, nw):)

    getEventStatisticByName name = EvStatT $
        (/60) . fromIntegral .
        length .
        filter ((name ==) . fst) <$>
        (takeLastHour <$> now <*> get)

    getAllEventStatistic = EvStatT $
        map calcRPM .
        groupBy (\x y -> fst x == fst y) <$>
        (takeLastHour <$> now <*> get)

    getStatistic = EvStatT $
        map calcRPM .
        groupBy (\x y -> fst x == fst y) <$>
        get
