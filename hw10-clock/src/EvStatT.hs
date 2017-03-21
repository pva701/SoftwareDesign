{-# LANGUAGE ScopedTypeVariables #-}
module EvStatT
       ( EvStatT (..)
       , runEvStatT
       ) where

import           Control.Monad.State (get, put)
import           Control.Monad.Trans (MonadTrans (..))
import           Data.List           (groupBy)
import           Data.Ratio          ((%))
import           Data.Time.Units     (Hour, Microsecond, Minute, subTime)
import           Universum

import           Class               (ClockClass (..), EvStatClass (..))

type Events = [(String, Microsecond)]

newtype EvStatT m a = EvStatT
    { getEvStatT :: StateT Events m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadState Events
               , MonadTrans
               , MonadIO
               )

runEvStatT :: Monad m => EvStatT m a -> m a
runEvStatT = flip evalStateT [] . getEvStatT

hour :: Hour
hour = fromIntegral 1

takeLastHour :: Microsecond -> Events -> Events
takeLastHour _ [] = []
takeLastHour nw (x:xs)
    | snd x >= subTime nw hour = (x:takeLastHour nw xs)
    | otherwise = []

calcRPM :: Int -> Events -> (String, Rational)
calcRPM (fromIntegral -> tot) xs@(x:_) = (fst x, (% tot) . fromIntegral . length $ xs)
calcRPM tot []                         = ("", 0)

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
        map (calcRPM 60) .
        groupBy (\x y -> fst x == fst y) <$>
        (takeLastHour <$> now <*> get)

    getStatistic = EvStatT $ do
        times <- get
        let mnTime = minimum $ map snd times
        nw <- now
        let (tot::Int) = fromIntegral $ subTime @Microsecond @Microsecond @Minute nw mnTime
        pure $ map (calcRPM tot) . groupBy (\x y -> fst x == fst y) $ times
