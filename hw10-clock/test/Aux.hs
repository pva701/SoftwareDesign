{-# LANGUAGE ViewPatterns #-}

-- | Auxilary stuff for tests.

module Aux
       ( PureWorkMode
       , WorkMode
       , runPure
       , runImpure

       , TEvent (..)
       , Test (..)
       ) where

import           Data.List                 (groupBy)
import           Data.Ratio                ((%))
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import           Test.QuickCheck.Gen       (Gen (..), choose, elements, vectorOf)
import           Universum

import           Class                     (EvStatClass (..))
import           ClockT                    (ClockT (..), addHM, addHours, addMinutes,
                                            runClockT)
import           EvStatT                   (EvStatT (..), runEvStatT)

type PureWorkMode a = EvStatT (ClockT Identity) a
type WorkMode m a = EvStatT (ClockT m) a

runPure :: PureWorkMode a -> a
runPure = runIdentity . runClockT . runEvStatT

runImpure :: Monad m => WorkMode m a -> m a
runImpure = runClockT . runEvStatT

data TEvent
    = IncMinute
    | AddEvent { evName :: !String }
    deriving Show

data Test = Test
    { events   :: ![TEvent]
    , lastHour :: ![(String, Rational)]
    , allTime  :: ![(String, Rational)]
    } deriving Show

instance Arbitrary Test where
    arbitrary = do
        prob <- choose (0, 100)
        --n <- choose (0, 100000)
        n <- choose (0, 100)
        namesLen <- choose (1, 100)
        names <- vectorOf namesLen genName

        events <- probabilisticGenerator names prob n
        let addEvents = filter isAddEvent events
        let totalTime = n + 1 - length addEvents
        let allTimeRPM = computeRPM totalTime addEvents
        let hourRPM = computeRPM 60 $ takeLastNMins 60 $ reverse events
        pure $ Test events hourRPM allTimeRPM

genName :: Gen String
genName = do
    n <- choose (1, 20)
    vectorOf n arbitrary

takeLastNMins :: Int -> [TEvent] -> [TEvent]
takeLastNMins 0 (IncMinute:_)     = []
takeLastNMins mins (IncMinute:xs) = takeLastNMins (mins - 1) xs
takeLastNMins mins (x:xs)         = x : takeLastNMins mins xs
takeLastNMins _ []             = []

computeRPM :: Int -> [TEvent] -> [(String, Rational)]
computeRPM totalTime addEvents = do
    let groupsByName = groupBy (\x y -> evName x == evName y) addEvents
    map (calcRPM totalTime) groupsByName

calcRPM :: Int -> [TEvent] -> (String, Rational)
calcRPM (fromIntegral -> tot) xs@(x:_) =
    (evName x, (% tot) . fromIntegral . length $ xs)
calcRPM _ []         = ("", 0)

isAddEvent :: TEvent -> Bool
isAddEvent IncMinute    = False
isAddEvent (AddEvent _) = True

probabilisticGenerator :: [String] -> Int -> Int -> Gen [TEvent]
probabilisticGenerator _ _ 0      = pure [IncMinute]
probabilisticGenerator names prob len = do
    r <- choose (0, 99)
    if r < prob then (IncMinute:) <$> probabilisticGenerator names prob (len - 1)
    else do
        name <- elements names
        (AddEvent name:) <$> probabilisticGenerator names prob (len - 1)
