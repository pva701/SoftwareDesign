{-# LANGUAGE FlexibleContexts #-}

import           Test.Hspec            (Expectation, Spec, describe, hspec, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (.&&.), (===))
import           Universum

import           Aux                   (TEvent (..), Test (..), runImpure, runPure)
import           Class                 (EvStatClass (..))
import           ClockT                (ClockT (..), addHM, addHours, addMinutes,
                                        runClockT)
import           EvStatT               (EvStatT (..), runEvStatT)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Event Statistics tests" $ do
    prop "Expire event" expireEventP
    prop "Not expire event" notExpireEventP
    it "Event per minute" $ eventPerMinuteS "event"
    prop "Random test" $ testP

expireEventP :: String -> Property
expireEventP name = (0 === ) . runPure $ do
    incEvent name
    lift $ addHM 1 1
    getEventStatisticByName name

notExpireEventP :: String -> Property
notExpireEventP name = (1 / 60 === ) . runPure $ do
    incEvent name
    lift $ addHours 1
    getEventStatisticByName name

eventPerMinuteS :: String -> Expectation
eventPerMinuteS name = runImpure $ action 0
  where
    action 60 = pure ()
    action n = do
        incEvent name
        rpm <- getEventStatisticByName name
        lift $ lift $ rpm `shouldBe` ((n + 1) / 60)
        lift $ addMinutes 1
        action $ n + 1

testP :: Test -> Property
testP (Test seq ansLH ansAT) = do
    let (res1, res2) = runPure $ action seq
    res1 === ansLH
  where
    action [] = (,) <$> getAllEventStatistic <*> getStatistic
    action (IncMinute:xs) = do
        lift $ addMinutes 1
        action xs
    action (AddEvent name:xs) = do
        incEvent name
        action xs
