{-# LANGUAGE FlexibleContexts #-}

import           Test.Hspec            (Expectation, Spec, describe, hspec, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===))
import           Universum

import           Class                 (EvStatClass (..))
import           ClockT                (ClockT (..), addHM, addHours, addMinutes,
                                        runClockT)
import           EvStatT               (EvStatT (..), runEvStatT)

type PureWorkMode a = EvStatT (ClockT Identity) a
type WorkMode m a = EvStatT (ClockT m) a

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Event Statistics tests" $ do
    prop "Expire event" expireEventP
    prop "Not expire event" notExpireEventP
    it "Event per minute" $ eventPerMinuteS "event"

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

--- Aux
runPure :: PureWorkMode a -> a
runPure = runIdentity . runClockT . runEvStatT

runImpure :: Monad m => WorkMode m a -> m a
runImpure = runClockT . runEvStatT
