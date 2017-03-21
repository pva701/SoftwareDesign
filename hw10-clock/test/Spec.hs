import           Test.Hspec            (Spec, describe, hspec, it)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===))
import           Universum

import           Class                 (EvStatClass (..))
import           ClockT                (ClockT (..), addHM, addHours, addMinutes,
                                        runClockT)
import           EvStatT               (EvStatT (..), runEvStatT)

type PureWorkMode a = EvStatT (ClockT Identity) a

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Event Statistics tests" $
    prop "Expire request" expireRequestP

runPure :: PureWorkMode a -> a
runPure = runIdentity . runClockT . runEvStatT

expireRequestP :: String -> Property
expireRequestP name = (runPure $ expireRequest name) === 0

expireRequest :: String -> PureWorkMode Rational
expireRequest name = do
    incEvent name
    lift $ addHM 1 1
    getEventStatisticByName name
