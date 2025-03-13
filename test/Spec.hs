import Control.Monad
import Control.Monad.IO.Class
import Data.Proxy
import Test.QuickCheck
import Test.QuickCheck.Classes

import Lib

instance Arbitrary1 Computation where
  liftArbitrary arb = frequency [(1, return Suspension), (3, liftM Result arb)]

  liftShrink shr (Result x) = Suspension : [ Result x' | x' <- shr x ]
  liftShrink _   Suspension  = []

instance Arbitrary a => Arbitrary (Computation a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance Arbitrary a => Arbitrary (Try a) where
  arbitrary = Try <$> arbitrary

main :: IO ()
main = do
  lawsCheck (applicativeLaws (Proxy :: Proxy Computation))
  lawsCheck (applicativeLaws (Proxy :: Proxy Try))
  lawsCheck (monadLaws (Proxy :: Proxy Try))
