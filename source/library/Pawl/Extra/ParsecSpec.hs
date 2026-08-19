module Pawl.Extra.ParsecSpec where

import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Spec as Spec
import qualified Text.Parsec as Parsec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Extra.Parsec" $ do
  Spec.describe s "parseString" $ do
    Spec.it s "fails with no match" $ do
      Spec.assertEq s (Parsec.parseString Parsec.digit "x") Nothing

    Spec.it s "succeeds with a match" $ do
      Spec.assertEq s (Parsec.parseString Parsec.digit "1") $ Just '1'

    Spec.it s "allows unconsumed input" $ do
      Spec.assertEq s (Parsec.parseString Parsec.digit "2x") $ Just '2'
