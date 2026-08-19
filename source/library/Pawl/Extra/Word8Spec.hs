module Pawl.Extra.Word8Spec where

import qualified Pawl.Extra.Word8 as Word8
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Extra.Word8" $ do
  Spec.describe s "toInt" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Word8.toInt 0) 0

    Spec.it s "works with the largest Word8" $ do
      Spec.assertEq s (Word8.toInt 255) 255
