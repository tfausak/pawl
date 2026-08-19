module Pawl.Extra.BuilderSpec where

import qualified Data.ByteString.Builder as Builder
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Extra.Builder" $ do
  Spec.describe s "toString" $ do
    Spec.it s "round trips the empty string" $ do
      Spec.assertEq s (Builder.toString mempty) ""

    Spec.it s "rounds trips a single UTF-8 byte" $ do
      Spec.assertEq s (Builder.toString $ Builder.stringUtf8 "A") "A"

    Spec.it s "round trips two UTF-8 bytes" $ do
      Spec.assertEq s (Builder.toString $ Builder.stringUtf8 "\xe9") "\xe9"

    Spec.it s "round trips three UTF-8 bytes" $ do
      Spec.assertEq s (Builder.toString $ Builder.stringUtf8 "\x20ac") "\x20ac"

    Spec.it s "round trips four UTF-8 bytes" $ do
      Spec.assertEq s (Builder.toString $ Builder.stringUtf8 "\x1d11e") "\x1d11e"

    Spec.it s "replaces invalid bytes" $ do
      Spec.assertEq s (Builder.toString $ Builder.word8 0xff) "\xfffd"
