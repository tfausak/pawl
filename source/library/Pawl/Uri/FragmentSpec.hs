module Pawl.Uri.FragmentSpec where

import qualified Data.ByteString.Builder as Builder
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Spec as Spec
import qualified Pawl.Uri.Fragment as Fragment

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Uri.Fragment" $ do
  let encode = Builder.toString . Fragment.encode . Builder.stringUtf8

  Spec.describe s "encode" $ do
    Spec.it s "leaves an empty input alone" $ do
      Spec.assertEq s (encode "") ""

    Spec.it s "leaves an unreserved octet alone" $ do
      Spec.assertEq s (encode "aZ0-._~") "aZ0-._~"

    Spec.it s "leaves the octets the fragment production adds alone" $ do
      Spec.assertEq s (encode "!$&'()*+,;=:@/?") "!$&'()*+,;=:@/?"

    Spec.it s "percent encodes percent" $ do
      Spec.assertEq s (encode "c%d") "c%25d"

    Spec.it s "percent encodes caret" $ do
      Spec.assertEq s (encode "e^f") "e%5ef"

    Spec.it s "percent encodes pipe" $ do
      Spec.assertEq s (encode "g|h") "g%7ch"

    Spec.it s "percent encodes backslash" $ do
      Spec.assertEq s (encode "i\\j") "i%5cj"

    Spec.it s "percent encodes space" $ do
      Spec.assertEq s (encode " ") "%20"

    Spec.it s "percent encodes non-ASCII as UTF-8 octets" $ do
      Spec.assertEq s (encode "é") "%c3%a9"
