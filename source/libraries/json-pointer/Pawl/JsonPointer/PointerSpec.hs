module Pawl.JsonPointer.PointerSpec where

import qualified Data.Text as Text
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.JsonPointer.Pointer as Pointer
import qualified Pawl.JsonPointer.Token as Token
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonPointer.Pointer" $ do
  let pointer = Pointer.MkPointer . fmap (Token.MkToken . Text.pack)

  Spec.describe s "decode" $ do
    Spec.it s "succeeds with empty string (root pointer)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "") . Just $ pointer []

    Spec.it s "succeeds with single slash (empty token)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/") . Just $ pointer [""]

    Spec.it s "succeeds with /foo" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/foo") . Just $ pointer ["foo"]

    Spec.it s "succeeds with /foo/bar" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/foo/bar") . Just $ pointer ["foo", "bar"]

    Spec.it s "succeeds with /foo/0" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/foo/0") . Just $ pointer ["foo", "0"]

    Spec.it s "succeeds with /a~1b (contains /)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/a~1b") . Just $ pointer ["a/b"]

    Spec.it s "succeeds with /m~0n (contains ~)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/m~0n") . Just $ pointer ["m~n"]

    Spec.it s "succeeds with /c%d (contains percent)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/c%d") . Just $ pointer ["c%d"]

    Spec.it s "succeeds with /e^f (contains caret)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/e^f") . Just $ pointer ["e^f"]

    Spec.it s "succeeds with /g|h (contains pipe)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/g|h") . Just $ pointer ["g|h"]

    Spec.it s "succeeds with /i\\\\j (contains backslash)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/i\\j") . Just $ pointer ["i\\j"]

    Spec.it s "succeeds with /k\"l (contains quote)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/k\"l") . Just $ pointer ["k\"l"]

    Spec.it s "succeeds with / / (contains space)" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "/ ") . Just $ pointer [" "]

    Spec.it s "handles multiple empty tokens" $ do
      Spec.assertEq s (Parsec.parseString Pointer.decode "///") . Just $ pointer ["", "", ""]

  Spec.describe s "encode" $ do
    Spec.it s "works with empty pointer (root)" $ do
      Spec.assertEq s (Builder.toString . Pointer.encode $ pointer []) ""

    Spec.it s "works with single empty token" $ do
      Spec.assertEq s (Builder.toString . Pointer.encode $ pointer [""]) "/"

    Spec.it s "works with /foo" $ do
      Spec.assertEq s (Builder.toString . Pointer.encode $ pointer ["foo"]) "/foo"

    Spec.it s "works with /foo/bar" $ do
      Spec.assertEq s (Builder.toString . Pointer.encode $ pointer ["foo", "bar"]) "/foo/bar"

    Spec.it s "works with /foo/0" $ do
      Spec.assertEq s (Builder.toString . Pointer.encode $ pointer ["foo", "0"]) "/foo/0"

    Spec.it s "encodes / in token as ~1" $ do
      Spec.assertEq s (Builder.toString . Pointer.encode $ pointer ["a/b"]) "/a~1b"

    Spec.it s "encodes ~ in token as ~0" $ do
      Spec.assertEq s (Builder.toString . Pointer.encode $ pointer ["m~n"]) "/m~0n"

    Spec.it s "does not escape percent" $ do
      Spec.assertEq s (Builder.toString . Pointer.encode $ pointer ["c%d"]) "/c%d"

    Spec.it s "handles multiple empty tokens" $ do
      Spec.assertEq s (Builder.toString . Pointer.encode $ pointer ["", "", ""]) "///"

  Spec.describe s "encodeFragment" $ do
    Spec.it s "works with empty pointer (root)" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer []) "#"

    Spec.it s "works with /foo" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer ["foo"]) "#/foo"

    Spec.it s "keeps RFC 6901 escaping for /" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer ["a/b"]) "#/a~1b"

    Spec.it s "keeps RFC 6901 escaping for ~" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer ["m~n"]) "#/m~0n"

    Spec.it s "percent encodes percent" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer ["c%d"]) "#/c%25d"

    Spec.it s "percent encodes caret" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer ["e^f"]) "#/e%5Ef"

    Spec.it s "percent encodes pipe" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer ["g|h"]) "#/g%7Ch"

    Spec.it s "percent encodes backslash" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer ["i\\j"]) "#/i%5Cj"

    Spec.it s "percent encodes space" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer [" "]) "#/%20"

    Spec.it s "percent encodes non-ASCII as UTF-8 octets" $ do
      Spec.assertEq s (Builder.toString . Pointer.encodeFragment $ pointer ["é"]) "#/%C3%A9"
