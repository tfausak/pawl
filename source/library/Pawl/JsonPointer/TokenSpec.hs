module Pawl.JsonPointer.TokenSpec where

import qualified Data.Text as Text
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.JsonPointer.Token as Token
import qualified Pawl.Spec as Spec
import qualified Text.Parsec as Parsec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonPointer.Token" $ do
  let token = Token.MkToken . Text.pack

  Spec.describe s "decode" $ do
    Spec.it s "succeeds with an empty token" $ do
      Spec.assertEq s (Parsec.parseString Token.decode "") . Just $ token ""

    Spec.it s "succeeds with a simple token" $ do
      Spec.assertEq s (Parsec.parseString Token.decode "foo") . Just $ token "foo"

    Spec.it s "succeeds with a numeric token" $ do
      Spec.assertEq s (Parsec.parseString Token.decode "0") . Just $ token "0"

    Spec.it s "decodes ~0 as tilde" $ do
      Spec.assertEq s (Parsec.parseString Token.decode "~0") . Just $ token "~"

    Spec.it s "decodes ~1 as slash" $ do
      Spec.assertEq s (Parsec.parseString Token.decode "~1") . Just $ token "/"

    Spec.it s "decodes ~01 as ~1 (not as /)" $ do
      Spec.assertEq s (Parsec.parseString Token.decode "~01") . Just $ token "~1"

    Spec.it s "decodes a~1b as a/b" $ do
      Spec.assertEq s (Parsec.parseString Token.decode "a~1b") . Just $ token "a/b"

    Spec.it s "decodes m~0n as m~n" $ do
      Spec.assertEq s (Parsec.parseString Token.decode "m~0n") . Just $ token "m~n"

    Spec.it s "stops at slash" $ do
      Spec.assertEq s (Parsec.parseString (Token.decode <* Parsec.char '/') "foo/") . Just $ token "foo"

    Spec.it s "handles multiple escapes" $ do
      Spec.assertEq s (Parsec.parseString Token.decode "~0~1~0~1") . Just $ token "~/~/"

  Spec.describe s "encode" $ do
    Spec.it s "works with an empty token" $ do
      Spec.assertEq s (Builder.toString . Token.encode $ token "") ""

    Spec.it s "works with a simple token" $ do
      Spec.assertEq s (Builder.toString . Token.encode $ token "foo") "foo"

    Spec.it s "works with a numeric token" $ do
      Spec.assertEq s (Builder.toString . Token.encode $ token "0") "0"

    Spec.it s "encodes tilde as ~0" $ do
      Spec.assertEq s (Builder.toString . Token.encode $ token "~") "~0"

    Spec.it s "encodes slash as ~1" $ do
      Spec.assertEq s (Builder.toString . Token.encode $ token "/") "~1"

    Spec.it s "encodes a/b as a~1b" $ do
      Spec.assertEq s (Builder.toString . Token.encode $ token "a/b") "a~1b"

    Spec.it s "encodes m~n as m~0n" $ do
      Spec.assertEq s (Builder.toString . Token.encode $ token "m~n") "m~0n"

    Spec.it s "handles multiple special chars" $ do
      Spec.assertEq s (Builder.toString . Token.encode $ token "~/~/") "~0~1~0~1"
