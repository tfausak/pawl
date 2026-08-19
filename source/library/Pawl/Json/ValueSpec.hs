module Pawl.Json.ValueSpec where

import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Json.Value" $ do
  Spec.describe s "decode" $ do
    Spec.it s "parses null" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "null") $ Just Value.null

    Spec.it s "parses a boolean" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "false") . Just $ Value.boolean False

    Spec.it s "parses a number" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "0") . Just $ Value.integer 0

    Spec.it s "parses a string" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "\"\"") . Just $ Value.string ""

    Spec.it s "parses an array" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "[]") . Just $ Value.array []

    Spec.it s "parses an object" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "{}") . Just $ Value.object []

    Spec.it s "accepts leading blanks" $ do
      Spec.assertEq s (Parsec.parseString Value.decode " null") $ Just Value.null

    Spec.it s "accepts trailing blanks" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "null ") $ Just Value.null

  Spec.describe s "encode" $ do
    Spec.it s "encodes null" $ do
      Spec.assertEq s (Builder.toString $ Value.encode Value.null) "null"

    Spec.it s "encodes a boolean" $ do
      Spec.assertEq s (Builder.toString . Value.encode $ Value.boolean False) "false"

    Spec.it s "encodes a number" $ do
      Spec.assertEq s (Builder.toString . Value.encode $ Value.integer 0) "0"

    Spec.it s "encodes a string" $ do
      Spec.assertEq s (Builder.toString . Value.encode $ Value.string "") "\"\""

    Spec.it s "encodes an array" $ do
      Spec.assertEq s (Builder.toString . Value.encode $ Value.array []) "[]"

    Spec.it s "encodes an object" $ do
      Spec.assertEq s (Builder.toString . Value.encode $ Value.object []) "{}"
