{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.ValueSpec where

import qualified Data.Text as Text
import qualified Pawl.Decimal as Decimal
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Boolean as Boolean
import qualified Pawl.Json.Null as Null
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Json.Value" $ do
  Spec.describe s "decode" $ do
    Spec.it s "parses null" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "null") . Just . Value.Null $ Null.MkNull ()

    Spec.it s "parses a boolean" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "false") . Just . Value.Boolean $ Boolean.MkBoolean False

    Spec.it s "parses a number" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "0") . Just . Value.Number . Number.MkNumber $ Decimal.mkDecimal 0 0

    Spec.it s "parses a string" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "\"\"") . Just . Value.String . String.MkString $ Text.pack ""

    Spec.it s "parses an array" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "[]") . Just . Value.Array $ Array.MkArray []

    Spec.it s "parses an object" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "{}") . Just . Value.Object $ Object.MkObject []

    Spec.it s "accepts leading blanks" $ do
      Spec.assertEq s (Parsec.parseString Value.decode " null") . Just . Value.Null $ Null.MkNull ()

    Spec.it s "accepts trailing blanks" $ do
      Spec.assertEq s (Parsec.parseString Value.decode "null ") . Just . Value.Null $ Null.MkNull ()

  Spec.describe s "encode" $ do
    Spec.it s "encodes null" $ do
      Spec.assertEq s (Builder.toString . Value.encode . Value.Null $ Null.MkNull ()) "null"

    Spec.it s "encodes a boolean" $ do
      Spec.assertEq s (Builder.toString . Value.encode . Value.Boolean $ Boolean.MkBoolean False) "false"

    Spec.it s "encodes a number" $ do
      Spec.assertEq s (Builder.toString . Value.encode . Value.Number . Number.MkNumber $ Decimal.mkDecimal 0 0) "0"

    Spec.it s "encodes a string" $ do
      Spec.assertEq s (Builder.toString . Value.encode . Value.String . String.MkString $ Text.pack "") "\"\""

    Spec.it s "encodes an array" $ do
      Spec.assertEq s (Builder.toString . Value.encode . Value.Array $ Array.MkArray []) "[]"

    Spec.it s "encodes an object" $ do
      Spec.assertEq s (Builder.toString . Value.encode . Value.Object $ Object.MkObject []) "{}"
