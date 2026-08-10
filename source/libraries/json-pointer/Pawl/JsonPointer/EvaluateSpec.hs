module Pawl.JsonPointer.EvaluateSpec where

import qualified Data.Text as Text
import qualified Pawl.Decimal as Decimal
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Null as Null
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonPointer.Evaluate as Evaluate
import qualified Pawl.JsonPointer.Pointer as Pointer
import qualified Pawl.JsonPointer.Token as Token
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonPointer.Evaluate" $ do
  let pointer = Pointer.MkPointer . fmap (Token.MkToken . Text.pack)
      null_ = Value.Null $ Null.MkNull ()
      number x = Value.Number . Number.MkNumber . Decimal.mkDecimal x
      integer = flip number 0
      string = Value.String . String.MkString . Text.pack
      array = Value.Array . Array.MkArray
      pair = Pair.MkPair . String.MkString . Text.pack
      object = Value.Object . Object.MkObject . fmap (uncurry pair)

  Spec.describe s "evaluate" $ do
    Spec.it s "empty pointer returns the document" $ do
      Spec.assertEq s (Evaluate.evaluate (pointer []) null_) $ Just null_

    Spec.it s "empty pointer returns the document (object)" $ do
      let doc = object [("foo", integer 1)]
      Spec.assertEq s (Evaluate.evaluate (pointer []) doc) $ Just doc

    Spec.it s "returns object member by name" $ do
      let doc = object [("foo", string "bar")]
      Spec.assertEq s (Evaluate.evaluate (pointer ["foo"]) doc) . Just $ string "bar"

    Spec.it s "returns nested object member" $ do
      let doc = object [("foo", object [("bar", integer 42)])]
      Spec.assertEq s (Evaluate.evaluate (pointer ["foo", "bar"]) doc) . Just $ integer 42

    Spec.it s "returns array element by index" $ do
      let doc = array [string "a", string "b", string "c"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["1"]) doc) . Just $ string "b"

    Spec.it s "returns first array element with index 0" $ do
      let doc = array [string "first", string "second"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["0"]) doc) . Just $ string "first"

    Spec.it s "returns Nothing for out-of-bounds array index" $ do
      let doc = array [string "a"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["5"]) doc) Nothing

    Spec.it s "returns Nothing for non-integer array index" $ do
      let doc = array [string "a"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["foo"]) doc) Nothing

    Spec.it s "returns Nothing for leading zero in array index" $ do
      let doc = array [string "a", string "b"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["01"]) doc) Nothing

    Spec.it s "returns Nothing for negative array index" $ do
      let doc = array [string "a", string "b"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["-1"]) doc) Nothing

    Spec.it s "returns Nothing for missing object key" $ do
      let doc = object [("foo", string "bar")]
      Spec.assertEq s (Evaluate.evaluate (pointer ["baz"]) doc) Nothing

    Spec.it s "returns Nothing when stepping into a scalar" $ do
      Spec.assertEq s (Evaluate.evaluate (pointer ["foo"]) (string "bar")) Nothing

    Spec.describe s "rfc 6901 section 5" $ do
      let rfc6901Doc =
            object
              [ ("foo", array [string "bar", string "baz"]),
                ("", integer 0),
                ("a/b", integer 1),
                ("c%d", integer 2),
                ("e^f", integer 3),
                ("g|h", integer 4),
                ("i\\j", integer 5),
                ("k\"l", integer 6),
                (" ", integer 7),
                ("m~n", integer 8)
              ]

      Spec.it s "empty pointer returns the whole document" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer []) rfc6901Doc) $ Just rfc6901Doc

      Spec.it s "/foo returns [\"bar\", \"baz\"]" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["foo"]) rfc6901Doc) . Just $ array [string "bar", string "baz"]

      Spec.it s "/foo/0 returns \"bar\"" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["foo", "0"]) rfc6901Doc) . Just $ string "bar"

      Spec.it s "/ (empty token) returns 0" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer [""]) rfc6901Doc) . Just $ integer 0

      Spec.it s "/a~1b (unescaped a/b) returns 1" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["a/b"]) rfc6901Doc) . Just $ integer 1

      Spec.it s "/c%d returns 2" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["c%d"]) rfc6901Doc) . Just $ integer 2

      Spec.it s "/e^f returns 3" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["e^f"]) rfc6901Doc) . Just $ integer 3

      Spec.it s "/g|h returns 4" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["g|h"]) rfc6901Doc) . Just $ integer 4

      Spec.it s "/i\\j returns 5" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["i\\j"]) rfc6901Doc) . Just $ integer 5

      Spec.it s "/k\"l returns 6" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["k\"l"]) rfc6901Doc) . Just $ integer 6

      Spec.it s "/ (space) returns 7" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer [" "]) rfc6901Doc) . Just $ integer 7

      Spec.it s "/m~0n (unescaped m~n) returns 8" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["m~n"]) rfc6901Doc) . Just $ integer 8

    Spec.it s "handles empty string key" $ do
      let doc = object [("", string "empty key")]
      Spec.assertEq s (Evaluate.evaluate (pointer [""]) doc) . Just $ string "empty key"

    Spec.it s "handles deeply nested path" $ do
      let doc = object [("a", object [("b", object [("c", string "deep")])])]
      Spec.assertEq s (Evaluate.evaluate (pointer ["a", "b", "c"]) doc) . Just $ string "deep"

    Spec.it s "handles mixed array and object traversal" $ do
      let doc = object [("items", array [object [("name", string "first")], object [("name", string "second")]])]
      Spec.assertEq s (Evaluate.evaluate (pointer ["items", "1", "name"]) doc) . Just $ string "second"
