module Pawl.JsonPointer.EvaluateSpec where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonPointer.Evaluate as Evaluate
import qualified Pawl.JsonPointer.Pointer as Pointer
import qualified Pawl.JsonPointer.Token as Token
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonPointer.Evaluate" $ do
  let pointer = Pointer.MkPointer . fmap (Token.MkToken . Text.pack)

  Spec.describe s "evaluate" $ do
    Spec.it s "empty pointer returns the document" $ do
      Spec.assertEq s (Evaluate.evaluate (pointer []) Value.null) $ Just Value.null

    Spec.it s "empty pointer returns the document (object)" $ do
      let doc = Value.object [Value.pair "foo" $ Value.integer 1]
      Spec.assertEq s (Evaluate.evaluate (pointer []) doc) $ Just doc

    Spec.it s "returns object member by name" $ do
      let doc = Value.object [Value.pair "foo" $ Value.string "bar"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["foo"]) doc) . Just $ Value.string "bar"

    Spec.it s "returns nested object member" $ do
      let doc = Value.object [Value.pair "foo" $ Value.object [Value.pair "bar" $ Value.integer 42]]
      Spec.assertEq s (Evaluate.evaluate (pointer ["foo", "bar"]) doc) . Just $ Value.integer 42

    Spec.it s "returns array element by index" $ do
      let doc = Value.array [Value.string "a", Value.string "b", Value.string "c"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["1"]) doc) . Just $ Value.string "b"

    Spec.it s "returns first array element with index 0" $ do
      let doc = Value.array [Value.string "first", Value.string "second"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["0"]) doc) . Just $ Value.string "first"

    Spec.it s "returns Nothing for out-of-bounds array index" $ do
      let doc = Value.array [Value.string "a"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["5"]) doc) Nothing

    Spec.it s "returns Nothing for non-integer array index" $ do
      let doc = Value.array [Value.string "a"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["foo"]) doc) Nothing

    Spec.it s "returns Nothing for leading zero in array index" $ do
      let doc = Value.array [Value.string "a", Value.string "b"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["01"]) doc) Nothing

    Spec.it s "returns Nothing for negative array index" $ do
      let doc = Value.array [Value.string "a", Value.string "b"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["-1"]) doc) Nothing

    Spec.it s "returns Nothing for missing object key" $ do
      let doc = Value.object [Value.pair "foo" $ Value.string "bar"]
      Spec.assertEq s (Evaluate.evaluate (pointer ["baz"]) doc) Nothing

    Spec.it s "returns Nothing when stepping into a scalar" $ do
      Spec.assertEq s (Evaluate.evaluate (pointer ["foo"]) (Value.string "bar")) Nothing

    Spec.describe s "rfc 6901 section 5" $ do
      let rfc6901Doc =
            Value.object
              [ Value.pair "foo" $ Value.array [Value.string "bar", Value.string "baz"],
                Value.pair "" $ Value.integer 0,
                Value.pair "a/b" $ Value.integer 1,
                Value.pair "c%d" $ Value.integer 2,
                Value.pair "e^f" $ Value.integer 3,
                Value.pair "g|h" $ Value.integer 4,
                Value.pair "i\\j" $ Value.integer 5,
                Value.pair "k\"l" $ Value.integer 6,
                Value.pair " " $ Value.integer 7,
                Value.pair "m~n" $ Value.integer 8
              ]

      Spec.it s "empty pointer returns the whole document" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer []) rfc6901Doc) $ Just rfc6901Doc

      Spec.it s "/foo returns [\"bar\", \"baz\"]" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["foo"]) rfc6901Doc) . Just $ Value.array [Value.string "bar", Value.string "baz"]

      Spec.it s "/foo/0 returns \"bar\"" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["foo", "0"]) rfc6901Doc) . Just $ Value.string "bar"

      Spec.it s "/ (empty token) returns 0" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer [""]) rfc6901Doc) . Just $ Value.integer 0

      Spec.it s "/a~1b (unescaped a/b) returns 1" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["a/b"]) rfc6901Doc) . Just $ Value.integer 1

      Spec.it s "/c%d returns 2" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["c%d"]) rfc6901Doc) . Just $ Value.integer 2

      Spec.it s "/e^f returns 3" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["e^f"]) rfc6901Doc) . Just $ Value.integer 3

      Spec.it s "/g|h returns 4" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["g|h"]) rfc6901Doc) . Just $ Value.integer 4

      Spec.it s "/i\\j returns 5" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["i\\j"]) rfc6901Doc) . Just $ Value.integer 5

      Spec.it s "/k\"l returns 6" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["k\"l"]) rfc6901Doc) . Just $ Value.integer 6

      Spec.it s "/ (space) returns 7" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer [" "]) rfc6901Doc) . Just $ Value.integer 7

      Spec.it s "/m~0n (unescaped m~n) returns 8" $ do
        Spec.assertEq s (Evaluate.evaluate (pointer ["m~n"]) rfc6901Doc) . Just $ Value.integer 8

    Spec.it s "handles empty string key" $ do
      let doc = Value.object [Value.pair "" $ Value.string "empty key"]
      Spec.assertEq s (Evaluate.evaluate (pointer [""]) doc) . Just $ Value.string "empty key"

    Spec.it s "handles deeply nested path" $ do
      let doc = Value.object [Value.pair "a" $ Value.object [Value.pair "b" $ Value.object [Value.pair "c" $ Value.string "deep"]]]
      Spec.assertEq s (Evaluate.evaluate (pointer ["a", "b", "c"]) doc) . Just $ Value.string "deep"

    Spec.it s "handles mixed array and object traversal" $ do
      let doc = Value.object [Value.pair "items" $ Value.array [Value.null, Value.object [Value.pair "name" $ Value.string "second"]]]
      Spec.assertEq s (Evaluate.evaluate (pointer ["items", "1", "name"]) doc) . Just $ Value.string "second"
