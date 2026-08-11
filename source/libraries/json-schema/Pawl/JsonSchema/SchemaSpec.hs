module Pawl.JsonSchema.SchemaSpec where

import qualified Data.Text as Text
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonSchema.Schema" $ do
  let str = Value.text . Text.pack
      obj = Value.Object . Object.MkObject
      arr = Value.Array . Array.MkArray

  Spec.it s "string is a type keyword" $ do
    Spec.assertEq s (Schema.unwrap Schema.string) . obj $ [Pair.fromString "type" (str "string")]

  Spec.it s "natural pins a lower bound" $ do
    Spec.assertEq s (Schema.unwrap Schema.natural)
      . obj
      $ [Pair.fromString "type" (str "integer"), Pair.fromString "minimum" (Value.integer 0)]

  Spec.it s "constant writes const" $ do
    Spec.assertEq s (Schema.unwrap (Schema.constant (Text.pack "Untap")))
      . obj
      $ [Pair.fromString "const" (str "Untap")]

  Spec.it s "object carries properties and required" $ do
    Spec.assertEq
      s
      (Schema.unwrap (Schema.object [Pair.fromString "a" (Schema.unwrap Schema.string)] [Text.pack "a"]))
      . obj
      $ [ Pair.fromString "type" (str "object"),
          Pair.fromString "properties" (obj [Pair.fromString "a" (Schema.unwrap Schema.string)]),
          Pair.fromString "required" (arr [str "a"])
        ]

  Spec.it s "nullable admits null beside the schema" $ do
    Spec.assertEq s (Schema.unwrap (Schema.nullable Schema.string))
      . obj
      $ [Pair.fromString "oneOf" (arr [Schema.unwrap Schema.string, Schema.unwrap Schema.null])]

  Spec.it s "withDefault appends to the schema's own keywords" $ do
    Spec.assertEq s (Schema.unwrap (Schema.withDefault (str "x") Schema.string))
      . obj
      $ [Pair.fromString "type" (str "string"), Pair.fromString "default" (str "x")]
