module Pawl.JsonSchema.SchemaSpec where

import qualified Data.Text as Text
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonSchema.Schema" $ do
  let str = Schema.text . Text.pack
      obj = Value.Object . Object.MkObject
      arr = Value.Array . Array.MkArray

  Spec.it s "string is a type keyword" $ do
    Spec.assertEq s (Schema.unwrap Schema.string) . obj $ [Schema.pair "type" (str "string")]

  Spec.it s "natural pins a lower bound" $ do
    Spec.assertEq s (Schema.unwrap Schema.natural)
      . obj
      $ [Schema.pair "type" (str "integer"), Schema.pair "minimum" (Schema.integerValue 0)]

  Spec.it s "constant writes const" $ do
    Spec.assertEq s (Schema.unwrap (Schema.constant (Text.pack "Untap")))
      . obj
      $ [Schema.pair "const" (str "Untap")]

  Spec.it s "object carries properties and required" $ do
    Spec.assertEq
      s
      (Schema.unwrap (Schema.object [Schema.pair "a" (Schema.unwrap Schema.string)] [Text.pack "a"]))
      . obj
      $ [ Schema.pair "type" (str "object"),
          Schema.pair "properties" (obj [Schema.pair "a" (Schema.unwrap Schema.string)]),
          Schema.pair "required" (arr [str "a"])
        ]

  Spec.it s "nullable admits null beside the schema" $ do
    Spec.assertEq s (Schema.unwrap (Schema.nullable Schema.string))
      . obj
      $ [Schema.pair "oneOf" (arr [Schema.unwrap Schema.string, Schema.unwrap Schema.null])]

  Spec.it s "withDefault appends to the schema's own keywords" $ do
    Spec.assertEq s (Schema.unwrap (Schema.withDefault (str "x") Schema.string))
      . obj
      $ [Schema.pair "type" (str "string"), Schema.pair "default" (str "x")]
