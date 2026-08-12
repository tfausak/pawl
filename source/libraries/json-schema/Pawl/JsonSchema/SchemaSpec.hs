module Pawl.JsonSchema.SchemaSpec where

import qualified Data.Text as Text
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonSchema.Schema" $ do
  let str = Value.text . Text.pack
      obj = Value.Object . Object.MkObject
      arr = Value.Array . Array.MkArray

  Spec.it s "string is a type keyword" $ do
    Spec.assertEq s (Schema.unwrap Schema.string) . obj $ [Value.pair "type" (str "string")]

  Spec.it s "natural pins a lower bound" $ do
    Spec.assertEq s (Schema.unwrap Schema.natural)
      . obj
      $ [Value.pair "type" (str "integer"), Value.pair "minimum" (Value.integer 0)]

  Spec.it s "constant writes const" $ do
    Spec.assertEq s (Schema.unwrap (Schema.constant (Text.pack "Untap")))
      . obj
      $ [Value.pair "const" (str "Untap")]

  Spec.it s "object carries properties and required" $ do
    Spec.assertEq
      s
      (Schema.unwrap (Schema.object [Value.pair "a" (Schema.unwrap Schema.string)] [Text.pack "a"]))
      . obj
      $ [ Value.pair "type" (str "object"),
          Value.pair "properties" (obj [Value.pair "a" (Schema.unwrap Schema.string)]),
          Value.pair "required" (arr [str "a"])
        ]

  Spec.it s "nullable admits null beside the schema" $ do
    Spec.assertEq s (Schema.unwrap (Schema.nullable Schema.string))
      . obj
      $ [Value.pair "oneOf" (arr [Schema.unwrap Schema.string, Schema.unwrap Schema.null])]

  Spec.it s "withDefault appends to the schema's own keywords" $ do
    Spec.assertEq s (Schema.unwrap (Schema.withDefault (str "x") Schema.string))
      . obj
      $ [Value.pair "type" (str "string"), Value.pair "default" (str "x")]

  Spec.it s "boolean is a type keyword" $ do
    Spec.assertEq s (Schema.unwrap Schema.boolean) . obj $ [Value.pair "type" (str "boolean")]

  Spec.it s "array carries an items schema" $ do
    Spec.assertEq s (Schema.unwrap (Schema.array Schema.string))
      . obj
      $ [Value.pair "type" (str "array"), Value.pair "items" (Schema.unwrap Schema.string)]

  Spec.it s "uniqueArray adds uniqueItems to array" $ do
    Spec.assertEq s (Schema.unwrap (Schema.uniqueArray Schema.string))
      . obj
      $ [ Value.pair "type" (str "array"),
          Value.pair "items" (Schema.unwrap Schema.string),
          Value.pair "uniqueItems" (Value.boolean True)
        ]

  Spec.it s "nonEmptyArray adds minItems: 1 to array" $ do
    Spec.assertEq s (Schema.unwrap (Schema.nonEmptyArray Schema.string))
      . obj
      $ [ Value.pair "type" (str "array"),
          Value.pair "items" (Schema.unwrap Schema.string),
          Value.pair "minItems" (Value.integer 1)
        ]

  Spec.it s "tupleOf pins prefixItems and both bounds" $ do
    Spec.assertEq s (Schema.unwrap (Schema.tupleOf [Schema.string, Schema.integer]))
      . obj
      $ [ Value.pair "type" (str "array"),
          Value.pair "prefixItems" (arr [Schema.unwrap Schema.string, Schema.unwrap Schema.integer]),
          Value.pair "minItems" (Value.integer 2),
          Value.pair "maxItems" (Value.integer 2)
        ]

  -- The value shape is pinned and the KEYS are not, which is exactly what
  -- Pawl.JsonCodec.Common.decodeTextMap accepts. No propertyNames: a schema
  -- constraining keys would claim more than the decoder guarantees. The exact
  -- keyword set is asserted, so this also pins mapOf apart from `object [] []`
  -- -- that emits properties and required, and would fail here.
  Spec.it s "mapOf pins the value shape and leaves keys unconstrained" $ do
    Spec.assertEq s (Schema.unwrap (Schema.mapOf Schema.integer))
      . obj
      $ [ Value.pair "type" (str "object"),
          Value.pair "additionalProperties" (Schema.unwrap Schema.integer)
        ]
