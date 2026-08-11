module Pawl.JsonSchema.DefineSpec where

import qualified Data.Text as Text
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonSchema.Define" $ do
  let name = Name.MkName . Text.pack
      obj = Value.Object . Object.MkObject
      document ref defs =
        obj
          [ Pair.fromString "$schema" (Value.text (Text.pack "https://json-schema.org/draft/2020-12/schema")),
            Pair.fromString "$ref" (Value.text (Text.pack ref)),
            Pair.fromString "$defs" (obj defs)
          ]

  Spec.it s "a definition becomes a reference" $ do
    Spec.assertEq
      s
      (Schema.unwrap (Define.reference (name "PlayerId")))
      (obj [Pair.fromString "$ref" (Value.text (Text.pack "#/$defs/PlayerId"))])

  Spec.it s "escapes a name's pointer characters in the fragment" $ do
    Spec.assertEq s (Define.fragment (name ":~/")) (Text.pack "#/$defs/:~0~1")

  Spec.it s "files the name raw in $defs and escaped in $ref" $ do
    Spec.assertEq
      s
      (Define.run (Define.define (name ":~/") (pure Schema.string)))
      (document "#/$defs/:~0~1" [Pair.fromString ":~/" (Schema.unwrap Schema.string)])

  -- Without define registering its name BEFORE evaluating its body, this does
  -- not terminate, and the suite's one-second timeout is what says so.
  Spec.it s "a self-referential definition terminates" $ do
    let recursive = Define.define (name "Loop") (fmap Schema.nullable recursive)
    Spec.assertEq
      s
      (Define.run recursive)
      ( document
          "#/$defs/Loop"
          [Pair.fromString "Loop" (Schema.unwrap (Schema.nullable (Define.reference (name "Loop"))))]
      )
