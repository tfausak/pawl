-- | The @StaticAbility ⇆ Json@ codec (#481).
module Pawl.Codec.StaticAbility where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Affected (affectedToJson, jsonToAffected)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Modification (jsonToModification, modificationToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.StaticAbility as StaticAbility

-- CR 613.6: the parts of one ability's effect travel together, so the wire format
-- is one affected set and an ARRAY of modifications -- never one entry per layer.
staticAbilityToJson :: StaticAbility.StaticAbility -> Value
staticAbilityToJson sa =
  Json.jObject
    [ (Text.pack "affected", affectedToJson (StaticAbility.affected sa)),
      (Text.pack "modifications", Json.nonEmptyTo modificationToJson (StaticAbility.modifications sa))
    ]

jsonToStaticAbility :: Value -> Either Text StaticAbility.StaticAbility
jsonToStaticAbility value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "affected") ps >>= jsonToAffected
  ms <- Json.field (Text.pack "modifications") ps >>= Json.nonEmptyFrom jsonToModification
  pure (StaticAbility.MkStaticAbility a ms)
