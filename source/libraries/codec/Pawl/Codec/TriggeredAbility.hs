module Pawl.Codec.TriggeredAbility where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.TriggerCondition as TriggerCondition
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

toJson :: (Eq card) => (card -> Value.Value) -> TriggeredAbility.TriggeredAbility card -> Value.Value
toJson codec ta =
  Common.object . concat $
    [ Common.requiredPair "condition" TriggerCondition.toJson (TriggeredAbility.condition ta),
      Common.requiredPair "modal" (Modal.toJson codec) (TriggeredAbility.modal ta),
      Common.optionalPair "intervening" Nothing (Common.encodeMaybe Condition.toJson) (TriggeredAbility.intervening ta)
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (TriggeredAbility.TriggeredAbility card)
fromJson decode value = do
  ps <- Common.asObject value
  c <- Common.field "condition" ps >>= TriggerCondition.fromJson
  m <- Common.field "modal" ps >>= Modal.fromJson decode
  i <- Common.defaultedField "intervening" Nothing (Common.decodeMaybe Condition.fromJson) ps
  pure
    TriggeredAbility.MkTriggeredAbility
      { TriggeredAbility.condition = c,
        TriggeredAbility.modal = m,
        TriggeredAbility.intervening = i
      }

-- A name-keyed map as a sorted array of entries, so the render is deterministic
-- and the file byte-stable.
toJsonDelayed :: (Eq card) => (card -> Value.Value) -> Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility card) -> Value.Value
toJsonDelayed codec m =
  Common.encodeList
    (\(k, v) -> Common.object [Common.pair "name" (AbilityName.toJson k), Common.pair "ability" (toJson codec v)])
    (Map.toAscList m)

fromJsonDelayed :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility card))
fromJsonDelayed decode value =
  let decodeEntry v = do
        ps <- Common.asObject v
        k <- Common.field "name" ps >>= AbilityName.fromJson
        a <- Common.field "ability" ps >>= fromJson decode
        pure (k, a)
   in Map.fromList <$> Common.decodeList decodeEntry value
