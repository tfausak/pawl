module Pawl.Codec.TriggeredAbility where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.TriggerCondition as TriggerCondition
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

toJson :: (Eq card) => (card -> Value.Value) -> TriggeredAbility.TriggeredAbility card -> Value.Value
toJson codec ta =
  Value.object . concat $
    [ Common.requiredPair "condition" (Codec.encode TriggerCondition.codec) (TriggeredAbility.condition ta),
      Common.requiredPair "modal" (Modal.toJson codec) (TriggeredAbility.modal ta),
      Common.optionalPair "intervening" Nothing (Common.encodeMaybe (Codec.encode Condition.codec)) (TriggeredAbility.intervening ta)
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (TriggeredAbility.TriggeredAbility card)
fromJson decode value = do
  ps <- Common.asObject value
  c <- Common.field "condition" ps >>= Codec.decode TriggerCondition.codec
  m <- Common.field "modal" ps >>= Modal.fromJson decode
  i <- Common.defaultedField "intervening" Nothing (Common.decodeMaybe (Codec.decode Condition.codec)) ps
  pure
    TriggeredAbility.MkTriggeredAbility
      { TriggeredAbility.condition = c,
        TriggeredAbility.modal = m,
        TriggeredAbility.intervening = i
      }

-- A name-keyed map as a JSON OBJECT keyed by the ability name.
toJsonDelayed :: (Eq card) => (card -> Value.Value) -> Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility card) -> Value.Value
toJsonDelayed codec = Common.encodeTextMap AbilityName.unwrap (toJson codec)

fromJsonDelayed :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility card))
fromJsonDelayed decode = Common.decodeTextMap AbilityName.MkAbilityName (fromJson decode)
