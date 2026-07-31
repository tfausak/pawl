-- | The @TriggeredAbility ⇆ Json@ codec (#481).
module Pawl.Codec.TriggeredAbility where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.AbilityName (abilityNameToJson, jsonToAbilityName)
import Pawl.Codec.Condition (conditionToJson, jsonToCondition)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Modal (jsonToModal, modalToJson)
import Pawl.Codec.TriggerCondition (jsonToTriggerCondition, triggerConditionToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

triggeredAbilityToJson :: (card -> Value) -> TriggeredAbility.TriggeredAbility card -> Value
triggeredAbilityToJson codec ta =
  Json.jObject
    ( [ (Text.pack "condition", triggerConditionToJson (TriggeredAbility.condition ta)),
        (Text.pack "modal", modalToJson codec (TriggeredAbility.modal ta))
      ]
        <> ( case TriggeredAbility.intervening ta of
               Nothing -> []
               Just c -> [(Text.pack "intervening", conditionToJson c)]
           )
    )

jsonToTriggeredAbility :: (Value -> Either Text card) -> Value -> Either Text (TriggeredAbility.TriggeredAbility card)
jsonToTriggeredAbility decode value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "condition") ps >>= jsonToTriggerCondition
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal decode
  i <- Json.maybeFrom jsonToCondition (Json.getOpt (Text.pack "intervening") ps)
  pure
    TriggeredAbility.MkTriggeredAbility
      { TriggeredAbility.condition = c,
        TriggeredAbility.modal = m,
        TriggeredAbility.intervening = i
      }

-- The targetSpecsToJson shape: a name-keyed map as a sorted array of entries, so
-- the render is deterministic and the file byte-stable.
delayedAbilitiesToJson :: (card -> Value) -> Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility card) -> Value
delayedAbilitiesToJson codec m =
  Json.listTo
    (\(k, v) -> Json.jObject [(Text.pack "name", abilityNameToJson k), (Text.pack "ability", triggeredAbilityToJson codec v)])
    (Map.toAscList m)

jsonToDelayedAbilities :: (Value -> Either Text card) -> Value -> Either Text (Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility card))
jsonToDelayedAbilities decode value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "name") ps >>= jsonToAbilityName
        a <- Json.field (Text.pack "ability") ps >>= jsonToTriggeredAbility decode
        pure (k, a)
   in Map.fromList <$> Json.listFrom decodeEntry value
