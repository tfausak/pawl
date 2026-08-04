{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TriggeredAbilitySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

-- | Every case below instantiates the `card` parameter at 'Text.Text', a
-- stand-in that is never a real card -- 'TriggeredAbility.toJson'/
-- 'TriggeredAbility.fromJson' reach it only through the supplied Modal codec,
-- exactly like 'Pawl.Codec.EffectSpec's own cardToJson/cardFromJson.
cardToJson :: Text.Text -> Value.Value
cardToJson = Common.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: TriggeredAbility.TriggeredAbility Text.Text -> Value.Value
toJson = TriggeredAbility.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (TriggeredAbility.TriggeredAbility Text.Text)
fromJson = TriggeredAbility.fromJson cardFromJson

-- One constructor (MkTriggeredAbility), so two cases cover both states of the
-- CR 603.4 `intervening` field: Sarcomancy's own two triggered abilities
-- (data/cards/sarcomancy.json), whose zombie-token trigger states no
-- intervening "if" and whose upkeep trigger states one.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TriggeredAbility" $ do
  Spec.it s "MkTriggeredAbility, Sarcomancy's zombie-token trigger (no intervening if)" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( TriggeredAbility.MkTriggeredAbility
          { TriggeredAbility.condition = TriggerCondition.SelfEnters,
            TriggeredAbility.modal =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.singleton (Effect.Create (Quantity.Literal 1) (Text.pack "Zombie Token") EntryRiders.defaultValue Nothing)) Map.empty Optionality.Mandatory))
                (ModeSelection.ChooseExactly 1),
            TriggeredAbility.intervening = Nothing
          }
      )
      """ {"condition":{"type":"SelfEnters"},"modal":{"modes":[{"effects":[{"type":"Create","value":[{"type":"Literal","value":1},"Zombie Token"]}]}]}} """
  -- CR 603.4's intervening "if" clause: emitted only when the ability states
  -- one, so Sarcomancy's upkeep trigger ("if no Zombies") writes the key and
  -- the case above (which states none) omits it.
  Spec.it s "MkTriggeredAbility, Sarcomancy's upkeep trigger (an intervening if)" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( TriggeredAbility.MkTriggeredAbility
          { TriggeredAbility.condition = TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn,
            TriggeredAbility.modal =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.singleton (Effect.DealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you"))) (Quantity.Literal 1))) Map.empty Optionality.Mandatory))
                (ModeSelection.ChooseExactly 1),
            TriggeredAbility.intervening =
              Just
                ( Condition.MkCondition
                    (Quantity.Count (Count.MkCount (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer) (Filter.HasSubtype Subtype.Zombie) Aggregation.Objects))
                    Comparison.Exactly
                    (Quantity.Literal 0)
                )
          }
      )
      """ {"condition":{"type":"StepBegins","value":[{"type":"Beginning","value":{"type":"Upkeep"}},{"type":"ControllersTurn"}]},"intervening":{"measured":{"type":"Count","value":{"scope":{"type":"InZone","value":[{"type":"Battlefield"},{"type":"EachPlayer"}]},"filter":{"type":"HasSubtype","value":{"type":"Zombie"}},"aggregation":{"type":"Objects"}}},"comparison":{"type":"Exactly"},"threshold":{"type":"Literal","value":0}},"modal":{"modes":[{"effects":[{"type":"DealDamage","value":["you",{"type":"Literal","value":1}]}]}]}} """
  -- CR 603.7: Card.delayedAbilities is a name-keyed map, rendered as a sorted
  -- array of entries so the render is deterministic and the file byte-stable
  -- (Pawl.Codec.TargetSpec.toJsonMap's own comment, and Pawl.Codec.Binding's).
  -- Two real delayed abilities (Full Throttle's "each combat" and Tidal
  -- Wave's "sacrifice it"), inserted here in DESCENDING name order, so a trip
  -- that emitted Map.toList's incidental order rather than Map.toAscList
  -- would fail this case even though both proved correct in isolation.
  Spec.it s "toJsonDelayed/fromJsonDelayed sorts by name (Full Throttle, Tidal Wave)" $
    Common.assertJsonCodec
      s
      (TriggeredAbility.toJsonDelayed cardToJson)
      (TriggeredAbility.fromJsonDelayed cardFromJson)
      ( Map.fromList
          [ ( AbilityName.MkAbilityName (Text.pack "sacrifice it"),
              TriggeredAbility.MkTriggeredAbility
                { TriggeredAbility.condition = TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn,
                  TriggeredAbility.modal =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode (Seq.singleton (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "token")))) Map.empty Optionality.Mandatory))
                      (ModeSelection.ChooseExactly 1),
                  TriggeredAbility.intervening = Nothing
                }
            ),
            ( AbilityName.MkAbilityName (Text.pack "each combat"),
              TriggeredAbility.MkTriggeredAbility
                { TriggeredAbility.condition = TriggerCondition.StepBegins (Phase.Combat CombatStep.BeginningOfCombat) TurnScope.EachTurn,
                  TriggeredAbility.modal =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode (Seq.singleton (Effect.Untap (ObjectRef.EachMatching Filter.AttackedThisTurn))) Map.empty Optionality.Mandatory))
                      (ModeSelection.ChooseExactly 1),
                  TriggeredAbility.intervening = Nothing
                }
            )
          ]
      )
      """ [{"name":"each combat","ability":{"condition":{"type":"StepBegins","value":[{"type":"Combat","value":{"type":"BeginningOfCombat"}},{"type":"EachTurn"}]},"modal":{"modes":[{"effects":[{"type":"Untap","value":{"type":"AttackedThisTurn"}}]}]}}},{"name":"sacrifice it","ability":{"condition":{"type":"StepBegins","value":[{"type":"Ending","value":{"type":"EndStep"}},{"type":"EachTurn"}]},"modal":{"modes":[{"effects":[{"type":"Sacrifice","value":"token"}]}]}}}] """
  -- CR 603.4: no intervening "if" is what a triggered ability that states none
  -- means, so only the two required keys survive -- 'condition' at CR 603.6a's
  -- simplest trigger and 'modal' at the minimal non-modal payload (CR 700.2).
  Spec.it s "an all-default value omits every optional key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      TriggeredAbility.MkTriggeredAbility
        { TriggeredAbility.condition = TriggerCondition.SelfEnters,
          TriggeredAbility.modal =
            Modal.MkModal
              (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory))
              (ModeSelection.ChooseExactly 1),
          TriggeredAbility.intervening = Nothing
        }
      """ {"condition":{"type":"SelfEnters"},"modal":{"modes":[{}]}} """
