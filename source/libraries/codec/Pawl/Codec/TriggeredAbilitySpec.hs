module Pawl.Codec.TriggeredAbilitySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
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
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

-- | The `card` parameter is instantiated at 'Text.Text' throughout.
-- 'TriggeredAbility.codec' reach it only through
-- the supplied Modal codec, so any type proves the shape.
cardCodec :: Codec.Codec Text.Text
cardCodec = Common.text

codec :: Codec.Codec (TriggeredAbility.TriggeredAbility Text.Text)
codec = TriggeredAbility.codec cardCodec

toJson :: TriggeredAbility.TriggeredAbility Text.Text -> Value.Value
toJson = Codec.encode codec

fromJson :: Value.Value -> Either Text.Text (TriggeredAbility.TriggeredAbility Text.Text)
fromJson = Codec.decode codec

-- One constructor, so three cases: both states of CR 603.4's `intervening`
-- field, and every field left at its default.
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
                (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Create (Create.MkCreate (Quantity.Literal 1) (Text.pack "Zombie Token") EntryRiders.defaultValue Nothing))))) Map.empty))
                (ModeSelection.ChooseExactly 1),
            TriggeredAbility.intervening = Nothing,
            TriggeredAbility.limit = TriggerLimit.Unlimited
          }
      )
      " {\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{\"clauses\":[{\"effects\":[{\"type\":\"Create\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":1},\"card\":\"Zombie Token\"}}]}]}]}} "
  -- CR 603.4's intervening "if" clause is emitted only when the ability states
  -- one, so this case writes the key and the one above omits it.
  Spec.it s "MkTriggeredAbility, Sarcomancy's upkeep trigger (an intervening if)" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( TriggeredAbility.MkTriggeredAbility
          { TriggeredAbility.condition = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn),
            TriggeredAbility.modal =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you"))) (Quantity.Literal 1) Nothing Nothing))))) Map.empty))
                (ModeSelection.ChooseExactly 1),
            TriggeredAbility.intervening =
              Just
                ( Condition.Compares
                    ( Compares.MkCompares
                        (Quantity.Count (Count.MkCount (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer)) (Filter.HasSubtype Subtype.Zombie) Aggregation.Members))
                        Comparison.Exactly
                        (Quantity.Literal 0)
                    )
                ),
            TriggeredAbility.limit = TriggerLimit.Unlimited
          }
      )
      " {\"condition\":{\"type\":\"StepBegins\",\"value\":{\"phase\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Upkeep\"}},\"scope\":{\"type\":\"ControllersTurn\"}}},\"modal\":{\"modes\":[{\"clauses\":[{\"effects\":[{\"type\":\"DealDamage\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"you\"},\"quantity\":{\"type\":\"Literal\",\"value\":1}}}]}]}]},\"intervening\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Count\",\"value\":{\"scope\":{\"type\":\"InZone\",\"value\":{\"zone\":{\"type\":\"Battlefield\"},\"player\":{\"type\":\"EachPlayer\"}}},\"filter\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Zombie\"}},\"aggregation\":{\"type\":\"Members\"}}},\"comparison\":{\"type\":\"Exactly\"},\"threshold\":{\"type\":\"Literal\",\"value\":0}}}} "
  -- CR 603.7: Face.delayedAbilities is a name-keyed map, rendered as a JSON
  -- OBJECT keyed by the name in ascending order. The two entries are inserted in
  -- DESCENDING name order, so a trip that emitted the map's incidental traversal
  -- order rather than Map.toAscList fails this case.
  Spec.it s "toJsonDelayed/fromJsonDelayed sorts by name (Full Throttle, Tidal Wave)" $
    Common.assertJsonCodec
      s
      (Codec.encode (TriggeredAbility.codecDelayed cardCodec))
      (Codec.decode (TriggeredAbility.codecDelayed cardCodec))
      ( Map.fromList
          [ ( AbilityName.MkAbilityName (Text.pack "sacrifice it"),
              TriggeredAbility.MkTriggeredAbility
                { TriggeredAbility.condition = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn),
                  TriggeredAbility.modal =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "token")))))) Map.empty))
                      (ModeSelection.ChooseExactly 1),
                  TriggeredAbility.intervening = Nothing,
                  TriggeredAbility.limit = TriggerLimit.Unlimited
                }
            ),
            ( AbilityName.MkAbilityName (Text.pack "each combat"),
              TriggeredAbility.MkTriggeredAbility
                { TriggeredAbility.condition = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Combat CombatStep.BeginningOfCombat) TurnScope.EachTurn),
                  TriggeredAbility.modal =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Untap (ObjectRef.EachMatching Filter.AttackedThisTurn))))) Map.empty))
                      (ModeSelection.ChooseExactly 1),
                  TriggeredAbility.intervening = Nothing,
                  TriggeredAbility.limit = TriggerLimit.Unlimited
                }
            )
          ]
      )
      " {\"each combat\":{\"condition\":{\"type\":\"StepBegins\",\"value\":{\"phase\":{\"type\":\"Combat\",\"value\":{\"type\":\"BeginningOfCombat\"}},\"scope\":{\"type\":\"EachTurn\"}}},\"modal\":{\"modes\":[{\"clauses\":[{\"effects\":[{\"type\":\"Untap\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"AttackedThisTurn\"}}}]}]}]}},\"sacrifice it\":{\"condition\":{\"type\":\"StepBegins\",\"value\":{\"phase\":{\"type\":\"Ending\",\"value\":{\"type\":\"EndStep\"}},\"scope\":{\"type\":\"EachTurn\"}}},\"modal\":{\"modes\":[{\"clauses\":[{\"effects\":[{\"type\":\"Sacrifice\",\"value\":\"token\"}]}]}]}}} "
  -- CR 603.4: an ability stating no intervening "if" leaves only the two
  -- required keys.
  Spec.it s "an all-default value omits every optional key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      TriggeredAbility.MkTriggeredAbility
        { TriggeredAbility.condition = TriggerCondition.SelfEnters,
          TriggeredAbility.modal =
            Modal.MkModal
              (Seq.singleton (Mode.MkMode Seq.empty Map.empty))
              (ModeSelection.ChooseExactly 1),
          TriggeredAbility.intervening = Nothing,
          TriggeredAbility.limit = TriggerLimit.Unlimited
        }
      " {\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}} "
