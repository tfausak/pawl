-- Covers Pawl.Codec.
module Pawl.CodecSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Cards as Cards
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as J
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.AbilityName as AbilityName
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.AdditionalCost as AdditionalCost
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.CountSpec as CountSpec
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Decimal as Decimal
import qualified Pawl.Type.DelayedTrigger as DelayedTrigger
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.Json as Json
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.StateCondition as StateCondition
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

roundTrip :: (Eq a, Show a) => String -> (a -> Json.Value) -> (Json.Value -> Either Text a) -> a -> HU.Assertion
roundTrip label enc dec x = HU.assertEqual label (Right x) (dec (enc x))

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Pawl.CodecSpec"
    [ Tasty.testGroup
        "leaf enums"
        [ HU.testCase "Color" $
            mapM_ (roundTrip "color" Codec.colorToJson Codec.jsonToColor) [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green],
          HU.testCase "Keyword" $
            roundTrip "kw" Codec.keywordToJson Codec.jsonToKeyword Keyword.Trample,
          HU.testCase "Zone" $
            roundTrip "zone" Codec.zoneToJson Codec.jsonToZone Zone.Graveyard,
          HU.testCase "unknown tag fails" $
            HU.assertBool "left" (either (const True) (const False) (Codec.jsonToColor (Json.Object [])))
        ],
      Tasty.testGroup
        "newtypes"
        [ HU.testCase "SlotName" $
            roundTrip "slot" Codec.slotNameToJson Codec.jsonToSlotName (SlotName.MkSlotName (Text.pack "x")),
          HU.testCase "ObjectId" $
            roundTrip "oid" Codec.objectIdToJson Codec.jsonToObjectId (ObjectId.MkObjectId 7)
        ],
      Tasty.testGroup
        "mana + quantity (tagged-sum trap)"
        [ HU.testCase "Quantity.Literal is a tagged object with numeric value" $
            HU.assertEqual
              "shape"
              (Json.Object [(Text.pack "type", Json.String (Text.pack "Literal")), (Text.pack "value", Json.Number (Decimal.mkDecimal 3 0))])
              (Codec.quantityToJson (Quantity.Literal 3)),
          HU.testCase "Quantity.ManaValue is nullary tagged" $
            roundTrip "mv" Codec.quantityToJson Codec.jsonToQuantity Quantity.ManaValue,
          HU.testCase "Quantity.Literal round-trips" $
            roundTrip "lit" Codec.quantityToJson Codec.jsonToQuantity (Quantity.Literal 5),
          HU.testCase "ManaCost round-trips" $
            roundTrip
              "cost"
              Codec.manaCostToJson
              Codec.jsonToManaCost
              (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]),
          HU.testCase "ManaSymbol.Variable round-trips" $
            roundTrip "var" Codec.manaSymbolToJson Codec.jsonToManaSymbol ManaSymbol.Variable,
          HU.testCase "Power round-trips" $
            roundTrip "pow" Codec.powerToJson Codec.jsonToPower (Power.MkPower (Quantity.Literal 2))
        ],
      Tasty.testGroup
        "modification + affected"
        [ HU.testCase "GainKeyword" $
            roundTrip "m1" Codec.modificationToJson Codec.jsonToModification (Modification.GainKeyword Keyword.Deathtouch),
          HU.testCase "SetBasePowerToughness" $
            roundTrip "m2" Codec.modificationToJson Codec.jsonToModification (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)),
          HU.testCase "ChangeSubtypeWord" $
            roundTrip "m3" Codec.modificationToJson Codec.jsonToModification (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island),
          HU.testCase "AllCreatures" $
            roundTrip "a1" Codec.affectedToJson Codec.jsonToAffected Affected.AllCreatures,
          HU.testCase "TheseObjects" $
            roundTrip "a2" Codec.affectedToJson Codec.jsonToAffected (Affected.TheseObjects (Set.fromList [ObjectId.MkObjectId 1]))
        ],
      Tasty.testGroup
        "effect"
        [ HU.testCase "DealDamage" $
            roundTrip "e1" Codec.effectToJson Codec.jsonToEffect (Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 3)),
          HU.testCase "ModifyTarget" $
            roundTrip "e2" Codec.effectToJson Codec.jsonToEffect (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (SlotName.MkSlotName (Text.pack "t"))),
          HU.testCase "AddMana" $
            roundTrip "e3" Codec.effectToJson Codec.jsonToEffect (Effect.AddMana (ManaType.Colored Color.Green)),
          HU.testCase "ExileAllGraveyards" $
            roundTrip "e4" Codec.effectToJson Codec.jsonToEffect Effect.ExileAllGraveyards,
          HU.testCase "Sacrifice round-trips" $
            roundTrip "e5" Codec.effectToJson Codec.jsonToEffect (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self"))),
          HU.testCase "PutCounters effect round-trips through the codec" $
            let effect = Effect.PutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (SlotName.MkSlotName (Text.pack "creature"))
             in HU.assertEqual "round-trip" (Right effect) (Codec.jsonToEffect (Codec.effectToJson effect)),
          HU.testCase "both CounterKinds round-trip" $ do
            HU.assertEqual "plus" (Right CounterKind.PlusOnePlusOne) (Codec.jsonToCounterKind (Codec.counterKindToJson CounterKind.PlusOnePlusOne))
            HU.assertEqual "minus" (Right CounterKind.MinusOneMinusOne) (Codec.jsonToCounterKind (Codec.counterKindToJson CounterKind.MinusOneMinusOne))
        ],
      Tasty.testGroup
        "records"
        [ HU.testCase "TypeLine" $
            roundTrip
              "tl"
              Codec.typeLineToJson
              Codec.jsonToTypeLine
              (TypeLine.MkTypeLine (Set.singleton Supertype.Basic) (Set.singleton CardType.Land) (Set.singleton Subtype.Mountain)),
          HU.testCase "ActivatedAbility" $
            roundTrip
              "aa"
              Codec.activatedAbilityToJson
              Codec.jsonToActivatedAbility
              ( ActivatedAbility.MkActivatedAbility
                  (AbilityCost.MkAbilityCost Nothing [AdditionalCost.TapSelf])
                  ( Modal.MkModal
                      ( Seq.singleton
                          ( Mode.MkMode
                              (Seq.fromList [Effect.AddMana (ManaType.Colored Color.Green)])
                              (Map.fromList [(SlotName.MkSlotName (Text.pack "t"), TargetSpec.CreatureTarget)])
                          )
                      )
                      (ModeSelection.ChooseExactly 1)
                  )
              ),
          HU.testCase "ReplacementEffect" $
            roundTrip
              "re"
              Codec.replacementEffectToJson
              Codec.jsonToReplacementEffect
              (ReplacementEffect.RedirectZoneChange Zone.Graveyard Zone.Exile)
        ],
      Tasty.testGroup
        "modal"
        [ HU.testCase "ModeIndex round-trips" $
            roundTrip "mi" Codec.modeIndexToJson Codec.jsonToModeIndex (ModeIndex.MkModeIndex 2),
          HU.testCase "ModeSelection round-trips" $
            roundTrip "ms" Codec.modeSelectionToJson Codec.jsonToModeSelection (ModeSelection.ChooseExactly 1),
          HU.testCase "Modal round-trips" $
            roundTrip
              "modal"
              Codec.modalToJson
              Codec.jsonToModal
              ( Modal.MkModal
                  ( Seq.fromList
                      [ Mode.MkMode
                          (Seq.fromList [Effect.DealDamage (SlotName.MkSlotName (Text.pack "creature")) (Quantity.Literal 1)])
                          (Map.singleton (SlotName.MkSlotName (Text.pack "creature")) TargetSpec.CreatureTarget)
                      ]
                  )
                  (ModeSelection.ChooseExactly 1)
              ),
          HU.testCase "empty modal is a decode error" $
            HU.assertBool
              "left"
              ( either
                  (const True)
                  (const False)
                  (Codec.jsonToModal (Json.Object [(Text.pack "modes", Json.Array []), (Text.pack "selection", Codec.modeSelectionToJson (ModeSelection.ChooseExactly 1))]))
              )
        ],
      Tasty.testGroup
        "honesty round-trip over allPrintings"
        [ HU.testCase "P1: jsonToPrinting . printingToJson == Right" $
            mapM_ (\p -> HU.assertEqual (show (CardT.name (Printing.card p))) (Right p) (Codec.jsonToPrinting (Codec.printingToJson p))) (Cards.allPrintings cards),
          HU.testCase "P2: through text" $
            mapM_
              (\p -> HU.assertEqual (show (CardT.name (Printing.card p))) (Right p) (J.parse (J.render (Codec.printingToJson p)) >>= Codec.jsonToPrinting))
              (Cards.allPrintings cards),
          HU.testCase "M4e Cancel loads as a single Counter effect targeting a spell" $
            let card = Printing.card (Cards.cancelPrinting cards)
             in do
                  HU.assertEqual
                    "effects"
                    [Effect.Counter (SlotName.MkSlotName (Text.pack "spell"))]
                    (Card.allEffects card)
                  HU.assertEqual
                    "target spec"
                    (Map.singleton (SlotName.MkSlotName (Text.pack "spell")) TargetSpec.SpellTarget)
                    (Card.allTargetSpecs card),
          HU.testCase "copyOnEnter True round-trips through the card codec" $
            let base = Printing.card (Cards.pikerPrinting cards)
                cloney = base {CardT.copyOnEnter = True}
             in HU.assertEqual "copyOnEnter preserved" (Right cloney) (Codec.jsonToCard (Codec.cardToJson cloney))
        ],
      Tasty.testGroup
        "P4 runtime types"
        [ HU.testCase "Phase round-trips" $
            mapM_
              (roundTrip "phase" Codec.phaseToJson Codec.jsonToPhase)
              [ Phase.Beginning BeginningStep.Upkeep,
                Phase.PrecombatMain,
                Phase.Combat CombatStep.DeclareBlockers,
                Phase.PostcombatMain,
                Phase.Ending EndingStep.EndStep
              ],
          -- A real permanent, not a projection of a nonexistent object: Typhoid
          -- Rats (1/1 deathtouch) populates keywords, colors, power, toughness,
          -- cardTypes and subtypes all at once, so a swapped field or a wrong
          -- JSON key would fail this round-trip instead of surviving it on an
          -- all-Nothing/all-empty value.
          HU.testCase "GameEvent.Moved round-trips with its snapshot" $
            let (ratId, gs) = S.addCreature (Cards.typhoidRatsPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                zc = ZoneChange.MkZoneChange ratId Zone.Battlefield Zone.Graveyard
                snapshot = Projection.project ratId gs
             in roundTrip "moved" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.Moved zc snapshot),
          HU.testCase "GameEvent.DamageDealt round-trips" $
            roundTrip
              "damage"
              Codec.gameEventToJson
              Codec.jsonToGameEvent
              (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 1) (Recipient.ToPlayer S.bob) 2 True DamageKind.Combat)),
          HU.testCase "GameEvent.StepBegan round-trips" $
            roundTrip "step" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice),
          HU.testCase "TurnScope round-trips" $
            mapM_ (roundTrip "scope" Codec.turnScopeToJson Codec.jsonToTurnScope) [TurnScope.EachTurn, TurnScope.ControllersTurn],
          HU.testCase "TriggerCondition.StepBegins round-trips" $
            roundTrip
              "cond"
              Codec.triggerConditionToJson
              Codec.jsonToTriggerCondition
              (TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn),
          HU.testCase "StateCondition round-trips" $
            mapM_
              (roundTrip "state" Codec.stateConditionToJson Codec.jsonToStateCondition)
              [StateCondition.YouControlNo Subtype.Swamp, StateCondition.NoPermanentsOfSubtype Subtype.Zombie],
          HU.testCase "TriggerCondition.StateIs round-trips" $
            roundTrip
              "cond"
              Codec.triggerConditionToJson
              Codec.jsonToTriggerCondition
              (TriggerCondition.StateIs (StateCondition.YouControlNo Subtype.Swamp)),
          HU.testCase "CountSpec.CreaturesDiedThisTurn round-trips" $
            roundTrip "count" Codec.countSpecToJson Codec.jsonToCountSpec CountSpec.CreaturesDiedThisTurn,
          HU.testCase "AbilityName round-trips" $
            roundTrip "name" Codec.abilityNameToJson Codec.jsonToAbilityName (AbilityName.MkAbilityName (Text.pack "sacrifice it")),
          HU.testCase "ArmDelayedTrigger round-trips" $
            roundTrip "arm" Codec.effectToJson Codec.jsonToEffect (Effect.ArmDelayedTrigger (AbilityName.MkAbilityName (Text.pack "sacrifice it"))),
          HU.testCase "DelayedTrigger round-trips with its captured bindings" $
            let ability =
                  TriggeredAbility.MkTriggeredAbility
                    { TriggeredAbility.condition = TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn,
                      TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)
                    }
                entry =
                  DelayedTrigger.MkDelayedTrigger
                    { DelayedTrigger.ability = ability,
                      DelayedTrigger.source = ObjectId.MkObjectId 4,
                      DelayedTrigger.controller = S.alice,
                      DelayedTrigger.bindings = Map.singleton (SlotName.MkSlotName (Text.pack "token")) (Binding.toObject (ObjectId.MkObjectId 9))
                    }
             in roundTrip "delayed" Codec.delayedTriggerToJson Codec.jsonToDelayedTrigger entry
        ]
    ]
