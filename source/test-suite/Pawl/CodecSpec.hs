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
import qualified Pawl.Type.AbilityName as AbilityName
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Binding as Binding.Type
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.CountSpec as CountSpec
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.CounterPattern as CounterPattern
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.DamagePattern as DamagePattern
import qualified Pawl.Type.DamageRewrite as DamageRewrite
import qualified Pawl.Type.Decimal as Decimal
import qualified Pawl.Type.DelayedTrigger as DelayedTrigger
import qualified Pawl.Type.DestructionRewrite as DestructionRewrite
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.EntryOption as EntryOption
import qualified Pawl.Type.EntryRewrite as EntryRewrite
-- Aliased Filter.Type, not Filter, for consistency with FilterSpec: the
-- evaluator module Pawl.Filter is not imported here today, but the alias
-- convention is fixed project-wide so a later import never collides.
import qualified Pawl.Type.Filter as Filter.Type
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
import qualified Pawl.Type.PermanentCriterion as PermanentCriterion
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PlayerEffect as PlayerEffect
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.PlayerScope as PlayerScope
import qualified Pawl.Type.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Scaling as Scaling
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.SpellCriterion as SpellCriterion
import qualified Pawl.Type.StateCondition as StateCondition
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.TokenPattern as TokenPattern
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern
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
          HU.testCase "Duration.UntilYourNextTurn round-trips" $
            HU.assertEqual "preserved" (Right Duration.UntilYourNextTurn) (Codec.jsonToDuration (Codec.durationToJson Duration.UntilYourNextTurn)),
          HU.testCase "StateCondition.YouControlSource round-trips" $
            HU.assertEqual "preserved" (Right StateCondition.YouControlSource) (Codec.jsonToStateCondition (Codec.stateConditionToJson StateCondition.YouControlSource)),
          HU.testCase "Duration.ForAsLongAs round-trips with its condition" $
            let d = Duration.ForAsLongAs StateCondition.YouControlSource
             in HU.assertEqual "preserved" (Right d) (Codec.jsonToDuration (Codec.durationToJson d)),
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
            HU.assertEqual "minus" (Right CounterKind.MinusOneMinusOne) (Codec.jsonToCounterKind (Codec.counterKindToJson CounterKind.MinusOneMinusOne)),
          HU.testCase "AffectPlayers round-trips" $
            roundTrip
              "e6"
              Codec.effectToJson
              Codec.jsonToEffect
              (Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells)
        ],
      Tasty.testGroup
        "player effects (P7)"
        [ HU.testCase "every PlayerScope round-trips" $
            mapM_
              (roundTrip "scope" Codec.playerScopeToJson Codec.jsonToPlayerScope)
              [PlayerScope.You, PlayerScope.Opponents, PlayerScope.EachPlayer],
          HU.testCase "every SpellCriterion round-trips" $
            mapM_
              (roundTrip "criterion" Codec.spellCriterionToJson Codec.jsonToSpellCriterion)
              [SpellCriterion.NoncreatureSpell, SpellCriterion.SpellOfColor Color.Blue],
          HU.testCase "every PlayerEffect round-trips" $
            mapM_
              (roundTrip "effect" Codec.playerEffectToJson Codec.jsonToPlayerEffect)
              [ PlayerEffect.CantCastSpells,
                PlayerEffect.CantCastMoreThan 1,
                PlayerEffect.IncreaseSpellCost SpellCriterion.NoncreatureSpell 1,
                PlayerEffect.ReduceSpellCost (SpellCriterion.SpellOfColor Color.Blue) 1,
                PlayerEffect.NoMaximumHandSize
              ],
          HU.testCase "PlayerStaticAbility round-trips" $
            roundTrip
              "ability"
              Codec.playerStaticAbilityToJson
              Codec.jsonToPlayerStaticAbility
              (PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.CantCastMoreThan 1)),
          HU.testCase "a Card carrying player abilities round-trips" $
            let base = Printing.card (Cards.bloodMoonPrinting cards)
                c = base {CardT.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.NoMaximumHandSize]}
             in roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
          -- Byte-stability: an empty list must not appear in the rendered JSON,
          -- or every committed card file changes. The same posture
          -- colorIndicator and delayedAbilities already take.
          HU.testCase "an empty playerAbilities list is omitted from the JSON" $
            let base = Printing.card (Cards.bloodMoonPrinting cards)
             in do
                  HU.assertEqual "the fixture really has none" [] (CardT.playerAbilities base)
                  case J.asObject (Codec.cardToJson base) of
                    Left err -> HU.assertFailure (Text.unpack err)
                    Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "playerAbilities") (map fst pairs))
        ],
      Tasty.testGroup
        "filter (P9)"
        [ HU.testCase "Filter round-trips including nested And/Or/Not" $
            let doomBlade = Filter.Type.Not (Filter.Type.HasColor Color.Black)
                terror = Filter.Type.And [Filter.Type.Not (Filter.Type.HasColor Color.Black), Filter.Type.Not (Filter.Type.HasCardType CardType.Artifact)]
                reprisal = Filter.Type.PowerAtLeast 4
                basicLand = Filter.Type.And [Filter.Type.HasCardType CardType.Land, Filter.Type.HasSupertype Supertype.Basic]
                angelicEdict = Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasCardType CardType.Enchantment]
                controlled = Filter.Type.ControlledBy PlayerRelation.Opponent
                bySubtype = Filter.Type.HasSubtype Subtype.Wall
             in mapM_
                  (roundTrip "filter" Codec.filterToJson Codec.jsonToFilter)
                  [doomBlade, terror, reprisal, basicLand, angelicEdict, controlled, bySubtype],
          HU.testCase "PlayerRelation round-trips" $
            mapM_
              (roundTrip "relation" Codec.playerRelationToJson Codec.jsonToPlayerRelation)
              [PlayerRelation.You, PlayerRelation.Opponent]
        ],
      Tasty.testGroup
        "records"
        [ HU.testCase "TypeLine" $
            roundTrip
              "tl"
              Codec.typeLineToJson
              Codec.jsonToTypeLine
              (TypeLine.MkTypeLine (Set.singleton Supertype.Basic) (Set.singleton CardType.Land) (Set.singleton Subtype.Mountain)),
          Tasty.testGroup
            "cost (P8)"
            [ HU.testCase "every CostComponent round-trips" $
                mapM_
                  (roundTrip "component" Codec.costComponentToJson Codec.jsonToCostComponent)
                  [ CostComponent.TapThis,
                    CostComponent.SacrificeThis,
                    CostComponent.PayLife 2,
                    CostComponent.Sacrifice 2 (PermanentCriterion.PermanentOfSubtype Subtype.Mountain)
                  ],
              HU.testCase "a Cost with a mana part and components round-trips" $
                roundTrip
                  "cost"
                  Codec.costToJson
                  Codec.jsonToCost
                  Cost.Type.MkCost
                    { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                      Cost.Type.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
                    },
              -- CR 118.5a: {0} is a real, payable cost, and ManaCost's empty list IS
              -- {0}. This is the shape every migrated ability now carries.
              HU.testCase "a {0} cost round-trips as Just an empty ManaCost" $
                roundTrip
                  "zero"
                  Codec.costToJson
                  Codec.jsonToCost
                  Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
              -- CR 118.6: an ABSENT mana field is an UNPAYABLE cost, not {0}. This is
              -- the footgun the corpus migration exists to avoid, pinned so a future
              -- card file cannot lose its mana field unnoticed.
              HU.testCase "an omitted mana field decodes to Nothing, not to {0}" $
                let value = Json.Object [(Text.pack "components", Json.Array [])]
                 in HU.assertEqual
                      "unpayable"
                      (Right Cost.Type.MkCost {Cost.Type.mana = Nothing, Cost.Type.components = []})
                      (Codec.jsonToCost value),
              HU.testCase "every PermanentCriterion round-trips" $
                mapM_
                  (roundTrip "criterion" Codec.permanentCriterionToJson Codec.jsonToPermanentCriterion)
                  [PermanentCriterion.AnyPermanent, PermanentCriterion.CreaturePermanent, PermanentCriterion.PermanentOfSubtype Subtype.Mountain],
              HU.testCase "a Card carrying an additional cost round-trips" $
                let base = Printing.card (Cards.lightningBoltPrinting cards)
                    c = base {CardT.additionalCosts = [CostComponent.Sacrifice 1 PermanentCriterion.CreaturePermanent]}
                 in roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
              -- Byte-stability: an empty list must not appear in the rendered JSON,
              -- or every committed card file changes. The posture colorIndicator,
              -- delayedAbilities and playerAbilities already take.
              HU.testCase "an empty additionalCosts list is omitted from the JSON" $
                let base = Printing.card (Cards.lightningBoltPrinting cards)
                 in do
                      HU.assertEqual "the fixture really has none" [] (CardT.additionalCosts base)
                      case J.asObject (Codec.cardToJson base) of
                        Left err -> HU.assertFailure (Text.unpack err)
                        Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "additionalCosts") (map fst pairs)),
              HU.testCase "a Card carrying an alternative cost round-trips" $
                let base = Printing.card (Cards.lightningBoltPrinting cards)
                    alt =
                      Cost.Type.MkCost
                        { Cost.Type.mana = Just (ManaCost.MkManaCost []),
                          Cost.Type.components = [CostComponent.Sacrifice 2 (PermanentCriterion.PermanentOfSubtype Subtype.Mountain)]
                        }
                    c = base {CardT.alternativeCosts = [alt]}
                 in roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
              HU.testCase "an empty alternativeCosts list is omitted from the JSON" $
                let base = Printing.card (Cards.lightningBoltPrinting cards)
                 in do
                      HU.assertEqual "the fixture really has none" [] (CardT.alternativeCosts base)
                      case J.asObject (Codec.cardToJson base) of
                        Left err -> HU.assertFailure (Text.unpack err)
                        Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "alternativeCosts") (map fst pairs))
            ],
          HU.testCase "a ZoneChangeR replacement round-trips" $
            let re =
                  ReplacementEffect.ZoneChangeR
                    ZoneChangePattern.MkZoneChangePattern
                      { ZoneChangePattern.whenDestination = Zone.Graveyard,
                        ZoneChangePattern.whoseObject = ControllerRelation.Anyones
                      }
                    Zone.Exile
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          HU.testCase "a CounterR replacement round-trips (pattern and scaling are data)" $
            let re =
                  ReplacementEffect.CounterR
                    CounterPattern.MkCounterPattern
                      { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
                        CounterPattern.whose = ControllerRelation.Yours,
                        CounterPattern.onWhat = PermanentCriterion.CreaturePermanent
                      }
                    (Scaling.AddMore 1)
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          HU.testCase "an EntryR ChoiceOf replacement round-trips (P/T and keywords)" $
            let re =
                  ReplacementEffect.EntryR
                    ( EntryRewrite.ChoiceOf
                        [ EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty},
                          EntryOption.MkEntryOption {EntryOption.power = 1, EntryOption.toughness = 6, EntryOption.keywords = Set.singleton Keyword.Defender}
                        ]
                    )
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          HU.testCase "an EntryR AsCopy replacement round-trips" $
            let re = ReplacementEffect.EntryR EntryRewrite.AsCopy
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          HU.testCase "a DamageR replacement round-trips (pattern and rewrite are data)" $
            let re =
                  ReplacementEffect.DamageR
                    DamagePattern.MkDamagePattern {DamagePattern.whichKind = Just DamageKind.Combat}
                    DamageRewrite.PreventAll
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          HU.testCase "a DestructionR replacement round-trips" $
            let re = ReplacementEffect.DestructionR DestructionRewrite.Regenerate
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          HU.testCase "a TokenR replacement round-trips (pattern and scaling are data)" $
            let re =
                  ReplacementEffect.TokenR
                    TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours}
                    (Scaling.Multiply 2)
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          HU.testCase "a CounterR replacement round-trips with whichKind = Nothing (explicit JSON null)" $
            let re =
                  ReplacementEffect.CounterR
                    CounterPattern.MkCounterPattern
                      { CounterPattern.whichKind = Nothing,
                        CounterPattern.whose = ControllerRelation.Anyones,
                        CounterPattern.onWhat = PermanentCriterion.AnyPermanent
                      }
                    (Scaling.Multiply 2)
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re))
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
              ),
          HU.testCase "TargetSpec.ArtifactTarget round-trips" $
            HU.assertEqual "preserved" (Right TargetSpec.ArtifactTarget) (Codec.jsonToTargetSpec (Codec.targetSpecToJson TargetSpec.ArtifactTarget)),
          HU.testCase "TargetSpec.OpponentCreatureTarget round-trips" $
            HU.assertEqual "preserved" (Right TargetSpec.OpponentCreatureTarget) (Codec.jsonToTargetSpec (Codec.targetSpecToJson TargetSpec.OpponentCreatureTarget))
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
                    (Card.allTargetSpecs card)
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
          HU.testCase "GameEvent.SpellCast round-trips" $
            roundTrip "ev" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.SpellCast S.alice),
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
          -- M-5 (fix pass 1): the "DelayedTrigger round-trips" test below exercises
          -- only a Binding's `target` field via Binding.toObject. The codec is
          -- meant to be total over every Binding field -- subtypes, amount, modes,
          -- and copy too -- so round-trip a Binding with all five populated at
          -- once, exercising jsonToSubtypePair along the way. No real slot ever
          -- carries all five together (copy lives only under the dedicated
          -- copySource slot in practice); this is a codec totality check, not a
          -- claim about a reachable game state.
          HU.testCase "a Binding with every field populated round-trips" $
            let binding =
                  Binding.Type.MkBinding
                    { Binding.Type.target = Just (Recipient.ToPlayer S.alice),
                      Binding.Type.subtypes = Just (Subtype.Mountain, Subtype.Island),
                      Binding.Type.amount = Just 3,
                      Binding.Type.modes = Just (Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2]),
                      Binding.Type.copy = Just S.emptyCharacteristics
                    }
             in roundTrip "binding" Codec.bindingToJson Codec.jsonToBinding binding,
          HU.testCase "DelayedTrigger round-trips with its captured bindings" $
            let ability =
                  TriggeredAbility.MkTriggeredAbility
                    { TriggeredAbility.condition = TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn,
                      TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1),
                      TriggeredAbility.intervening = Nothing
                    }
                entry =
                  DelayedTrigger.MkDelayedTrigger
                    { DelayedTrigger.ability = ability,
                      DelayedTrigger.source = ObjectId.MkObjectId 4,
                      DelayedTrigger.controller = S.alice,
                      DelayedTrigger.bindings = Map.singleton (SlotName.MkSlotName (Text.pack "token")) (Binding.toObject (ObjectId.MkObjectId 9))
                    }
             in roundTrip "delayed" Codec.delayedTriggerToJson Codec.jsonToDelayedTrigger entry,
          HU.testCase "a TriggeredAbility with an intervening if round-trips" $
            let ability =
                  TriggeredAbility.MkTriggeredAbility
                    { TriggeredAbility.condition = TriggerCondition.SelfEnters,
                      TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1),
                      TriggeredAbility.intervening = Just (StateCondition.NoPermanentsOfSubtype Subtype.Zombie)
                    }
             in roundTrip "ta" Codec.triggeredAbilityToJson Codec.jsonToTriggeredAbility ability
        ]
    ]
