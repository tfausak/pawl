-- Covers Pawl.Codec.
module Pawl.CodecSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as J
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.AbilityName as AbilityName
import qualified Pawl.Type.ActivationTiming as ActivationTiming
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Aggregation as Aggregation
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Binding as Binding.Type
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CastingPermission as CastingPermission
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.Comparison as Comparison
import qualified Pawl.Type.Condition as Condition.Type
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.Count as Count.Type
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.CounterPattern as CounterPattern
import qualified Pawl.Type.Counterability as Counterability
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
import qualified Pawl.Type.EventShape as EventShape
-- Aliased Filter.Type, not Filter, for consistency with FilterSpec: the
-- evaluator module Pawl.Filter is not imported here today, but the alias
-- convention is fixed project-wide so a later import never collides.
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.Json as Json
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaProduction as ManaProduction
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.MonarchTarget as MonarchTarget
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.ObjectRef as ObjectRef
import qualified Pawl.Type.Optionality as Optionality
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PhasePattern as PhasePattern
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Type.PlayerEffect as PlayerEffect
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.PlayerScope as PlayerScope
import qualified Pawl.Type.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Regenerability as Regenerability
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Scaling as Scaling
import qualified Pawl.Type.Scope as Scope
import qualified Pawl.Type.SearchDestination as SearchDestination
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.StaticAbility as StaticAbility
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.TokenEntry as TokenEntry
import qualified Pawl.Type.TokenPattern as TokenPattern
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Type.ZoneChangeSubject as ZoneChangeSubject
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

roundTrip :: (Eq a, Show a) => String -> (a -> Json.Value) -> (Json.Value -> Either Text a) -> a -> HU.Assertion
roundTrip label enc dec x = HU.assertEqual label (Right x) (dec (enc x))

-- The first element of an encoded effect's positional payload -- for Destroy,
-- the ObjectRef. Json.Null when the effect is nullary or the payload is not an
-- array, neither of which any caller passes.
payloadHead :: Json.Value -> Json.Value
payloadHead value = case J.tag value of
  Right (_, Just (Json.Array (h : _))) -> h
  _ -> Json.Null

-- The `optionality` key of an encoded Mode, or Nothing when it was omitted (CR
-- 603.5's Mandatory default).
optionalityKey :: Json.Value -> Maybe Json.Value
optionalityKey value = case value of
  Json.Object ps -> J.optField (Text.pack "optionality") ps
  _ -> Nothing

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Pawl.CodecSpec"
    [ Tasty.testGroup
        "leaf enums"
        [ HU.testCase "Color" $
            mapM_ (roundTrip "color" Codec.colorToJson Codec.jsonToColor) [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green],
          HU.testCase "Keyword" $
            roundTrip "kw" Codec.keywordToJson Codec.jsonToKeyword Keyword.Trample,
          HU.testCase "Keyword.Infect" $
            roundTrip "infect" Codec.keywordToJson Codec.jsonToKeyword Keyword.Infect,
          -- CR 702.164a's N rides the constructor, so this is the first keyword
          -- that is not a bare tag.
          HU.testCase "Keyword.Toxic carries its N" $ do
            roundTrip "toxic 1" Codec.keywordToJson Codec.jsonToKeyword (Keyword.Toxic 1)
            roundTrip "toxic 2" Codec.keywordToJson Codec.jsonToKeyword (Keyword.Toxic 2)
            HU.assertBool "toxic 1 and toxic 2 encode differently" (Codec.keywordToJson (Keyword.Toxic 1) /= Codec.keywordToJson (Keyword.Toxic 2)),
          -- CR 702.70a's N rides the constructor the same way. The two payloaded
          -- keywords must not share a tag, or Snake Cult Initiation would decode
          -- as toxic 3.
          HU.testCase "Keyword.Poisonous carries its N" $ do
            roundTrip "poisonous 1" Codec.keywordToJson Codec.jsonToKeyword (Keyword.Poisonous 1)
            roundTrip "poisonous 3" Codec.keywordToJson Codec.jsonToKeyword (Keyword.Poisonous 3)
            HU.assertBool "poisonous 3 is not toxic 3" (Codec.keywordToJson (Keyword.Poisonous 3) /= Codec.keywordToJson (Keyword.Toxic 3)),
          -- CR 702.34a's payload is a whole Cost, not a number -- the first
          -- keyword whose parameter is itself a composite.
          HU.testCase "Keyword.Flashback carries its cost" $ do
            let flashback n =
                  Keyword.Flashback
                    Cost.Type.MkCost
                      { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic n]),
                        Cost.Type.components = []
                      }
            roundTrip "flashback {1}" Codec.keywordToJson Codec.jsonToKeyword (flashback 1)
            roundTrip "flashback {4}" Codec.keywordToJson Codec.jsonToKeyword (flashback 4)
            HU.assertBool "the cost is part of the encoding" (Codec.keywordToJson (flashback 1) /= Codec.keywordToJson (flashback 4)),
          HU.testCase "CastingPermission" $ do
            roundTrip "library" Codec.castingPermissionToJson Codec.jsonToCastingPermission CastingPermission.CastFromLibraryWhileSearching
            roundTrip "graveyard" Codec.castingPermissionToJson Codec.jsonToCastingPermission CastingPermission.CastFromGraveyard,
          HU.testCase "ZoneChangeSubject" $ do
            roundTrip "any" Codec.zoneChangeSubjectToJson Codec.jsonToZoneChangeSubject ZoneChangeSubject.AnyObject
            roundTrip "source" Codec.zoneChangeSubjectToJson Codec.jsonToZoneChangeSubject ZoneChangeSubject.TheSource,
          HU.testCase "PlayerCounterKind" $ do
            roundTrip "energy" Codec.playerCounterKindToJson Codec.jsonToPlayerCounterKind PlayerCounterKind.Energy
            roundTrip "poison" Codec.playerCounterKindToJson Codec.jsonToPlayerCounterKind PlayerCounterKind.Poison,
          HU.testCase "CounterKind" $ do
            HU.assertEqual "plus" (Right CounterKind.PlusOnePlusOne) (Codec.jsonToCounterKind (Codec.counterKindToJson CounterKind.PlusOnePlusOne))
            HU.assertEqual "minus" (Right CounterKind.MinusOneMinusOne) (Codec.jsonToCounterKind (Codec.counterKindToJson CounterKind.MinusOneMinusOne)),
          HU.testCase "Zone" $
            roundTrip "zone" Codec.zoneToJson Codec.jsonToZone Zone.Graveyard,
          HU.testCase "Zone.Command" $
            roundTrip "command" Codec.zoneToJson Codec.jsonToZone Zone.Command,
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
          -- CR 208.1, Ghitu Fire-Eater's "damage equal to its power". Nullary like
          -- ManaValue, and NOT to be confused with the Power newtype round-tripped
          -- further down, which wraps a printed power/toughness box.
          HU.testCase "Quantity.Power is nullary tagged" $
            roundTrip "pwr" Codec.quantityToJson Codec.jsonToQuantity Quantity.Power,
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
          HU.testCase "SetControllerToSource" $
            roundTrip "m4" Codec.modificationToJson Codec.jsonToModification Modification.SetControllerToSource,
          HU.testCase "Affected round-trips (TheseObjects, Matching, and Matching's \"each other\" shape)" $
            mapM_
              (roundTrip "affected" Codec.affectedToJson Codec.jsonToAffected)
              [ Affected.TheseObjects (Set.fromList [ObjectId.MkObjectId 1, ObjectId.MkObjectId 2]),
                Affected.Matching (Filter.Type.HasCardType CardType.Creature),
                -- Opalescence's shape: its own "each other" card text (not a
                -- rule) as Not IsSource.
                Affected.Matching (Filter.Type.And [Filter.Type.HasCardType CardType.Enchantment, Filter.Type.Not (Filter.Type.HasSubtype Subtype.Mountain), Filter.Type.Not Filter.Type.IsSource])
              ]
        ],
      Tasty.testGroup
        "effect"
        [ HU.testCase "DealDamage" $
            roundTrip "e1" Codec.effectToJson Codec.jsonToEffect (Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 3)),
          HU.testCase "ModifyTarget" $
            roundTrip "e2" Codec.effectToJson Codec.jsonToEffect (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (SlotName.MkSlotName (Text.pack "t"))),
          HU.testCase "AddMana" $
            roundTrip "e3" Codec.effectToJson Codec.jsonToEffect (Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green))),
          HU.testCase "AddMana of any color" $
            roundTrip "e3b" Codec.effectToJson Codec.jsonToEffect (Effect.AddMana ManaProduction.AnyColor),
          HU.testCase "ExileAllGraveyards" $
            roundTrip "e4" Codec.effectToJson Codec.jsonToEffect Effect.ExileAllGraveyards,
          HU.testCase "Proliferate" $
            roundTrip "e4b" Codec.effectToJson Codec.jsonToEffect Effect.Proliferate,
          HU.testCase "Destroy carries its CR 701.19c rider both ways" $ do
            roundTrip "e5a" Codec.effectToJson Codec.jsonToEffect (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.Regenerable)
            roundTrip "e5b" Codec.effectToJson Codec.jsonToEffect (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.CantBeRegenerated),
          -- An ObjectRef is untagged and told apart by JSON type, so the two arms
          -- have to be pinned together: a string is the slot, an object is the
          -- filter-selected set. A round trip alone would not catch a decoder that
          -- read every payload as one arm, so the wire form is spelled out.
          HU.testCase "ObjectRef's two arms are told apart by JSON type, not by a tag" $ do
            let slotted = Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Regenerability.Regenerable
                swept = Effect.Destroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)) Regenerability.Regenerable
            roundTrip "e5c" Codec.effectToJson Codec.jsonToEffect swept
            HU.assertEqual
              "a slot is still a bare string, so every Destroy card on disk is unchanged"
              (J.jText (Text.pack "target"))
              (payloadHead (Codec.effectToJson slotted))
            HU.assertEqual
              "and a set is the Filter object"
              (Codec.filterToJson (Filter.Type.HasCardType CardType.Creature))
              (payloadHead (Codec.effectToJson swept)),
          HU.testCase "ExileHandThenDraw" $
            roundTrip "e-powder" Codec.effectToJson Codec.jsonToEffect Effect.ExileHandThenDraw,
          HU.testCase "PlayerSacrifices" $
            roundTrip "e6" Codec.effectToJson Codec.jsonToEffect (Effect.PlayerSacrifices (SlotName.MkSlotName (Text.pack "t")) (Filter.Type.HasCardType CardType.Creature) (Quantity.Literal 1)),
          -- CR 701.3: the destination filter travels in the payload, which is what
          -- distinguishes this arm's wire format from Attach's bare slot.
          HU.testCase "AttachTarget" $
            roundTrip "e-crown" Codec.effectToJson Codec.jsonToEffect (Effect.AttachTarget (SlotName.MkSlotName (Text.pack "target")) (Filter.Type.HasCardType CardType.Creature)),
          HU.testCase "Sacrifice round-trips" $
            roundTrip "e5" Codec.effectToJson Codec.jsonToEffect (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self"))),
          HU.testCase "PutCounters effect round-trips through the codec" $
            let effect = Effect.PutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (SlotName.MkSlotName (Text.pack "creature"))
             in HU.assertEqual "round-trip" (Right effect) (Codec.jsonToEffect (Codec.effectToJson effect)),
          HU.testCase "AffectPlayers round-trips" $
            roundTrip
              "e6"
              Codec.effectToJson
              Codec.jsonToEffect
              (Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells),
          -- Every PlayerRef shape the opcode accepts: the self-scoped one every
          -- card in the pool uses, and the slot read CR 702.70a's "that player"
          -- needs.
          HU.testCase "GainPlayerCounters" $ do
            roundTrip "gpc" Codec.effectToJson Codec.jsonToEffect (Effect.GainPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2))
            roundTrip "gpc slot" Codec.effectToJson Codec.jsonToEffect (Effect.GainPlayerCounters (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) PlayerCounterKind.Poison (Quantity.Literal 3)),
          -- Both of Draw's proven PlayerRef shapes: Divination's controller draw
          -- and Ancestral Recall's targeted one (#272).
          HU.testCase "Draw" $ do
            roundTrip "draw" Codec.effectToJson Codec.jsonToEffect (Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
            roundTrip "draw slot" Codec.effectToJson Codec.jsonToEffect (Effect.Draw (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3)),
          -- Sign in Blood's targeted loss, and the `Relative You` arm that no
          -- card in the pool uses yet -- the codec accepts every PlayerRef either way.
          HU.testCase "LoseLife" $ do
            roundTrip "lose slot" Codec.effectToJson Codec.jsonToEffect (Effect.LoseLife (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2))
            roundTrip "lose you" Codec.effectToJson Codec.jsonToEffect (Effect.LoseLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)),
          -- Soul Warden's "you gain 1 life", plus the slot arm no card uses
          -- yet -- the same coverage LoseLife above gets, on the sibling opcode.
          HU.testCase "GainLife" $ do
            roundTrip "gain you" Codec.effectToJson Codec.jsonToEffect (Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))
            roundTrip "gain slot" Codec.effectToJson Codec.jsonToEffect (Effect.GainLife (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2)),
          HU.testCase "CreateEmblem" $ do
            piker <- Registry.printing registry "Goblin Piker"
            roundTrip "emblem" Codec.effectToJson Codec.jsonToEffect (Effect.CreateEmblem (Printing.card piker)),
          HU.testCase "BecomeMonarch" $
            roundTrip "e" Codec.effectToJson Codec.jsonToEffect (Effect.BecomeMonarch MonarchTarget.TheController),
          -- Both constructors, even though the encoder only ever emits
          -- SorcerySpeed (AnyTime is the absent key on a card). Round-tripping
          -- the pair is what keeps the decoder honest about the form it accepts.
          HU.testCase "ActivationTiming round-trips both ways" $ do
            roundTrip "timing" Codec.activationTimingToJson Codec.jsonToActivationTiming ActivationTiming.AnyTime
            roundTrip "timing" Codec.activationTimingToJson Codec.jsonToActivationTiming ActivationTiming.SorcerySpeed,
          HU.testCase "ExileUntilMonarch" $
            roundTrip "eum" Codec.effectToJson Codec.jsonToEffect (Effect.ExileUntilMonarch (SlotName.MkSlotName (Text.pack "target"))),
          HU.testCase "PlaySubgame round-trips" $
            let e = Effect.PlaySubgame (SlotName.MkSlotName (Text.pack "loser"))
             in HU.assertEqual "PlaySubgame round-trips" (Right e) (Codec.jsonToEffect (Codec.effectToJson e))
        ],
      Tasty.testGroup
        "duration + condition"
        [ HU.testCase "Duration.UntilYourNextTurn round-trips" $
            HU.assertEqual "preserved" (Right Duration.UntilYourNextTurn) (Codec.jsonToDuration (Codec.durationToJson Duration.UntilYourNextTurn)),
          HU.testCase "S.youControlSource round-trips as a Condition" $
            HU.assertEqual "preserved" (Right S.youControlSource) (Codec.jsonToCondition (Codec.conditionToJson S.youControlSource)),
          HU.testCase "Duration.ForAsLongAs round-trips with its condition" $
            let d = Duration.ForAsLongAs S.youControlSource
             in HU.assertEqual "preserved" (Right d) (Codec.jsonToDuration (Codec.durationToJson d))
        ],
      Tasty.testGroup
        "player effects (P7)"
        [ HU.testCase "every PlayerScope round-trips" $
            mapM_
              (roundTrip "scope" Codec.playerScopeToJson Codec.jsonToPlayerScope)
              [PlayerScope.You, PlayerScope.Opponents, PlayerScope.EachPlayer],
          HU.testCase "every PlayerEffect round-trips" $
            mapM_
              (roundTrip "effect" Codec.playerEffectToJson Codec.jsonToPlayerEffect)
              [ PlayerEffect.CantCastSpells,
                PlayerEffect.CantCastMoreThan 1,
                PlayerEffect.IncreaseSpellCost (Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)) 1,
                PlayerEffect.ReduceSpellCost (Filter.Type.HasColor Color.Blue) (ManaCost.MkManaCost [ManaSymbol.Generic 1]),
                -- Edgewalker's: the reduction that names a mana type, which the
                -- Medallion's generic one would not catch a regression in.
                PlayerEffect.ReduceSpellCost
                  (Filter.Type.HasSubtype Subtype.Cleric)
                  (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.White), ManaSymbol.OfType (ManaType.Colored Color.Black)]),
                PlayerEffect.NoMaximumHandSize,
                PlayerEffect.DontLoseUnspentMana
              ],
          -- CR 613.6 made a static ability "one affected set, one or more parts",
          -- so the wire format has an array where it used to have a single
          -- modification -- and an array can be empty where a single value could
          -- not. An ability with no parts is one that does nothing, which no card
          -- means, so it is a decode FAILURE rather than a permanent that quietly
          -- under-performs its own text. NonEmpty is what makes that structural;
          -- this pins that the boundary really says no.
          HU.testCase "a static ability with an empty modifications array is rejected" $ do
            let value =
                  Json.Object
                    [ (Text.pack "affected", Codec.affectedToJson Affected.Attached),
                      (Text.pack "modifications", Json.Array [])
                    ]
            HU.assertBool
              "an empty array does not decode"
              (either (const True) (const False) (Codec.jsonToStaticAbility value))
            roundTrip
              "one part still round-trips"
              Codec.staticAbilityToJson
              Codec.jsonToStaticAbility
              (StaticAbility.MkStaticAbility Affected.Attached (NonEmpty.singleton (Modification.GainKeyword Keyword.Flying)))
            roundTrip
              "and so do several"
              Codec.staticAbilityToJson
              Codec.jsonToStaticAbility
              ( StaticAbility.MkStaticAbility
                  Affected.Attached
                  (Modification.LoseAllAbilities NonEmpty.:| [Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)])
              ),
          HU.testCase "PlayerStaticAbility round-trips" $
            roundTrip
              "ability"
              Codec.playerStaticAbilityToJson
              Codec.jsonToPlayerStaticAbility
              (PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.CantCastMoreThan 1)),
          HU.testCase "a Card carrying player abilities round-trips" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
                c = base {CardT.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.NoMaximumHandSize]}
            roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
          -- Byte-stability: an empty list must not appear in the rendered JSON,
          -- or every committed card file changes. The same posture
          -- colorIndicator and delayedAbilities already take.
          HU.testCase "an empty playerAbilities list is omitted from the JSON" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
            HU.assertEqual "the fixture really has none" [] (CardT.playerAbilities base)
            case J.asObject (Codec.cardToJson base) of
              Left err -> HU.assertFailure (Text.unpack err)
              Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "playerAbilities") (fmap fst pairs)),
          HU.testCase "a Card carrying a CR 103.5b mulligan action round-trips" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
                c = base {CardT.mulliganAction = [Effect.ExileHandThenDraw]}
            roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
          -- Byte-stability: an empty list must not appear in the rendered JSON,
          -- or every committed card file changes. The same posture
          -- playerAbilities and additionalCosts already take.
          HU.testCase "an empty mulliganAction list is omitted from the JSON" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
            HU.assertEqual "the fixture really has none" [] (CardT.mulliganAction base)
            case J.asObject (Codec.cardToJson base) of
              Left err -> HU.assertFailure (Text.unpack err)
              Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "mulliganAction") (fmap fst pairs)),
          HU.testCase "a Card carrying a CR 103.6 opening-hand action round-trips" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
                c = base {CardT.openingHandAction = [Effect.MoveToZone Binding.triggerSource Zone.Battlefield]}
            roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
          HU.testCase "an empty openingHandAction list is omitted from the JSON" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
            HU.assertEqual "the fixture really has none" [] (CardT.openingHandAction base)
            case J.asObject (Codec.cardToJson base) of
              Left err -> HU.assertFailure (Text.unpack err)
              Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "openingHandAction") (fmap fst pairs))
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
                isSource = Filter.Type.IsSource
                ravenousRats = Filter.Type.IsPlayer PlayerRelation.Opponent
                killShot = Filter.Type.IsAttacking
                crownOfTheAges = Filter.Type.And [Filter.Type.HasSubtype Subtype.Aura, Filter.Type.IsAttachedToCreature]
             in mapM_
                  (roundTrip "filter" Codec.filterToJson Codec.jsonToFilter)
                  [doomBlade, terror, reprisal, basicLand, angelicEdict, controlled, bySubtype, isSource, ravenousRats, killShot, crownOfTheAges],
          HU.testCase "PlayerRelation round-trips" $
            mapM_
              (roundTrip "relation" Codec.playerRelationToJson Codec.jsonToPlayerRelation)
              [PlayerRelation.You, PlayerRelation.Opponent]
        ],
      -- Sits beside "filter (P9)": a TargetSpec is Pool + Maybe Filter, so these
      -- exercise the Filter arm above in its embedded position. Covers a bare
      -- pool (Nothing filter, omitted key), a filtered pool, and the Not
      -- IsSource conjunct that carries CR 601.2c's "another" (#163).
      Tasty.testGroup
        "target spec (P9)"
        [ HU.testCase "TargetSpec bare pool round-trips" $
            let spec = TargetSpec.MkTargetSpec Pool.Creatures Nothing
             in HU.assertEqual "preserved" (Right spec) (Codec.jsonToTargetSpec (Codec.targetSpecToJson spec)),
          HU.testCase "TargetSpec filtered pool round-trips" $
            let spec = TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact))
             in HU.assertEqual "preserved" (Right spec) (Codec.jsonToTargetSpec (Codec.targetSpecToJson spec)),
          HU.testCase "TargetSpec \"another\" (Not IsSource) round-trips" $
            let spec = TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.And [Filter.Type.Not (Filter.Type.HasCardType CardType.Land), Filter.Type.Not Filter.Type.IsSource]))
             in HU.assertEqual "preserved" (Right spec) (Codec.jsonToTargetSpec (Codec.targetSpecToJson spec))
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
                    CostComponent.Sacrifice 2 (Filter.Type.HasSubtype Subtype.Mountain)
                  ],
              HU.testCase "PayEnergy" $
                roundTrip "pe" Codec.costComponentToJson Codec.jsonToCostComponent (CostComponent.PayEnergy 2),
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
              HU.testCase "a Card carrying an additional cost round-trips" $ do
                lightningBolt <- Registry.printing registry "Lightning Bolt"
                let base = Printing.card lightningBolt
                    c = base {CardT.additionalCosts = [CostComponent.Sacrifice 1 (Filter.Type.HasCardType CardType.Creature)]}
                roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
              -- Byte-stability: an empty list must not appear in the rendered JSON,
              -- or every committed card file changes. The posture colorIndicator,
              -- delayedAbilities and playerAbilities already take.
              HU.testCase "an empty additionalCosts list is omitted from the JSON" $ do
                lightningBolt <- Registry.printing registry "Lightning Bolt"
                let base = Printing.card lightningBolt
                HU.assertEqual "the fixture really has none" [] (CardT.additionalCosts base)
                case J.asObject (Codec.cardToJson base) of
                  Left err -> HU.assertFailure (Text.unpack err)
                  Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "additionalCosts") (fmap fst pairs)),
              HU.testCase "a Card carrying an alternative cost round-trips" $ do
                lightningBolt <- Registry.printing registry "Lightning Bolt"
                let base = Printing.card lightningBolt
                    alt =
                      Cost.Type.MkCost
                        { Cost.Type.mana = Just (ManaCost.MkManaCost []),
                          Cost.Type.components = [CostComponent.Sacrifice 2 (Filter.Type.HasSubtype Subtype.Mountain)]
                        }
                    c = base {CardT.alternativeCosts = [alt]}
                roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
              HU.testCase "an empty alternativeCosts list is omitted from the JSON" $ do
                lightningBolt <- Registry.printing registry "Lightning Bolt"
                let base = Printing.card lightningBolt
                HU.assertEqual "the fixture really has none" [] (CardT.alternativeCosts base)
                case J.asObject (Codec.cardToJson base) of
                  Left err -> HU.assertFailure (Text.unpack err)
                  Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "alternativeCosts") (fmap fst pairs))
            ],
          HU.testCase "a ZoneChangeR replacement round-trips" $
            let re =
                  ReplacementEffect.ZoneChangeR
                    ZoneChangePattern.MkZoneChangePattern
                      { ZoneChangePattern.whenDestination = Zone.Graveyard,
                        ZoneChangePattern.whichObject = ZoneChangeSubject.AnyObject,
                        ZoneChangePattern.whoseObject = ControllerRelation.Anyones
                      }
                    Zone.Exile
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          -- Leyline of the Void's shape: the relation that distinguishes it from
          -- Rest in Peace has to survive the wire too.
          HU.testCase "a ZoneChangeR carrying Opponents round-trips" $
            let re =
                  ReplacementEffect.ZoneChangeR
                    ZoneChangePattern.MkZoneChangePattern
                      { ZoneChangePattern.whenDestination = Zone.Graveyard,
                        ZoneChangePattern.whichObject = ZoneChangeSubject.AnyObject,
                        ZoneChangePattern.whoseObject = ControllerRelation.Opponents
                      }
                    Zone.Exile
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          HU.testCase "a CounterR replacement round-trips (pattern and scaling are data)" $
            let re =
                  ReplacementEffect.CounterR
                    CounterPattern.MkCounterPattern
                      { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
                        CounterPattern.whose = ControllerRelation.Yours,
                        CounterPattern.onWhat = Filter.Type.HasCardType CardType.Creature
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
                        CounterPattern.onWhat = Filter.Type.And []
                      }
                    (Scaling.Multiply 2)
             in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
          -- CR 614.1b: a skip carries a pattern and no rewrite, so the payload is
          -- the pattern itself rather than the usual two-element array.
          HU.testCase "a PhaseR replacement round-trips" $
            let re = ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = Phase.Beginning BeginningStep.Upkeep}
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
                          (Map.singleton (SlotName.MkSlotName (Text.pack "creature")) (TargetSpec.MkTargetSpec Pool.Creatures Nothing))
                          Optionality.Mandatory
                      ]
                  )
                  (ModeSelection.ChooseExactly 1)
              ),
          -- CR 603.5: an Optional mode is what a printed "may" encodes to, and
          -- the key is emitted only for that value.
          HU.testCase "Optionality round-trips" $ do
            roundTrip "mandatory" Codec.optionalityToJson Codec.jsonToOptionality Optionality.Mandatory
            roundTrip "optional" Codec.optionalityToJson Codec.jsonToOptionality Optionality.Optional,
          HU.testCase "an Optional mode round-trips, and says so in the JSON" $ do
            let m = Mode.MkMode Seq.empty Map.empty Optionality.Optional
            roundTrip "optional mode" Codec.modeToJson Codec.jsonToMode m
            HU.assertEqual
              "the optionality key is present"
              (Just (Codec.optionalityToJson Optionality.Optional))
              (optionalityKey (Codec.modeToJson m)),
          -- The byte-identity guarantee for every card file that prints no
          -- "may": a Mandatory mode emits no key, and a mode with no key decodes
          -- back to Mandatory. The counterability precedent.
          HU.testCase "a Mandatory mode omits the key, and an omitted key decodes to Mandatory" $ do
            let m = Mode.MkMode Seq.empty Map.empty Optionality.Mandatory
            HU.assertEqual "no optionality key" Nothing (optionalityKey (Codec.modeToJson m))
            HU.assertEqual "decodes to Mandatory" (Right m) (Codec.jsonToMode (Codec.modeToJson m)),
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
        [ HU.testCase "P1: jsonToPrinting . printingToJson == Right" $ do
            ps <- S.allPrintings registry
            mapM_ (\p -> HU.assertEqual (show (CardT.name (Printing.card p))) (Right p) (Codec.jsonToPrinting (Codec.printingToJson p))) ps,
          HU.testCase "P2: through text" $ do
            ps <- S.allPrintings registry
            mapM_
              (\p -> HU.assertEqual (show (CardT.name (Printing.card p))) (Right p) (J.parse (J.render (Codec.printingToJson p)) >>= Codec.jsonToPrinting))
              ps,
          HU.testCase "M4e Cancel loads as a single Counter effect targeting a spell" $ do
            cancel <- Registry.printing registry "Cancel"
            let card = Printing.card cancel
            HU.assertEqual
              "effects"
              [Effect.Counter (SlotName.MkSlotName (Text.pack "spell"))]
              (Card.allEffects card)
            HU.assertEqual
              "target spec"
              (Map.singleton (SlotName.MkSlotName (Text.pack "spell")) (TargetSpec.MkTargetSpec Pool.Spells Nothing))
              (Card.allTargetSpecs card),
          -- The key is omitted when Counterable, so this pins BOTH directions of
          -- that default: the one card that prints the clause decodes as
          -- CantBeCountered, and a card that says nothing decodes as Counterable
          -- rather than as whatever a missing key might otherwise become.
          HU.testCase "CR 113.6g counterability decodes from the card, and defaults when the key is absent" $ do
            rendingVolley <- Registry.printing registry "Rending Volley"
            cancel <- Registry.printing registry "Cancel"
            HU.assertEqual
              "Rending Volley says it"
              Counterability.CantBeCountered
              (CardT.counterability (Printing.card rendingVolley))
            HU.assertEqual
              "Cancel does not, and its file has no counterability key"
              Counterability.Counterable
              (CardT.counterability (Printing.card cancel))
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
          HU.testCase "GameEvent.Moved round-trips with its snapshot" $ do
            typhoidRats <- Registry.printing registry "Typhoid Rats"
            let (ratId, gs) = S.addCreature typhoidRats S.alice (Setup.emptyGame S.bothPlayers)
                zc = ZoneChange.MkZoneChange ratId ratId Zone.Battlefield Zone.Graveyard
                snapshot = Projection.project ratId gs
            roundTrip "moved" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.Moved zc snapshot),
          -- The snapshot's keywords are counted per keyword (CR 702.164b), so a
          -- COUNT has to survive the wire and not just a membership: the
          -- array-with-repeats encoding is what carries it. A Set-shaped encoder
          -- would pass every OTHER round-trip test in this group and still halve
          -- the Stalker's total toxic value on replay, which is why the count is
          -- asserted here before the round-trip rather than left to Eq alone.
          HU.testCase "a doubled keyword survives the Moved snapshot round-trip" $ do
            stalker <- Registry.printing registry "Branchblight Stalker"
            let (oid, gs0) = S.addCreature stalker S.alice (Setup.emptyGame S.bothPlayers)
                grant ts = S.withEffectAt oid (Timestamp.MkTimestamp ts) (Modification.GainKeyword (Keyword.Toxic 1))
                snapshot = Projection.project oid (grant 101 (grant 100 gs0))
                zc = ZoneChange.MkZoneChange oid oid Zone.Battlefield Zone.Graveyard
            HU.assertEqual "the fixture really does carry toxic 1 twice" (Just 2) (Map.lookup (Keyword.Toxic 1) (PC.keywords snapshot))
            roundTrip "moved" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.Moved zc snapshot),
          HU.testCase "GameEvent.DamageDealt round-trips" $
            roundTrip
              "damage"
              Codec.gameEventToJson
              Codec.jsonToGameEvent
              -- A NONZERO toxic value, so the CR 702.164b rider is round-tripped
              -- rather than defaulted past.
              (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 1) (Recipient.ToPlayer S.bob) 2 True False 3 DamageKind.Combat)),
          HU.testCase "GameEvent.StepBegan round-trips" $
            roundTrip "step" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice),
          HU.testCase "GameEvent.SpellCast round-trips" $
            roundTrip "ev" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.SpellCast S.alice),
          HU.testCase "MonarchTarget" $ do
            roundTrip "tc" Codec.monarchTargetToJson Codec.jsonToMonarchTarget MonarchTarget.TheController
            roundTrip "cos" Codec.monarchTargetToJson Codec.jsonToMonarchTarget MonarchTarget.ControllerOfSource,
          HU.testCase "GameEvent.BecameMonarch" $
            roundTrip "bm" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.BecameMonarch S.alice),
          -- CR 702.29c's event, carrying the incarnation the cycled card became.
          -- CR 702.29e: the typecycling filter rides the same keyword arm, absent
          -- for plain cycling -- so both spellings have to survive the trip.
          HU.testCase "Keyword.Cycling round-trips with and without a typecycling filter" $ do
            let cost = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
            roundTrip "cyc" Codec.keywordToJson Codec.jsonToKeyword (Keyword.Cycling cost Nothing)
            roundTrip "typecyc" Codec.keywordToJson Codec.jsonToKeyword (Keyword.Cycling cost (Just (Filter.Type.HasCardType CardType.Land))),
          HU.testCase "SearchDestination round-trips" $
            mapM_ (roundTrip "dest" Codec.searchDestinationToJson Codec.jsonToSearchDestination) [SearchDestination.BattlefieldTapped, SearchDestination.RevealThenHand],
          HU.testCase "GameEvent.Cycled round-trips" $
            roundTrip "cyc" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.Cycled (ObjectId.MkObjectId 7)),
          -- CR 701.20a: the reveal's whole payload IS the snapshot, so it is the
          -- one GameEvent whose round-trip failing would silently erase what the
          -- players were shown rather than merely mislabel it. Typhoid Rats for
          -- the reason the Moved case gives -- every snapshot field populated.
          HU.testCase "GameEvent.Revealed round-trips with its snapshot" $ do
            typhoidRats <- Registry.printing registry "Typhoid Rats"
            let (ratId, gs) = S.addLibraryCard typhoidRats S.alice (Setup.emptyGame S.bothPlayers)
            roundTrip "revealed" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.Revealed S.alice (Projection.project ratId gs)),
          HU.testCase "TriggerCondition.SelfCycled round-trips" $
            roundTrip "sc" Codec.triggerConditionToJson Codec.jsonToTriggerCondition TriggerCondition.SelfCycled,
          HU.testCase "TriggerCondition.SelfAttacks round-trips" $
            roundTrip "sa" Codec.triggerConditionToJson Codec.jsonToTriggerCondition TriggerCondition.SelfAttacks,
          HU.testCase "GameEvent.AttackerDeclared round-trips" $
            roundTrip "ad" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.AttackerDeclared (ObjectId.MkObjectId 3)),
          -- Create's TokenEntry is ELIDED when it is the CR 110.5b default, so
          -- the round trip has to hold for all four shapes the encoder emits --
          -- and the two three-element ones (a slot, or an entry) are told apart
          -- by JSON type alone, which is the part that could silently confuse
          -- them.
          HU.testCase "Effect.Create round-trips with and without a TokenEntry and a slot" $ do
            piker <- Registry.printing registry "Goblin Piker"
            let card = Printing.card piker
                attacking = TokenEntry.MkTokenEntry {TokenEntry.tapped = TapState.Tapped, TokenEntry.attacking = True}
                plain = TokenEntry.MkTokenEntry {TokenEntry.tapped = TapState.Untapped, TokenEntry.attacking = False}
                slot = SlotName.MkSlotName (Text.pack "token")
            roundTrip "plain" Codec.effectToJson Codec.jsonToEffect (Effect.Create (Quantity.Literal 2) card plain Nothing)
            roundTrip "plain+slot" Codec.effectToJson Codec.jsonToEffect (Effect.Create (Quantity.Literal 1) card plain (Just slot))
            roundTrip "entry" Codec.effectToJson Codec.jsonToEffect (Effect.Create (Quantity.Literal 2) card attacking Nothing)
            roundTrip "entry+slot" Codec.effectToJson Codec.jsonToEffect (Effect.Create (Quantity.Literal 1) card attacking (Just slot))
            -- The elision itself: a default entry adds nothing to the payload,
            -- which is what keeps every token-making card file written before
            -- this one byte-identical.
            HU.assertEqual
              "a default TokenEntry is not written"
              (Codec.effectToJson (Effect.Create (Quantity.Literal 2) card plain Nothing))
              (J.tagged (Text.pack "Create") (Just (Json.Array [Codec.quantityToJson (Quantity.Literal 2), Codec.cardToJson card]))),
          -- CR 113.6k's condition (Narcomoeba's), the first that names a zone
          -- pair rather than the battlefield.
          HU.testCase "TriggerCondition.SelfPutIntoGraveyardFromLibrary round-trips" $
            roundTrip "spigfl" Codec.triggerConditionToJson Codec.jsonToTriggerCondition TriggerCondition.SelfPutIntoGraveyardFromLibrary,
          -- CR 603.6c's condition (Doomed Traveler's), the other zone pair.
          HU.testCase "TriggerCondition.SelfDies round-trips" $
            roundTrip "dies" Codec.triggerConditionToJson Codec.jsonToTriggerCondition TriggerCondition.SelfDies,
          -- CR 603.6a's "[type]" is a whole Filter, so the nested And/Not that
          -- spells Soul Warden's "another creature" has to survive the trip.
          HU.testCase "TriggerCondition.PermanentEnters round-trips with its Filter" $
            roundTrip
              "pe"
              Codec.triggerConditionToJson
              Codec.jsonToTriggerCondition
              (TriggerCondition.PermanentEnters (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.IsSource])),
          HU.testCase "TurnScope round-trips" $
            mapM_ (roundTrip "scope" Codec.turnScopeToJson Codec.jsonToTurnScope) [TurnScope.EachTurn, TurnScope.ControllersTurn],
          HU.testCase "TriggerCondition.StepBegins round-trips" $
            roundTrip
              "cond"
              Codec.triggerConditionToJson
              Codec.jsonToTriggerCondition
              (TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn),
          HU.testCase "Barbarian Outcast / Sarcomancy shaped Conditions round-trip" $
            mapM_
              (roundTrip "condition" Codec.conditionToJson Codec.jsonToCondition)
              [S.youControlNoSwamps, noZombiesOnBattlefield],
          HU.testCase "TriggerCondition.StateIs round-trips" $
            roundTrip
              "cond"
              Codec.triggerConditionToJson
              Codec.jsonToTriggerCondition
              (TriggerCondition.StateIs S.youControlNoSwamps),
          HU.testCase "CreatureDealtCombatDamageToMonarch" $
            roundTrip "cd" Codec.triggerConditionToJson Codec.jsonToTriggerCondition TriggerCondition.CreatureDealtCombatDamageToMonarch,
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
                      TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1),
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
                      TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1),
                      TriggeredAbility.intervening = Just noZombiesOnBattlefield
                    }
             in roundTrip "ta" Codec.triggeredAbilityToJson Codec.jsonToTriggeredAbility ability
        ],
      Tasty.testGroup
        "count + condition (M5.5 T2)"
        [ HU.testCase "Count round-trips" $
            roundTrip
              "count"
              Codec.countToJson
              Codec.jsonToCount
              ( Count.Type.MkCount
                  (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
                  (Filter.Type.And [Filter.Type.HasSubtype Subtype.Swamp, Filter.Type.ControlledBy PlayerRelation.You])
                  Aggregation.Objects
              ),
          HU.testCase "Count over the event history round-trips" $
            roundTrip
              "history"
              Codec.countToJson
              Codec.jsonToCount
              ( Count.Type.MkCount
                  (Scope.InHistory (EventShape.MovedBetween Zone.Battlefield Zone.Graveyard))
                  (Filter.Type.HasCardType CardType.Creature)
                  Aggregation.DistinctCardTypes
              ),
          HU.testCase "Count scoped to a slot round-trips" $
            roundTrip
              "slot"
              Codec.countToJson
              Codec.jsonToCount
              ( Count.Type.MkCount
                  (Scope.InZone Zone.Hand (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
                  (Filter.Type.And [])
                  Aggregation.Objects
              ),
          HU.testCase "Quantity.Count round-trips (Task 5: shares the Count tag, not double-tagged)" $
            roundTrip
              "qcount"
              Codec.quantityToJson
              Codec.jsonToQuantity
              ( Quantity.Count
                  ( Count.Type.MkCount
                      (Scope.InZone Zone.Graveyard PlayerRef.EachPlayer)
                      (Filter.Type.And [])
                      Aggregation.DistinctCardTypes
                  )
              ),
          -- One with the Machine's aggregation, and the arm that proves the
          -- payload is a whole Quantity rather than a nullary tag: a Greatest
          -- whose per-member quantity is itself a Count round-trips, which is
          -- the recursion Pawl.Type.Quantity's parameter exists to permit.
          HU.testCase "Greatest round-trips, including a nested Count payload" $ do
            roundTrip
              "greatest"
              Codec.countToJson
              Codec.jsonToCount
              ( Count.Type.MkCount
                  (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Artifact, Filter.Type.ControlledBy PlayerRelation.You])
                  (Aggregation.Greatest Quantity.ManaValue)
              )
            roundTrip
              "greatest nested"
              Codec.countToJson
              Codec.jsonToCount
              ( Count.Type.MkCount
                  (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
                  (Filter.Type.And [])
                  ( Aggregation.Greatest
                      ( Quantity.Count
                          ( Count.Type.MkCount
                              (Scope.InZone Zone.Graveyard PlayerRef.EachPlayer)
                              (Filter.Type.And [])
                              Aggregation.DistinctCardTypes
                          )
                      )
                  )
              ),
          HU.testCase "Condition round-trips at every comparison" $
            mapM_
              (roundTrip "condition" Codec.conditionToJson Codec.jsonToCondition)
              [ Condition.Type.MkCondition zeroSwamps Comparison.Exactly (Quantity.Literal 0),
                Condition.Type.MkCondition zeroSwamps Comparison.AtLeast (Quantity.Literal 3),
                Condition.Type.MkCondition zeroSwamps Comparison.AtMost (Quantity.Literal 1)
              ]
        ]
    ]

-- A count with every axis non-default, so a codec that drops one is caught.
zeroSwamps :: Count.Type.Count Quantity.Quantity
zeroSwamps =
  Count.Type.MkCount
    (Scope.InZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.Opponent))
    (Filter.Type.HasSubtype Subtype.Swamp)
    Aggregation.Objects

-- Sarcomancy's migrated intervening "if" (retired
-- StateCondition.NoPermanentsOfSubtype Zombie -- CR 603.4): ANY player's
-- Zombies, unlike S.youControlNoSwamps's ControlledBy conjunct.
noZombiesOnBattlefield :: Condition.Type.Condition
noZombiesOnBattlefield =
  Condition.Type.MkCondition
    ( Count.Type.MkCount
        (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
        (Filter.Type.HasSubtype Subtype.Zombie)
        Aggregation.Objects
    )
    Comparison.Exactly
    (Quantity.Literal 0)
