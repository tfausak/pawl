-- Covers Pawl.Codec.
module Pawl.CodecSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.AbilityName (abilityNameToJson, jsonToAbilityName)
import Pawl.Codec.ActivationTiming (activationTimingToJson, jsonToActivationTiming)
import Pawl.Codec.Affected (affectedToJson, jsonToAffected)
import Pawl.Codec.Binding (bindingToJson, jsonToBinding)
import Pawl.Codec.Card (cardToJson, jsonToCard)
import Pawl.Codec.CastingPermission (castingPermissionToJson, jsonToCastingPermission)
import Pawl.Codec.CastingRestriction (castingRestrictionToJson, jsonToCastingRestriction)
import Pawl.Codec.Color (colorToJson, jsonToColor)
import Pawl.Codec.Condition (conditionToJson, jsonToCondition)
import Pawl.Codec.Cost (costToJson, jsonToCost)
import Pawl.Codec.CostComponent (costComponentToJson, jsonToCostComponent)
import Pawl.Codec.Count (countToJson, jsonToCount)
import Pawl.Codec.CounterKind (counterKindToJson, jsonToCounterKind)
import Pawl.Codec.DelayedTrigger (delayedTriggerToJson, jsonToDelayedTrigger)
import Pawl.Codec.DiscardCause (discardCauseToJson, jsonToDiscardCause)
import Pawl.Codec.Duration (durationToJson, jsonToDuration)
import Pawl.Codec.Effect (effectToJson, jsonToEffect)
import Pawl.Codec.EntryRewrite (entryRewriteToJson, jsonToEntryRewrite)
import Pawl.Codec.Filter (filterToJson, jsonToFilter)
import Pawl.Codec.GameEvent (gameEventToJson, jsonToGameEvent)
import qualified Pawl.Codec.Json as J
import Pawl.Codec.Keyword (jsonToKeyword, keywordToJson)
import Pawl.Codec.Loyalty (jsonToLoyalty, loyaltyToJson)
import Pawl.Codec.ManaCost (jsonToManaCost, manaCostToJson)
import Pawl.Codec.ManaSymbol (jsonToManaSymbol, manaSymbolToJson)
import Pawl.Codec.Modal (jsonToModal, modalToJson)
import Pawl.Codec.Mode (jsonToMode, modeToJson)
import Pawl.Codec.ModeIndex (jsonToModeIndex, modeIndexToJson)
import Pawl.Codec.ModeSelection (jsonToModeSelection, modeSelectionToJson)
import Pawl.Codec.Modification (jsonToModification, modificationToJson)
import Pawl.Codec.MonarchTarget (jsonToMonarchTarget, monarchTargetToJson)
import Pawl.Codec.ObjectId (jsonToObjectId, objectIdToJson)
import Pawl.Codec.Optionality (jsonToOptionality, optionalityToJson)
import Pawl.Codec.Phase (jsonToPhase, phaseToJson)
import Pawl.Codec.PlayerCounterKind (jsonToPlayerCounterKind, playerCounterKindToJson)
import Pawl.Codec.PlayerEffect (jsonToPlayerEffect, playerEffectToJson)
import Pawl.Codec.PlayerRelation (jsonToPlayerRelation, playerRelationToJson)
import Pawl.Codec.PlayerScope (jsonToPlayerScope, playerScopeToJson)
import Pawl.Codec.PlayerStaticAbility (jsonToPlayerStaticAbility, playerStaticAbilityToJson)
import Pawl.Codec.Power (jsonToPower, powerToJson)
import Pawl.Codec.Printing (jsonToPrinting, printingToJson)
import Pawl.Codec.Quantity (jsonToQuantity, quantityToJson)
import Pawl.Codec.ReplacementEffect (jsonToReplacementEffect, replacementEffectToJson)
import Pawl.Codec.SearchDestination (jsonToSearchDestination, searchDestinationToJson)
import Pawl.Codec.SlotName (jsonToSlotName, slotNameToJson)
import Pawl.Codec.StaticAbility (jsonToStaticAbility, staticAbilityToJson)
import Pawl.Codec.Supertype (jsonToSupertype, supertypeToJson)
import Pawl.Codec.TargetSpec (jsonToTargetSpec, targetSpecToJson)
import Pawl.Codec.TriggerCondition (jsonToTriggerCondition, triggerConditionToJson)
import Pawl.Codec.TriggeredAbility (jsonToTriggeredAbility, triggeredAbilityToJson)
import Pawl.Codec.TurnScope (jsonToTurnScope, turnScopeToJson)
import Pawl.Codec.TypeLine (jsonToTypeLine, typeLineToJson)
import Pawl.Codec.Zone (jsonToZone, zoneToJson)
import Pawl.Codec.ZoneChangeSubject (jsonToZoneChangeSubject, zoneChangeSubjectToJson)
import qualified Pawl.Decimal as Decimal
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
-- Aliased Filter.Type, not Filter, for consistency with FilterSpec: the
-- evaluator module Pawl.Engine.Filter is not imported here today, but the alias
-- convention is fixed project-wide so a later import never collides.

import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Binding as Binding.Type
import qualified Pawl.Types.Card as CardT
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.CastingRestriction as CastingRestriction
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TokenEntry as TokenEntry
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

roundTrip :: (Applicative m, Eq a, Show a) => Spec.Spec m n -> String -> (a -> Value.Value) -> (Value.Value -> Either Text a) -> a -> m ()
roundTrip s label enc dec x = Spec.assertEqWith s label (dec (enc x)) (Right x)

-- The first element of an encoded effect's positional payload -- for Destroy,
-- the ObjectRef. JSON null when the effect is nullary or the payload is not an
-- array, neither of which any caller passes.
payloadHead :: Value.Value -> Value.Value
payloadHead value = case J.tag value of
  Right (_, Just (Value.Array (Array.MkArray (h : _)))) -> h
  _ -> J.jNull

-- How many elements a tagged effect's array payload holds -- what an ELIDED
-- optional trailing element is asserted by. -1 for a payload that is not an
-- array, so a wrong shape fails loudly rather than matching a real length.
payloadLength :: Value.Value -> Int
payloadLength value = case J.tag value of
  Right (_, Just (Value.Array (Array.MkArray xs))) -> length xs
  _ -> -1

-- The `optionality` key of an encoded Mode, or Nothing when it was omitted (CR
-- 603.5's Mandatory default).
optionalityKey :: Value.Value -> Maybe Value.Value
optionalityKey value = case J.asObject value of
  Right ps -> J.optField (Text.pack "optionality") ps
  Left _ -> Nothing

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Pawl.Codec" $ do
  Spec.describe s "leaf enums" $ do
    Spec.it s "Color" $
      mapM_ (roundTrip s "color" colorToJson jsonToColor) [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green]
    Spec.it s "Keyword" $
      roundTrip s "kw" keywordToJson jsonToKeyword Keyword.Trample
    Spec.it s "Keyword.Infect" $
      roundTrip s "infect" keywordToJson jsonToKeyword Keyword.Infect
    -- CR 702.18a's shroud is nullary, so what this pins is the TAG: a Blurred
    -- Mongoose that decoded as anything else would be a legal Doom Blade target.
    Spec.it s "Keyword.Shroud" $ do
      roundTrip s "shroud" keywordToJson jsonToKeyword Keyword.Shroud
      Spec.assertBool s (keywordToJson Keyword.Shroud /= keywordToJson Keyword.Trample) "shroud is not trample"
    -- CR 702.164a's N rides the constructor, so this is the first keyword
    -- that is not a bare tag.
    Spec.it s "Keyword.Toxic carries its N" $ do
      roundTrip s "toxic 1" keywordToJson jsonToKeyword (Keyword.Toxic 1)
      roundTrip s "toxic 2" keywordToJson jsonToKeyword (Keyword.Toxic 2)
      Spec.assertBool s (keywordToJson (Keyword.Toxic 1) /= keywordToJson (Keyword.Toxic 2)) "toxic 1 and toxic 2 encode differently"
    -- CR 702.70a's N rides the constructor the same way. The two payloaded
    -- keywords must not share a tag, or Snake Cult Initiation would decode
    -- as toxic 3.
    Spec.it s "Keyword.Poisonous carries its N" $ do
      roundTrip s "poisonous 1" keywordToJson jsonToKeyword (Keyword.Poisonous 1)
      roundTrip s "poisonous 3" keywordToJson jsonToKeyword (Keyword.Poisonous 3)
      Spec.assertBool s (keywordToJson (Keyword.Poisonous 3) /= keywordToJson (Keyword.Toxic 3)) "poisonous 3 is not toxic 3"
    -- CR 702.34a's payload is a whole Cost, not a number -- the first
    -- keyword whose parameter is itself a composite.
    Spec.it s "Keyword.Flashback carries its cost" $ do
      let flashback n =
            Keyword.Flashback
              Cost.Type.MkCost
                { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic n]),
                  Cost.Type.components = []
                }
      roundTrip s "flashback {1}" keywordToJson jsonToKeyword (flashback 1)
      roundTrip s "flashback {4}" keywordToJson jsonToKeyword (flashback 4)
      Spec.assertBool s (keywordToJson (flashback 1) /= keywordToJson (flashback 4)) "the cost is part of the encoding"
    -- CR 702.42a's payload is a whole Cost too, and it must not share
    -- Flashback's tag: Dream's Grip may not decode as a card castable from
    -- a graveyard.
    Spec.it s "Keyword.Entwine carries its cost, and is not Flashback" $ do
      let entwine n =
            Keyword.Entwine
              Cost.Type.MkCost
                { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic n]),
                  Cost.Type.components = []
                }
          flashbackOf n =
            Keyword.Flashback
              Cost.Type.MkCost
                { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic n]),
                  Cost.Type.components = []
                }
      roundTrip s "entwine {1}" keywordToJson jsonToKeyword (entwine 1)
      roundTrip s "entwine {3}" keywordToJson jsonToKeyword (entwine 3)
      Spec.assertBool s (keywordToJson (entwine 1) /= keywordToJson (entwine 3)) "the cost is part of the encoding"
      Spec.assertBool s (keywordToJson (entwine 1) /= keywordToJson (flashbackOf 1)) "entwine {1} is not flashback {1}"
    -- CR 702.14a's "[type]" rides the constructor, so swampwalk and
    -- islandwalk are DIFFERENT keywords and must encode differently -- a
    -- Bog Wraith that decoded as an islandwalker would be blockable
    -- exactly when it should not be.
    Spec.it s "Keyword.Landwalk carries its land type" $ do
      roundTrip s "swampwalk" keywordToJson jsonToKeyword (Keyword.Landwalk Subtype.Swamp)
      roundTrip s "islandwalk" keywordToJson jsonToKeyword (Keyword.Landwalk Subtype.Island)
      Spec.assertBool
        s
        (keywordToJson (Keyword.Landwalk Subtype.Swamp) /= keywordToJson (Keyword.Landwalk Subtype.Island))
        "swampwalk and islandwalk encode differently"
    Spec.it s "CastingPermission" $ do
      roundTrip s "library" castingPermissionToJson jsonToCastingPermission CastingPermission.CastFromLibraryWhileSearching
      roundTrip s "graveyard" castingPermissionToJson jsonToCastingPermission CastingPermission.CastFromGraveyard
    Spec.it s "CastingRestriction" $ do
      let declareAttackers = CastingRestriction.DuringPhase (Phase.Combat CombatStep.DeclareAttackers)
          upkeep = CastingRestriction.DuringPhase (Phase.Beginning BeginningStep.Upkeep)
      roundTrip s "declare attackers" castingRestrictionToJson jsonToCastingRestriction declareAttackers
      roundTrip s "upkeep" castingRestrictionToJson jsonToCastingRestriction upkeep
      roundTrip s "attacked" castingRestrictionToJson jsonToCastingRestriction CastingRestriction.AttackedThisStep
      Spec.assertBool s (castingRestrictionToJson declareAttackers /= castingRestrictionToJson upkeep) "the phase is part of the encoding"
    Spec.it s "ZoneChangeSubject" $ do
      roundTrip s "any" zoneChangeSubjectToJson jsonToZoneChangeSubject ZoneChangeSubject.AnyObject
      roundTrip s "source" zoneChangeSubjectToJson jsonToZoneChangeSubject ZoneChangeSubject.TheSource
    Spec.it s "PlayerCounterKind" $ do
      roundTrip s "energy" playerCounterKindToJson jsonToPlayerCounterKind PlayerCounterKind.Energy
      roundTrip s "poison" playerCounterKindToJson jsonToPlayerCounterKind PlayerCounterKind.Poison
    Spec.it s "CounterKind" $ do
      Spec.assertEqWith s "plus" (jsonToCounterKind (counterKindToJson CounterKind.PlusOnePlusOne)) (Right CounterKind.PlusOnePlusOne)
      Spec.assertEqWith s "minus" (jsonToCounterKind (counterKindToJson CounterKind.MinusOneMinusOne)) (Right CounterKind.MinusOneMinusOne)
      -- CR 122.1e, the first kind that modifies no characteristic.
      Spec.assertEqWith s "loyalty" (jsonToCounterKind (counterKindToJson CounterKind.Loyalty)) (Right CounterKind.Loyalty)
    Spec.it s "Zone" $
      roundTrip s "zone" zoneToJson jsonToZone Zone.Graveyard
    Spec.it s "Zone.Command" $
      roundTrip s "command" zoneToJson jsonToZone Zone.Command
    Spec.it s "unknown tag fails" $
      Spec.assertBool s (either (const True) (const False) (jsonToColor (J.jObject []))) "left"
  Spec.describe s "newtypes" $ do
    Spec.it s "SlotName" $
      roundTrip s "slot" slotNameToJson jsonToSlotName (SlotName.MkSlotName (Text.pack "x"))
    Spec.it s "ObjectId" $
      roundTrip s "oid" objectIdToJson jsonToObjectId (ObjectId.MkObjectId 7)
  Spec.describe s "mana + quantity (tagged-sum trap)" $ do
    Spec.it s "Quantity.Literal is a tagged object with numeric value" $
      Spec.assertEqWith
        s
        "shape"
        (quantityToJson (Quantity.Literal 3))
        (J.jObject [(Text.pack "type", Value.String (String.MkString (Text.pack "Literal"))), (Text.pack "value", Value.Number (Number.MkNumber (Decimal.mkDecimal 3 0)))])
    Spec.it s "Quantity.ManaValue is nullary tagged" $
      roundTrip s "mv" quantityToJson jsonToQuantity Quantity.ManaValue
    -- CR 208.1, Ghitu Fire-Eater's "damage equal to its power". Nullary like
    -- ManaValue, and NOT to be confused with the Power newtype round-tripped
    -- further down, which wraps a printed power/toughness box.
    Spec.it s "Quantity.Power is nullary tagged" $
      roundTrip s "pwr" quantityToJson jsonToQuantity Quantity.Power
    Spec.it s "Quantity.Literal round-trips" $
      roundTrip s "lit" quantityToJson jsonToQuantity (Quantity.Literal 5)
    -- Bane of Progress' "for each permanent destroyed this way": a number an
    -- earlier effect of the same resolution bound into a slot. Unlike X, it
    -- carries the slot name on the wire, so the payload is asserted rather
    -- than only round-tripped -- and nested under Plus, since composition is
    -- where a recursive decoder loses a payload.
    Spec.it s "Quantity.InSlot carries its slot name, bare and nested" $ do
      let slot = SlotName.MkSlotName (Text.pack "destroyed")
      roundTrip s "qslot" quantityToJson jsonToQuantity (Quantity.InSlot slot)
      roundTrip s "qslot+" quantityToJson jsonToQuantity (Quantity.Plus (Quantity.Literal 1) (Quantity.InSlot slot))
      Spec.assertEqWith
        s
        "the slot name is on the wire"
        (quantityToJson (Quantity.InSlot slot))
        (J.tagged (Text.pack "InSlot") (Just (slotNameToJson slot)))
    Spec.it s "ManaCost round-trips" $
      roundTrip
        s
        "cost"
        manaCostToJson
        jsonToManaCost
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)])
    Spec.it s "ManaSymbol.Variable round-trips" $
      roundTrip s "var" manaSymbolToJson jsonToManaSymbol ManaSymbol.Variable
    Spec.it s "Power round-trips" $
      roundTrip s "pow" powerToJson jsonToPower (Power.MkPower (Quantity.Literal 2))
  Spec.describe s "modification + affected" $ do
    Spec.it s "GainKeyword" $
      roundTrip s "m1" modificationToJson jsonToModification (Modification.GainKeyword Keyword.Deathtouch)
    Spec.it s "SetBasePowerToughness" $
      roundTrip s "m2" modificationToJson jsonToModification (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1))
    Spec.it s "ChangeSubtypeWord" $
      roundTrip s "m3" modificationToJson jsonToModification (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island)
    Spec.it s "SetControllerToSource" $
      roundTrip s "m4" modificationToJson jsonToModification Modification.SetControllerToSource
    Spec.it s "Affected round-trips (TheseObjects, Matching, Matching's \"each other\" shape, and AttachedPlayerControls)" $
      mapM_
        (roundTrip s "affected" affectedToJson jsonToAffected)
        [ Affected.TheseObjects (Set.fromList [ObjectId.MkObjectId 1, ObjectId.MkObjectId 2]),
          Affected.Matching (Filter.Type.HasCardType CardType.Creature),
          -- Opalescence's shape: its own "each other" card text (not a
          -- rule) as Not IsSource.
          Affected.Matching (Filter.Type.And [Filter.Type.HasCardType CardType.Enchantment, Filter.Type.Not (Filter.Type.HasSubtype Subtype.Mountain), Filter.Type.Not Filter.Type.IsSource]),
          -- CR 303.4m through a player: Curse of Death's Hold's shape.
          Affected.AttachedPlayerControls (Filter.Type.HasCardType CardType.Creature)
        ]
  Spec.describe s "effect" $ do
    Spec.it s "DealDamage" $
      roundTrip s "e1" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 3))
    -- ModifyTarget takes the same untagged ObjectRef Destroy and Untap do,
    -- so both arms have to survive the trip: Giant Growth's slot and
    -- Trumpet Blast's filter-selected set.
    Spec.it s "ModifyTarget round-trips both ObjectRef arms" $ do
      roundTrip s "e2" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))))
      roundTrip s "e2b" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.EachMatching Filter.Type.IsAttacking))
    Spec.it s "AddMana" $
      roundTrip s "e3" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green)))
    Spec.it s "AddMana of any color" $
      roundTrip s "e3b" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AddMana ManaProduction.AnyColor)
    Spec.it s "ExileAllGraveyards" $
      roundTrip s "e4" (effectToJson cardToJson) (jsonToEffect jsonToCard) Effect.ExileAllGraveyards
    Spec.it s "Proliferate" $
      roundTrip s "e4b" (effectToJson cardToJson) (jsonToEffect jsonToCard) Effect.Proliferate
    -- Both shapes in the pool: Aggravated Assault's pair and Full
    -- Throttle's two combat phases with no main phase between them.
    Spec.it s "AddPhases round-trips the pair" $
      roundTrip s "e4c" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain])
    Spec.it s "AddPhases round-trips a repeated phase" $
      roundTrip s "e4c1" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraCombat])
    -- Untap takes the same untagged ObjectRef Destroy does, so both arms
    -- have to survive the trip: Act of Treason's slot and Aggravated
    -- Assault's swept set.
    Spec.it s "Untap round-trips both ObjectRef arms" $ do
      roundTrip s "e4d" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      roundTrip s "e4e" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Untap (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)))
    -- GainControl's own two arms: Act of Treason's slot and Aura Thief's
    -- "all enchantments". Its Duration is what tells the two cards apart on
    -- the wire, so both durations ride along.
    Spec.it s "GainControl round-trips both ObjectRef arms" $ do
      roundTrip s "e4f" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainControl Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      roundTrip s "e4g" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainControl Duration.Indefinite (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Enchantment)))
    -- CR 701.26a's Tap is Untap's mirror and shares its wire shape, so the
    -- two must not collapse into one tag: Dream's Grip prints both modes
    -- on one card and a decoder that confused them would silently swap
    -- them.
    Spec.it s "Tap round-trips both ObjectRef arms, and is not Untap" $ do
      roundTrip s "e4f" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      roundTrip s "e4g" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Tap (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)))
      Spec.assertBool
        s
        ( effectToJson cardToJson (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
            /= effectToJson cardToJson (Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
        )
        "Tap and Untap of the same slot encode differently"
    Spec.it s "Destroy carries its CR 701.19c rider both ways" $ do
      roundTrip s "e5a" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.Regenerable Nothing)
      roundTrip s "e5b" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.CantBeRegenerated Nothing)
    -- An ObjectRef is untagged and told apart by JSON type, so the two arms
    -- have to be pinned together: a string is the slot, an object is the
    -- filter-selected set. A round trip alone would not catch a decoder that
    -- read every payload as one arm, so the wire form is spelled out.
    Spec.it s "ObjectRef's two arms are told apart by JSON type, not by a tag" $ do
      let slotted = Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Regenerability.Regenerable Nothing
          swept = Effect.Destroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing
      roundTrip s "e5c" (effectToJson cardToJson) (jsonToEffect jsonToCard) swept
      Spec.assertEqWith
        s
        "a slot is still a bare string, so every Destroy card on disk is unchanged"
        (payloadHead (effectToJson cardToJson slotted))
        (J.jText (Text.pack "target"))
      Spec.assertEqWith
        s
        "and a set is the Filter object"
        (payloadHead (effectToJson cardToJson swept))
        (filterToJson (Filter.Type.HasCardType CardType.Creature))
    -- Bane of Progress' "destroyed this way": the third element is the slot
    -- the sweep binds its count into, and it is ELIDED when absent -- so
    -- every Destroy already on disk keeps its two-element payload.
    Spec.it s "Destroy's bound-count slot round-trips and is elided when absent" $ do
      let counting = Effect.Destroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Artifact)) Regenerability.Regenerable (Just (SlotName.MkSlotName (Text.pack "destroyed")))
          plain = Effect.Destroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Artifact)) Regenerability.Regenerable Nothing
      roundTrip s "e5d" (effectToJson cardToJson) (jsonToEffect jsonToCard) counting
      Spec.assertEqWith
        s
        "a Destroy that binds nothing writes two elements"
        (payloadLength (effectToJson cardToJson plain))
        2
      Spec.assertEqWith
        s
        "and one that binds a count writes three"
        (payloadLength (effectToJson cardToJson counting))
        3
    Spec.it s "ExileHandThenDraw" $
      roundTrip s "e-powder" (effectToJson cardToJson) (jsonToEffect jsonToCard) Effect.ExileHandThenDraw
    Spec.it s "PlayerSacrifices" $
      roundTrip s "e6" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.PlayerSacrifices (SlotName.MkSlotName (Text.pack "t")) (Filter.Type.HasCardType CardType.Creature) (Quantity.Literal 1))
    -- CR 701.3: the destination filter travels in the payload, which is what
    -- distinguishes this arm's wire format from Attach's bare slot.
    Spec.it s "AttachTarget" $
      roundTrip s "e-crown" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AttachTarget (SlotName.MkSlotName (Text.pack "target")) (Filter.Type.HasCardType CardType.Creature))
    Spec.it s "Sacrifice round-trips" $
      roundTrip s "e5" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self")))
    Spec.it s "PutCounters effect round-trips through the codec" $
      let effect = Effect.PutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (SlotName.MkSlotName (Text.pack "creature"))
       in Spec.assertEqWith s "round-trip" (jsonToEffect jsonToCard (effectToJson cardToJson effect)) (Right effect)
    Spec.it s "AffectPlayers round-trips" $
      roundTrip
        s
        "e6"
        (effectToJson cardToJson)
        (jsonToEffect jsonToCard)
        (Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells)
    -- CR 614.10a: Fatigue's slot read, plus the self-scoped arm Avizoa's
    -- "you skip your next untap step" would write -- and Stonehorn
    -- Dignitary's whole-phase selector, the arm a Phase alone cannot spell
    -- (CR 500.1).
    Spec.it s "SkipNextPhase" $ do
      roundTrip s "skip slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep)))
      roundTrip s "skip you" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.SkipNextPhase (PlayerRef.Relative PlayerRelation.You) (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap)))
      roundTrip s "skip a whole phase" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) PhaseSelector.CombatPhase)
    -- CR 500.7: Time Warp's slot read, whose skip set is empty, plus Savor
    -- the Moment's self-scoped arm carrying CR 500.11's skip of one step of
    -- the turn it creates. The many-selector case has no producer -- no
    -- printed card skips two windows of the turn it makes -- but the field
    -- is a Set, so the wire format has to survive more than one.
    Spec.it s "TakeExtraTurn" $ do
      roundTrip s "extra turn slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.TakeExtraTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Set.empty)
      roundTrip s "extra turn you" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.TakeExtraTurn (PlayerRef.Relative PlayerRelation.You) Set.empty)
      roundTrip
        s
        "extra turn skipping its own untap step"
        (effectToJson cardToJson)
        (jsonToEffect jsonToCard)
        (Effect.TakeExtraTurn (PlayerRef.Relative PlayerRelation.You) (Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap))))
      roundTrip
        s
        "extra turn skipping a step and a whole phase"
        (effectToJson cardToJson)
        (jsonToEffect jsonToCard)
        (Effect.TakeExtraTurn PlayerRef.EachPlayer (Set.fromList [PhaseSelector.Step (Phase.Beginning BeginningStep.Untap), PhaseSelector.CombatPhase]))
    -- Every PlayerRef shape the opcode accepts: the self-scoped one every
    -- card in the pool uses, and the slot read CR 702.70a's "that player"
    -- needs.
    Spec.it s "GainPlayerCounters" $ do
      roundTrip s "gpc" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2))
      roundTrip s "gpc slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainPlayerCounters (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) PlayerCounterKind.Poison (Quantity.Literal 3))
    -- Both of Draw's proven PlayerRef shapes: Divination's controller draw
    -- and Ancestral Recall's targeted one (#272).
    Spec.it s "Draw" $ do
      roundTrip s "draw" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
      roundTrip s "draw slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Draw (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3))
    -- Sign in Blood's targeted loss, and the `Relative You` arm that no
    -- card in the pool uses yet -- the codec accepts every PlayerRef either way.
    Spec.it s "LoseLife" $ do
      roundTrip s "lose slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.LoseLife (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2))
      roundTrip s "lose you" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.LoseLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))
    -- Soul Warden's "you gain 1 life", plus the slot arm no card uses
    -- yet -- the same coverage LoseLife above gets, on the sibling opcode.
    Spec.it s "GainLife" $ do
      roundTrip s "gain you" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))
      roundTrip s "gain slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainLife (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2))
    Spec.it s "CreateEmblem" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      roundTrip s "emblem" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.CreateEmblem (Printing.card piker))
    Spec.it s "BecomeMonarch" $
      roundTrip s "e" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.BecomeMonarch MonarchTarget.TheController)
    -- Every constructor, even though the encoder never emits AnyTime (it is
    -- the absent key on a card). Round-tripping the whole family is what
    -- keeps the decoder honest about the forms it accepts -- including that
    -- the two nullary arms still render as a bare tag now that DuringPhase
    -- has made the encoder a tagged one.
    Spec.it s "ActivationTiming round-trips every way" $ do
      roundTrip s "timing" activationTimingToJson jsonToActivationTiming ActivationTiming.AnyTime
      roundTrip s "timing" activationTimingToJson jsonToActivationTiming ActivationTiming.SorcerySpeed
      -- Desert's own rider (CR 511.1), and a stepless phase alongside it:
      -- Pawl.Types.Phase spans both, so the arm has to carry both.
      roundTrip s "timing" activationTimingToJson jsonToActivationTiming (ActivationTiming.DuringPhase (Phase.Combat CombatStep.EndOfCombat))
      roundTrip s "timing" activationTimingToJson jsonToActivationTiming (ActivationTiming.DuringPhase Phase.PostcombatMain)
    Spec.it s "ExileUntilMonarch" $
      roundTrip s "eum" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ExileUntilMonarch (SlotName.MkSlotName (Text.pack "target")))
    Spec.it s "PlaySubgame round-trips" $
      let e = Effect.PlaySubgame (SlotName.MkSlotName (Text.pack "loser"))
       in Spec.assertEqWith s "PlaySubgame round-trips" (jsonToEffect jsonToCard (effectToJson cardToJson e)) (Right e)
  Spec.describe s "duration + condition" $ do
    Spec.it s "Duration.UntilYourNextTurn round-trips" $
      Spec.assertEqWith s "preserved" (jsonToDuration (durationToJson Duration.UntilYourNextTurn)) (Right Duration.UntilYourNextTurn)
    Spec.it s "S.youControlSource round-trips as a Condition" $
      Spec.assertEqWith s "preserved" (jsonToCondition (conditionToJson S.youControlSource)) (Right S.youControlSource)
    Spec.it s "Duration.ForAsLongAs round-trips with its condition" $
      let d = Duration.ForAsLongAs S.youControlSource
       in Spec.assertEqWith s "preserved" (jsonToDuration (durationToJson d)) (Right d)
  Spec.describe s "player effects (P7)" $ do
    Spec.it s "every PlayerScope round-trips" $
      mapM_
        (roundTrip s "scope" playerScopeToJson jsonToPlayerScope)
        [PlayerScope.You, PlayerScope.Opponents, PlayerScope.EachPlayer]
    Spec.it s "every PlayerEffect round-trips" $
      mapM_
        (roundTrip s "effect" playerEffectToJson jsonToPlayerEffect)
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
        ]
    -- CR 613.6 made a static ability "one affected set, one or more parts",
    -- so the wire format has an array where it used to have a single
    -- modification -- and an array can be empty where a single value could
    -- not. An ability with no parts is one that does nothing, which no card
    -- means, so it is a decode FAILURE rather than a permanent that quietly
    -- under-performs its own text. NonEmpty is what makes that structural;
    -- this pins that the boundary really says no.
    Spec.it s "a static ability with an empty modifications array is rejected" $ do
      let value =
            J.jObject
              [ (Text.pack "affected", affectedToJson Affected.Attached),
                (Text.pack "modifications", J.jArray [])
              ]
      Spec.assertBool
        s
        (either (const True) (const False) (jsonToStaticAbility value))
        "an empty array does not decode"
      roundTrip
        s
        "one part still round-trips"
        staticAbilityToJson
        jsonToStaticAbility
        (StaticAbility.MkStaticAbility Affected.Attached (NonEmpty.singleton (Modification.GainKeyword Keyword.Flying)))
      roundTrip
        s
        "and so do several"
        staticAbilityToJson
        jsonToStaticAbility
        ( StaticAbility.MkStaticAbility
            Affected.Attached
            (Modification.LoseAllAbilities NonEmpty.:| [Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)])
        )
    Spec.it s "PlayerStaticAbility round-trips" $
      roundTrip
        s
        "ability"
        playerStaticAbilityToJson
        jsonToPlayerStaticAbility
        (PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.CantCastMoreThan 1))
    Spec.it s "a Card carrying player abilities round-trips" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
          c = base {CardT.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.NoMaximumHandSize]}
      roundTrip s "card" cardToJson jsonToCard c
    -- Byte-stability: an empty list must not appear in the rendered JSON,
    -- or every committed card file changes. The same posture
    -- colorIndicator and delayedAbilities already take.
    Spec.it s "an empty playerAbilities list is omitted from the JSON" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
      Spec.assertEqWith s "the fixture really has none" (CardT.playerAbilities base) []
      case J.asObject (cardToJson base) of
        Left err -> Spec.assertFailure s (Text.unpack err)
        Right pairs -> Spec.assertBool s (notElem (Text.pack "playerAbilities") (fmap fst pairs)) "key absent"
    Spec.it s "a Card carrying a CR 103.5b mulligan action round-trips" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
          c = base {CardT.mulliganAction = [Effect.ExileHandThenDraw]}
      roundTrip s "card" cardToJson jsonToCard c
    -- Byte-stability: an empty list must not appear in the rendered JSON,
    -- or every committed card file changes. The same posture
    -- playerAbilities and additionalCosts already take.
    Spec.it s "an empty mulliganAction list is omitted from the JSON" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
      Spec.assertEqWith s "the fixture really has none" (CardT.mulliganAction base) []
      case J.asObject (cardToJson base) of
        Left err -> Spec.assertFailure s (Text.unpack err)
        Right pairs -> Spec.assertBool s (notElem (Text.pack "mulliganAction") (fmap fst pairs)) "key absent"
    Spec.it s "a Card carrying a CR 103.6 opening-hand action round-trips" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
          c = base {CardT.openingHandAction = [Effect.MoveToZone Binding.triggerSource Zone.Battlefield]}
      roundTrip s "card" cardToJson jsonToCard c
    Spec.it s "an empty openingHandAction list is omitted from the JSON" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
      Spec.assertEqWith s "the fixture really has none" (CardT.openingHandAction base) []
      case J.asObject (cardToJson base) of
        Left err -> Spec.assertFailure s (Text.unpack err)
        Right pairs -> Spec.assertBool s (notElem (Text.pack "openingHandAction") (fmap fst pairs)) "key absent"
  Spec.describe s "filter (P9)" $ do
    Spec.it s "Filter round-trips including nested And/Or/Not" $
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
          relentlessAssault = Filter.Type.AttackedThisTurn
          crownOfTheAges = Filter.Type.And [Filter.Type.HasSubtype Subtype.Aura, Filter.Type.IsAttachedToCreature]
          labyrinthOfSkophos = Filter.Type.Or [Filter.Type.IsAttacking, Filter.Type.IsBlocking]
          auraGraftTarget = Filter.Type.And [Filter.Type.HasSubtype Subtype.Aura, Filter.Type.IsAttachedToPermanent]
          auraGraftDestination = Filter.Type.CanHostSubject
       in mapM_
            (roundTrip s "filter" filterToJson jsonToFilter)
            [doomBlade, terror, reprisal, basicLand, angelicEdict, controlled, bySubtype, isSource, ravenousRats, killShot, relentlessAssault, crownOfTheAges, labyrinthOfSkophos, auraGraftTarget, auraGraftDestination]
    Spec.it s "PlayerRelation round-trips" $
      mapM_
        (roundTrip s "relation" playerRelationToJson jsonToPlayerRelation)
        [PlayerRelation.You, PlayerRelation.Opponent]
    -- Every supertype the type models, so a new constructor whose codec
    -- arm is forgotten fails here rather than at the one card that carries
    -- it. CR 205.4a lists five; Ongoing is scheme-only (#131).
    Spec.it s "every Supertype round-trips" $
      mapM_
        (roundTrip s "supertype" supertypeToJson jsonToSupertype)
        [Supertype.Basic, Supertype.Legendary, Supertype.Snow, Supertype.World]
  -- Sits beside "filter (P9)": a TargetSpec is Pool + Maybe Filter, so these
  -- exercise the Filter arm above in its embedded position. Covers a bare
  -- pool (Nothing filter, omitted key), a filtered pool, and the Not
  -- IsSource conjunct that carries CR 601.2c's "another" (#163).
  Spec.describe s "target spec (P9)" $ do
    Spec.it s "TargetSpec bare pool round-trips" $
      let spec' = TargetSpec.MkTargetSpec Pool.Creatures Nothing
       in Spec.assertEqWith s "preserved" (jsonToTargetSpec (targetSpecToJson spec')) (Right spec')
    Spec.it s "TargetSpec filtered pool round-trips" $
      let spec' = TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact))
       in Spec.assertEqWith s "preserved" (jsonToTargetSpec (targetSpecToJson spec')) (Right spec')
    Spec.it s "TargetSpec \"another\" (Not IsSource) round-trips" $
      let spec' = TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.And [Filter.Type.Not (Filter.Type.HasCardType CardType.Land), Filter.Type.Not Filter.Type.IsSource]))
       in Spec.assertEqWith s "preserved" (jsonToTargetSpec (targetSpecToJson spec')) (Right spec')
  Spec.describe s "records" $ do
    Spec.it s "TypeLine" $
      roundTrip
        s
        "tl"
        typeLineToJson
        jsonToTypeLine
        (TypeLine.MkTypeLine (Set.singleton Supertype.Basic) (Set.singleton CardType.Land) (Set.singleton Subtype.Mountain))
    -- CR 308.1/308.2: the kindred shape -- two card types, and a CREATURE
    -- subtype on a card that is not a creature. Bitterblossom's type line,
    -- and the only one in the pool where the subtype's family and the
    -- card types disagree.
    Spec.it s "TypeLine (kindred)" $
      roundTrip
        s
        "tl-kindred"
        typeLineToJson
        jsonToTypeLine
        (TypeLine.MkTypeLine Set.empty (Set.fromList [CardType.Kindred, CardType.Enchantment]) (Set.singleton Subtype.Faerie))
    -- CR 306.3 / 205.3j: Jace Beleren's, and the first type line whose
    -- subtype is a planeswalker type.
    Spec.it s "TypeLine (planeswalker)" $
      roundTrip
        s
        "tl-planeswalker"
        typeLineToJson
        jsonToTypeLine
        (TypeLine.MkTypeLine (Set.singleton Supertype.Legendary) (Set.singleton CardType.Planeswalker) (Set.singleton Subtype.Jace))
    -- CR 306.5a: the printed loyalty number.
    Spec.it s "Loyalty" $
      roundTrip s "loyalty" loyaltyToJson jsonToLoyalty (Loyalty.MkLoyalty 3)
    -- CR 614.1c / 306.5b: the intrinsic enters-with-counters rewrite.
    Spec.it s "EntryRewrite (with counters)" $
      roundTrip s "entry-counters" entryRewriteToJson jsonToEntryRewrite (EntryRewrite.WithCounters CounterKind.Loyalty 3)
    -- CR 606.3's record.
    Spec.it s "GameEvent (loyalty ability activated)" $
      roundTrip s "loyalty-activated" gameEventToJson jsonToGameEvent (GameEvent.LoyaltyAbilityActivated (ObjectId.MkObjectId 7))
    Spec.describe s "cost (P8)" $ do
      Spec.it s "every CostComponent round-trips" $
        mapM_
          (roundTrip s "component" costComponentToJson jsonToCostComponent)
          [ CostComponent.TapThis,
            CostComponent.SacrificeThis,
            CostComponent.PayLife 2,
            CostComponent.Sacrifice 2 (Filter.Type.HasSubtype Subtype.Mountain)
          ]
      Spec.it s "PayEnergy" $
        roundTrip s "pe" costComponentToJson jsonToCostComponent (CostComponent.PayEnergy 2)
      -- CR 606.4's two halves, Jace Beleren's +2 and -1.
      Spec.it s "loyalty costs" $ do
        roundTrip s "add" costComponentToJson jsonToCostComponent (CostComponent.AddLoyaltyToThis 2)
        roundTrip s "remove" costComponentToJson jsonToCostComponent (CostComponent.RemoveLoyaltyFromThis 1)
      Spec.it s "a Cost with a mana part and components round-trips" $
        roundTrip
          s
          "cost"
          costToJson
          jsonToCost
          Cost.Type.MkCost
            { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
              Cost.Type.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
            }
      -- CR 118.5a: {0} is a real, payable cost, and ManaCost's empty list IS
      -- {0}. This is the shape every migrated ability now carries.
      Spec.it s "a {0} cost round-trips as Just an empty ManaCost" $
        roundTrip
          s
          "zero"
          costToJson
          jsonToCost
          Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []}
      -- CR 118.6: an ABSENT mana field is an UNPAYABLE cost, not {0}. This is
      -- the footgun the corpus migration exists to avoid, pinned so a future
      -- card file cannot lose its mana field unnoticed.
      Spec.it s "an omitted mana field decodes to Nothing, not to {0}" $
        let value = J.jObject [(Text.pack "components", J.jArray [])]
         in Spec.assertEqWith
              s
              "unpayable"
              (jsonToCost value)
              (Right Cost.Type.MkCost {Cost.Type.mana = Nothing, Cost.Type.components = []})
      Spec.it s "a Card carrying an additional cost round-trips" $ do
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        let base = Printing.card lightningBolt
            c = base {CardT.additionalCosts = [CostComponent.Sacrifice 1 (Filter.Type.HasCardType CardType.Creature)]}
        roundTrip s "card" cardToJson jsonToCard c
      -- Byte-stability: an empty list must not appear in the rendered JSON,
      -- or every committed card file changes. The posture colorIndicator,
      -- delayedAbilities and playerAbilities already take.
      Spec.it s "an empty additionalCosts list is omitted from the JSON" $ do
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        let base = Printing.card lightningBolt
        Spec.assertEqWith s "the fixture really has none" (CardT.additionalCosts base) []
        case J.asObject (cardToJson base) of
          Left err -> Spec.assertFailure s (Text.unpack err)
          Right pairs -> Spec.assertBool s (notElem (Text.pack "additionalCosts") (fmap fst pairs)) "key absent"
      Spec.it s "a Card carrying an alternative cost round-trips" $ do
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        let base = Printing.card lightningBolt
            alt =
              Cost.Type.MkCost
                { Cost.Type.mana = Just (ManaCost.MkManaCost []),
                  Cost.Type.components = [CostComponent.Sacrifice 2 (Filter.Type.HasSubtype Subtype.Mountain)]
                }
            c = base {CardT.alternativeCosts = [alt]}
        roundTrip s "card" cardToJson jsonToCard c
      Spec.it s "an empty alternativeCosts list is omitted from the JSON" $ do
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        let base = Printing.card lightningBolt
        Spec.assertEqWith s "the fixture really has none" (CardT.alternativeCosts base) []
        case J.asObject (cardToJson base) of
          Left err -> Spec.assertFailure s (Text.unpack err)
          Right pairs -> Spec.assertBool s (notElem (Text.pack "alternativeCosts") (fmap fst pairs)) "key absent"
    Spec.it s "a ZoneChangeR replacement round-trips" $
      let re =
            ReplacementEffect.ZoneChangeR
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Zone.Graveyard,
                  ZoneChangePattern.whichObject = ZoneChangeSubject.AnyObject,
                  ZoneChangePattern.whoseObject = ControllerRelation.Anyones
                }
              Zone.Exile
       in Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
    -- Leyline of the Void's shape: the relation that distinguishes it from
    -- Rest in Peace has to survive the wire too.
    Spec.it s "a ZoneChangeR carrying Opponents round-trips" $
      let re =
            ReplacementEffect.ZoneChangeR
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Zone.Graveyard,
                  ZoneChangePattern.whichObject = ZoneChangeSubject.AnyObject,
                  ZoneChangePattern.whoseObject = ControllerRelation.Opponents
                }
              Zone.Exile
       in Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
    Spec.it s "a CounterR replacement round-trips (pattern and scaling are data)" $
      let re =
            ReplacementEffect.CounterR
              CounterPattern.MkCounterPattern
                { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
                  CounterPattern.whose = ControllerRelation.Yours,
                  CounterPattern.onWhat = Filter.Type.HasCardType CardType.Creature
                }
              (Scaling.AddMore 1)
       in Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
    Spec.it s "an EntryR ChoiceOf replacement round-trips (P/T and keywords)" $
      let re =
            ReplacementEffect.EntryR
              ( EntryRewrite.ChoiceOf
                  [ EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty},
                    EntryOption.MkEntryOption {EntryOption.power = 1, EntryOption.toughness = 6, EntryOption.keywords = Set.singleton Keyword.Defender}
                  ]
              )
       in Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
    Spec.it s "an EntryR AsCopy replacement round-trips" $
      let re = ReplacementEffect.EntryR EntryRewrite.AsCopy
       in Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
    Spec.it s "a DamageR replacement round-trips (pattern and rewrite are data)" $
      let re =
            ReplacementEffect.DamageR
              DamagePattern.MkDamagePattern {DamagePattern.whichKind = Just DamageKind.Combat}
              DamageRewrite.PreventAll
       in Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
    Spec.it s "a DestructionR replacement round-trips" $
      let re = ReplacementEffect.DestructionR DestructionRewrite.Regenerate
       in Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
    Spec.it s "a TokenR replacement round-trips (pattern and scaling are data)" $
      let re =
            ReplacementEffect.TokenR
              TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours}
              (Scaling.Multiply 2)
       in Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
    Spec.it s "a CounterR replacement round-trips with whichKind = Nothing (explicit JSON null)" $
      let re =
            ReplacementEffect.CounterR
              CounterPattern.MkCounterPattern
                { CounterPattern.whichKind = Nothing,
                  CounterPattern.whose = ControllerRelation.Anyones,
                  CounterPattern.onWhat = Filter.Type.And []
                }
              (Scaling.Multiply 2)
       in Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
    -- CR 614.1b: a skip carries a pattern and no rewrite, so the payload is
    -- the pattern itself rather than the usual two-element array.
    --
    -- Both `whosePhase` shapes: Eon Hub's symmetric Nothing, which is what
    -- card JSON writes, and the baked Just that only Resolve's
    -- SkipNextPhase arm produces (Fatigue). The second is runtime-only, and
    -- is covered here for the same reason SetController's PlayerId is --
    -- the codec has to carry it either way.
    Spec.it s "a PhaseR replacement round-trips" $ do
      let re = ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.Step (Phase.Beginning BeginningStep.Upkeep), PhasePattern.whosePhase = Nothing}
      Spec.assertEqWith s "preserved" (jsonToReplacementEffect (replacementEffectToJson re)) (Right re)
      let scoped = ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep), PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)}
      Spec.assertEqWith s "and so does a player-scoped one" (jsonToReplacementEffect (replacementEffectToJson scoped)) (Right scoped)
      -- CR 500.1: the whole-phase arm, which is the shape a Phase value
      -- cannot carry -- Stonehorn Dignitary's, once Resolve has baked the
      -- player its resolution named.
      let wholePhase = ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.CombatPhase, PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)}
      Spec.assertEqWith s "and so does a whole-phase one" (jsonToReplacementEffect (replacementEffectToJson wholePhase)) (Right wholePhase)
  Spec.describe s "modal" $ do
    Spec.it s "ModeIndex round-trips" $
      roundTrip s "mi" modeIndexToJson jsonToModeIndex (ModeIndex.MkModeIndex 2)
    Spec.it s "ModeSelection round-trips" $
      roundTrip s "ms" modeSelectionToJson jsonToModeSelection (ModeSelection.ChooseExactly 1)
    Spec.it s "Modal round-trips" $
      roundTrip
        s
        "modal"
        (modalToJson cardToJson)
        (jsonToModal jsonToCard)
        ( Modal.MkModal
            ( Seq.fromList
                [ Mode.MkMode
                    (Seq.fromList [Effect.DealDamage (SlotName.MkSlotName (Text.pack "creature")) (Quantity.Literal 1)])
                    (Map.singleton (SlotName.MkSlotName (Text.pack "creature")) (TargetSpec.MkTargetSpec Pool.Creatures Nothing))
                    Optionality.Mandatory
                ]
            )
            (ModeSelection.ChooseExactly 1)
        )
    -- CR 603.5: an Optional mode is what a printed "may" encodes to, and
    -- the key is emitted only for that value.
    Spec.it s "Optionality round-trips" $ do
      roundTrip s "mandatory" optionalityToJson jsonToOptionality Optionality.Mandatory
      roundTrip s "optional" optionalityToJson jsonToOptionality Optionality.Optional
    Spec.it s "an Optional mode round-trips, and says so in the JSON" $ do
      let m = Mode.MkMode Seq.empty Map.empty Optionality.Optional
      roundTrip s "optional mode" (modeToJson cardToJson) (jsonToMode jsonToCard) m
      Spec.assertEqWith
        s
        "the optionality key is present"
        (optionalityKey (modeToJson cardToJson m))
        (Just (optionalityToJson Optionality.Optional))
    -- The byte-identity guarantee for every card file that prints no
    -- "may": a Mandatory mode emits no key, and a mode with no key decodes
    -- back to Mandatory. The counterability precedent.
    Spec.it s "a Mandatory mode omits the key, and an omitted key decodes to Mandatory" $ do
      let m = Mode.MkMode Seq.empty Map.empty Optionality.Mandatory
      Spec.assertEqWith s "no optionality key" (optionalityKey (modeToJson cardToJson m)) Nothing
      Spec.assertEqWith s "decodes to Mandatory" (jsonToMode jsonToCard (modeToJson cardToJson m)) (Right m)
    Spec.it s "empty modal is a decode error" $
      Spec.assertBool
        s
        ( either
            (const True)
            (const False)
            (jsonToModal jsonToCard (J.jObject [(Text.pack "modes", J.jArray []), (Text.pack "selection", modeSelectionToJson (ModeSelection.ChooseExactly 1))]))
        )
        "left"
  Spec.describe s "honesty round-trip over allPrintings" $ do
    Spec.it s "P1: jsonToPrinting . printingToJson == Right" $ do
      ps <- S.allPrintings s
      mapM_ (\p -> Spec.assertEqWith s (show (CardT.name (Printing.card p))) (jsonToPrinting (printingToJson p)) (Right p)) ps
    Spec.it s "P2: through text" $ do
      ps <- S.allPrintings s
      mapM_
        (\p -> Spec.assertEqWith s (show (CardT.name (Printing.card p))) (J.parse (J.render (printingToJson p)) >>= jsonToPrinting) (Right p))
        ps
    Spec.it s "M4e Cancel loads as a single Counter effect targeting a spell" $ do
      cancel <- S.printingOf s registry "Cancel"
      let card = Printing.card cancel
      Spec.assertEqWith
        s
        "effects"
        (Card.allEffects card)
        [Effect.Counter (SlotName.MkSlotName (Text.pack "spell"))]
      Spec.assertEqWith
        s
        "target spec"
        (Card.allTargetSpecs card)
        (Map.singleton (SlotName.MkSlotName (Text.pack "spell")) (TargetSpec.MkTargetSpec Pool.Spells Nothing))
    -- The key is omitted when Counterable, so this pins BOTH directions of
    -- that default: the one card that prints the clause decodes as
    -- CantBeCountered, and a card that says nothing decodes as Counterable
    -- rather than as whatever a missing key might otherwise become.
    Spec.it s "CR 113.6g counterability decodes from the card, and defaults when the key is absent" $ do
      rendingVolley <- S.printingOf s registry "Rending Volley"
      cancel <- S.printingOf s registry "Cancel"
      Spec.assertEqWith
        s
        "Rending Volley says it"
        (CardT.counterability (Printing.card rendingVolley))
        Counterability.CantBeCountered
      Spec.assertEqWith
        s
        "Cancel does not, and its file has no counterability key"
        (CardT.counterability (Printing.card cancel))
        Counterability.Counterable
  Spec.describe s "P4 runtime types" $ do
    Spec.it s "Phase round-trips" $
      mapM_
        (roundTrip s "phase" phaseToJson jsonToPhase)
        [ Phase.Beginning BeginningStep.Upkeep,
          Phase.PrecombatMain,
          Phase.Combat CombatStep.DeclareBlockers,
          Phase.PostcombatMain,
          Phase.Ending EndingStep.EndStep
        ]
    -- A real permanent, not a projection of a nonexistent object: Typhoid
    -- Rats (1/1 deathtouch) populates keywords, colors, power, toughness,
    -- cardTypes and subtypes all at once, so a swapped field or a wrong
    -- JSON key would fail this round-trip instead of surviving it on an
    -- all-Nothing/all-empty value.
    Spec.it s "GameEvent.Moved round-trips with its snapshot" $ do
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      let (ratId, gs) = S.addCreature typhoidRats S.alice (Setup.emptyGame S.bothPlayers)
          zc = ZoneChange.MkZoneChange ratId ratId Zone.Battlefield Zone.Graveyard
          snapshot = Projection.project ratId gs
      roundTrip s "moved" gameEventToJson jsonToGameEvent (GameEvent.Moved zc snapshot)
    -- The snapshot's keywords are counted per keyword (CR 702.164b), so a
    -- COUNT has to survive the wire and not just a membership: the
    -- array-with-repeats encoding is what carries it. A Set-shaped encoder
    -- would pass every OTHER round-trip test in this group and still halve
    -- the Stalker's total toxic value on replay, which is why the count is
    -- asserted here before the round-trip rather than left to Eq alone.
    Spec.it s "a doubled keyword survives the Moved snapshot round-trip" $ do
      stalker <- S.printingOf s registry "Branchblight Stalker"
      let (oid, gs0) = S.addCreature stalker S.alice (Setup.emptyGame S.bothPlayers)
          grant ts = S.withEffectAt oid (Timestamp.MkTimestamp ts) (Modification.GainKeyword (Keyword.Toxic 1))
          snapshot = Projection.project oid (grant 101 (grant 100 gs0))
          zc = ZoneChange.MkZoneChange oid oid Zone.Battlefield Zone.Graveyard
      Spec.assertEqWith s "the fixture really does carry toxic 1 twice" (Map.lookup (Keyword.Toxic 1) (PC.keywords snapshot)) (Just 2)
      roundTrip s "moved" gameEventToJson jsonToGameEvent (GameEvent.Moved zc snapshot)
    Spec.it s "GameEvent.DamageDealt round-trips" $
      roundTrip
        s
        "damage"
        gameEventToJson
        jsonToGameEvent
        -- A NONZERO toxic value, so the CR 702.164b rider is round-tripped
        -- rather than defaulted past.
        (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 1) (Recipient.ToPlayer S.bob) 2 True False 3 DamageKind.Combat))
    -- CR 120.3c's recipient tag is a different arm of Recipient from the one
    -- above, and a CR 608.2i record the codec cannot write is one no replay can
    -- read back.
    Spec.it s "GameEvent.DamageDealt to a planeswalker round-trips" $
      roundTrip
        s
        "damage"
        gameEventToJson
        jsonToGameEvent
        (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 1) (Recipient.ToPlaneswalker (ObjectId.MkObjectId 2)) 3 False False 0 DamageKind.Noncombat))
    Spec.it s "GameEvent.StepBegan round-trips" $
      roundTrip s "step" gameEventToJson jsonToGameEvent (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)
    Spec.it s "GameEvent.SpellCast round-trips" $
      roundTrip s "ev" gameEventToJson jsonToGameEvent (GameEvent.SpellCast S.alice)
    Spec.it s "MonarchTarget" $ do
      roundTrip s "tc" monarchTargetToJson jsonToMonarchTarget MonarchTarget.TheController
      roundTrip s "cos" monarchTargetToJson jsonToMonarchTarget MonarchTarget.ControllerOfSource
    Spec.it s "GameEvent.BecameMonarch" $
      roundTrip s "bm" gameEventToJson jsonToGameEvent (GameEvent.BecameMonarch S.alice)
    -- CR 702.29c's event, carrying the incarnation the cycled card became.
    -- CR 702.29e: the typecycling filter rides the same keyword arm, absent
    -- for plain cycling -- so both spellings have to survive the trip.
    Spec.it s "Keyword.Cycling round-trips with and without a typecycling filter" $ do
      let cost = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
      roundTrip s "cyc" keywordToJson jsonToKeyword (Keyword.Cycling cost Nothing)
      roundTrip s "typecyc" keywordToJson jsonToKeyword (Keyword.Cycling cost (Just (Filter.Type.HasCardType CardType.Land)))
    Spec.it s "SearchDestination round-trips" $
      mapM_ (roundTrip s "dest" searchDestinationToJson jsonToSearchDestination) [SearchDestination.BattlefieldTapped, SearchDestination.RevealThenHand]
    -- CR 701.9a's event, carrying the incarnation the discarded card
    -- became. Both causes, because the cause is what tells a cycle from an
    -- ordinary discard (CR 702.29c) and a trip that flattened it would
    -- leave the two indistinguishable.
    Spec.it s "GameEvent.Discarded round-trips with either cause" $ do
      roundTrip s "disc" gameEventToJson jsonToGameEvent (GameEvent.Discarded S.alice (ObjectId.MkObjectId 7) DiscardCause.Ordinary)
      roundTrip s "cyc" gameEventToJson jsonToGameEvent (GameEvent.Discarded S.bob (ObjectId.MkObjectId 7) DiscardCause.ToPayCyclingCost)
    Spec.it s "DiscardCause round-trips" $
      mapM_ (roundTrip s "cause" discardCauseToJson jsonToDiscardCause) [DiscardCause.Ordinary, DiscardCause.ToPayCyclingCost]
    -- CR 701.20a: the reveal's whole payload IS the snapshot, so it is the
    -- one GameEvent whose round-trip failing would silently erase what the
    -- players were shown rather than merely mislabel it. Typhoid Rats for
    -- the reason the Moved case gives -- every snapshot field populated.
    Spec.it s "GameEvent.Revealed round-trips with its snapshot" $ do
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      let (ratId, gs) = S.addLibraryCard typhoidRats S.alice (Setup.emptyGame S.bothPlayers)
      roundTrip s "revealed" gameEventToJson jsonToGameEvent (GameEvent.Revealed S.alice (Projection.project ratId gs))
    Spec.it s "TriggerCondition.SelfCycled round-trips" $
      roundTrip s "sc" triggerConditionToJson jsonToTriggerCondition TriggerCondition.SelfCycled
    -- Both relations, since the PlayerRelation is the whole content of
    -- Megrim's "an OPPONENT discards a card".
    Spec.it s "TriggerCondition.PlayerDiscards round-trips both relations" $ do
      roundTrip s "pdo" triggerConditionToJson jsonToTriggerCondition (TriggerCondition.PlayerDiscards PlayerRelation.Opponent)
      roundTrip s "pdy" triggerConditionToJson jsonToTriggerCondition (TriggerCondition.PlayerDiscards PlayerRelation.You)
    Spec.it s "TriggerCondition.SelfAttacks round-trips both frequencies" $ do
      roundTrip s "sa" triggerConditionToJson jsonToTriggerCondition (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime)
      roundTrip s "sa1" triggerConditionToJson jsonToTriggerCondition (TriggerCondition.SelfAttacks TriggerFrequency.FirstTimeEachTurn)
    Spec.it s "GameEvent.AttackerDeclared round-trips" $
      roundTrip s "ad" gameEventToJson jsonToGameEvent (GameEvent.AttackerDeclared (ObjectId.MkObjectId 3))
    -- CR 701.6a's event. Three DISTINCT payload values, two of them
    -- ObjectIds: a trip that swapped the countered spell for the countering
    -- source would survive equal ids and fail here.
    Spec.it s "GameEvent.SpellCountered round-trips" $
      roundTrip
        s
        "countered"
        gameEventToJson
        jsonToGameEvent
        (GameEvent.SpellCountered (Countering.MkCountering (ObjectId.MkObjectId 4) (ObjectId.MkObjectId 5) S.bob))
    -- Both relations, for the reason the discard condition's case gives:
    -- the PlayerRelation is the whole content of Baral, Chief of
    -- Compliance's "a spell or ability YOU CONTROL counters a spell".
    Spec.it s "TriggerCondition.SpellOrAbilityCounters round-trips both relations" $ do
      roundTrip s "socy" triggerConditionToJson jsonToTriggerCondition (TriggerCondition.SpellOrAbilityCounters PlayerRelation.You)
      roundTrip s "soco" triggerConditionToJson jsonToTriggerCondition (TriggerCondition.SpellOrAbilityCounters PlayerRelation.Opponent)
    -- Create's TokenEntry is ELIDED when it is the CR 110.5b default, so
    -- the round trip has to hold for all four shapes the encoder emits --
    -- and the two three-element ones (a slot, or an entry) are told apart
    -- by JSON type alone, which is the part that could silently confuse
    -- them.
    Spec.it s "Effect.Create round-trips with and without a TokenEntry and a slot" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let card = Printing.card piker
          attacking = TokenEntry.MkTokenEntry {TokenEntry.tapped = TapState.Tapped, TokenEntry.attacking = True}
          plain = TokenEntry.MkTokenEntry {TokenEntry.tapped = TapState.Untapped, TokenEntry.attacking = False}
          slot = SlotName.MkSlotName (Text.pack "token")
      roundTrip s "plain" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Create (Quantity.Literal 2) card plain Nothing)
      roundTrip s "plain+slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Create (Quantity.Literal 1) card plain (Just slot))
      roundTrip s "entry" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Create (Quantity.Literal 2) card attacking Nothing)
      roundTrip s "entry+slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Create (Quantity.Literal 1) card attacking (Just slot))
      -- The elision itself: a default entry adds nothing to the payload,
      -- which is what keeps every token-making card file written before
      -- this one byte-identical.
      Spec.assertEqWith
        s
        "a default TokenEntry is not written"
        (J.tagged (Text.pack "Create") (Just (J.jArray [quantityToJson (Quantity.Literal 2), cardToJson card])))
        (effectToJson cardToJson (Effect.Create (Quantity.Literal 2) card plain Nothing))
    -- CR 113.6k's condition (Narcomoeba's), the first that names a zone
    -- pair rather than the battlefield.
    Spec.it s "TriggerCondition.SelfPutIntoGraveyardFromLibrary round-trips" $
      roundTrip s "spigfl" triggerConditionToJson jsonToTriggerCondition TriggerCondition.SelfPutIntoGraveyardFromLibrary
    -- CR 603.6c's condition (Doomed Traveler's), the other zone pair.
    Spec.it s "TriggerCondition.SelfDies round-trips" $
      roundTrip s "dies" triggerConditionToJson jsonToTriggerCondition TriggerCondition.SelfDies
    -- The same rule's wider written form (Thragtusk's), which is a separate tag
    -- rather than a payload on the one above: the two must never decode to each
    -- other.
    Spec.it s "TriggerCondition.SelfLeavesTheBattlefield round-trips" $
      roundTrip s "ltb" triggerConditionToJson jsonToTriggerCondition TriggerCondition.SelfLeavesTheBattlefield
    -- CR 603.6a's "[type]" is a whole Filter, so the nested And/Not that
    -- spells Soul Warden's "another creature" has to survive the trip.
    Spec.it s "TriggerCondition.PermanentEnters round-trips with its Filter" $
      roundTrip
        s
        "pe"
        triggerConditionToJson
        jsonToTriggerCondition
        (TriggerCondition.PermanentEnters (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.IsSource]))
    Spec.it s "TurnScope round-trips" $
      mapM_ (roundTrip s "scope" turnScopeToJson jsonToTurnScope) [TurnScope.EachTurn, TurnScope.ControllersTurn]
    Spec.it s "TriggerCondition.StepBegins round-trips" $
      roundTrip
        s
        "cond"
        triggerConditionToJson
        jsonToTriggerCondition
        (TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn)
    Spec.it s "Barbarian Outcast / Sarcomancy shaped Conditions round-trip" $
      mapM_
        (roundTrip s "condition" conditionToJson jsonToCondition)
        [S.youControlNoSwamps, noZombiesOnBattlefield]
    Spec.it s "TriggerCondition.StateIs round-trips" $
      roundTrip
        s
        "cond"
        triggerConditionToJson
        jsonToTriggerCondition
        (TriggerCondition.StateIs S.youControlNoSwamps)
    Spec.it s "CreatureDealtCombatDamageToMonarch" $
      roundTrip s "cd" triggerConditionToJson jsonToTriggerCondition TriggerCondition.CreatureDealtCombatDamageToMonarch
    Spec.it s "AbilityName round-trips" $
      roundTrip s "name" abilityNameToJson jsonToAbilityName (AbilityName.MkAbilityName (Text.pack "sacrifice it"))
    Spec.it s "ArmDelayedTrigger round-trips" $
      roundTrip s "arm" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ArmDelayedTrigger (AbilityName.MkAbilityName (Text.pack "sacrifice it")) Nothing)
    -- CR 603.7b's stated duration takes the two-element form; the absent
    -- one above must keep the bare shape, so both have to survive.
    Spec.it s "ArmDelayedTrigger round-trips a stated duration" $
      roundTrip s "arm1" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ArmDelayedTrigger (AbilityName.MkAbilityName (Text.pack "each combat")) (Just Duration.UntilEndOfTurn))
    -- M-5 (fix pass 1): the "DelayedTrigger round-trips" test below exercises
    -- only a Binding's `target` field via Binding.toObject. The codec is
    -- meant to be total over every Binding field -- subtypes, amount, modes,
    -- and copy too -- so round-trip a Binding with all five populated at
    -- once, exercising jsonToSubtypePair along the way. No real slot ever
    -- carries all five together (copy lives only under the dedicated
    -- copySource slot in practice); this is a codec totality check, not a
    -- claim about a reachable game state.
    Spec.it s "a Binding with every field populated round-trips" $
      let binding =
            Binding.Type.MkBinding
              { Binding.Type.target = Just (Recipient.ToPlayer S.alice),
                Binding.Type.subtypes = Just (Subtype.Mountain, Subtype.Island),
                Binding.Type.amount = Just 3,
                Binding.Type.modes = Just (Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2]),
                Binding.Type.copy = Just S.emptyCharacteristics
              }
       in roundTrip s "binding" bindingToJson jsonToBinding binding
    Spec.it s "DelayedTrigger round-trips with its captured bindings" $
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
                DelayedTrigger.bindings = Map.singleton (SlotName.MkSlotName (Text.pack "token")) (Binding.toObject (ObjectId.MkObjectId 9)),
                DelayedTrigger.expiry = Nothing
              }
       in do
            -- CR 603.7b's default and its stated-duration exception both
            -- have to survive: the absent expiry is elided to null, and a
            -- present one must come back as itself.
            roundTrip s "delayed" delayedTriggerToJson jsonToDelayedTrigger entry
            roundTrip s "delayed1" delayedTriggerToJson jsonToDelayedTrigger entry {DelayedTrigger.expiry = Just Expiry.AtCleanup}
    Spec.it s "a TriggeredAbility with an intervening if round-trips" $
      let ability =
            TriggeredAbility.MkTriggeredAbility
              { TriggeredAbility.condition = TriggerCondition.SelfEnters,
                TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1),
                TriggeredAbility.intervening = Just noZombiesOnBattlefield
              }
       in roundTrip s "ta" (triggeredAbilityToJson cardToJson) (jsonToTriggeredAbility jsonToCard) ability
  Spec.describe s "count + condition (M5.5 T2)" $ do
    Spec.it s "Count round-trips" $
      roundTrip
        s
        "count"
        (countToJson quantityToJson)
        (jsonToCount jsonToQuantity)
        ( Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.And [Filter.Type.HasSubtype Subtype.Swamp, Filter.Type.ControlledBy PlayerRelation.You])
            Aggregation.Objects
        )
    Spec.it s "Count over the event history round-trips" $
      roundTrip
        s
        "history"
        (countToJson quantityToJson)
        (jsonToCount jsonToQuantity)
        ( Count.Type.MkCount
            (Scope.InHistory (EventShape.MovedBetween Zone.Battlefield Zone.Graveyard))
            (Filter.Type.HasCardType CardType.Creature)
            Aggregation.DistinctCardTypes
        )
    Spec.it s "Count scoped to a slot round-trips" $
      roundTrip
        s
        "slot"
        (countToJson quantityToJson)
        (jsonToCount jsonToQuantity)
        ( Count.Type.MkCount
            (Scope.InZone Zone.Hand (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
            (Filter.Type.And [])
            Aggregation.Objects
        )
    Spec.it s "Quantity.Count round-trips (Task 5: shares the Count tag, not double-tagged)" $
      roundTrip
        s
        "qcount"
        quantityToJson
        jsonToQuantity
        ( Quantity.Count
            ( Count.Type.MkCount
                (Scope.InZone Zone.Graveyard PlayerRef.EachPlayer)
                (Filter.Type.And [])
                Aggregation.DistinctCardTypes
            )
        )
    -- One with the Machine's aggregation, and the arm that proves the
    -- payload is a whole Quantity rather than a nullary tag: a Greatest
    -- whose per-member quantity is itself a Count round-trips, which is
    -- the recursion Pawl.Types.Quantity's parameter exists to permit.
    Spec.it s "Greatest round-trips, including a nested Count payload" $ do
      roundTrip
        s
        "greatest"
        (countToJson quantityToJson)
        (jsonToCount jsonToQuantity)
        ( Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.And [Filter.Type.HasCardType CardType.Artifact, Filter.Type.ControlledBy PlayerRelation.You])
            (Aggregation.Greatest Quantity.ManaValue)
        )
      roundTrip
        s
        "greatest nested"
        (countToJson quantityToJson)
        (jsonToCount jsonToQuantity)
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
        )
    Spec.it s "Condition round-trips at every comparison" $
      mapM_
        (roundTrip s "condition" conditionToJson jsonToCondition)
        [ Condition.Type.MkCondition (Quantity.Count zeroSwamps) Comparison.Exactly (Quantity.Literal 0),
          Condition.Type.MkCondition (Quantity.Count zeroSwamps) Comparison.AtLeast (Quantity.Literal 3),
          Condition.Type.MkCondition (Quantity.Count zeroSwamps) Comparison.AtMost (Quantity.Literal 1),
          -- Both sides non-Count, which the Count-on-the-left shape could
          -- not say at all: Deathknell Berserker's "if its power was 3 or
          -- greater" (CR 603.4).
          Condition.Type.MkCondition Quantity.Power Comparison.AtLeast (Quantity.Literal 3)
        ]
  -- Pawl.Types.Effect is parametric in `card` so that Pawl.Types stays an
  -- acyclic module graph, and the codec mirrors that: the encoder reaches
  -- its card payload ONLY through the codec it is handed. Proving it at two
  -- different `card` types is what lets Pawl.Codec.Effect sit below
  -- Pawl.Codec.Card rather than in a cycle with it (#481).
  Spec.describe s "parametricity" $ do
    Spec.it s "effectToJson reaches card only through the supplied codec" $
      Spec.assertEqWith
        s
        "the emblem payload comes from the argument, not the card"
        (effectToJson (const sentinel) (Effect.CreateEmblem "a wholly different card type"))
        (effectToJson (const sentinel) (Effect.CreateEmblem ()))

-- The stand-in a parametricity test hands over in place of a real card codec:
-- any Value at all, so long as both instantiations are given the same one.
sentinel :: Value.Value
sentinel = J.jText (Text.pack "SENTINEL")

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
    ( Quantity.Count
        ( Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.HasSubtype Subtype.Zombie)
            Aggregation.Objects
        )
    )
    Comparison.Exactly
    (Quantity.Literal 0)
