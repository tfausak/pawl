-- | The sole authority for @Card ⇆ Json@ (§2 of the M3.5 spec), mirroring
-- 'Pawl.Engine.Resolve' (the sole @case@-on-@Effect@ home). Free @xToJson@\/@jsonToX@
-- functions -- no type classes -- over the transitive closure of @Card@'s
-- fields. Every @Pawl.Types.*@ module stays JSON-free; casing on an effect's
-- identity here is open-half machinery, not the rules core.
module Pawl.Codec.All (module Pawl.Codec.All, module Pawl.Codec.AbilityName, module Pawl.Codec.ActivationTiming, module Pawl.Codec.Affected, module Pawl.Codec.Aggregation, module Pawl.Codec.AttackRequirement, module Pawl.Codec.BeginningStep, module Pawl.Codec.BlockRequirement, module Pawl.Codec.CardType, module Pawl.Codec.CastingPermission, module Pawl.Codec.CastingRestriction, module Pawl.Codec.Color, module Pawl.Codec.CombatStep, module Pawl.Codec.Comparison, module Pawl.Codec.Condition, module Pawl.Codec.ControllerRelation, module Pawl.Codec.Cost, module Pawl.Codec.CostComponent, module Pawl.Codec.Count, module Pawl.Codec.CounterKind, module Pawl.Codec.CounterPattern, module Pawl.Codec.Counterability, module Pawl.Codec.DamageEvent, module Pawl.Codec.DamageKind, module Pawl.Codec.DamagePattern, module Pawl.Codec.DamageRewrite, module Pawl.Codec.DestructionRewrite, module Pawl.Codec.DiscardCause, module Pawl.Codec.Duration, module Pawl.Codec.EndingStep, module Pawl.Codec.EntryOption, module Pawl.Codec.EntryRewrite, module Pawl.Codec.EventShape, module Pawl.Codec.Expiry, module Pawl.Codec.ExtraPhase, module Pawl.Codec.Filter, module Pawl.Codec.Keyword, module Pawl.Codec.ManaCost, module Pawl.Codec.ManaProduction, module Pawl.Codec.ManaSymbol, module Pawl.Codec.ManaType, module Pawl.Codec.ModeIndex, module Pawl.Codec.ModeSelection, module Pawl.Codec.Modification, module Pawl.Codec.MonarchTarget, module Pawl.Codec.ObjectId, module Pawl.Codec.ObjectRef, module Pawl.Codec.Optionality, module Pawl.Codec.Phase, module Pawl.Codec.PhasePattern, module Pawl.Codec.PhaseSelector, module Pawl.Codec.PlayerCounterKind, module Pawl.Codec.PlayerEffect, module Pawl.Codec.PlayerId, module Pawl.Codec.PlayerRef, module Pawl.Codec.PlayerRelation, module Pawl.Codec.PlayerScope, module Pawl.Codec.PlayerStaticAbility, module Pawl.Codec.Pool, module Pawl.Codec.Power, module Pawl.Codec.Quantity, module Pawl.Codec.Recipient, module Pawl.Codec.Regenerability, module Pawl.Codec.ReplacementEffect, module Pawl.Codec.Scaling, module Pawl.Codec.Scope, module Pawl.Codec.SearchDestination, module Pawl.Codec.SlotName, module Pawl.Codec.StaticAbility, module Pawl.Codec.Subtype, module Pawl.Codec.Supertype, module Pawl.Codec.TapState, module Pawl.Codec.TargetSpec, module Pawl.Codec.TokenEntry, module Pawl.Codec.TokenPattern, module Pawl.Codec.Toughness, module Pawl.Codec.TriggerCondition, module Pawl.Codec.TriggerFrequency, module Pawl.Codec.TurnScope, module Pawl.Codec.TypeLine, module Pawl.Codec.Uses, module Pawl.Codec.Zone, module Pawl.Codec.ZoneChange, module Pawl.Codec.ZoneChangePattern, module Pawl.Codec.ZoneChangeSubject) where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.AbilityName (abilityNameToJson, jsonToAbilityName)
import Pawl.Codec.ActivationTiming (activationTimingToJson, jsonToActivationTiming)
import Pawl.Codec.Affected (affectedToJson, jsonToAffected)
import Pawl.Codec.Aggregation (aggregationToJson, jsonToAggregation)
import Pawl.Codec.AttackRequirement (attackRequirementToJson, jsonToAttackRequirement)
import Pawl.Codec.BeginningStep (beginningStepToJson, jsonToBeginningStep)
import Pawl.Codec.BlockRequirement (blockRequirementToJson, jsonToBlockRequirement)
import Pawl.Codec.CardType (cardTypeToJson, jsonToCardType)
import Pawl.Codec.CastingPermission (castingPermissionToJson, jsonToCastingPermission)
import Pawl.Codec.CastingRestriction (castingRestrictionToJson, jsonToCastingRestriction)
import Pawl.Codec.Color (colorToJson, jsonToColor)
import Pawl.Codec.CombatStep (combatStepToJson, jsonToCombatStep)
import Pawl.Codec.Comparison (comparisonToJson, jsonToComparison)
import Pawl.Codec.Condition (conditionToJson, jsonToCondition)
import Pawl.Codec.ControllerRelation (controllerRelationToJson, jsonToControllerRelation)
import Pawl.Codec.Cost (costToJson, jsonToCost)
import Pawl.Codec.CostComponent (costComponentToJson, jsonToCostComponent)
import Pawl.Codec.Count (countToJson, jsonToCount)
import Pawl.Codec.CounterKind (counterKindToJson, jsonToCounterKind)
import Pawl.Codec.CounterPattern (counterPatternToJson, jsonToCounterPattern)
import Pawl.Codec.Counterability (counterabilityToJson, jsonToCounterability, jsonToCounterabilityDefault)
import Pawl.Codec.DamageEvent (damageEventToJson, jsonToDamageEvent)
import Pawl.Codec.DamageKind (damageKindToJson, jsonToDamageKind)
import Pawl.Codec.DamagePattern (damagePatternToJson, jsonToDamagePattern)
import Pawl.Codec.DamageRewrite (damageRewriteToJson, jsonToDamageRewrite)
import Pawl.Codec.DestructionRewrite (destructionRewriteToJson, jsonToDestructionRewrite)
import Pawl.Codec.DiscardCause (discardCauseToJson, jsonToDiscardCause)
import Pawl.Codec.Duration (durationToJson, jsonToDuration)
import Pawl.Codec.EndingStep (endingStepToJson, jsonToEndingStep)
import Pawl.Codec.EntryOption (entryOptionToJson, jsonToEntryOption)
import Pawl.Codec.EntryRewrite (entryRewriteToJson, jsonToEntryRewrite)
import Pawl.Codec.EventShape (eventShapeToJson, jsonToEventShape)
import Pawl.Codec.Expiry (expiryToJson, jsonToExpiry)
import Pawl.Codec.ExtraPhase (extraPhaseToJson, jsonToExtraPhase)
import Pawl.Codec.Filter (filterToJson, jsonToFilter, optionalFilter)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Keyword (jsonToKeyword, keywordToJson)
import Pawl.Codec.ManaCost (jsonToManaCost, manaCostToJson)
import Pawl.Codec.ManaProduction (jsonToManaProduction, manaProductionToJson)
import Pawl.Codec.ManaSymbol (jsonToManaSymbol, manaSymbolToJson)
import Pawl.Codec.ManaType (jsonToManaType, manaTypeToJson)
import Pawl.Codec.ModeIndex (jsonToModeIndex, modeIndexToJson)
import Pawl.Codec.ModeSelection (jsonToModeSelection, modeSelectionToJson)
import Pawl.Codec.Modification (jsonToModification, modificationToJson)
import Pawl.Codec.MonarchTarget (jsonToMonarchTarget, monarchTargetToJson)
import Pawl.Codec.ObjectId (jsonToObjectId, objectIdToJson)
import Pawl.Codec.ObjectRef (jsonToObjectRef, objectRefToJson)
import Pawl.Codec.Optionality (jsonToOptionality, jsonToOptionalityDefault, optionalityToJson)
import Pawl.Codec.Phase (jsonToPhase, phaseToJson)
import Pawl.Codec.PhasePattern (jsonToPhasePattern, phasePatternToJson)
import Pawl.Codec.PhaseSelector (jsonToPhaseSelector, phaseSelectorToJson)
import Pawl.Codec.PlayerCounterKind (jsonToPlayerCounterKind, playerCounterKindToJson)
import Pawl.Codec.PlayerEffect (jsonToPlayerEffect, playerEffectToJson)
import Pawl.Codec.PlayerId (jsonToPlayerId, playerIdToJson)
import Pawl.Codec.PlayerRef (jsonToPlayerRef, playerRefToJson)
import Pawl.Codec.PlayerRelation (jsonToPlayerRelation, playerRelationToJson)
import Pawl.Codec.PlayerScope (jsonToPlayerScope, playerScopeToJson)
import Pawl.Codec.PlayerStaticAbility (jsonToPlayerStaticAbility, playerStaticAbilityToJson)
import Pawl.Codec.Pool (jsonToPool, poolToJson)
import Pawl.Codec.Power (jsonToPower, powerToJson)
import Pawl.Codec.Quantity (jsonToQuantity, jsonToQuantityPair, quantityToJson)
import Pawl.Codec.Recipient (jsonToRecipient, recipientToJson)
import Pawl.Codec.Regenerability (jsonToRegenerability, regenerabilityToJson)
import Pawl.Codec.ReplacementEffect (jsonToReplacementEffect, replacementEffectToJson)
import Pawl.Codec.Scaling (jsonToScaling, scalingToJson)
import Pawl.Codec.Scope (jsonToScope, scopeToJson)
import Pawl.Codec.SearchDestination (jsonToSearchDestination, searchDestinationToJson)
import Pawl.Codec.SlotName (jsonToSlotName, slotNameToJson)
import Pawl.Codec.StaticAbility (jsonToStaticAbility, staticAbilityToJson)
import Pawl.Codec.Subtype (jsonToSubtype, jsonToSubtypePair, subtypeToJson)
import Pawl.Codec.Supertype (jsonToSupertype, supertypeToJson)
import Pawl.Codec.TapState (jsonToTapState, tapStateToJson)
import Pawl.Codec.TargetSpec (jsonToTargetSpec, jsonToTargetSpecs, targetSpecToJson, targetSpecsToJson)
import Pawl.Codec.TokenEntry (defaultTokenEntry, jsonToTokenEntry, tokenEntryToJson)
import Pawl.Codec.TokenPattern (jsonToTokenPattern, tokenPatternToJson)
import Pawl.Codec.Toughness (jsonToToughness, toughnessToJson)
import Pawl.Codec.TriggerCondition (jsonToTriggerCondition, triggerConditionToJson)
import Pawl.Codec.TriggerFrequency (jsonToTriggerFrequency, triggerFrequencyToJson)
import Pawl.Codec.TurnScope (jsonToTurnScope, turnScopeToJson)
import Pawl.Codec.TypeLine (jsonToTypeLine, typeLineToJson)
import Pawl.Codec.Uses (jsonToUses, usesToJson)
import Pawl.Codec.Zone (jsonToZone, zoneToJson)
import Pawl.Codec.ZoneChange (jsonToZoneChange, zoneChangeToJson)
import Pawl.Codec.ZoneChangePattern (jsonToZoneChangePattern, zoneChangePatternToJson)
import Pawl.Codec.ZoneChangeSubject (jsonToZoneChangeSubject, zoneChangeSubjectToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array, Object))
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.Card as CardT
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- Helpers --------------------------------------------------------------------

-- Leaf enums -----------------------------------------------------------------

-- Modification, affected -----------------------------------------------------

projectedCharacteristicsToJson :: PC.ProjectedCharacteristics -> Value
projectedCharacteristicsToJson pc =
  Json.jObject
    [ (Text.pack "name", Json.jText (PC.name pc)),
      (Text.pack "supertypes", Json.setTo supertypeToJson (PC.supertypes pc)),
      (Text.pack "keywords", Json.multisetTo keywordToJson (PC.keywords pc)),
      (Text.pack "colors", Json.setTo colorToJson (PC.colors pc)),
      (Text.pack "power", Json.maybeTo Json.jInt (PC.power pc)),
      (Text.pack "toughness", Json.maybeTo Json.jInt (PC.toughness pc)),
      (Text.pack "characteristicPT", Json.maybeTo (\(p, t) -> Array (MkArray [quantityToJson p, quantityToJson t])) (PC.characteristicPT pc)),
      (Text.pack "cardTypes", Json.setTo cardTypeToJson (PC.cardTypes pc)),
      (Text.pack "subtypes", Json.setTo subtypeToJson (PC.subtypes pc)),
      (Text.pack "activatedAbilities", Json.listTo (activatedAbilityToJson cardToJson) (PC.activatedAbilities pc)),
      (Text.pack "replacementEffects", Json.listTo replacementEffectToJson (PC.replacementEffects pc)),
      (Text.pack "triggeredAbilities", Json.listTo (triggeredAbilityToJson cardToJson) (PC.triggeredAbilities pc))
    ]

jsonToProjectedCharacteristics :: Value -> Either Text PC.ProjectedCharacteristics
jsonToProjectedCharacteristics value = do
  ps <- Json.asObject value
  nm <- Json.field (Text.pack "name") ps >>= Json.asText
  sups <- Json.field (Text.pack "supertypes") ps >>= Json.setFrom jsonToSupertype
  kws <- Json.field (Text.pack "keywords") ps >>= Json.multisetFrom jsonToKeyword
  cols <- Json.field (Text.pack "colors") ps >>= Json.setFrom jsonToColor
  -- power/toughness/characteristicPT are encoded as required keys (Json.maybeTo
  -- writes JSON null for Nothing, never omits the key), so decoding them is
  -- Json.field (required) >>= Json.maybeFrom (Null -> Nothing), exactly like every
  -- other field here -- not the optional Json.getOpt a truly-omittable key would need.
  pow <- Json.field (Text.pack "power") ps >>= Json.maybeFrom Json.asInteger
  tou <- Json.field (Text.pack "toughness") ps >>= Json.maybeFrom Json.asInteger
  cda <- Json.field (Text.pack "characteristicPT") ps >>= Json.maybeFrom jsonToQuantityPair
  cts <- Json.field (Text.pack "cardTypes") ps >>= Json.setFrom jsonToCardType
  subs <- Json.field (Text.pack "subtypes") ps >>= Json.setFrom jsonToSubtype
  acts <- Json.field (Text.pack "activatedAbilities") ps >>= Json.listFrom (jsonToActivatedAbility jsonToCard)
  reps <- Json.field (Text.pack "replacementEffects") ps >>= Json.listFrom jsonToReplacementEffect
  trigs <- Json.field (Text.pack "triggeredAbilities") ps >>= Json.listFrom (jsonToTriggeredAbility jsonToCard)
  pure
    PC.MkProjectedCharacteristics
      { PC.name = nm,
        PC.supertypes = sups,
        PC.keywords = kws,
        PC.colors = cols,
        PC.power = pow,
        PC.toughness = tou,
        PC.characteristicPT = cda,
        PC.cardTypes = cts,
        PC.subtypes = subs,
        PC.activatedAbilities = acts,
        PC.replacementEffects = reps,
        PC.triggeredAbilities = trigs
      }

gameEventToJson :: GameEvent.GameEvent -> Value
gameEventToJson e = case e of
  GameEvent.Moved zc pc -> Json.tagged (Text.pack "Moved") (Just (Array (MkArray [zoneChangeToJson zc, projectedCharacteristicsToJson pc])))
  GameEvent.DamageDealt ev -> Json.tagged (Text.pack "DamageDealt") (Just (damageEventToJson ev))
  GameEvent.StepBegan p pid -> Json.tagged (Text.pack "StepBegan") (Just (Array (MkArray [phaseToJson p, playerIdToJson pid])))
  GameEvent.SpellCast pid -> Json.tagged (Text.pack "SpellCast") (Just (playerIdToJson pid))
  GameEvent.BecameMonarch pid -> Json.tagged (Text.pack "BecameMonarch") (Just (playerIdToJson pid))
  GameEvent.Discarded pid oid cause ->
    Json.tagged (Text.pack "Discarded") (Just (Array (MkArray [playerIdToJson pid, objectIdToJson oid, discardCauseToJson cause])))
  GameEvent.Revealed pid pc -> Json.tagged (Text.pack "Revealed") (Just (Array (MkArray [playerIdToJson pid, projectedCharacteristicsToJson pc])))
  GameEvent.AttackerDeclared oid -> Json.tagged (Text.pack "AttackerDeclared") (Just (objectIdToJson oid))

jsonToGameEvent :: Value -> Either Text GameEvent.GameEvent
jsonToGameEvent value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Moved", Just (Array (MkArray [zc, pc]))) -> GameEvent.Moved <$> jsonToZoneChange zc <*> jsonToProjectedCharacteristics pc
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> jsonToDamageEvent v
    ("StepBegan", Just (Array (MkArray [p, pid]))) -> GameEvent.StepBegan <$> jsonToPhase p <*> jsonToPlayerId pid
    ("SpellCast", Just v) -> GameEvent.SpellCast <$> jsonToPlayerId v
    ("BecameMonarch", Just v) -> GameEvent.BecameMonarch <$> jsonToPlayerId v
    ("Discarded", Just (Array (MkArray [pid, oid, cause]))) ->
      GameEvent.Discarded <$> jsonToPlayerId pid <*> jsonToObjectId oid <*> jsonToDiscardCause cause
    ("Revealed", Just (Array (MkArray [pid, pc]))) -> GameEvent.Revealed <$> jsonToPlayerId pid <*> jsonToProjectedCharacteristics pc
    ("AttackerDeclared", Just v) -> GameEvent.AttackerDeclared <$> jsonToObjectId v
    _ -> Left (Text.pack "unknown GameEvent: " <> t)

-- MonarchTarget ----------------------------------------------------------------

-- TokenEntry -----------------------------------------------------------------

-- Effect ---------------------------------------------------------------------

effectToJson :: (card -> Value) -> Effect.Effect card -> Value
effectToJson codec e = case e of
  Effect.DealDamage s q -> Json.tagged (Text.pack "DealDamage") (Just (Array (MkArray [slotNameToJson s, quantityToJson q])))
  Effect.ModifyTarget d m r -> Json.tagged (Text.pack "ModifyTarget") (Just (Array (MkArray [durationToJson d, modificationToJson m, objectRefToJson r])))
  Effect.ChangeText s -> Json.tagged (Text.pack "ChangeText") (Just (slotNameToJson s))
  Effect.AddMana production -> Json.tagged (Text.pack "AddMana") (Just (manaProductionToJson production))
  Effect.Search f d -> Json.tagged (Text.pack "Search") (Just (Array (MkArray [filterToJson f, searchDestinationToJson d])))
  Effect.ExileAllGraveyards -> Json.nullary (Text.pack "ExileAllGraveyards")
  Effect.Proliferate -> Json.nullary (Text.pack "Proliferate")
  Effect.ExileHandThenDraw -> Json.nullary (Text.pack "ExileHandThenDraw")
  Effect.PlayerSacrifices slot f q -> Json.tagged (Text.pack "PlayerSacrifices") (Just (Array (MkArray [slotNameToJson slot, filterToJson f, quantityToJson q])))
  Effect.RestartGame -> Json.nullary (Text.pack "RestartGame")
  Effect.ControlPlayerNextTurn s -> Json.tagged (Text.pack "ControlPlayerNextTurn") (Just (slotNameToJson s))
  -- The bound-count slot is ELIDED when absent, the posture Create's TokenEntry
  -- and ArmDelayedTrigger's duration take, so every card that says nothing about
  -- counting its sweep stays byte-for-byte as it was written.
  Effect.Destroy s r ms ->
    Json.tagged (Text.pack "Destroy") . Just . Array . MkArray $
      [objectRefToJson s, regenerabilityToJson r] <> fmap slotNameToJson (Maybe.maybeToList ms)
  Effect.Sacrifice s -> Json.tagged (Text.pack "Sacrifice") (Just (slotNameToJson s))
  Effect.RemoveFromCombat s -> Json.tagged (Text.pack "RemoveFromCombat") (Just (slotNameToJson s))
  Effect.Counter s -> Json.tagged (Text.pack "Counter") (Just (slotNameToJson s))
  Effect.MoveToZone s z -> Json.tagged (Text.pack "MoveToZone") (Just (Array (MkArray [slotNameToJson s, zoneToJson z])))
  Effect.Draw r q -> Json.tagged (Text.pack "Draw") (Just (Array (MkArray [playerRefToJson r, quantityToJson q])))
  Effect.Mill s q -> Json.tagged (Text.pack "Mill") (Just (Array (MkArray [slotNameToJson s, quantityToJson q])))
  Effect.Discard s q -> Json.tagged (Text.pack "Discard") (Just (Array (MkArray [slotNameToJson s, quantityToJson q])))
  Effect.LoseLife r q -> Json.tagged (Text.pack "LoseLife") (Just (Array (MkArray [playerRefToJson r, quantityToJson q])))
  Effect.GainLife r q -> Json.tagged (Text.pack "GainLife") (Just (Array (MkArray [playerRefToJson r, quantityToJson q])))
  -- Create's payload is positional, and the TokenEntry is ELIDED when it is the
  -- CR 110.5b default (defaultTokenEntry) -- the same posture `counterability`
  -- takes, so a card file that says nothing about how its tokens enter stays
  -- exactly as it was written. The three-element form is therefore two shapes,
  -- told apart on decode by JSON TYPE rather than by position: a slot name is a
  -- string (slotNameToJson) and a TokenEntry is an object, so the two can never
  -- be confused.
  Effect.Create q c te ms ->
    Json.tagged (Text.pack "Create") . Just . Array . MkArray $
      [quantityToJson q, codec c]
        <> (if te == defaultTokenEntry then [] else [tokenEntryToJson te])
        <> fmap slotNameToJson (Maybe.maybeToList ms)
  Effect.Replace d u re -> Json.tagged (Text.pack "Replace") (Just (Array (MkArray [durationToJson d, usesToJson u, replacementEffectToJson re])))
  Effect.SkipNextPhase r sel -> Json.tagged (Text.pack "SkipNextPhase") (Just (Array (MkArray [playerRefToJson r, phaseSelectorToJson sel])))
  Effect.PutCounters k q s -> Json.tagged (Text.pack "PutCounters") (Just (Array (MkArray [counterKindToJson k, quantityToJson q, slotNameToJson s])))
  Effect.GainPlayerCounters r k q -> Json.tagged (Text.pack "GainPlayerCounters") (Just (Array (MkArray [playerRefToJson r, playerCounterKindToJson k, quantityToJson q])))
  Effect.Tap r -> Json.tagged (Text.pack "Tap") (Just (objectRefToJson r))
  Effect.Untap r -> Json.tagged (Text.pack "Untap") (Just (objectRefToJson r))
  Effect.AddPhases ps -> Json.tagged (Text.pack "AddPhases") (Just (Array (MkArray (fmap extraPhaseToJson ps))))
  Effect.GainControl d r -> Json.tagged (Text.pack "GainControl") (Just (Array (MkArray [durationToJson d, objectRefToJson r])))
  -- The duration is ELIDED when absent, which is CR 603.7b's default -- so
  -- Tidal Wave's one-shot entry stays a bare ability name and only a card that
  -- states a duration writes the two-element form.
  Effect.ArmDelayedTrigger n md ->
    Json.tagged (Text.pack "ArmDelayedTrigger") . Just $ case md of
      Nothing -> abilityNameToJson n
      Just d -> Array (MkArray [abilityNameToJson n, durationToJson d])
  Effect.AffectPlayers d s pe -> Json.tagged (Text.pack "AffectPlayers") (Just (Array (MkArray [durationToJson d, playerScopeToJson s, playerEffectToJson pe])))
  Effect.CreateEmblem c -> Json.tagged (Text.pack "CreateEmblem") (Just (codec c))
  Effect.BecomeMonarch t -> Json.tagged (Text.pack "BecomeMonarch") (Just (monarchTargetToJson t))
  Effect.ExileUntilMonarch s -> Json.tagged (Text.pack "ExileUntilMonarch") (Just (slotNameToJson s))
  Effect.Attach s -> Json.tagged (Text.pack "Attach") (Just (slotNameToJson s))
  Effect.AttachTarget s f -> Json.tagged (Text.pack "AttachTarget") (Just (Array (MkArray [slotNameToJson s, filterToJson f])))
  Effect.PlaySubgame s -> Json.tagged (Text.pack "PlaySubgame") (Just (slotNameToJson s))
  Effect.TakeExtraTurn r -> Json.tagged (Text.pack "TakeExtraTurn") (Just (playerRefToJson r))

jsonToEffect :: (Value -> Either Text card) -> Value -> Either Text (Effect.Effect card)
jsonToEffect decode value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "DealDamage" -> case mv of
      Just (Array (MkArray [s, q])) -> Effect.DealDamage <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "DealDamage expects [slot, quantity]")
    "ModifyTarget" -> case mv of
      Just (Array (MkArray [d, m, r])) -> Effect.ModifyTarget <$> jsonToDuration d <*> jsonToModification m <*> jsonToObjectRef r
      _ -> Left (Text.pack "ModifyTarget expects [duration, modification, objectRef]")
    "ChangeText" -> Json.withValue mv (fmap Effect.ChangeText . jsonToSlotName)
    "AddMana" -> Json.withValue mv (fmap Effect.AddMana . jsonToManaProduction)
    "Search" -> case mv of
      Just (Array (MkArray [f, d])) -> Effect.Search <$> jsonToFilter f <*> jsonToSearchDestination d
      _ -> Left (Text.pack "Search expects [filter, destination]")
    "ExileAllGraveyards" -> Right Effect.ExileAllGraveyards
    "Proliferate" -> Right Effect.Proliferate
    "ExileHandThenDraw" -> Right Effect.ExileHandThenDraw
    "PlayerSacrifices" -> case mv of
      Just (Array (MkArray [sv, fv, qv])) -> Effect.PlayerSacrifices <$> jsonToSlotName sv <*> jsonToFilter fv <*> jsonToQuantity qv
      _ -> Left (Text.pack "PlayerSacrifices expects [slot, filter, quantity]")
    "RestartGame" -> Right Effect.RestartGame
    "ControlPlayerNextTurn" -> Json.withValue mv (fmap Effect.ControlPlayerNextTurn . jsonToSlotName)
    "Destroy" -> case mv of
      Just (Array (MkArray [sv, rv])) -> Effect.Destroy <$> jsonToObjectRef sv <*> jsonToRegenerability rv <*> pure Nothing
      Just (Array (MkArray [sv, rv, nv])) -> Effect.Destroy <$> jsonToObjectRef sv <*> jsonToRegenerability rv <*> (Just <$> jsonToSlotName nv)
      _ -> Left (Text.pack "Destroy expects [objectRef, regenerability], optionally with a slot")
    "Sacrifice" -> Json.withValue mv (fmap Effect.Sacrifice . jsonToSlotName)
    "RemoveFromCombat" -> Json.withValue mv (fmap Effect.RemoveFromCombat . jsonToSlotName)
    "Counter" -> Json.withValue mv (fmap Effect.Counter . jsonToSlotName)
    "MoveToZone" -> case mv of
      Just (Array (MkArray [s, z])) -> Effect.MoveToZone <$> jsonToSlotName s <*> jsonToZone z
      _ -> Left (Text.pack "MoveToZone expects [slot, zone]")
    "Draw" -> case mv of
      Just (Array (MkArray [r, q])) -> Effect.Draw <$> jsonToPlayerRef r <*> jsonToQuantity q
      _ -> Left (Text.pack "Draw expects [playerRef, quantity]")
    "Mill" -> case mv of
      Just (Array (MkArray [s, q])) -> Effect.Mill <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Mill expects [slot, quantity]")
    "Discard" -> case mv of
      Just (Array (MkArray [s, q])) -> Effect.Discard <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Discard expects [slot, quantity]")
    "LoseLife" -> case mv of
      Just (Array (MkArray [r, q])) -> Effect.LoseLife <$> jsonToPlayerRef r <*> jsonToQuantity q
      _ -> Left (Text.pack "LoseLife expects [playerRef, quantity]")
    "GainLife" -> case mv of
      Just (Array (MkArray [r, q])) -> Effect.GainLife <$> jsonToPlayerRef r <*> jsonToQuantity q
      _ -> Left (Text.pack "GainLife expects [playerRef, quantity]")
    -- The four shapes the encoder above can emit. The three-element one is read
    -- by JSON type: an Object is the TokenEntry, anything else is the slot name
    -- (a string), which is what lets the entry be elided when it is the default
    -- without a hole in the array.
    "Create" -> case mv of
      Just (Array (MkArray [q, c])) -> Effect.Create <$> jsonToQuantity q <*> decode c <*> pure defaultTokenEntry <*> pure Nothing
      Just (Array (MkArray [q, c, e@(Object _)])) -> Effect.Create <$> jsonToQuantity q <*> decode c <*> jsonToTokenEntry e <*> pure Nothing
      Just (Array (MkArray [q, c, s])) -> Effect.Create <$> jsonToQuantity q <*> decode c <*> pure defaultTokenEntry <*> (Just <$> jsonToSlotName s)
      Just (Array (MkArray [q, c, e, s])) -> Effect.Create <$> jsonToQuantity q <*> decode c <*> jsonToTokenEntry e <*> (Just <$> jsonToSlotName s)
      _ -> Left (Text.pack "Create expects [Quantity, Card], optionally with a TokenEntry and/or a slot")
    "ArmDelayedTrigger" -> case mv of
      Just (Array (MkArray [n, d])) -> Effect.ArmDelayedTrigger <$> jsonToAbilityName n <*> fmap Just (jsonToDuration d)
      _ -> Json.withValue mv (fmap (`Effect.ArmDelayedTrigger` Nothing) . jsonToAbilityName)
    "Replace" -> case mv of
      Just (Array (MkArray [d, u, re])) -> do
        duration <- jsonToDuration d
        uses <- jsonToUses u
        effect <- jsonToReplacementEffect re
        pure (Effect.Replace duration uses effect)
      _ -> Left (Text.pack "Replace expects [Duration, Uses, ReplacementEffect]")
    "SkipNextPhase" -> case mv of
      Just (Array (MkArray [r, sel])) -> Effect.SkipNextPhase <$> jsonToPlayerRef r <*> jsonToPhaseSelector sel
      _ -> Left (Text.pack "SkipNextPhase expects [playerRef, phaseSelector]")
    "PutCounters" -> case mv of
      Just (Array (MkArray [k, q, s])) -> Effect.PutCounters <$> jsonToCounterKind k <*> jsonToQuantity q <*> jsonToSlotName s
      _ -> Left (Text.pack "PutCounters expects [counterKind, quantity, slot]")
    "GainPlayerCounters" -> case mv of
      Just (Array (MkArray [r, k, q])) -> Effect.GainPlayerCounters <$> jsonToPlayerRef r <*> jsonToPlayerCounterKind k <*> jsonToQuantity q
      _ -> Left (Text.pack "GainPlayerCounters expects [playerRef, playerCounterKind, quantity]")
    "Tap" -> Json.withValue mv (fmap Effect.Tap . jsonToObjectRef)
    "Untap" -> Json.withValue mv (fmap Effect.Untap . jsonToObjectRef)
    "AddPhases" -> case mv of
      Just (Array (MkArray ps)) -> Effect.AddPhases <$> traverse jsonToExtraPhase ps
      _ -> Left (Text.pack "AddPhases expects [ExtraPhase]")
    "GainControl" -> case mv of
      Just (Array (MkArray [d, r])) -> Effect.GainControl <$> jsonToDuration d <*> jsonToObjectRef r
      _ -> Left (Text.pack "GainControl expects [duration, objectRef]")
    "AffectPlayers" -> case mv of
      Just (Array (MkArray [d, s, pe])) -> Effect.AffectPlayers <$> jsonToDuration d <*> jsonToPlayerScope s <*> jsonToPlayerEffect pe
      _ -> Left (Text.pack "AffectPlayers expects [Duration, PlayerScope, PlayerEffect]")
    "CreateEmblem" -> Json.withValue mv (fmap Effect.CreateEmblem . decode)
    "BecomeMonarch" -> Json.withValue mv (fmap Effect.BecomeMonarch . jsonToMonarchTarget)
    "ExileUntilMonarch" -> Json.withValue mv (fmap Effect.ExileUntilMonarch . jsonToSlotName)
    "Attach" -> Json.withValue mv (fmap Effect.Attach . jsonToSlotName)
    "AttachTarget" -> case mv of
      Just (Array (MkArray [s, f])) -> Effect.AttachTarget <$> jsonToSlotName s <*> jsonToFilter f
      _ -> Left (Text.pack "AttachTarget expects [slot, filter]")
    "PlaySubgame" -> Json.withValue mv (fmap Effect.PlaySubgame . jsonToSlotName)
    "TakeExtraTurn" -> Json.withValue mv (fmap Effect.TakeExtraTurn . jsonToPlayerRef)
    _ -> Left (Text.pack "unknown Effect: " <> t)

-- Records & abilities --------------------------------------------------------

activatedAbilityToJson :: (card -> Value) -> ActivatedAbility.ActivatedAbility card -> Value
activatedAbilityToJson codec aa =
  Json.jObject $
    [ (Text.pack "cost", costToJson (ActivatedAbility.cost aa)),
      (Text.pack "modal", modalToJson codec (ActivatedAbility.modal aa))
    ]
      -- CR 307.5: emitted only for a restricted ability, so the absence of the
      -- key means "no timing rider" -- the same optional-field shape Card.enchant
      -- takes, and it leaves every card without one byte-identical.
      <> ( case ActivatedAbility.timing aa of
             ActivationTiming.AnyTime -> []
             _ -> [(Text.pack "timing", activationTimingToJson (ActivatedAbility.timing aa))]
         )

jsonToActivatedAbility :: (Value -> Either Text card) -> Value -> Either Text (ActivatedAbility.ActivatedAbility card)
jsonToActivatedAbility decode value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "cost") ps >>= jsonToCost
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal decode
  t <- case Json.optField (Text.pack "timing") ps of
    Nothing -> pure ActivationTiming.AnyTime
    Just v -> jsonToActivationTiming v
  pure (ActivatedAbility.MkActivatedAbility c m t)

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

-- Runtime-only, never in card JSON -- covered for the same reason SetController's
-- PlayerId is: the codec must stay total over the transitive closure of what the
-- game state carries.
bindingToJson :: Binding.Binding -> Value
bindingToJson b =
  Json.jObject
    [ (Text.pack "target", Json.maybeTo recipientToJson (Binding.target b)),
      (Text.pack "subtypes", Json.maybeTo (\(f, t) -> Array (MkArray [subtypeToJson f, subtypeToJson t])) (Binding.subtypes b)),
      (Text.pack "amount", Json.maybeTo Json.natTo (Binding.amount b)),
      (Text.pack "modes", Json.maybeTo (Json.setTo modeIndexToJson) (Binding.modes b)),
      (Text.pack "copy", Json.maybeTo projectedCharacteristicsToJson (Binding.copy b))
    ]

jsonToBinding :: Value -> Either Text Binding.Binding
jsonToBinding value = do
  ps <- Json.asObject value
  t <- Json.maybeFrom jsonToRecipient (Json.getOpt (Text.pack "target") ps)
  s <- Json.maybeFrom jsonToSubtypePair (Json.getOpt (Text.pack "subtypes") ps)
  a <- Json.maybeFrom Json.natFrom (Json.getOpt (Text.pack "amount") ps)
  m <- Json.maybeFrom (Json.setFrom jsonToModeIndex) (Json.getOpt (Text.pack "modes") ps)
  c <- Json.maybeFrom jsonToProjectedCharacteristics (Json.getOpt (Text.pack "copy") ps)
  pure
    Binding.MkBinding
      { Binding.target = t,
        Binding.subtypes = s,
        Binding.amount = a,
        Binding.modes = m,
        Binding.copy = c
      }

bindingsToJson :: Map.Map SlotName.SlotName Binding.Binding -> Value
bindingsToJson m =
  Json.listTo
    (\(k, v) -> Json.jObject [(Text.pack "slot", slotNameToJson k), (Text.pack "binding", bindingToJson v)])
    (Map.toAscList m)

jsonToBindings :: Value -> Either Text (Map.Map SlotName.SlotName Binding.Binding)
jsonToBindings value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "slot") ps >>= jsonToSlotName
        b <- Json.field (Text.pack "binding") ps >>= jsonToBinding
        pure (k, b)
   in Map.fromList <$> Json.listFrom decodeEntry value

delayedTriggerToJson :: DelayedTrigger.DelayedTrigger -> Value
delayedTriggerToJson d =
  Json.jObject
    [ (Text.pack "ability", triggeredAbilityToJson cardToJson (DelayedTrigger.ability d)),
      (Text.pack "source", objectIdToJson (DelayedTrigger.source d)),
      (Text.pack "controller", playerIdToJson (DelayedTrigger.controller d)),
      (Text.pack "bindings", bindingsToJson (DelayedTrigger.bindings d)),
      -- CR 603.7b: absent for an ability with no stated duration, which is the
      -- rule's default and every entry in the pool but Full Throttle's.
      (Text.pack "expiry", Json.maybeTo expiryToJson (DelayedTrigger.expiry d))
    ]

jsonToDelayedTrigger :: Value -> Either Text DelayedTrigger.DelayedTrigger
jsonToDelayedTrigger value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "ability") ps >>= jsonToTriggeredAbility jsonToCard
  s <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  c <- Json.field (Text.pack "controller") ps >>= jsonToPlayerId
  b <- Json.field (Text.pack "bindings") ps >>= jsonToBindings
  e <- Json.maybeFrom jsonToExpiry (Json.getOpt (Text.pack "expiry") ps)
  pure
    DelayedTrigger.MkDelayedTrigger
      { DelayedTrigger.ability = a,
        DelayedTrigger.source = s,
        DelayedTrigger.controller = c,
        DelayedTrigger.bindings = b,
        DelayedTrigger.expiry = e
      }

-- Modal -----------------------------------------------------------------------

modeToJson :: (card -> Value) -> Mode.Mode card -> Value
modeToJson codec m =
  Json.jObject
    ( [ (Text.pack "effects", Json.seqTo (effectToJson codec) (Mode.effects m)),
        (Text.pack "targetSpecs", targetSpecsToJson (Mode.targetSpecs m))
      ]
        -- Omitted when Mandatory; see jsonToOptionalityDefault.
        <> ( case Mode.optionality m of
               Optionality.Mandatory -> []
               Optionality.Optional -> [(Text.pack "optionality", optionalityToJson (Mode.optionality m))]
           )
    )

jsonToMode :: (Value -> Either Text card) -> Value -> Either Text (Mode.Mode card)
jsonToMode decode value = do
  ps <- Json.asObject value
  es <- Json.field (Text.pack "effects") ps >>= Json.seqFrom (jsonToEffect decode)
  ts <- Json.field (Text.pack "targetSpecs") ps >>= jsonToTargetSpecs
  o <- jsonToOptionalityDefault (Json.getOpt (Text.pack "optionality") ps)
  pure (Mode.MkMode es ts o)

modalToJson :: (card -> Value) -> Modal.Modal card -> Value
modalToJson codec m =
  Json.jObject
    [ (Text.pack "modes", Json.seqTo (modeToJson codec) (Modal.modes m)),
      (Text.pack "selection", modeSelectionToJson (Modal.selection m))
    ]

jsonToModal :: (Value -> Either Text card) -> Value -> Either Text (Modal.Modal card)
jsonToModal decode value = do
  ps <- Json.asObject value
  ms <- Json.field (Text.pack "modes") ps >>= Json.seqFrom (jsonToMode decode)
  if Seq.null ms
    then Left (Text.pack "modal has no modes")
    else do
      sel <- Json.field (Text.pack "selection") ps >>= jsonToModeSelection
      pure (Modal.MkModal ms sel)

-- Card & Printing ------------------------------------------------------------

cardToJson :: CardT.Card -> Value
cardToJson c =
  Json.jObject
    ( [ (Text.pack "name", Json.jText (CardT.name c)),
        (Text.pack "manaCost", Json.maybeTo manaCostToJson (CardT.manaCost c)),
        (Text.pack "typeLine", typeLineToJson (CardT.typeLine c)),
        (Text.pack "power", Json.maybeTo powerToJson (CardT.power c)),
        (Text.pack "toughness", Json.maybeTo toughnessToJson (CardT.toughness c)),
        (Text.pack "keywords", Json.setTo keywordToJson (CardT.keywords c)),
        (Text.pack "staticAbilities", Json.listTo staticAbilityToJson (CardT.staticAbilities c)),
        (Text.pack "spell", modalToJson cardToJson (CardT.spell c)),
        (Text.pack "activatedAbilities", Json.listTo (activatedAbilityToJson cardToJson) (CardT.activatedAbilities c)),
        (Text.pack "replacementEffects", Json.listTo replacementEffectToJson (CardT.replacementEffects c)),
        (Text.pack "triggeredAbilities", Json.listTo (triggeredAbilityToJson cardToJson) (CardT.triggeredAbilities c)),
        (Text.pack "castingPermissions", Json.listTo castingPermissionToJson (CardT.castingPermissions c))
      ]
        <> ( if Set.null (CardT.colorIndicator c)
               then []
               else [(Text.pack "colorIndicator", Json.setTo colorToJson (CardT.colorIndicator c))]
           )
        <> ( case CardT.characteristicPT c of
               Nothing -> []
               Just q -> [(Text.pack "characteristicPT", quantityToJson q)]
           )
        <> ( if Map.null (CardT.delayedAbilities c)
               then []
               else [(Text.pack "delayedAbilities", delayedAbilitiesToJson cardToJson (CardT.delayedAbilities c))]
           )
        <> ( if null (CardT.playerAbilities c)
               then []
               else [(Text.pack "playerAbilities", Json.listTo playerStaticAbilityToJson (CardT.playerAbilities c))]
           )
        <> ( if null (CardT.blockRequirements c)
               then []
               else [(Text.pack "blockRequirements", Json.listTo blockRequirementToJson (CardT.blockRequirements c))]
           )
        <> ( if null (CardT.attackRequirements c)
               then []
               else [(Text.pack "attackRequirements", Json.listTo attackRequirementToJson (CardT.attackRequirements c))]
           )
        <> ( if null (CardT.additionalCosts c)
               then []
               else [(Text.pack "additionalCosts", Json.listTo costComponentToJson (CardT.additionalCosts c))]
           )
        <> ( if null (CardT.alternativeCosts c)
               then []
               else [(Text.pack "alternativeCosts", Json.listTo costToJson (CardT.alternativeCosts c))]
           )
        -- Omitted when Counterable, the posture every other defaulted key here
        -- takes: one card in the pool prints "this spell can't be countered", and
        -- a required key would have meant editing every other card file to say
        -- nothing.
        <> ( case CardT.counterability c of
               Counterability.Counterable -> []
               Counterability.CantBeCountered -> [(Text.pack "counterability", counterabilityToJson (CardT.counterability c))]
           )
        <> ( if null (CardT.mulliganAction c)
               then []
               else [(Text.pack "mulliganAction", Json.listTo (effectToJson cardToJson) (CardT.mulliganAction c))]
           )
        <> ( if null (CardT.openingHandAction c)
               then []
               else [(Text.pack "openingHandAction", Json.listTo (effectToJson cardToJson) (CardT.openingHandAction c))]
           )
        <> ( case CardT.enchant c of
               Nothing -> []
               Just spec -> [(Text.pack "enchant", targetSpecToJson spec)]
           )
        -- Omitted when empty, unlike the required `castingPermissions` key it
        -- mirrors: one card in the pool prints a casting restriction, and a
        -- required key would have meant editing every other card file to say
        -- nothing.
        <> ( if null (CardT.castingRestrictions c)
               then []
               else [(Text.pack "castingRestrictions", Json.listTo castingRestrictionToJson (CardT.castingRestrictions c))]
           )
    )

jsonToCard :: Value -> Either Text CardT.Card
jsonToCard value = do
  ps <- Json.asObject value
  name <- Json.field (Text.pack "name") ps >>= Json.asText
  manaCost <- Json.maybeFrom jsonToManaCost (Json.getOpt (Text.pack "manaCost") ps)
  typeLine <- Json.field (Text.pack "typeLine") ps >>= jsonToTypeLine
  power <- Json.maybeFrom jsonToPower (Json.getOpt (Text.pack "power") ps)
  toughness <- Json.maybeFrom jsonToToughness (Json.getOpt (Text.pack "toughness") ps)
  keywords <- Json.field (Text.pack "keywords") ps >>= Json.setFrom jsonToKeyword
  statics <- Json.field (Text.pack "staticAbilities") ps >>= Json.listFrom jsonToStaticAbility
  spell <- Json.field (Text.pack "spell") ps >>= jsonToModal jsonToCard
  activated <- Json.field (Text.pack "activatedAbilities") ps >>= Json.listFrom (jsonToActivatedAbility jsonToCard)
  replacements <- Json.field (Text.pack "replacementEffects") ps >>= Json.listFrom jsonToReplacementEffect
  triggered <- Json.field (Text.pack "triggeredAbilities") ps >>= Json.listFrom (jsonToTriggeredAbility jsonToCard)
  permissions <- Json.field (Text.pack "castingPermissions") ps >>= Json.listFrom jsonToCastingPermission
  restrictions <- Json.listFromDefault jsonToCastingRestriction (Json.getOpt (Text.pack "castingRestrictions") ps)
  colorIndicator <- Json.setFromDefault jsonToColor (Json.getOpt (Text.pack "colorIndicator") ps)
  characteristicPT <- Json.maybeFrom jsonToQuantity (Json.getOpt (Text.pack "characteristicPT") ps)
  delayed <- Json.mapFromDefault (jsonToDelayedAbilities jsonToCard) (Json.getOpt (Text.pack "delayedAbilities") ps)
  playerAbilities <- Json.listFromDefault jsonToPlayerStaticAbility (Json.getOpt (Text.pack "playerAbilities") ps)
  blockRequirements <- Json.listFromDefault jsonToBlockRequirement (Json.getOpt (Text.pack "blockRequirements") ps)
  attackRequirements <- Json.listFromDefault jsonToAttackRequirement (Json.getOpt (Text.pack "attackRequirements") ps)
  additionalCosts <- Json.listFromDefault jsonToCostComponent (Json.getOpt (Text.pack "additionalCosts") ps)
  alternativeCosts <- Json.listFromDefault jsonToCost (Json.getOpt (Text.pack "alternativeCosts") ps)
  mulliganAction <- Json.listFromDefault (jsonToEffect jsonToCard) (Json.getOpt (Text.pack "mulliganAction") ps)
  openingHandAction <- Json.listFromDefault (jsonToEffect jsonToCard) (Json.getOpt (Text.pack "openingHandAction") ps)
  enchant <- Json.maybeFrom jsonToTargetSpec (Json.getOpt (Text.pack "enchant") ps)
  counterability <- jsonToCounterabilityDefault (Json.getOpt (Text.pack "counterability") ps)
  pure
    CardT.MkCard
      { CardT.name = name,
        CardT.manaCost = manaCost,
        CardT.typeLine = typeLine,
        CardT.power = power,
        CardT.toughness = toughness,
        CardT.keywords = keywords,
        CardT.staticAbilities = statics,
        CardT.spell = spell,
        CardT.activatedAbilities = activated,
        CardT.replacementEffects = replacements,
        CardT.triggeredAbilities = triggered,
        CardT.castingPermissions = permissions,
        CardT.castingRestrictions = restrictions,
        CardT.colorIndicator = colorIndicator,
        CardT.characteristicPT = characteristicPT,
        CardT.delayedAbilities = delayed,
        CardT.playerAbilities = playerAbilities,
        CardT.blockRequirements = blockRequirements,
        CardT.attackRequirements = attackRequirements,
        CardT.additionalCosts = additionalCosts,
        CardT.alternativeCosts = alternativeCosts,
        CardT.mulliganAction = mulliganAction,
        CardT.openingHandAction = openingHandAction,
        CardT.enchant = enchant,
        CardT.counterability = counterability
      }

printingToJson :: Printing.Printing -> Value
printingToJson (Printing.MkPrinting c) = cardToJson c

jsonToPrinting :: Value -> Either Text Printing.Printing
jsonToPrinting value = Printing.MkPrinting <$> jsonToCard value
