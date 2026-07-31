-- | The sole authority for @Card ⇆ Json@ (§2 of the M3.5 spec), mirroring
-- 'Pawl.Engine.Resolve' (the sole @case@-on-@Effect@ home). Free @xToJson@\/@jsonToX@
-- functions -- no type classes -- over the transitive closure of @Card@'s
-- fields. Every @Pawl.Types.*@ module stays JSON-free; casing on an effect's
-- identity here is open-half machinery, not the rules core.
module Pawl.Codec.All (module Pawl.Codec.All, module Pawl.Codec.AbilityName, module Pawl.Codec.ActivatedAbility, module Pawl.Codec.ActivationTiming, module Pawl.Codec.Affected, module Pawl.Codec.Aggregation, module Pawl.Codec.AttackRequirement, module Pawl.Codec.BeginningStep, module Pawl.Codec.BlockRequirement, module Pawl.Codec.Card, module Pawl.Codec.CardType, module Pawl.Codec.CastingPermission, module Pawl.Codec.CastingRestriction, module Pawl.Codec.Color, module Pawl.Codec.CombatStep, module Pawl.Codec.Comparison, module Pawl.Codec.Condition, module Pawl.Codec.ControllerRelation, module Pawl.Codec.Cost, module Pawl.Codec.CostComponent, module Pawl.Codec.Count, module Pawl.Codec.CounterKind, module Pawl.Codec.CounterPattern, module Pawl.Codec.Counterability, module Pawl.Codec.DamageEvent, module Pawl.Codec.DamageKind, module Pawl.Codec.DamagePattern, module Pawl.Codec.DamageRewrite, module Pawl.Codec.DestructionRewrite, module Pawl.Codec.DiscardCause, module Pawl.Codec.Duration, module Pawl.Codec.Effect, module Pawl.Codec.EndingStep, module Pawl.Codec.EntryOption, module Pawl.Codec.EntryRewrite, module Pawl.Codec.EventShape, module Pawl.Codec.Expiry, module Pawl.Codec.ExtraPhase, module Pawl.Codec.Filter, module Pawl.Codec.Keyword, module Pawl.Codec.ManaCost, module Pawl.Codec.ManaProduction, module Pawl.Codec.ManaSymbol, module Pawl.Codec.ManaType, module Pawl.Codec.Modal, module Pawl.Codec.Mode, module Pawl.Codec.ModeIndex, module Pawl.Codec.ModeSelection, module Pawl.Codec.Modification, module Pawl.Codec.MonarchTarget, module Pawl.Codec.ObjectId, module Pawl.Codec.ObjectRef, module Pawl.Codec.Optionality, module Pawl.Codec.Phase, module Pawl.Codec.PhasePattern, module Pawl.Codec.PhaseSelector, module Pawl.Codec.PlayerCounterKind, module Pawl.Codec.PlayerEffect, module Pawl.Codec.PlayerId, module Pawl.Codec.PlayerRef, module Pawl.Codec.PlayerRelation, module Pawl.Codec.PlayerScope, module Pawl.Codec.PlayerStaticAbility, module Pawl.Codec.Pool, module Pawl.Codec.Power, module Pawl.Codec.Quantity, module Pawl.Codec.Recipient, module Pawl.Codec.Regenerability, module Pawl.Codec.ReplacementEffect, module Pawl.Codec.Scaling, module Pawl.Codec.Scope, module Pawl.Codec.SearchDestination, module Pawl.Codec.SlotName, module Pawl.Codec.StaticAbility, module Pawl.Codec.Subtype, module Pawl.Codec.Supertype, module Pawl.Codec.TapState, module Pawl.Codec.TargetSpec, module Pawl.Codec.TokenEntry, module Pawl.Codec.TokenPattern, module Pawl.Codec.Toughness, module Pawl.Codec.TriggerCondition, module Pawl.Codec.TriggerFrequency, module Pawl.Codec.TriggeredAbility, module Pawl.Codec.TurnScope, module Pawl.Codec.TypeLine, module Pawl.Codec.Uses, module Pawl.Codec.Zone, module Pawl.Codec.ZoneChange, module Pawl.Codec.ZoneChangePattern, module Pawl.Codec.ZoneChangeSubject) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.AbilityName (abilityNameToJson, jsonToAbilityName)
import Pawl.Codec.ActivatedAbility (activatedAbilityToJson, jsonToActivatedAbility)
import Pawl.Codec.ActivationTiming (activationTimingToJson, jsonToActivationTiming)
import Pawl.Codec.Affected (affectedToJson, jsonToAffected)
import Pawl.Codec.Aggregation (aggregationToJson, jsonToAggregation)
import Pawl.Codec.AttackRequirement (attackRequirementToJson, jsonToAttackRequirement)
import Pawl.Codec.BeginningStep (beginningStepToJson, jsonToBeginningStep)
import Pawl.Codec.BlockRequirement (blockRequirementToJson, jsonToBlockRequirement)
import Pawl.Codec.Card (cardToJson, jsonToCard)
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
import Pawl.Codec.Effect (effectToJson, jsonToEffect)
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
import Pawl.Codec.Modal (jsonToModal, modalToJson)
import Pawl.Codec.Mode (jsonToMode, modeToJson)
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
import Pawl.Codec.TriggeredAbility (delayedAbilitiesToJson, jsonToDelayedAbilities, jsonToTriggeredAbility, triggeredAbilityToJson)
import Pawl.Codec.TurnScope (jsonToTurnScope, turnScopeToJson)
import Pawl.Codec.TypeLine (jsonToTypeLine, typeLineToJson)
import Pawl.Codec.Uses (jsonToUses, usesToJson)
import Pawl.Codec.Zone (jsonToZone, zoneToJson)
import Pawl.Codec.ZoneChange (jsonToZoneChange, zoneChangeToJson)
import Pawl.Codec.ZoneChangePattern (jsonToZoneChangePattern, zoneChangePatternToJson)
import Pawl.Codec.ZoneChangeSubject (jsonToZoneChangeSubject, zoneChangeSubjectToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.SlotName as SlotName

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

-- Records & abilities --------------------------------------------------------

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

-- Card & Printing ------------------------------------------------------------

printingToJson :: Printing.Printing -> Value
printingToJson (Printing.MkPrinting c) = cardToJson c

jsonToPrinting :: Value -> Either Text Printing.Printing
jsonToPrinting value = Printing.MkPrinting <$> jsonToCard value
