{-# LANGUAGE ApplicativeDo #-}

-- | A game in progress, on the wire (#126).
module Pawl.Codec.GameState where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Codec.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.Codec.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Codec.ActiveBlockProhibition as ActiveBlockProhibition
import qualified Pawl.Codec.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Codec.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Codec.ActiveReplacement as ActiveReplacement
import qualified Pawl.Codec.ActiveUnregeneratable as ActiveUnregeneratable
import qualified Pawl.Codec.BattlefieldCandidate as BattlefieldCandidate
import qualified Pawl.Codec.Binding as Binding
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Combat as Combat
import qualified Pawl.Codec.ContinuousEffect as ContinuousEffect
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Daytime as Daytime
import qualified Pawl.Codec.Decider as Decider
import qualified Pawl.Codec.DelayedTrigger as DelayedTrigger
import qualified Pawl.Codec.EndTurnSignal as EndTurnSignal
import qualified Pawl.Codec.EventGroup as EventGroup
import qualified Pawl.Codec.ExtraTurn as ExtraTurn
import qualified Pawl.Codec.GameSettings as GameSettings
import qualified Pawl.Codec.IgnoredAbility as IgnoredAbility
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.LastKnown as LastKnown
import qualified Pawl.Codec.LoggedEvent as LoggedEvent
import qualified Pawl.Codec.Mana as Mana
import qualified Pawl.Codec.MonarchWatch as MonarchWatch
import qualified Pawl.Codec.Object as Object
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.OutsideObject as OutsideObject
import qualified Pawl.Codec.PendingEntryEffect as PendingEntryEffect
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.PhasedOut as PhasedOut
import qualified Pawl.Codec.Player as Player
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Prevention as Prevention
import qualified Pawl.Codec.Printing as Printing
import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.RestartSignal as RestartSignal
import qualified Pawl.Codec.Result as Result
import qualified Pawl.Codec.ReturnWatch as ReturnWatch
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName.Type
import qualified Pawl.Types.EndTurnSignal as EndTurnSignal.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Printing as Printing.Type
import qualified Pawl.Types.PrintingId as PrintingId.Type
import qualified Pawl.Types.RestartSignal as RestartSignal.Type
import qualified Pawl.Types.SlotName as SlotName.Type

-- | Every field but one, in the record's own order.
--
-- 'Fields.required' is kept for the fields no game can be missing -- `settings`,
-- `objects`, `players`, `turnOrder`, `activePlayer`, `phase` -- and for the
-- counters and clocks whose zero is a position rather than an absence:
-- `nextEventGroup`, `nextObjectId`, `nextPrintingId`, `nextTimestamp`,
-- `lastChoice`, `turnNumber`, and `remaining`, which is the rest of the turn
-- and not a collection that happens to be empty. `combat` stays required too,
-- and costs almost nothing now that Pawl.Codec.Combat defaults its own fields:
-- a cleared combat (CR 511.3) writes @{"struckFirst":null}@.
--
-- Everything else is 'Fields.defaulted', which omits a field equal to its
-- default and reads an absence back as that same value, so the round trip is
-- unchanged and a state that has done nothing writes almost nothing. The trap
-- it introduces is that a mistyped key now decodes as the default instead of
-- failing as a missing one; rejecting unknown keys is not the fix, since
-- 'Pawl.JsonCodec.Arm.armObject' already declines @additionalProperties@ for
-- forward compatibility.
--
-- The resolver goes to `printings` and nowhere else: Pawl.Codec.Printing's
-- 'Printing.reference' writes a card's NAME where the resolver reproduces it and
-- the whole record where none can. It is the caller's, because @pawl:registry@
-- sits above @pawl:codec@ and a decoder cannot reach one.
--
-- GameState.printingIds is NOT written. It is `printings` read backwards, which
-- Pawl.Types.GameState says at the field itself, so writing it would put the
-- same table on the wire twice and let the two disagree -- a document whose two
-- halves contradict each other has no meaning this decoder could pick. It is
-- rebuilt by `invert` below instead.
--
-- Its key is also a whole Printing, so it could not be an object key at all: it
-- would need an array of key/value pairs, duplicating every printing's entire
-- record a second time. That is the size argument, and it is the lesser one.
codec :: (CardName.Type.CardName -> Maybe Card.Type.Card) -> Codec.Codec GameState.GameState
codec resolve = Fields.object $ do
  settings <- Fields.required "settings" GameSettings.codec GameState.settings
  objects <- Fields.required "objects" (Common.naturalMap ObjectId.codec Object.codec) GameState.objects
  library <- Fields.defaulted "library" Map.empty (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)) GameState.library
  hand <- Fields.defaulted "hand" Map.empty (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)) GameState.hand
  graveyard <- Fields.defaulted "graveyard" Map.empty (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)) GameState.graveyard
  battlefield <- Fields.defaulted "battlefield" Set.empty (Common.set ObjectId.codec) GameState.battlefield
  phasedOut <- Fields.defaulted "phasedOut" Map.empty (Common.naturalMap ObjectId.codec PhasedOut.codec) GameState.phasedOut
  exile <- Fields.defaulted "exile" Set.empty (Common.set ObjectId.codec) GameState.exile
  command <- Fields.defaulted "command" Set.empty (Common.set ObjectId.codec) GameState.command
  stack <- Fields.defaulted "stack" [] (Common.list ObjectId.codec) GameState.stack
  players <- Fields.required "players" (Common.naturalMap PlayerId.codec Player.codec) GameState.players
  outsideObjects <- Fields.defaulted "outsideObjects" Map.empty (Common.naturalMap ObjectId.codec OutsideObject.codec) GameState.outsideObjects
  broughtIn <- Fields.defaulted "broughtIn" Seq.empty (Common.seq ObjectId.codec) GameState.broughtIn
  manaPool <- Fields.defaulted "manaPool" Map.empty (Common.naturalMap PlayerId.codec Mana.codec) GameState.manaPool
  combat <- Fields.required "combat" Combat.codec GameState.combat
  events <- Fields.defaulted "events" Seq.empty (Common.seq LoggedEvent.codec) GameState.events
  nextEventGroup <- Fields.required "nextEventGroup" EventGroup.codec GameState.nextEventGroup
  eventGroupDepth <- Fields.defaulted "eventGroupDepth" 0 Common.natural GameState.eventGroupDepth
  lastKnown <- Fields.defaulted "lastKnown" Map.empty (Common.naturalMap ObjectId.codec LastKnown.codec) GameState.lastKnown
  scannedThrough <- Fields.defaulted "scannedThrough" 0 Common.natural GameState.scannedThrough
  battlefieldWhenTriggered <- Fields.defaulted "battlefieldWhenTriggered" Map.empty (Common.naturalMap EventGroup.codec (Common.naturalMap ObjectId.codec (BattlefieldCandidate.codec ProjectedCharacteristics.codec))) GameState.battlefieldWhenTriggered
  controlSample <- Fields.defaulted "controlSample" Map.empty (Common.naturalMap ObjectId.codec PlayerId.codec) GameState.controlSample
  damageScannedThrough <- Fields.defaulted "damageScannedThrough" 0 Common.natural GameState.damageScannedThrough
  delayedTriggers <- Fields.defaulted "delayedTriggers" Seq.empty (Common.seq DelayedTrigger.codec) GameState.delayedTriggers
  continuousEffects <- Fields.defaulted "continuousEffects" [] (Common.list (ContinuousEffect.codec Card.codec)) GameState.continuousEffects
  replacements <- Fields.defaulted "replacements" [] (Common.list ActiveReplacement.codec) GameState.replacements
  pendingPreventionRiders <- Fields.defaulted "pendingPreventionRiders" Seq.empty (Common.seq Prevention.codec) GameState.pendingPreventionRiders
  ambientAmounts <- Fields.defaulted "ambientAmounts" Map.empty (Common.textMap SlotName.Type.unwrap (Right . SlotName.Type.MkSlotName) Common.natural) GameState.ambientAmounts
  detachedBindings <- Fields.defaulted "detachedBindings" Map.empty (Common.naturalMap ObjectId.codec Binding.codecMap) GameState.detachedBindings
  pendingEntryEffects <- Fields.defaulted "pendingEntryEffects" Seq.empty (Common.seq PendingEntryEffect.codec) GameState.pendingEntryEffects
  enteringBeside <- Fields.defaulted "enteringBeside" Set.empty (Common.set ObjectId.codec) GameState.enteringBeside
  enteringSubjects <- Fields.defaulted "enteringSubjects" Set.empty (Common.set ObjectId.codec) GameState.enteringSubjects
  enteringCounters <- Fields.defaulted "enteringCounters" Map.empty (Common.naturalMap ObjectId.codec (Common.multiset (CounterKind.codec Keyword.codec))) GameState.enteringCounters
  playerEffects <- Fields.defaulted "playerEffects" [] (Common.list ActivePlayerEffect.codec) GameState.playerEffects
  blockRequirements <- Fields.defaulted "blockRequirements" [] (Common.list ActiveBlockRequirement.codec) GameState.blockRequirements
  attackRequirements <- Fields.defaulted "attackRequirements" [] (Common.list ActiveAttackRequirement.codec) GameState.attackRequirements
  unregeneratables <- Fields.defaulted "unregeneratables" [] (Common.list ActiveUnregeneratable.codec) GameState.unregeneratables
  blockProhibitions <- Fields.defaulted "blockProhibitions" [] (Common.list ActiveBlockProhibition.codec) GameState.blockProhibitions
  attackProhibitions <- Fields.defaulted "attackProhibitions" [] (Common.list ActiveAttackProhibition.codec) GameState.attackProhibitions
  ignoredAbilities <- Fields.defaulted "ignoredAbilities" [] (Common.list IgnoredAbility.codec) GameState.ignoredAbilities
  turnOrder <- Fields.required "turnOrder" (Common.list PlayerId.codec) GameState.turnOrder
  activePlayer <- Fields.required "activePlayer" PlayerId.codec GameState.activePlayer
  phase <- Fields.required "phase" Phase.codec GameState.phase
  remaining <- Fields.required "remaining" (Common.seq Phase.codec) GameState.remaining
  priority <- Fields.defaulted "priority" Nothing (Common.maybe PlayerId.codec) GameState.priority
  passes <- Fields.defaulted "passes" 0 Common.natural GameState.passes
  turnNumber <- Fields.required "turnNumber" Common.natural GameState.turnNumber
  result <- Fields.defaulted "result" Nothing (Common.maybe Result.codec) GameState.result
  restartSignal <- Fields.defaulted "restartSignal" RestartSignal.Type.Playing RestartSignal.codec GameState.restartSignal
  endTurnSignal <- Fields.defaulted "endTurnSignal" EndTurnSignal.Type.Running EndTurnSignal.codec GameState.endTurnSignal
  nextObjectId <- Fields.required "nextObjectId" ObjectId.codec GameState.nextObjectId
  printings <- Fields.defaulted "printings" Map.empty (Common.naturalMap PrintingId.codec (Printing.reference resolve)) GameState.printings
  nextPrintingId <- Fields.required "nextPrintingId" PrintingId.codec GameState.nextPrintingId
  nextTimestamp <- Fields.required "nextTimestamp" Timestamp.codec GameState.nextTimestamp
  lastChoice <- Fields.required "lastChoice" Timestamp.codec GameState.lastChoice
  drewFromEmpty <- Fields.defaulted "drewFromEmpty" Set.empty (Common.set PlayerId.codec) GameState.drewFromEmpty
  landsPlayed <- Fields.defaulted "landsPlayed" Map.empty (Common.naturalMap PlayerId.codec Common.natural) GameState.landsPlayed
  drawsThisTurn <- Fields.defaulted "drawsThisTurn" Map.empty (Common.naturalMap PlayerId.codec Common.natural) GameState.drawsThisTurn
  speedIncreasedThisTurn <- Fields.defaulted "speedIncreasedThisTurn" Set.empty (Common.set PlayerId.codec) GameState.speedIncreasedThisTurn
  pendingControl <- Fields.defaulted "pendingControl" Map.empty (Common.naturalMap PlayerId.codec Decider.codec) GameState.pendingControl
  activeControl <- Fields.defaulted "activeControl" Nothing (Common.maybe Decider.codec) GameState.activeControl
  monarch <- Fields.defaulted "monarch" Nothing (Common.maybe PlayerId.codec) GameState.monarch
  initiative <- Fields.defaulted "initiative" Nothing (Common.maybe PlayerId.codec) GameState.initiative
  daytime <- Fields.defaulted "daytime" Nothing (Common.maybe Daytime.codec) GameState.daytime
  spellsCastLastTurn <- Fields.defaulted "spellsCastLastTurn" 0 Common.natural GameState.spellsCastLastTurn
  castsLastTurn <- Fields.defaulted "castsLastTurn" Map.empty (Common.naturalMap PlayerId.codec Common.natural) GameState.castsLastTurn
  exiledUntilMonarch <- Fields.defaulted "exiledUntilMonarch" Map.empty (Common.naturalMap ObjectId.codec MonarchWatch.codec) GameState.exiledUntilMonarch
  movedUntilSourceLeaves <- Fields.defaulted "movedUntilSourceLeaves" Map.empty (Common.naturalMap ObjectId.codec ReturnWatch.codec) GameState.movedUntilSourceLeaves
  haunting <- Fields.defaulted "haunting" Map.empty (Common.naturalMap ObjectId.codec ObjectId.codec) GameState.haunting
  exiledWith <- Fields.defaulted "exiledWith" Map.empty (Common.naturalMap ObjectId.codec ObjectId.codec) GameState.exiledWith
  exilePiles <- Fields.defaulted "exilePiles" Map.empty (Common.naturalMap ObjectId.codec Timestamp.codec) GameState.exilePiles
  extraTurns <- Fields.defaulted "extraTurns" [] (Common.list ExtraTurn.codec) GameState.extraTurns
  turnAnchor <- Fields.defaulted "turnAnchor" Nothing (Common.maybe PlayerId.codec) GameState.turnAnchor
  pure
    GameState.MkGameState
      { GameState.settings = settings,
        GameState.objects = objects,
        GameState.library = library,
        GameState.hand = hand,
        GameState.graveyard = graveyard,
        GameState.battlefield = battlefield,
        GameState.phasedOut = phasedOut,
        GameState.exile = exile,
        GameState.command = command,
        GameState.stack = stack,
        GameState.players = players,
        GameState.outsideObjects = outsideObjects,
        GameState.broughtIn = broughtIn,
        GameState.manaPool = manaPool,
        GameState.combat = combat,
        GameState.events = events,
        GameState.nextEventGroup = nextEventGroup,
        GameState.eventGroupDepth = eventGroupDepth,
        GameState.lastKnown = lastKnown,
        GameState.scannedThrough = scannedThrough,
        GameState.battlefieldWhenTriggered = battlefieldWhenTriggered,
        GameState.controlSample = controlSample,
        GameState.damageScannedThrough = damageScannedThrough,
        GameState.delayedTriggers = delayedTriggers,
        GameState.continuousEffects = continuousEffects,
        GameState.replacements = replacements,
        GameState.pendingPreventionRiders = pendingPreventionRiders,
        GameState.ambientAmounts = ambientAmounts,
        GameState.detachedBindings = detachedBindings,
        GameState.pendingEntryEffects = pendingEntryEffects,
        GameState.enteringBeside = enteringBeside,
        GameState.enteringSubjects = enteringSubjects,
        GameState.enteringCounters = enteringCounters,
        GameState.playerEffects = playerEffects,
        GameState.blockRequirements = blockRequirements,
        GameState.attackRequirements = attackRequirements,
        GameState.unregeneratables = unregeneratables,
        GameState.blockProhibitions = blockProhibitions,
        GameState.attackProhibitions = attackProhibitions,
        GameState.ignoredAbilities = ignoredAbilities,
        GameState.turnOrder = turnOrder,
        GameState.activePlayer = activePlayer,
        GameState.phase = phase,
        GameState.remaining = remaining,
        GameState.priority = priority,
        GameState.passes = passes,
        GameState.turnNumber = turnNumber,
        GameState.result = result,
        GameState.restartSignal = restartSignal,
        GameState.endTurnSignal = endTurnSignal,
        GameState.nextObjectId = nextObjectId,
        GameState.printings = printings,
        GameState.nextPrintingId = nextPrintingId,
        GameState.nextTimestamp = nextTimestamp,
        GameState.lastChoice = lastChoice,
        GameState.drewFromEmpty = drewFromEmpty,
        GameState.landsPlayed = landsPlayed,
        GameState.drawsThisTurn = drawsThisTurn,
        GameState.speedIncreasedThisTurn = speedIncreasedThisTurn,
        GameState.pendingControl = pendingControl,
        GameState.activeControl = activeControl,
        GameState.monarch = monarch,
        GameState.initiative = initiative,
        GameState.daytime = daytime,
        GameState.spellsCastLastTurn = spellsCastLastTurn,
        GameState.castsLastTurn = castsLastTurn,
        GameState.exiledUntilMonarch = exiledUntilMonarch,
        GameState.movedUntilSourceLeaves = movedUntilSourceLeaves,
        GameState.haunting = haunting,
        GameState.exiledWith = exiledWith,
        GameState.exilePiles = exilePiles,
        GameState.extraTurns = extraTurns,
        GameState.turnAnchor = turnAnchor,
        -- Derived rather than written: see the note above.
        GameState.printingIds = invert printings
      }

-- | `printings` read backwards, which is what GameState.printingIds holds.
--
-- Exact, because Pawl.Engine.Game.intern is idempotent: a printing already in
-- the table answers with the id it already has, so no two ids name one printing
-- and the inversion loses nothing. That idempotence is asserted by Pawl.GameSpec's
-- "intern is idempotent for a printing already in the table", and the inversion
-- itself by Pawl.CodecIntegrationSpec's "the intern table survives, though only
-- half of it is written".
invert :: Map.Map PrintingId.Type.PrintingId Printing.Type.Printing -> Map.Map Printing.Type.Printing PrintingId.Type.PrintingId
invert = Map.fromList . fmap (\(i, p) -> (p, i)) . Map.toList
