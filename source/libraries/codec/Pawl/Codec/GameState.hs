{-# LANGUAGE ApplicativeDo #-}

-- | A game in progress, on the wire (#126).
module Pawl.Codec.GameState where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Codec.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Codec.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Codec.ActiveReplacement as ActiveReplacement
import qualified Pawl.Codec.ActiveUnregeneratable as ActiveUnregeneratable
import qualified Pawl.Codec.BattlefieldCandidate as BattlefieldCandidate
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
import qualified Pawl.Codec.IgnoredAbility as IgnoredAbility
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.LastKnown as LastKnown
import qualified Pawl.Codec.LoggedEvent as LoggedEvent
import qualified Pawl.Codec.Mana as Mana
import qualified Pawl.Codec.MonarchWatch as MonarchWatch
import qualified Pawl.Codec.Object as Object
import qualified Pawl.Codec.ObjectId as ObjectId
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
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Printing as Printing.Type
import qualified Pawl.Types.PrintingId as PrintingId.Type
import qualified Pawl.Types.SlotName as SlotName.Type

-- | Every field but one, in the record's own order.
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
codec :: Codec.Codec GameState.GameState
codec = Fields.object $ do
  objects <- Fields.required "objects" (Common.naturalMap ObjectId.codec Object.codec) GameState.objects
  library <- Fields.required "library" (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)) GameState.library
  hand <- Fields.required "hand" (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)) GameState.hand
  graveyard <- Fields.required "graveyard" (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)) GameState.graveyard
  battlefield <- Fields.required "battlefield" (Common.set ObjectId.codec) GameState.battlefield
  phasedOut <- Fields.required "phasedOut" (Common.naturalMap ObjectId.codec PhasedOut.codec) GameState.phasedOut
  exile <- Fields.required "exile" (Common.set ObjectId.codec) GameState.exile
  command <- Fields.required "command" (Common.set ObjectId.codec) GameState.command
  stack <- Fields.required "stack" (Common.list ObjectId.codec) GameState.stack
  players <- Fields.required "players" (Common.naturalMap PlayerId.codec Player.codec) GameState.players
  manaPool <- Fields.required "manaPool" (Common.naturalMap PlayerId.codec Mana.codec) GameState.manaPool
  combat <- Fields.required "combat" Combat.codec GameState.combat
  events <- Fields.required "events" (Common.seq LoggedEvent.codec) GameState.events
  nextEventGroup <- Fields.required "nextEventGroup" EventGroup.codec GameState.nextEventGroup
  eventGroupDepth <- Fields.required "eventGroupDepth" Common.natural GameState.eventGroupDepth
  lastKnown <- Fields.required "lastKnown" (Common.naturalMap ObjectId.codec LastKnown.codec) GameState.lastKnown
  scannedThrough <- Fields.required "scannedThrough" Common.natural GameState.scannedThrough
  battlefieldWhenTriggered <- Fields.required "battlefieldWhenTriggered" (Common.naturalMap EventGroup.codec (Common.naturalMap ObjectId.codec (BattlefieldCandidate.codec ProjectedCharacteristics.codec))) GameState.battlefieldWhenTriggered
  controlSample <- Fields.required "controlSample" (Common.naturalMap ObjectId.codec PlayerId.codec) GameState.controlSample
  damageScannedThrough <- Fields.required "damageScannedThrough" Common.natural GameState.damageScannedThrough
  delayedTriggers <- Fields.required "delayedTriggers" (Common.seq DelayedTrigger.codec) GameState.delayedTriggers
  continuousEffects <- Fields.required "continuousEffects" (Common.list (ContinuousEffect.codec Card.codec)) GameState.continuousEffects
  replacements <- Fields.required "replacements" (Common.list ActiveReplacement.codec) GameState.replacements
  pendingPreventionRiders <- Fields.required "pendingPreventionRiders" (Common.seq Prevention.codec) GameState.pendingPreventionRiders
  ambientAmounts <- Fields.required "ambientAmounts" (Common.textMap SlotName.Type.unwrap (Right . SlotName.Type.MkSlotName) Common.natural) GameState.ambientAmounts
  pendingEntryEffects <- Fields.required "pendingEntryEffects" (Common.seq PendingEntryEffect.codec) GameState.pendingEntryEffects
  enteringBeside <- Fields.required "enteringBeside" (Common.set ObjectId.codec) GameState.enteringBeside
  enteringCounters <- Fields.required "enteringCounters" (Common.naturalMap ObjectId.codec (Common.multiset (CounterKind.codec Keyword.codec))) GameState.enteringCounters
  playerEffects <- Fields.required "playerEffects" (Common.list ActivePlayerEffect.codec) GameState.playerEffects
  blockRequirements <- Fields.required "blockRequirements" (Common.list ActiveBlockRequirement.codec) GameState.blockRequirements
  attackRequirements <- Fields.required "attackRequirements" (Common.list ActiveAttackRequirement.codec) GameState.attackRequirements
  unregeneratables <- Fields.required "unregeneratables" (Common.list ActiveUnregeneratable.codec) GameState.unregeneratables
  ignoredAbilities <- Fields.required "ignoredAbilities" (Common.list IgnoredAbility.codec) GameState.ignoredAbilities
  turnOrder <- Fields.required "turnOrder" (Common.list PlayerId.codec) GameState.turnOrder
  activePlayer <- Fields.required "activePlayer" PlayerId.codec GameState.activePlayer
  phase <- Fields.required "phase" Phase.codec GameState.phase
  remaining <- Fields.required "remaining" (Common.seq Phase.codec) GameState.remaining
  priority <- Fields.required "priority" (Common.maybe PlayerId.codec) GameState.priority
  passes <- Fields.required "passes" Common.natural GameState.passes
  turnNumber <- Fields.required "turnNumber" Common.natural GameState.turnNumber
  result <- Fields.required "result" (Common.maybe Result.codec) GameState.result
  restartSignal <- Fields.required "restartSignal" RestartSignal.codec GameState.restartSignal
  endTurnSignal <- Fields.required "endTurnSignal" EndTurnSignal.codec GameState.endTurnSignal
  nextObjectId <- Fields.required "nextObjectId" ObjectId.codec GameState.nextObjectId
  printings <- Fields.required "printings" (Common.naturalMap PrintingId.codec Printing.codec) GameState.printings
  nextPrintingId <- Fields.required "nextPrintingId" PrintingId.codec GameState.nextPrintingId
  nextTimestamp <- Fields.required "nextTimestamp" Timestamp.codec GameState.nextTimestamp
  lastChoice <- Fields.required "lastChoice" Timestamp.codec GameState.lastChoice
  drewFromEmpty <- Fields.required "drewFromEmpty" (Common.set PlayerId.codec) GameState.drewFromEmpty
  landsPlayed <- Fields.required "landsPlayed" (Common.naturalMap PlayerId.codec Common.natural) GameState.landsPlayed
  drawsThisTurn <- Fields.required "drawsThisTurn" (Common.naturalMap PlayerId.codec Common.natural) GameState.drawsThisTurn
  speedIncreasedThisTurn <- Fields.required "speedIncreasedThisTurn" (Common.set PlayerId.codec) GameState.speedIncreasedThisTurn
  pendingControl <- Fields.required "pendingControl" (Common.naturalMap PlayerId.codec Decider.codec) GameState.pendingControl
  activeControl <- Fields.required "activeControl" (Common.maybe Decider.codec) GameState.activeControl
  monarch <- Fields.required "monarch" (Common.maybe PlayerId.codec) GameState.monarch
  daytime <- Fields.required "daytime" (Common.maybe Daytime.codec) GameState.daytime
  spellsCastLastTurn <- Fields.required "spellsCastLastTurn" Common.natural GameState.spellsCastLastTurn
  castsLastTurn <- Fields.required "castsLastTurn" (Common.naturalMap PlayerId.codec Common.natural) GameState.castsLastTurn
  exiledUntilMonarch <- Fields.required "exiledUntilMonarch" (Common.naturalMap ObjectId.codec MonarchWatch.codec) GameState.exiledUntilMonarch
  haunting <- Fields.required "haunting" (Common.naturalMap ObjectId.codec ObjectId.codec) GameState.haunting
  exiledWith <- Fields.required "exiledWith" (Common.naturalMap ObjectId.codec ObjectId.codec) GameState.exiledWith
  extraTurns <- Fields.required "extraTurns" (Common.list ExtraTurn.codec) GameState.extraTurns
  turnAnchor <- Fields.required "turnAnchor" (Common.maybe PlayerId.codec) GameState.turnAnchor
  pure
    GameState.MkGameState
      { GameState.objects = objects,
        GameState.library = library,
        GameState.hand = hand,
        GameState.graveyard = graveyard,
        GameState.battlefield = battlefield,
        GameState.phasedOut = phasedOut,
        GameState.exile = exile,
        GameState.command = command,
        GameState.stack = stack,
        GameState.players = players,
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
        GameState.pendingEntryEffects = pendingEntryEffects,
        GameState.enteringBeside = enteringBeside,
        GameState.enteringCounters = enteringCounters,
        GameState.playerEffects = playerEffects,
        GameState.blockRequirements = blockRequirements,
        GameState.attackRequirements = attackRequirements,
        GameState.unregeneratables = unregeneratables,
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
        GameState.daytime = daytime,
        GameState.spellsCastLastTurn = spellsCastLastTurn,
        GameState.castsLastTurn = castsLastTurn,
        GameState.exiledUntilMonarch = exiledUntilMonarch,
        GameState.haunting = haunting,
        GameState.exiledWith = exiledWith,
        GameState.extraTurns = extraTurns,
        GameState.turnAnchor = turnAnchor,
        -- Derived rather than written: see the note above.
        GameState.printingIds = invert printings
      }

-- | `printings` read backwards, which is what GameState.printingIds holds.
--
-- Exact, because Pawl.Engine.Game.intern is idempotent: a printing already in
-- the table answers with the id it already has, so no two ids name one printing
-- and the inversion loses nothing. That idempotence is asserted by
-- Pawl.Codec.GameStateSpec and by Pawl.GameSpec's "intern is idempotent for a
-- printing already in the table".
invert :: Map.Map PrintingId.Type.PrintingId Printing.Type.Printing -> Map.Map Printing.Type.Printing PrintingId.Type.PrintingId
invert = Map.fromList . fmap (\(i, p) -> (p, i)) . Map.toList
