module Pawl.Types.GameState where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.ActiveBlockProhibition as ActiveBlockProhibition
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.ActiveUnregeneratable as ActiveUnregeneratable
import qualified Pawl.Types.BattlefieldCandidate as BattlefieldCandidate
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.EndTurnSignal as EndTurnSignal
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OutsideObject as OutsideObject
import qualified Pawl.Types.PendingEntryEffect as PendingEntryEffect
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasedOut as PhasedOut
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prevention as Prevention
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.RestartSignal as RestartSignal
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.ReturnWatch as ReturnWatch
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Timestamp as Timestamp

data GameState = MkGameState
  { -- | CR 800.2: the options this game was started with; never written after
    -- the game begins.
    settings :: GameSettings.GameSettings,
    objects :: Map.Map ObjectId.ObjectId Object.Object,
    library :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    hand :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    graveyard :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    -- | CR 400.1's battlefield, minus its phased-out permanents, which
    -- `phasedOut` holds instead.
    battlefield :: Set.Set ObjectId.ObjectId,
    -- | CR 702.26b: the phased-out permanents, each with the player who
    -- controlled it when it phased out (CR 702.26a) and which of rule 702.26's
    -- schedules it is on. A separate set so every battlefield reader treats
    -- them as not existing by construction; see Pawl.Types.PhasedOut.
    phasedOut :: Map.Map ObjectId.ObjectId PhasedOut.PhasedOut,
    exile :: Set.Set ObjectId.ObjectId,
    -- | CR 400.1: the command zone, shared rather than per-player.
    command :: Set.Set ObjectId.ObjectId,
    stack :: [ObjectId.ObjectId],
    players :: Map.Map PlayerId.PlayerId Player.Player,
    -- | CR 729.4: the cards outside this game that sit in a game on hold; empty
    -- for a game nobody is nested inside.
    outsideObjects :: Map.Map ObjectId.ObjectId OutsideObject.OutsideObject,
    -- | CR 729.4a: the outer ids this game has brought in, in crossing order,
    -- read by Pawl.Engine.Setup.applyCrossings.
    broughtIn :: Seq.Seq ObjectId.ObjectId,
    -- | CR 106.4. Absent from the map means an empty pool.
    manaPool :: Map.Map PlayerId.PlayerId Mana.Mana,
    -- | CR 508/509. Lives for one combat phase; cleared at CR 511.
    combat :: Combat.Combat,
    -- | CR 608.2i: what happened this turn, in order, each entry stamped with
    -- its EventGroup (CR 608.2f's single event). Cleared at turn handoff
    -- (Engine.handoffTurn), never by a reader.
    events :: Seq.Seq LoggedEvent.LoggedEvent,
    -- | The group Event.recordEvent stamps on the next event; advanced per
    -- record except inside an Event.simultaneously bracket. Not reset at turn
    -- handoff.
    nextEventGroup :: EventGroup.EventGroup,
    -- | How deep the Event.simultaneously brackets are nested; the outermost
    -- bracket's group wins.
    eventGroupDepth :: Natural.Natural,
    -- | CR 608.2h / 113.7a: last known information, keyed by the id an object
    -- had before it left a zone (CR 400.7). Grows for the whole game.
    lastKnown :: Map.Map ObjectId.ObjectId LastKnown.LastKnown,
    -- | CR 117.5: how far the trigger scan has consumed the event log.
    scannedThrough :: Natural.Natural,
    -- | CR 603.10: the battlefield as it stood immediately after each unscanned
    -- event, keyed by group, one lazy entry per group; read by
    -- Event.eventTriggers, cleared as the scan consumes the log.
    battlefieldWhenTriggered :: Map.Map EventGroup.EventGroup (Map.Map ObjectId.ObjectId (BattlefieldCandidate.BattlefieldCandidate ProjectedCharacteristics.ProjectedCharacteristics)),
    -- | Who controlled each permanent the last time
    -- Pawl.Engine.Engine.sampleControl looked; diffing it against the live
    -- projection is what yields GameEvent.ControlChanged (CR 603.2). Rebuilt
    -- at every sample, so a first sighting is not a change.
    controlSample :: Map.Map ObjectId.ObjectId PlayerId.PlayerId,
    -- | How far the state-based action check has consumed the event log --
    -- CR 704.5h's and CR 903.9a's "since the last state-based action check".
    damageScannedThrough :: Natural.Natural,
    -- | CR 603.7: delayed triggered abilities awaiting their event, in creation
    -- order; not cleared at turn handoff.
    delayedTriggers :: Seq.Seq DelayedTrigger.DelayedTrigger,
    -- | CR 611.2: stored continuous effects from resolutions, each with an
    -- expiry; static-ability effects are re-derived live instead.
    continuousEffects :: [ContinuousEffect.ContinuousEffect Card.Card],
    -- | CR 614.3 / 615.3: floating replacement effects from resolutions, each
    -- with an expiry and a use count; static replacement abilities are
    -- re-derived live instead.
    replacements :: [ActiveReplacement.ActiveReplacement],
    -- | CR 615.5: prevention applications whose additional effect has not run
    -- yet, drained by Resolve.runPreventionRiders before the next state-based
    -- action check (CR 704.3). Empty at every priority window.
    pendingPreventionRiders :: Seq.Seq Prevention.Prevention,
    -- | CR 615.5's amount channel: the amount bindings a prevention rider can
    -- read outside any object's environment, its recipient possibly being a
    -- player. Empty at every priority window.
    ambientAmounts :: Map.Map SlotName.SlotName Natural.Natural,
    -- | CR 729.5: the slots a resolution filled after its own object ceased to
    -- exist, keyed by that object's id; the fallback Pawl.Engine.Resolve reads
    -- for an object it can no longer find. Never pruned.
    detachedBindings :: Map.Map ObjectId.ObjectId (Map.Map SlotName.SlotName Binding.Binding),
    -- | CR 614.1c: as-enters rewrites whose effects have not run yet, drained
    -- first thing in Pawl.Engine.Engine.performSettle, before the SBA pass and
    -- the trigger scan. Empty at every priority window.
    --
    -- Not implemented: the effects running inside the entry, where CR 614.1c puts
    -- them. A resolution that puts a permanent onto the battlefield and then reads
    -- the board itself would see the pre-effect one (#1639); no card in the pool
    -- does that.
    pendingEntryEffects :: Seq.Seq PendingEntryEffect.PendingEntryEffect,
    -- | CR 113.6 / 614.12: the permanents entering beside the one whose entry
    -- loop is running -- materialized but not yet entered, so their static
    -- abilities do not function. Never holds the loop's own subject. Empty at
    -- every priority window.
    enteringBeside :: Set.Set ObjectId.ObjectId,
    -- | CR 614.12: the subject of every entry loop currently running,
    -- materialized but not entered; with `enteringBeside`, what
    -- Pawl.Engine.Projection.boardAsEntering subtracts. Empty at every priority
    -- window.
    enteringSubjects :: Set.Set ObjectId.ObjectId,
    -- | CR 614.1c: the counters each entering permanent is so far going to
    -- enter with, pending until runEntry flushes them onto the object. Empty
    -- outside an entry.
    enteringCounters :: Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural),
    -- | CR 611.1 / 613.11: stored player- and rules-modifying continuous effects
    -- from resolutions, each with an expiry; printed ones are re-derived live.
    playerEffects :: [ActivePlayerEffect.ActivePlayerEffect],
    -- | CR 509.1c / 613.11: stored blocking requirements from resolutions, each
    -- with an expiry; printed ones are re-derived live.
    blockRequirements :: [ActiveBlockRequirement.ActiveBlockRequirement],
    -- | CR 508.1d / 613.11: stored attacking requirements from resolutions, each
    -- with an expiry; printed ones are re-derived live.
    attackRequirements :: [ActiveAttackRequirement.ActiveAttackRequirement],
    -- | CR 701.19c / 611.1: stored regeneration prohibitions from resolutions,
    -- each with an expiry, read at Pawl.Engine.Event.resolveDestruction.
    unregeneratables :: [ActiveUnregeneratable.ActiveUnregeneratable],
    -- | CR 509.1b / 611.1: stored blocking restrictions from resolutions, each
    -- with an expiry; printed ones are re-derived live.
    blockProhibitions :: [ActiveBlockProhibition.ActiveBlockProhibition],
    -- | CR 508.1c / 611.1: stored attacking restrictions from resolutions, each
    -- with an expiry; printed ones are re-derived live.
    attackProhibitions :: [ActiveAttackProhibition.ActiveAttackProhibition],
    -- | CR 116.2d: the ignores players have paid for, each with an expiry, read
    -- by Pawl.Engine.PlayerEffect.applying.
    ignoredAbilities :: [IgnoredAbility.IgnoredAbility],
    -- | The seating order (CR 800.5) rotated so the starting player is first,
    -- which CR 103.1 makes the turn order. Lists every player who began the
    -- game and is never shortened -- CR 800.4a and 800.4m need a departed
    -- player's seat; who is still in is Game.stillPlaying.
    turnOrder :: [PlayerId.PlayerId],
    activePlayer :: PlayerId.PlayerId,
    phase :: Phase.Phase,
    -- | CR 500: the steps still scheduled this turn, in order, `phase` being the
    -- one in progress; CR 508.8 drops steps and CR 510.4, 500.8 and 500.9
    -- splice them in. Refilled from `Turn.allPhases` at handoff.
    remaining :: Seq.Seq Phase.Phase,
    priority :: Maybe PlayerId.PlayerId,
    passes :: Natural.Natural,
    turnNumber :: Natural.Natural,
    result :: Maybe Result.Result,
    -- | CR 727.4: raised while a restart has replaced this game underneath the
    -- frames still running it; Engine.runStep lowers it as turn 1's untap step
    -- begins.
    restartSignal :: RestartSignal.RestartSignal,
    -- | CR 724.1: raised while an effect that ends the turn unwinds through the
    -- frames running the ended step; Engine.runStep lowers it as CR 724.1d's
    -- cleanup step begins.
    endTurnSignal :: EndTurnSignal.EndTurnSignal,
    nextObjectId :: ObjectId.ObjectId,
    -- | Every printing this game knows, by the id the objects carry; two ids
    -- naming one card is benign (CR 109.3). Append-only and never collected.
    printings :: Map.Map PrintingId.PrintingId Printing.Printing,
    -- | `printings` read backwards, so Pawl.Engine.Game.intern answers with the
    -- id a printing already has. A deck listing its commander among its cards
    -- too (CR 903.5b forbids it; #940 means pawl does not enforce it) behaves
    -- like a well-formed one.
    printingIds :: Map.Map Printing.Printing PrintingId.PrintingId,
    nextPrintingId :: PrintingId.PrintingId,
    -- | CR 613.7: the monotonic source of timestamps for objects and stored
    -- continuous effects. See Timestamp.
    nextTimestamp :: Timestamp.Timestamp,
    -- | CR 104.4b: the timestamp as of the last optional action offered to a
    -- player, advanced only by Pawl.Engine.Game.choose (conceding excepted, CR
    -- 104.3a); its gap from nextTimestamp is
    -- Pawl.Engine.Engine.checkMandatoryLoop's heuristic.
    lastChoice :: Timestamp.Timestamp,
    drewFromEmpty :: Set.Set PlayerId.PlayerId,
    -- | CR 305.2a: how many lands each player has played this turn, a count
    -- because CR 305.2 lets an effect raise the allowance; cleared per player
    -- at their untap step.
    landsPlayed :: Map.Map PlayerId.PlayerId Natural.Natural,
    -- | CR 121.1: how many cards each player has drawn this turn, stamped as the
    -- ordinal onto GameEvent.Drew; cleared for every player at turn handoff,
    -- and after CR 103.3's opening hands.
    drawsThisTurn :: Map.Map PlayerId.PlayerId Natural.Natural,
    -- | CR 723.1: pending player-controlling effects, keyed by the player to be
    -- controlled; last created wins (CR 723.1a), promoted to activeControl at
    -- that player's turn (CR 723.1b).
    pendingControl :: Map.Map PlayerId.PlayerId Decider.Decider,
    -- | CR 723.1 / 723.3: the decider controlling the active player this turn,
    -- overwritten at every turn start.
    activeControl :: Maybe Decider.Decider,
    -- | CR 725.1 / 725.3: the monarch, a single game-wide designation.
    monarch :: Maybe PlayerId.PlayerId,
    -- | CR 726.1 / 726.3: the initiative, the monarch's sibling designation.
    initiative :: Maybe PlayerId.PlayerId,
    -- | CR 731.1: the game's day\/night designation; Nothing is the rule's "neither",
    -- unreachable once either has been gained. Written only by
    -- Pawl.Engine.Daytime.
    daytime :: Maybe Daytime.Daytime,
    -- | CR 502.2 / 731.2: how many spells the previous turn's active player cast
    -- that turn, snapshotted at handoff from the outgoing log, which is cleared
    -- there. Not implemented: CR 731.2a's reading for the shared team turns
    -- option, which asks about a whole team (#2848).
    spellsCastLastTurn :: Natural.Natural,
    -- | CR 601.2i / 608.2i: how many spells each player cast during the turn
    -- just ended, sparse, snapshotted at the same handoff; spellsCastLastTurn
    -- is read out of it by Engine.beginTurnOf so the two cannot disagree.
    castsLastTurn :: Map.Map PlayerId.PlayerId Natural.Natural,
    -- | CR 725: objects exiled "until an opponent becomes the monarch", keyed
    -- by the exiled incarnation, swept by
    -- Pawl.Engine.Monarch.returnExiledForMonarch. Not an Expiry, which cannot
    -- perform a zone change.
    exiledUntilMonarch :: Map.Map ObjectId.ObjectId MonarchWatch.MonarchWatch,
    -- | CR 610.3: objects moved "until this leaves the battlefield", keyed by
    -- the incarnation the move minted, swept by
    -- Pawl.Engine.MoveDuration.returnMoved.
    movedUntilSourceLeaves :: Map.Map ObjectId.ObjectId ReturnWatch.ReturnWatch,
    -- | CR 702.55b: which object each haunting card haunts, keyed by the exiled
    -- incarnation; the value is never cleaned up, the haunted creature dying
    -- being the point.
    haunting :: Map.Map ObjectId.ObjectId ObjectId.ObjectId,
    -- | CR 607.2's linked set as a relation: the object each exiled card is
    -- linked to, keyed by the exiled incarnation and written by
    -- Pawl.Engine.Resolve.applyEffectWith (rule 607.2a) and Pawl.Engine.Event's
    -- zone change funnel (rule 607.2b) as a difference over GameState.exile,
    -- never a case over the opcode. Cleaned up by key only.
    --
    -- Scoped to the OBJECT and not to the printed ABILITY, where rule 607.2a
    -- scopes it to the ability. The two differ only for a card with two exiling
    -- abilities and two referring ones, since pawl has no ability identity to key
    -- on at all -- Pawl.Types.Source embeds the ability value, not an index. No
    -- printing in the pool is in that shape (#1535).
    exiledWith :: Map.Map ObjectId.ObjectId ObjectId.ObjectId,
    -- | CR 406.4: which pile each face-down exiled card is in, a name drawn from
    -- nextTimestamp and shared by every card one execution of one instruction
    -- exiled (Pawl.Engine.Resolve.recordExilePile). Cleaned up by key only.
    exilePiles :: Map.Map ObjectId.ObjectId Timestamp.Timestamp,
    -- | CR 500.7: the extra turns created and not yet taken, most recently
    -- created first, each carrying the steps that turn skips (CR 500.11).
    extraTurns :: [ExtraTurn.ExtraTurn],
    -- | CR 500.7 / 103.1: while an extra turn is under way, the seat the
    -- ordinary turn order resumes from; Nothing on an ordinary turn.
    turnAnchor :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
