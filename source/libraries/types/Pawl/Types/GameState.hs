module Pawl.Types.GameState where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasedOut as PhasedOut
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prevention as Prevention
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.RestartSignal as RestartSignal
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Timestamp as Timestamp

data GameState = MkGameState
  { objects :: Map.Map ObjectId.ObjectId Object.Object,
    library :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    hand :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    graveyard :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    -- | CR 400.1's battlefield, MINUS its phased-out permanents, which
    -- `phasedOut` below holds instead.
    battlefield :: Set.Set ObjectId.ObjectId,
    -- | CR 702.26b: the permanents whose status is "phased out", each mapped to
    -- the player who controlled it WHEN IT PHASED OUT. They are still
    -- ON the battlefield -- CR 702.26d says the phasing event changes no zone, and
    -- their `Object.zone` stays `Zone.Battlefield` to say so -- but rule 702.26b
    -- says that "except for rules and effects that specifically mention
    -- phased-out permanents, a phased-out permanent is treated as though it does
    -- not exist".
    --
    -- Keeping them in a SEPARATE set, rather than flagging them inside
    -- `battlefield`, is what makes that sentence true by construction: every
    -- reader that walks the battlefield -- the projection, targeting, the
    -- state-based actions, combat, cost payment -- is asking about existing
    -- permanents, and none of them has to remember phasing to get the right
    -- answer. Three rules sit on the other side of rule 702.26b's "except", and
    -- each is answered somewhere different:
    --
    --   * CR 502.1's phasing event, which reads and writes this field
    --     (Pawl.Engine.Phasing).
    --   * CR 702.26k, a phased-out permanent leaving the game with its owner,
    --     which deletes from it (Pawl.Engine.Game.removeFromZones).
    --   * CR 514.2's damage sweep, which does NOT name this field and does not
    --     have to: Pawl.Engine.Damage.removeAllDamage clears every object rather
    --     than every permanent, so a phased-out one is already covered.
    --
    -- The PLAYER is stored rather than recomputed because rule 702.26a asks who
    -- controlled the permanent "when it phased out", and CR 702.26e takes the
    -- live answer away: a phased-out permanent is not in the set of objects a
    -- continuous effect affects, so a control-changing effect that would name it
    -- must not be read while it is gone -- nor may one that expires meanwhile
    -- (CR 702.26f) hand it back to somebody else. A stored player is that
    -- sentence; Pawl.Engine.Projection.controllerOf is not.
    --
    -- The row also says WHICH of rule 702.26's two schedules the permanent is
    -- on, which is CR 702.26g's own distinction: a permanent that phased out
    -- directly phases in during its player's next untap step, while one that
    -- phased out indirectly "won't phase in by itself" and comes back with the
    -- permanent it is attached to. See Pawl.Types.PhasedOut.
    --
    -- What DOESN'T live here is why a permanent phased out DIRECTLY. Phasing
    -- back in on one's own is CR 702.26a's business and is read off the
    -- permanent's keywords; an unattached permanent phased out by an effect has
    -- no phasing ability and so is not among those that phase in.
    phasedOut :: Map.Map ObjectId.ObjectId PhasedOut.PhasedOut,
    exile :: Set.Set ObjectId.ObjectId,
    -- | CR 400.1: the command zone -- a shared collection (not per-player), keyed
    -- into `objects` like `battlefield`/`exile`. Emblems live here.
    command :: Set.Set ObjectId.ObjectId,
    stack :: [ObjectId.ObjectId],
    players :: Map.Map PlayerId.PlayerId Player.Player,
    -- | CR 106.4. Absent from the map means an empty pool.
    manaPool :: Map.Map PlayerId.PlayerId Mana.Mana,
    -- | CR 508/509. Lives for one combat phase; cleared at CR 511.
    combat :: Combat.Combat,
    -- | CR 608.2i: what happened this turn, in order. Appended by the
    -- change-and-emit funnels and by Engine.runStep's step-begin emission; NEVER
    -- cleared by a reader. Cleared with both watermarks at turn handoff
    -- (Engine.handoffTurn) -- not at cleanup, which is still part of this turn.
    --
    -- Each entry carries the EventGroup it belongs to (CR 704.3 / CR 608.2f's
    -- "single event"), stamped by Event.recordEvent. The log stays FLAT and both
    -- watermarks below stay counts of ELEMENTS: a group can legitimately be half
    -- consumed, since scannedThrough and damageScannedThrough drain the same log
    -- at different cadences. Groups are non-decreasing along the log, because
    -- recordEvent only ever mints a fresh one or repeats the frozen one.
    events :: Seq.Seq (EventGroup.EventGroup, GameEvent.GameEvent),
    -- | The group Event.recordEvent stamps on the next event it records.
    --
    -- Advanced past on each record, EXCEPT inside an Event.simultaneously
    -- bracket, which is what makes every event in the bracket's body share a
    -- group. NOT reset at turn handoff, where the log itself is cleared: nothing
    -- compares a group against a value, only against another group, so a monotone
    -- sequence keeps "later group" meaning what it says and the gap costs
    -- nothing. A restarted game (CR 727.1) and a subgame (CR 729.1a) do start the
    -- sequence over, because each is a new game rather than a later turn of this
    -- one.
    nextEventGroup :: EventGroup.EventGroup,
    -- | How deep the Event.simultaneously brackets are nested. Zero means each
    -- recorded event is its own group.
    --
    -- The OUTERMOST bracket wins, which is what a counter buys over a flag: a CR
    -- 704.3 state-based-action pass whose destruction batch is itself a CR 608.2f
    -- simultaneous action is still one event, and the nested bracket must not end
    -- the outer one's group early.
    eventGroupDepth :: Natural.Natural,
    -- | CR 608.2h / 113.7a: last known information, keyed by the id an object had
    -- BEFORE it left a zone.
    --
    -- Every zone change mints a fresh id (CR 400.7), so a departed object's OLD
    -- id names nothing in `objects`. That is exactly the condition under which
    -- this map is the answer, and it is why the key is the pre-move id: the id
    -- an ability on the stack still carries as its source is the old one.
    --
    -- Written by the same funnel that records the Moved event, from the same
    -- snapshot value, so the two cannot disagree. Both are kept because they
    -- answer different questions: the log answers "what happened, in order"
    -- (CR 608.2i) and needs the NEW id for an enters trigger to scan, while this
    -- answers "what was that object" by the OLD id, in one lookup. They are also
    -- read TOGETHER by Event.eventTriggers, which is how a permanent that enters
    -- and dies inside one CR 117.5 settle still gets its CR 603.6a entry trigger
    -- scanned.
    --
    -- Grows for the whole game, deliberately: an entry can be needed arbitrarily
    -- later (a delayed trigger's source, CR 603.7d), so there is no point at
    -- which pruning is provably safe.
    lastKnown :: Map.Map ObjectId.ObjectId LastKnown.LastKnown,
    -- | CR 117.5: how far the trigger scan has consumed. Everything at or after
    -- this index is unscanned. Consumption is an index bump; the record stays.
    scannedThrough :: Natural.Natural,
    -- | CR 603.10's first sentence: "objects that exist immediately after an event
    -- are checked to see if the event matched any trigger conditions, and
    -- continuous effects that exist at that time are used to determine what the
    -- trigger conditions are". The trigger scan runs once at the CR 117.5 boundary
    -- rather than at each event, so this is what "at that time" costs: the
    -- battlefield as it stood immediately after each unscanned event, keyed by that
    -- event's group.
    --
    -- Three answers ride on it, and the live board at the scan gets all three
    -- wrong on some board: WHICH permanents were there to be checked, WHAT
    -- abilities each of them had, and CR 603.3a's controller of each. Written by
    -- the one append point (Event.recordEvent) and read by Event.eventTriggers,
    -- which falls back to the live board for a group this does not name.
    --
    -- ONE ENTRY PER GROUP, last write winning, because a group is CR 608.2f's
    -- single event: its members are recorded one after another as the bracket
    -- applies them, so the LAST of them is the one standing "immediately after"
    -- the whole thing. Between groups the entries genuinely differ, which a
    -- per-batch sample cannot express -- a permanent that entered at the second
    -- group was not there to be checked against the first.
    --
    -- LAZY, and worth saying because it decides the cost: each entry is a thunk
    -- over the state at that moment, so a group whose entry the scan never reads
    -- costs nothing, and one it does costs the projection it would have paid at
    -- the scan anyway. Cleared as the scan consumes the log, which is what bounds
    -- both the map and the states its thunks retain.
    battlefieldWhenTriggered :: Map.Map EventGroup.EventGroup (Map.Map ObjectId.ObjectId (PlayerId.PlayerId, ProjectedCharacteristics.ProjectedCharacteristics)),
    -- | Who controlled each permanent on the battlefield the last time
    -- Pawl.Engine.Engine.sampleControl looked. The OBSERVATION POINT for a control
    -- change: control is derived (CR 613.1b layer 2), so nothing announces a
    -- change, and comparing this snapshot against the live projection is what
    -- turns one into the GameEvent.ControlChanged that CR 603.2 needs.
    --
    -- ONE READING, not a history, unlike battlefieldWhenTriggered above: a diff
    -- compares the board against the last look rather than against a named moment,
    -- so the entry a group would key is exactly what it must not keep.
    --
    -- REBUILT from the battlefield at every sample rather than updated in place,
    -- which is what prunes it: a permanent that left has no entry to go stale, and
    -- one that comes back is a new object by CR 400.7 with an id this never saw.
    -- An id absent from the snapshot is therefore first-sighted, and a first
    -- sighting is not a change -- which is what keeps a permanent ENTERING under
    -- a player who is not its owner (CR 110.2's Object.enteredUnder) from reading
    -- as a control change it never was.
    controlSample :: Map.Map ObjectId.ObjectId PlayerId.PlayerId,
    -- | How far the STATE-BASED ACTION check has consumed the event log --
    -- "since the last state-based action check", the boundary CR 704.5h names.
    --
    -- Not damage-only despite the name it was given for its first reader: CR
    -- 903.9a asks the same question of the same boundary ("that object was put
    -- into that zone since the last time state-based actions were checked"), and
    -- Pawl.Engine.Commander.returnable reads it through
    -- Pawl.Engine.Event.unscannedSbaEvents. One watermark, because there is one
    -- check -- a second field would be a duplicate that could only ever drift.
    damageScannedThrough :: Natural.Natural,
    -- | CR 603.7: delayed triggered abilities awaiting their event, in creation
    -- order. An entry is removed as it fires (CR 603.7b) unless it states a
    -- duration, in which case one of the Pawl.Engine.Expiry sweeps ends it
    -- instead. NOT cleared at turn handoff -- "at the beginning of the next end
    -- step" survives into the next turn if this turn's end step passed before
    -- the ability was armed.
    delayedTriggers :: Seq.Seq DelayedTrigger.DelayedTrigger,
    -- | CR 611.2: stored continuous effects from resolutions (Giant Growth,
    -- Serpent's Gift), each with an expiry the Pawl.Engine.Expiry sweeps consult.
    -- Static-ability effects are NOT here -- the projection re-derives those live.
    continuousEffects :: [ContinuousEffect.ContinuousEffect],
    -- | CR 614.3 / 615.3: floating replacement effects from resolutions (Fog's
    -- prevention, Drudge Skeletons' regeneration shield), each with an expiry the
    -- Pawl.Engine.Expiry sweeps consult (CR 514.2) and a use count (CR 614.3).
    -- The event-pipeline analog of continuousEffects; a permanent's STATIC
    -- replacement abilities are not here -- the projection re-derives those live.
    replacements :: [ActiveReplacement.ActiveReplacement],
    -- | CR 615.5: prevention applications whose additional effect has not run
    -- yet, oldest first. A QUEUE rather than a return value, because the module
    -- that applies a prevention (Pawl.Engine.Damage, below Pawl.Engine.Resolve)
    -- cannot run a card's effects and the module that can is two funnels away --
    -- widening the return type instead would reach every DamageSpec call site.
    --
    -- Filled by Damage.applyDamage with the preventions that carry a rider and
    -- drained by Resolve.runPreventionRiders, which its two callers run
    -- immediately after the damage and BEFORE the next state-based action check
    -- (CR 704.3). That gap is where the rule lives: the counters Test of Faith
    -- puts on are on before CR 704.5g asks whether the creature died.
    --
    -- Empty at every priority window the engine reaches, so nothing else has to
    -- know it exists.
    pendingPreventionRiders :: Seq.Seq Prevention.Prevention,
    -- | CR 611.1 / 613.11: stored PLAYER and RULES-modifying continuous effects
    -- from resolutions (Silence), each with an expiry the Pawl.Engine.Expiry
    -- sweeps consult. A permanent's printed player abilities are NOT here --
    -- Pawl.Engine.PlayerEffect re-derives those live.
    playerEffects :: [ActivePlayerEffect.ActivePlayerEffect],
    -- | CR 509.1c / 613.11: stored BLOCKING REQUIREMENTS from resolutions
    -- (provoke), each with an expiry the Pawl.Engine.Expiry sweeps consult. A
    -- permanent's printed block requirements are NOT here --
    -- Pawl.Engine.BlockRequirement re-derives those live.
    blockRequirements :: [ActiveBlockRequirement.ActiveBlockRequirement],
    -- | CR 116.2d: the ignores players have paid for, each with an expiry the
    -- Pawl.Engine.Expiry sweeps consult. Read by Pawl.Engine.PlayerEffect.applying,
    -- which drops an ignored permanent's abilities for the ignoring player alone
    -- -- so this suppresses, where every neighbour above adds.
    ignoredAbilities :: [IgnoredAbility.IgnoredAbility],
    -- | The seating order. CR 800.5 (or CR 806.3 for Grand Melee) only says
    -- players determine SOME seating order; CR 103.1's last sentence is what
    -- makes it the turn order, beginning with the starting player -- so this
    -- field is that seating order, rotated so the starting player is first. It
    -- lists every player who BEGAN this game and is never shortened. Who is
    -- still IN the game is Game.stillPlaying, and every departure-aware read
    -- filters through that on top of this.
    --
    -- Three rules depend on a departed player keeping their seat:
    --   * CR 800.4m -- the seat is how the handoff knows when a departed player's
    --     turn WOULD have begun.
    --   * CR 800.4a -- finding the departed player's successor in turn order
    --     needs their own position.
    --   * CR 729.1b, whose real customer is Shahrazad's "each player who doesn't
    --     win the subgame": the full starting roster minus the winner (#138).
    turnOrder :: [PlayerId.PlayerId],
    activePlayer :: PlayerId.PlayerId,
    phase :: Phase.Phase,
    -- | CR 500. The steps still scheduled this turn, in order; `phase` is the one
    -- in progress. The turn is DATA: CR 508.8 drops steps from this, CR 510.4 and
    -- 500.8/500.9 splice steps and phases into it. `Turn.allPhases` is the
    -- template a new turn refills from (see Engine.handoffTurn).
    remaining :: Seq.Seq Phase.Phase,
    priority :: Maybe PlayerId.PlayerId,
    passes :: Natural.Natural,
    turnNumber :: Natural.Natural,
    result :: Maybe Result.Result,
    -- | CR 727.4: raised while a restart has replaced this game underneath the
    -- frames still running it, so Engine.priorityLoop and Engine.runStep unwind
    -- to the rebuilt turn 1 instead of acting on it. Transient: Engine.runStep
    -- lowers it as that turn's untap step begins.
    restartSignal :: RestartSignal.RestartSignal,
    nextObjectId :: ObjectId.ObjectId,
    -- | CR 613.7: the monotonic source of timestamps for objects (at creation) and
    -- stored continuous effects (at CR 611 creation). See Timestamp.
    nextTimestamp :: Timestamp.Timestamp,
    -- | CR 104.4b: the timestamp as of the last time a player was offered an
    -- OPTIONAL action. The gap between this and nextTimestamp is how many events
    -- have happened with no player able to decide anything, which is
    -- Pawl.Engine.Engine.checkMandatoryLoop's heuristic for a loop of mandatory
    -- actions.
    --
    -- Advanced only by Pawl.Engine.Game.choose, so a new prompt site cannot
    -- forget to reset it. Conceding is deliberately not one of its callers: CR
    -- 104.3a lets a player concede at any time, so if that counted as the
    -- optional action, no loop would ever be mandatory.
    --
    -- Pawl.Engine.Setup also SETS it, at the three seams where a game begins
    -- (emptyGame, restartGame, subgameStateFrom) and where one ends back into its
    -- parent (funnelBack). Each sets it to the timestamp supply's value there, so
    -- neither a fresh game nor a resumed one inherits a gap run up elsewhere.
    lastChoice :: Timestamp.Timestamp,
    drewFromEmpty :: Set.Set PlayerId.PlayerId,
    -- | CR 305.2a: how many lands each player has already played this turn --
    -- the right-hand side of that rule's comparison, whose left-hand side is
    -- Pawl.Engine.PlayerEffect.landPlaysAllowed. A player with no row here has
    -- played none.
    --
    -- A COUNT and not a yes/no, because CR 305.2 lets a continuous effect raise
    -- the number a player may play, so one is not the only interesting
    -- threshold (Exploration, Azusa Lost but Seeking).
    --
    -- Cleared PER PLAYER at that player's untap step (Engine.runTurnBasedActions),
    -- which is what makes it "this turn".
    landsPlayed :: Map.Map PlayerId.PlayerId Natural.Natural,
    -- | CR 121.1: how many cards each player has already drawn this turn. A player
    -- with no row here has drawn none. Written by Pawl.Engine.Event.drawCard, the
    -- one funnel every draw goes through, and read only there -- to stamp the
    -- ordinal onto the GameEvent.Drew it records, which is what a card asking
    -- "your second card each turn" -- or CR 702.94a's "the first card you've drawn
    -- this turn" -- actually matches against.
    --
    -- STORED rather than folded out of the turn-scoped event log, which would
    -- otherwise be the Event.declarationsOf precedent: CR 103.3's opening hands
    -- are drawn through the same funnel BEFORE the first turn begins, and that
    -- log is not cleared between the two. A field can be -- and is, at the end of
    -- Mulligan.openingHands -- while the log cannot without discarding the rest
    -- of setup's history.
    --
    -- Cleared for EVERY player at the turn handoff (Engine.beginTurnOf), not per
    -- player at their untap step as landsPlayed above is: a player draws on other
    -- players' turns (Howling Mine, an instant), so the tally that resets is the
    -- whole map.
    drawsThisTurn :: Map.Map PlayerId.PlayerId Natural.Natural,
    -- | CR 702.179d: the players whose inherent speed-increase ability has
    -- already triggered this turn -- that rule's "this ability triggers only
    -- once each turn", which nothing else in the game state records.
    --
    -- STORED rather than derived off the turn-scoped event log, unlike CR
    -- 508.3a's once-a-turn (Event.declarationsOf) and CR 606.3's
    -- (Activate.loyaltyActivatedThisTurn). Those two read a log entry that IS the
    -- thing being limited; this ability's limit is on the TRIGGER, and the log's
    -- nearest entry is the life loss, which is not the same event: an opponent
    -- losing life while the player has no speed (CR 702.179b) fires nothing,
    -- because CR 702.179d hangs the ability off "a player having 1 or more
    -- speed" -- and a log-derived limit would spend the turn's one trigger on it.
    --
    -- Cleared at the turn handoff (Engine.beginTurnOf), beside the event log, so
    -- "each turn" needs nothing to reset it per player. A Set and not a Bool
    -- because the rule scopes the limit to the ability, and each player controls
    -- their own.
    speedIncreasedThisTurn :: Set.Set PlayerId.PlayerId,
    -- | CR 723.1: pending player-controlling effects, keyed by the player to be
    -- controlled. Map.insert overwrites (CR 723.1a, last created wins). Promoted
    -- to activeControl at the actual start of that player's turn (CR 723.1b).
    pendingControl :: Map.Map PlayerId.PlayerId Decider.Decider,
    -- | CR 723.1/723.3: the decider controlling the ACTIVE player this turn, if any.
    -- Only the active player is ever controlled during their turn, so one Maybe
    -- suffices. Overwritten every turn start, so control ends at the next turn's
    -- beginning (CR 723.1).
    activeControl :: Maybe Decider.Decider,
    -- | CR 725.1/725.3: the monarch, a single game-wide player designation (at most
    -- one at a time). Nothing until a player becomes the monarch. On GameState,
    -- not Player, because it is one designation, not a per-player counter.
    monarch :: Maybe PlayerId.PlayerId,
    -- | CR 731.1: the game's day/night designation. Nothing is the rule's own
    -- third state, "the game starts with neither designation", and it is the only
    -- state that is not a Daytime -- CR 731.1's last sentence makes it
    -- unreachable once either designation has been gained, so nothing ever writes
    -- Nothing back.
    --
    -- On GameState and not on Object, for CR 725.1's reason rather than CR
    -- 701.54b's: "day and night are designations THE GAME ITSELF can have", which
    -- is the monarch's footing, where the Ring-bearer is a permanent's and rides
    -- Object.ringBearerFor. Pawl.Engine.Ring's haddock draws that line.
    --
    -- Written only by Pawl.Engine.Daytime, because becoming day or night also
    -- turns daybound and nightbound permanents over (CR 702.145c/f).
    daytime :: Maybe Daytime.Daytime,
    -- | CR 502.2 / 731.2: how many spells the PREVIOUS turn's active player cast
    -- during that turn -- the whole of what the untap step's day/night check asks
    -- about the turn just ended.
    --
    -- A stored snapshot rather than a fold over the event log, which is the one
    -- thing it cannot be: GameState.events is cleared at the handoff
    -- (Engine.beginTurnOf), and this is read AFTER that, in the next turn's untap
    -- step. Written by beginTurnOf from the outgoing turn's log, which is the last
    -- moment the count exists.
    --
    -- ONE number and not a map, because rule 731.2 names exactly one player, "the
    -- previous turn's active player". CR 731.2a's shared-team-turns variant asks
    -- about a whole team and would need more, and pawl has no teams (#175).
    spellsCastLastTurn :: Natural.Natural,
    -- | CR 725 (Palace Jailer): objects exiled "until an opponent becomes the
    -- monarch", keyed by the exiled incarnation id to the watch that ends the
    -- exile -- the effect's controller, plus the monarch as of the last look, so
    -- that a CHANGE of crown can be told from an opponent merely holding it. Not
    -- an Expiry: the Expiry sweeps are delete-and-recompute and cannot perform
    -- the return zone change.
    exiledUntilMonarch :: Map.Map ObjectId.ObjectId MonarchWatch.MonarchWatch,
    -- | CR 702.55b: which object each haunting card haunts, keyed by the exiled
    -- incarnation Effect.ExileHaunting minted and answering with the object that
    -- haunt ability targeted. Read by TriggerCondition.HauntedCreatureDies, which
    -- is the only thing rule 702.55b's link is for.
    --
    -- Board state rather than a field on the exiled Object, for exiledUntilMonarch's
    -- reason one field up: it is a relation between two ids that outlives neither,
    -- and CR 400.7 would strip it from an object that moved again anyway.
    --
    -- Never cleaned up on the VALUE side: rule 702.55b keeps naming the object the
    -- haunt ability targeted after that object is gone (the haunted creature dying
    -- is the whole point), so only Pawl.Engine.Departure's CR 800.4a sweep removes
    -- an entry, and only by its key.
    haunting :: Map.Map ObjectId.ObjectId ObjectId.ObjectId,
    -- | CR 607.2a's linked set, as a relation: which object's ability exiled each
    -- card now in exile. Keyed by the EXILED incarnation (the id CR 400.7 minted
    -- as the card arrived in exile), valued by the source of the ability whose
    -- instruction exiled it. Read only by ObjectRef.EachCardExiledWithSource.
    --
    -- Board state rather than a field on the exiled Object, for `haunting`'s
    -- reason one field up: it is a relation between two ids that outlives neither.
    -- The VALUE is what forces it -- Hoarding Dragon's second ability is a dies
    -- trigger, so the object the link names is already gone by the time the link
    -- is read, and CR 603.10a's look-back is what makes the trigger's source the
    -- battlefield incarnation that did the exiling rather than the graveyard card.
    --
    -- Written by Pawl.Engine.Resolve.applyEffectWith, which files every card that
    -- ARRIVED in exile while an effect ran against that effect's source. A
    -- difference over GameState.exile and not a case over the opcode: rule 607.2a
    -- asks which ability's instruction exiled the card, never which instruction it
    -- was, so the rules core stays off effect identity and Search's exile, a
    -- MoveToZone's and a keyword's are all one road.
    --
    -- Scoped to the OBJECT and not to the printed ABILITY, where rule 607.2a
    -- scopes it to the ability. The two differ only for a card with two exiling
    -- abilities and two referring ones, since pawl has no ability identity to key
    -- on at all -- Pawl.Types.Source embeds the ability value, not an index. No
    -- printing in the pool is in that shape (#1535). Not implemented either: an
    -- exile another object's REPLACEMENT effect causes inside that window is filed
    -- against the resolving ability's source, where CR 607.2b links it to the
    -- replacement's own object (#1536).
    --
    -- Cleaned up by KEY only, never by value, for `haunting`'s reason: an entry
    -- whose value is gone is exactly the entry Hoarding Dragon reads. Two sweeps
    -- do it -- recordExiledWith drops the key of a card that has left exile,
    -- since CR 400.7 gives that card a new id and the old one can never be named
    -- again, and Pawl.Engine.Departure drops the keys CR 800.4a takes out of the
    -- game with their owner.
    exiledWith :: Map.Map ObjectId.ObjectId ObjectId.ObjectId,
    -- | CR 500.7: the extra turns that have been created and not yet taken, MOST
    -- RECENTLY CREATED FIRST. A stack, not a queue, and a list precisely because
    -- the style guide reserves lists for stacks (GameState.stack is the other
    -- one). Popped by Engine.handoffTurn before the seating order is consulted.
    --
    -- One entry per turn, so two effects giving one player an extra turn each
    -- are two entries: CR 500.7 adds extra turns one at a time, which makes them
    -- countable rather than a set of players. Each entry also carries the steps
    -- and phases THAT turn skips, which is how a CR 500.11 skip printed as "the
    -- untap step of that turn" (Savor the Moment) names a turn without a
    -- reference that could dangle.
    extraTurns :: [ExtraTurn.ExtraTurn],
    -- | CR 500.7 / 103.1: while an EXTRA turn is under way, the seat the ordinary
    -- turn order resumes from -- the active player of the most recent turn that
    -- was not an extra one. Nothing on an ordinary turn, where that seat IS
    -- GameState.activePlayer and there is nothing to remember.
    --
    -- CR 500.7 adds a turn directly after the specified turn and takes nothing
    -- away, so the turn that would have followed it still follows it. Anchoring
    -- the CR 103.1 walk on activePlayer instead would make an extra turn CONSUME
    -- the taker's ordinary turn whenever the two are different players -- Time
    -- Warp aimed at an opponent.
    turnAnchor :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Show)
