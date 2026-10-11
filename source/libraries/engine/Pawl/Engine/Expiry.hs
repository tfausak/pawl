-- CR 611.2: the life cycle of a stored effect's duration. The ONLY module that
-- may case on Pawl.Types.Expiry -- the standing Pawl.Engine.Resolve has over
-- Effect and Pawl.Engine.Projection over Modification. It owns the
-- transformation from the PRINTED Duration to the STORED Expiry (`arm`) and
-- every sweep that ends one, over the carriers traverseExpiries names, which
-- share one expiry vocabulary and so share one sweep.
--
-- Carriers that hold no Expiry at all are swept here anyway: rule 701.35a fixes
-- a detain's duration, rule 701.15a a goad's and rule 702.171b a saddle's, so
-- each is a field on an object rather than a vocabulary of durations, and one
-- sweep alone reaches it. See clearedDetentions, clearedGoads and
-- clearedSaddles.
--
-- CR 800.4c is NOT asked here, and deliberately: a sweep that ends a
-- control-changing effect leaves the object reading as controlled by its CR
-- 110.2 default controller, and Pawl.Engine.Departure.exileOrphanedByEndedControl
-- answers the rule off that board at the next CR 117.5 settle. Nothing can
-- observe the gap, the cleanup step included (CR 514.3a).
module Pawl.Engine.Expiry where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Const as Const
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Players as Players
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as View
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.ActiveCopy as ActiveCopy
import qualified Pawl.Types.ActiveEvasion as ActiveEvasion
import qualified Pawl.Types.ActiveObjectProhibition as ActiveObjectProhibition
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AfterObjectTurn as AfterObjectTurn
import qualified Pawl.Types.AfterTurn as AfterTurn
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Designation as Designation
import Pawl.Types.Duration (Duration)
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import Pawl.Types.Expiry (Expiry)
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PaidExpiry as PaidExpiry
import qualified Pawl.Types.Phase as Phase
import Pawl.Types.PhaseSelector (PhaseSelector)
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import Pawl.Types.Recipient (Recipient)
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.While as While

-- CR 611.2: the moment a duration BEGINS. `controller` is the effect's
-- controller -- CR 109.5's "you" -- and `source` is the object the effect comes
-- from. Nothing means the duration never started, so per CR 611.2b the effect
-- does nothing and is never stored at all.
--
-- `targets` is the resolution's binding environment -- CR 601.2c's chosen
-- recipients, already filtered by CR 608.2b. Projected to the seats it names
-- (Binding.playersIn) it is what a CR 611.2b condition saying "that player" is
-- baked against, HERE and only here -- see the ForAsLongAs arm. It is also what
-- UntilEndOfNextTurnOf's reference is sampled from, which is why the whole
-- environment is passed rather than the seats alone: that arm may name a slot
-- holding an OBJECT. Empty for a caller with no resolution behind it, whose
-- durations name no slot.
--
-- CR 611.2b's second sentence is vacuous here: this runs once, at the point the
-- effect would be stored, and no opcode both ends and restarts a condition
-- mid-resolution.
arm :: Map.Map SlotName (Set.Set Recipient) -> PlayerId -> ObjectId -> Duration -> GameState -> Maybe Expiry
arm targets controller source duration gs = case duration of
  Duration.UntilEndOfTurn -> Just Expiry.AtCleanup
  Duration.Indefinite -> Just Expiry.Never
  -- Alchemy's "perpetually": Never's lifetime under an arm a zone change can
  -- find (Pawl.Engine.Event.perpetuate).
  Duration.Perpetual -> Just Expiry.Perpetual
  Duration.UntilYourNextTurn -> Just (Expiry.AtTurnOf controller)
  -- CR 503 / 611.2a: the arm above's seat, ended at that player's next upkeep.
  Duration.UntilYourNextUpkeep -> Just (Expiry.AtUpkeepOf controller)
  -- CR 611.2a: "until the end of your next turn". Two samples, both taken here
  -- and neither ever rewritten: the controller (CR 109.5's "you", as above) and
  -- the turn this duration began on. dropAtCleanup ends it at the first turn of
  -- the controller's numbered ABOVE that one -- so a duration that began during
  -- their own turn survives that turn's cleanup, which is the whole difference
  -- between this arm and the one above.
  Duration.UntilEndOfYourNextTurn ->
    Just (Expiry.AtEndOfTurnOf (AfterTurn.MkAfterTurn controller (GameState.turnNumber gs)))
  -- CR 611.2a: the arm above's window with the seat SAMPLED from a reference
  -- instead of taken from CR 109.5's "you". The turn number is sampled the same
  -- way and for the same reason, so the two arms share dropAtCleanup's reading
  -- whole and differ only in whose turn is counted.
  --
  -- Nothing where the reference names nobody, which is CR 611.2b's "the duration
  -- never started": a slot emptied by CR 608.2b holds no recipient, and an effect
  -- whose window cannot begin is not stored. Silent, deliberately -- the
  -- alternative is arming against some other seat.
  Duration.UntilEndOfNextTurnOf ref ->
    fmap
      (\pid -> Expiry.AtEndOfTurnOf (AfterTurn.MkAfterTurn pid (GameState.turnNumber gs)))
      (seatOf targets controller gs ref)
  -- CR 611.2a: the same seat and the same turn number as the arm above, under an
  -- arm that also states a BEGINNING. Sampled through seatOf for that arm's
  -- reasons, and Nothing where the reference names nobody for that arm's reason
  -- too -- a window that cannot begin stores nothing.
  --
  -- A ControllerOfBound naming an object still in the game is NOT sampled: its
  -- seat is read live and pinned once its controller's declare attackers step
  -- has passed (pinAfterDeclareAttackers), so a control change before then
  -- moves the window (CR 611.2a).
  Duration.DuringNextTurnOf ref -> case ref of
    PlayerRef.ControllerOfBound slot
      | Just oid <- Map.lookup slot (Binding.objectsIn targets),
        Maybe.isJust (Game.lookupObject oid gs) ->
          Just (Expiry.DuringTurnOfControllerOf (AfterObjectTurn.MkAfterObjectTurn oid (GameState.turnNumber gs)))
    _ ->
      fmap
        (\pid -> Expiry.DuringTurnOf (AfterTurn.MkAfterTurn pid (GameState.turnNumber gs)))
        (seatOf targets controller gs ref)
  -- CR 611.2a: the arm above's window with the seat taken from CR 109.5's "you",
  -- as UntilYourNextTurn takes it. Never Nothing -- a controller is always a
  -- seat, so this window always begins.
  Duration.DuringYourNextTurn ->
    Just (Expiry.DuringTurnOf (AfterTurn.MkAfterTurn controller (GameState.turnNumber gs)))
  -- CR 611.2a / 500.7: the extra turn this resolution created, named by its
  -- creation stamp as Onset.FromThatExtraTurn names it (Turn.thatExtraTurn).
  -- Nothing where no such turn is pending: the window cannot begin, so per CR
  -- 611.2b's reading above nothing is stored.
  Duration.DuringThatExtraTurn -> fmap Expiry.DuringExtraTurn (Turn.thatExtraTurn source controller gs)
  -- BAKED, and stored baked: the condition outlives the resolution that stored
  -- it, and sweepConditional below re-reads it off the effect's
  -- SOURCE, whose bindings never held the resolution's slots. An InSlot left
  -- standing would answer Nothing there, Condition.holds would collapse that to
  -- False, and the effect would silently end at the first settle -- or, for an
  -- ability, never start at all, since the same unresolvable reference is read
  -- one line below.
  Duration.ForAsLongAs cond ->
    let baked = Condition.bakeBound targets cond
     in if Condition.holds (Projection.fullView gs) (Projection.sourceContext gs (Just controller) source) gs source baked
          then Just (Expiry.While (While.MkWhile controller baked))
          else Nothing
  -- CR 500.5a / 511.2: "until end of combat" is the end of the combat PHASE, so
  -- the stored window is PhaseSelector.CombatPhase and never the end of combat
  -- step. Naming the phase is the whole of the arming: unlike UntilYourNextTurn
  -- there is nothing about the game to bake in, because every producer arms it
  -- during combat and the sweep ends the effect at the first combat phase whose
  -- end it sees.
  Duration.UntilEndOfCombat -> Just (Expiry.AtEndOf PhaseSelector.CombatPhase)
  -- CR 500.5a / 611.2a: UntilEndOfYourNextTurn's two samples, so the combat
  -- phase that ends it is one of the controller's LATER turns and never the
  -- current one's.
  Duration.UntilEndOfCombatOnYourNextTurn ->
    Just (Expiry.AtEndOfCombatOn (AfterTurn.MkAfterTurn controller (GameState.turnNumber gs)))
  -- CR 116.2c: nothing about the game's clock is baked in, because no window of
  -- the turn ends this -- the PRICE is carried through unchanged so the offer
  -- below can quote it and the payment charge it. The SEAT is baked, as
  -- UntilYourNextTurn's is: `controller` here is the resolving object's
  -- controller, stamped at the ability's creation (CR 602.2a, Resolve's
  -- effectController), which is exactly CR 109.5's "the player who activated the
  -- ability" -- and unlike that rule's static-ability sentence it must not follow
  -- the source's control afterwards.
  Duration.UntilPaid cost -> Just (Expiry.WhenPaid (PaidExpiry.MkPaidExpiry controller cost))
  -- CR 611.2a: nothing about the game's clock or a payment is baked in --
  -- Pawl.Engine.PlayerEffect.spentByCast/spentByLandPlay are what end this
  -- early, matching the stored effect's own Filter against whatever was just
  -- cast or played. Those read only a player effect on PlayerEffect.castUse's
  -- axes, so Pawl.EffectLintSpec refuses the duration on every other carrier
  -- and axis.
  Duration.UntilUsed -> Just Expiry.WhenUsed

-- The ONE seat a Duration.UntilEndOfNextTurnOf or DuringNextTurnOf reference
-- names, sampled as the window begins. One stored row names one turn, so a
-- reference naming several seats arms nothing; no printing states such a window
-- (Scryfall o:"each opponent's next turn", o:"opponents' next turns",
-- 2026-09-25, no hit).
--
-- Pawl.Engine.Players' reading over the resolution's slots, CR 608.2h's last
-- known information answering a controller or owner: Suspend Aggression's "its
-- owner" is asked of a card already in exile, which CR 108.4 leaves with no
-- controller, and CR 108.4a then answers with the owner.
--
-- Not cut to the controller's range: a window names a turn and affects nobody,
-- which is what CR 801.10 cuts.
--
-- Candidate names nobody here: it is the member a per-player fold has reached,
-- and the one fold that states such a window substitutes the member as Specific
-- before arming (perSeat).
seatOf :: Map.Map SlotName (Set.Set Recipient) -> PlayerId -> GameState -> PlayerRef.PlayerRef -> Maybe PlayerId
seatOf targets controller gs ref =
  let given = Players.resolution (`Projection.controllerWithLastKnown` gs) (`Game.ownerWithLastKnown` gs) targets controller gs
   in case Players.named given {Players.reaches = const True} gs ref of
        Just [pid] -> Just pid
        _ -> Nothing

-- CR 611.2a: "each opponent can't cast instant or sorcery spells during THAT
-- PLAYER's next turn" (Sphinx's Decree) -- a window whose seat is the member a
-- per-player fold has reached, so one resolution states one window per member.
-- Just the duration with that member baked in as PlayerRef.Specific, for a
-- duration naming the fold's Candidate; Nothing for every other duration, whose
-- window is the same for every member. Pawl.Engine.Resolve.Effect's
-- Effect.AffectPlayers arm is the fold.
perSeat :: Duration -> Maybe (PlayerId -> Duration)
perSeat duration = case duration of
  Duration.UntilEndOfNextTurnOf PlayerRef.Candidate -> Just (Duration.UntilEndOfNextTurnOf . PlayerRef.Specific)
  Duration.UntilEndOfNextTurnOf _ -> Nothing
  Duration.DuringNextTurnOf PlayerRef.Candidate -> Just (Duration.DuringNextTurnOf . PlayerRef.Specific)
  Duration.DuringNextTurnOf _ -> Nothing
  Duration.UntilEndOfTurn -> Nothing
  Duration.Indefinite -> Nothing
  Duration.Perpetual -> Nothing
  Duration.UntilYourNextTurn -> Nothing
  Duration.UntilYourNextUpkeep -> Nothing
  Duration.UntilEndOfYourNextTurn -> Nothing
  Duration.DuringYourNextTurn -> Nothing
  Duration.DuringThatExtraTurn -> Nothing
  Duration.ForAsLongAs _ -> Nothing
  Duration.UntilEndOfCombat -> Nothing
  Duration.UntilEndOfCombatOnYourNextTurn -> Nothing
  Duration.UntilPaid _ -> Nothing
  Duration.UntilUsed -> Nothing

-- Does a stored effect under this duration FOLLOW its objects across a zone
-- change? CR 400.7's default is no -- the object that arrives is a new object
-- with no memory of the old one -- and Alchemy's "perpetually" is the one
-- duration that says otherwise. The only reader is
-- Pawl.Engine.Event.perpetuate, which cannot ask the question itself: this
-- module is the only one that may case on Pawl.Types.Expiry.
--
-- Never answers False, which is the whole reason Perpetual is a separate arm:
-- the two share a lifetime and differ here.
follows :: Expiry -> Bool
follows expiry = case expiry of
  Expiry.Perpetual -> True
  Expiry.AtCleanup -> False
  Expiry.Never -> False
  Expiry.While {} -> False
  Expiry.AtTurnOf _ -> False
  Expiry.AtUpkeepOf _ -> False
  Expiry.AtEndOfTurnOf _ -> False
  Expiry.DuringTurnOf _ -> False
  Expiry.DuringTurnOfControllerOf _ -> False
  Expiry.DuringExtraTurn _ -> False
  Expiry.AtEndOf _ -> False
  Expiry.AtEndOfCombatOn _ -> False
  Expiry.WhenPaid _ -> False
  Expiry.WhenUsed -> False

-- CR 611.2a: has this duration's window BEGUN? Every arm but DuringTurnOf and
-- DuringExtraTurn states only an end, so the answer for them is True from the
-- moment the effect is stored and a sweep is the whole of their life cycle.
-- Those two name a turn that has not started yet, and a reader that asks only
-- whether the row was swept applies it early.
--
-- The reading is dropAtCleanup's, one turn shifted: the window is the first turn
-- of the named player numbered ABOVE the one the duration began on, so it is
-- open exactly while that player is active on such a turn. The row is dropped at
-- that turn's cleanup, so no LATER turn of theirs can be mistaken for it and the
-- bound needs no upper half.
--
-- Asked by Pawl.Engine.CombatRestriction, Pawl.Engine.AttackRequirement,
-- Pawl.Engine.Cast.permitsPlayFromExile and Pawl.Engine.PlayerEffect.applying;
-- Pawl.Types.Expiry says why no other carrier asks.
begun :: GameState -> Expiry -> Bool
begun gs expiry = case expiry of
  Expiry.DuringTurnOf afterTurn ->
    Turn.isActive gs (AfterTurn.player afterTurn)
      && GameState.turnNumber gs > AfterTurn.turn afterTurn
  -- Read live until pinned: open on a turn that is its window.
  Expiry.DuringTurnOfControllerOf afterObjectTurn -> windowReached gs afterObjectTurn
  -- CR 500.7: open exactly while the extra turn it names is the one under way.
  Expiry.DuringExtraTurn stamp -> GameState.extraTurnUnderWay gs == Just stamp
  Expiry.AtCleanup -> True
  Expiry.Never -> True
  Expiry.Perpetual -> True
  Expiry.While {} -> True
  Expiry.AtTurnOf _ -> True
  Expiry.AtUpkeepOf _ -> True
  Expiry.AtEndOfTurnOf _ -> True
  Expiry.AtEndOf _ -> True
  Expiry.AtEndOfCombatOn _ -> True
  Expiry.WhenPaid _ -> True
  Expiry.WhenUsed -> True

-- CR 601.3: may this player use this object's exile permission right now? It
-- names them, its duration's window has begun (`begun`), and its own condition
-- holds -- Hama, the Bloodbender's "during your turn", read live against the
-- permission's source as sweepConditional reads a duration's. The one question
-- Pawl.Engine.Cast.permitsPlayFromExile and Pawl.Engine.Cost's exile arm share,
-- so a cast allowed by some other permission outside this one's window is never
-- priced by it.
permissionOpen :: PlayerId -> ExilePlayPermission.ExilePlayPermission -> GameState -> Bool
permissionOpen pid permission gs =
  let source = ExilePlayPermission.source permission
   in ExilePlayPermission.player permission == pid
        && begun gs (ExilePlayPermission.expiry permission)
        && all
          (Condition.holds (Projection.fullView gs) (Projection.sourceContext gs (Just pid) source) gs source)
          (ExilePlayPermission.condition permission)

-- CR 514.2: "until end of turn" and "this turn" effects end during the cleanup
-- step. Delete-and-recompute (design.md 2.5): dropping the stored entry makes
-- the next projection revert -- nothing is explicitly undone.
--
-- A turn whose ENDING PHASE was skipped never reaches the cleanup step's
-- turn-based actions, and Engine.cleanupSecondAction runs this there anyway: CR
-- 611.2a ends the duration when the turn ends, whether or not the step that
-- normally sweeps it happened.
dropAtCleanup :: GameState -> GameState
dropAtCleanup gs =
  let survives expiry = case expiry of
        Expiry.AtCleanup -> False
        Expiry.Never -> True
        -- Alchemy's "perpetually" lasts for the rest of the game, as Never does.
        Expiry.Perpetual -> True
        Expiry.While {} -> True
        Expiry.AtTurnOf _ -> True
        Expiry.AtUpkeepOf _ -> True
        -- CR 611.2a: "until the end of your next turn" ends as that turn ends,
        -- and the cleanup step is where a turn's effects end -- so the sweep
        -- CR 514.2 runs for the until-end-of-turn ones ends this too, one NAMED
        -- turn later. The turn is named by the pair (see Pawl.Types.AfterTurn):
        -- this cleanup belongs to that player, and its number is above the one
        -- the duration began on, so it is not the duration's own turn.
        Expiry.AtEndOfTurnOf afterTurn ->
          not (Turn.isActive gs (AfterTurn.player afterTurn))
            || GameState.turnNumber gs <= AfterTurn.turn afterTurn
        -- CR 611.2a: "during that player's next turn" ends where the arm above
        -- ends, off the same pair and by the same reading -- the window it
        -- states is that one turn, so the cleanup that ends the turn ends it.
        -- The BEGINNING the arm also states is `begun`'s half and is not asked
        -- here: a row swept before it ever began is a row whose turn came and
        -- went, which is the cleanup this arm reaches.
        Expiry.DuringTurnOf afterTurn ->
          not (Turn.isActive gs (AfterTurn.player afterTurn))
            || GameState.turnNumber gs <= AfterTurn.turn afterTurn
        -- Unpinned. Ended by the cleanup of a turn that was its window but had
        -- no declare attackers step to pin it (CR 500.11, a skipped combat);
        -- otherwise kept while the object exists, and dropped as hygiene once
        -- it does not.
        Expiry.DuringTurnOfControllerOf afterObjectTurn ->
          Maybe.isJust (Game.lookupObject (AfterObjectTurn.object afterObjectTurn) gs)
            && not (windowReached gs afterObjectTurn)
        -- CR 611.2a / 500.7: kept while the turn it names is still pending, so
        -- the cleanup that ends that turn -- it was popped as it began -- ends
        -- it, and so does the first cleanup after CR 800.4k spent it unbegun.
        -- Hygiene: `begun` alone decides whether the row applies.
        Expiry.DuringExtraTurn stamp -> any ((== stamp) . ExtraTurn.createdAt) (GameState.extraTurns gs)
        Expiry.AtEndOf _ -> True
        -- The named turn is over, so a turn that had no combat phase takes the
        -- effect with it rather than handing it to a later turn of theirs, which
        -- is not "your next turn".
        Expiry.AtEndOfCombatOn afterTurn ->
          not (Turn.isActive gs (AfterTurn.player afterTurn))
            || GameState.turnNumber gs <= AfterTurn.turn afterTurn
        -- CR 116.2c: only a payment ends this, and the cleanup step is not one.
        Expiry.WhenPaid _ -> True
        -- CR 611.2a: every printed producer also says "this turn", so cleanup
        -- ends an unused grant exactly as it ends AtCleanup's.
        Expiry.WhenUsed -> False
      -- CR 116.2d's ignores are among the rows swept: every printed one says
      -- until end of turn, so Leonin Arbiter stops the next turn's searches
      -- again, with nothing to reinstate.
      swept = keepSurvivors survives gs
   in -- Rule 702.171b's mark holds no Expiry, so traverseExpiries does not walk it.
      swept {GameState.objects = clearedSaddles (GameState.objects swept)}

-- CR 611.2b: drop every While whose condition has stopped holding. The effect
-- is DELETED, not masked: the duration is one continuous period, so an effect
-- that has ended stays ended even if the condition becomes true again. Reports
-- whether it changed anything, so Engine.settleForPriority knows to run again.
--
-- CR 704.3 fixes the coarsest moment anything can OBSERVE the condition, and
-- settleForPriority runs at exactly the points where the board can change, so
-- checking here is indistinguishable from checking continuously.
--
-- `changed` is a scan over sourcedExpiries rather than a compare of rebuilt
-- carriers, and `State.put` is skipped when nothing changed, so a no-op sweep
-- -- the usual one, since this runs at every settle -- rewrites nothing.
sweepConditional :: Game Bool
sweepConditional = do
  gs <- State.get
  let survives source expiry = case expiry of
        Expiry.While (While.MkWhile you cond) -> Condition.holds (Projection.fullView gs) (Projection.sourceContext gs (Just you) source) gs source cond
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        -- Alchemy's "perpetually" lasts for the rest of the game, as Never does.
        Expiry.Perpetual -> True
        Expiry.AtTurnOf _ -> True
        Expiry.AtUpkeepOf _ -> True
        Expiry.AtEndOfTurnOf _ -> True
        Expiry.DuringTurnOf _ -> True
        Expiry.DuringTurnOfControllerOf _ -> True
        Expiry.DuringExtraTurn _ -> True
        Expiry.AtEndOf _ -> True
        Expiry.AtEndOfCombatOn _ -> True
        -- CR 116.2c states a price, not a condition, so no board change ends it.
        Expiry.WhenPaid _ -> True
        -- Consumed only by Pawl.Engine.PlayerEffect.spentByCast/spentByLandPlay,
        -- which run outside this sweep.
        Expiry.WhenUsed -> True
      changed = not (all (uncurry survives) (sourcedExpiries gs))
  Monad.when changed $ State.put (keepWhere survives gs)
  pure changed

-- CR 701.35a's duration, ended: a detain lasts "until the next turn of the
-- controller of that spell or ability", which is dropAtTurnOf's own moment, so
-- that seat is dropped from every permanent detained until it. CLEARED rather
-- than dropped: the carrier is a field on an object that stays.
--
-- Rule 701.35a fixes the duration, so Object.detainedUntil holds no
-- Pawl.Types.Expiry, remembers only whose turn ends it, and this sweep is the
-- only one that can reach it. A permanent detained by two players loses one
-- seat here and stays detained by the other.
--
-- Scanned before rebuilding: almost every board has nothing detained, and this
-- runs at every seat of every handoff.
clearedDetentions :: PlayerId -> Map.Map ObjectId Object.Object -> Map.Map ObjectId Object.Object
clearedDetentions pid objects =
  if any (Set.member pid . Object.detainedUntil) objects
    then Map.map (\o -> o {Object.detainedUntil = Set.delete pid (Object.detainedUntil o)}) objects
    else objects

-- CR 701.15a's duration, ended: a goad lasts "until the next turn of the
-- controller of that spell or ability", which is dropAtTurnOf's own moment, so
-- that seat is dropped from every permanent goaded by it. clearedDetentions'
-- shape and every one of its reasons, down to the scan before the rebuild -- CR
-- 701.15c is why a permanent two players goaded loses one seat here and stays
-- goaded by the other.
clearedGoads :: PlayerId -> Map.Map ObjectId Object.Object -> Map.Map ObjectId Object.Object
clearedGoads pid objects =
  if any (Set.member pid . Object.goadedBy) objects
    then Map.map (\o -> o {Object.goadedBy = Set.delete pid (Object.goadedBy o)}) objects
    else objects

-- CR 702.171b's second ending: saddled lasts "until the end of the turn or it
-- leaves the battlefield", and this is the first of those -- the second is CR
-- 400.7's new object, which needs no sweep. The only designation with a clock;
-- see Pawl.Types.Designation. clearedGoads' shape, without its seat: rule
-- 702.171b's mark names no player.
clearedSaddles :: Map.Map ObjectId Object.Object -> Map.Map ObjectId Object.Object
clearedSaddles objects =
  if any (Set.member Designation.Saddled . Object.designations) objects
    then Map.map (\o -> o {Object.designations = Set.delete Designation.Saddled (Object.designations o)}) objects
    else objects

-- CR 611.2a: a duration a spell or ability states lasts as long as it says, so
-- an until-your-next-turn duration ends as that player's turn begins.
--
-- Takes the player EXPLICITLY rather than reading GameState.activePlayer,
-- because CR 800.4m needs this to fire for a seat whose turn does not begin: a
-- departed player's durations last until their turn WOULD have begun. Engine's
-- turn handoff walks the seating order and calls this at every seat it passes.
--
-- Dropping at the handoff is observably identical to dropping "as the turn
-- begins": CR 500.12, CR 502.4 and CR 704.3 leave nothing that could observe
-- the difference. The first observation point is the upkeep step (CR 503.1).
dropAtTurnOf :: PlayerId -> GameState -> GameState
dropAtTurnOf pid gs =
  let -- CR 800.4m: "or until a specific point in that turn". A departed player's
      -- turn never begins, so the cleanup sweep that would end an
      -- until-the-END-of-their-next-turn effect never runs and the effect would
      -- last for the rest of the game. The rule ends it here instead, at the
      -- point that turn would have begun -- the same moment, and the same call,
      -- as the arm above. For a player still in the game this is the beginning of
      -- a turn their effect is meant to survive, so it is left alone.
      departed = List.notElem pid (Game.stillPlaying gs)
      survives expiry = case expiry of
        Expiry.AtTurnOf p -> p /= pid
        -- CR 800.4m's "a specific point in that turn": a departed player's
        -- upkeep never begins, so the duration ends where that turn would have.
        Expiry.AtUpkeepOf p -> not (departed && p == pid)
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        -- Alchemy's "perpetually" lasts for the rest of the game, as Never does.
        Expiry.Perpetual -> True
        Expiry.While {} -> True
        Expiry.AtEndOfTurnOf afterTurn -> not (departed && AfterTurn.player afterTurn == pid)
        -- CR 800.4m reaches this arm for the arm above's reason and one more: a
        -- departed player's turn never begins, so the window never begins either
        -- and the effect could do nothing for the rest of the game. A player
        -- still in the game keeps it, and this is the very moment `begun` starts
        -- answering True for the turn it names.
        Expiry.DuringTurnOf afterTurn -> not (departed && AfterTurn.player afterTurn == pid)
        -- Pinned by dropAtEndOf rather than ended here.
        Expiry.DuringTurnOfControllerOf _ -> True
        -- Named by a turn rather than a seat, so no seat's handoff ends it; an
        -- extra turn CR 800.4k spent unbegun is dropAtCleanup's to end.
        Expiry.DuringExtraTurn _ -> True
        -- CR 800.4m's "a specific point in that turn", for the two arms above' reason.
        Expiry.AtEndOfCombatOn afterTurn -> not (departed && AfterTurn.player afterTurn == pid)
        Expiry.AtEndOf _ -> True
        -- CR 116.2c: no turn of anyone's ends it, and CR 800.4m does not reach it
        -- either -- the offer goes away with the departed player's objects rather
        -- than at a moment this sweep can name.
        Expiry.WhenPaid _ -> True
        -- No seat's turn beginning is a use.
        Expiry.WhenUsed -> True
      swept = keepSurvivors survives gs
   in swept {GameState.objects = clearedGoads pid (clearedDetentions pid (GameState.objects swept))}

-- CR 611.2a: "during its controller's next turn" pinned to a seat as a declare
-- attackers step ends on a turn that is its window (`windowReached`). From here
-- the row is an ordinary DuringTurnOf, so it lasts the rest of this turn (an
-- extra combat included) and dropAtCleanup ends it. A control change before
-- then moves the window, one after does not (Gideon, Battle-Forged's 2015-06-22
-- ruling). A turn with no combat reaches no declare attackers step, so
-- dropAtCleanup asks `windowReached` itself.
--
-- data/scenarios/combat/cr-611-2a-wall-of-dust-follows-the-attacker-to-its-new-controller.json,
-- cr-611-2a-gideon-s-requirement-follows-a-creature-handed-over-before-combat.json,
-- cr-611-2a-a-skipped-combat-still-spends-wall-of-dust-s-window.json,
-- cr-611-2a-a-creature-taken-during-its-new-controller-s-turn-spends-that-turn-s-window.json
-- and cr-611-2a-a-window-spent-at-declare-attackers-stays-spent-after-a-later-control-change.json
-- prove it.
pinAfterDeclareAttackers :: GameState -> GameState
pinAfterDeclareAttackers gs =
  let pin expiry = case expiry of
        Expiry.DuringTurnOfControllerOf afterObjectTurn
          | windowReached gs afterObjectTurn,
            Just pid <- View.controllerOf (AfterObjectTurn.object afterObjectTurn) gs ->
              Expiry.DuringTurnOf (AfterTurn.MkAfterTurn pid (AfterObjectTurn.turn afterObjectTurn))
        _ -> expiry
   in mapExpiries pin gs

-- Is this turn the window "during its controller's next turn" names? A later
-- turn whose active player controls the object now. Whether the creature could
-- legally attack does not matter: the Gideon ruling's first paragraph keeps a
-- tapped or summoning-sick creature inside the window, where it just doesn't
-- attack. A phased-out permanent is not under its controller's control (CR
-- 702.26d), so its window waits.
windowReached :: GameState -> AfterObjectTurn.AfterObjectTurn -> Bool
windowReached gs afterObjectTurn =
  let oid = AfterObjectTurn.object afterObjectTurn
   in GameState.turnNumber gs > AfterObjectTurn.turn afterObjectTurn
        && Set.member oid (GameState.battlefield gs)
        && maybe False (Turn.isActive gs) (View.controllerOf oid gs)

-- CR 611.2: every stored row that carries an expiry, walked once. The ONE
-- place that names the carriers: every sweep, the rewrite and the offer in this
-- module go through it, so a carrier added here reaches all of them and one
-- left out reaches none. `edit` is handed each row's source and expiry and
-- answers the expiry the row keeps, or Nothing to end it.
--
-- A row ended is DROPPED from its list, except an object's play permission
-- (CR 601.3, Object.playableFromExile), which is CLEARED on an object that
-- stays. A delayed trigger stating no duration (CR 603.7b) has no expiry to
-- hand over, so `edit` never sees it and it is kept.
traverseExpiries :: (Applicative f) => (ObjectId -> Expiry -> f (Maybe Expiry)) -> GameState -> f GameState
traverseExpiries edit gs =
  let delayed x = case DelayedTrigger.expiry x of
        Nothing -> pure (Just x)
        Just e -> fmap (fmap (\e' -> x {DelayedTrigger.expiry = Just e'})) (edit (DelayedTrigger.source x) e)
      object o = case Object.playableFromExile o of
        Nothing -> pure o
        Just p ->
          fmap
            (\kept -> o {Object.playableFromExile = fmap (\e -> p {ExilePlayPermission.expiry = e}) kept})
            (edit (ExilePlayPermission.source p) (ExilePlayPermission.expiry p))
      set ce co re pe br ar op ap ev ia dt ob =
        gs
          { GameState.continuousEffects = ce,
            GameState.copyEffects = co,
            GameState.replacements = re,
            GameState.playerEffects = pe,
            GameState.blockRequirements = br,
            GameState.attackRequirements = ar,
            GameState.objectProhibitions = op,
            GameState.attackProhibitions = ap,
            GameState.evasions = ev,
            GameState.ignoredAbilities = ia,
            GameState.delayedTriggers = dt,
            GameState.objects = ob
          }
   in set
        <$> rows edit ContinuousEffect.source ContinuousEffect.expiry (\x e -> x {ContinuousEffect.expiry = e}) (GameState.continuousEffects gs)
        <*> rows edit ActiveCopy.source ActiveCopy.expiry (\x e -> x {ActiveCopy.expiry = e}) (GameState.copyEffects gs)
        <*> rows edit ActiveReplacement.source ActiveReplacement.expiry (\x e -> x {ActiveReplacement.expiry = e}) (GameState.replacements gs)
        <*> rows edit ActivePlayerEffect.source ActivePlayerEffect.expiry (\x e -> x {ActivePlayerEffect.expiry = e}) (GameState.playerEffects gs)
        <*> rows edit ActiveBlockRequirement.source ActiveBlockRequirement.expiry (\x e -> x {ActiveBlockRequirement.expiry = e}) (GameState.blockRequirements gs)
        <*> rows edit ActiveAttackRequirement.source ActiveAttackRequirement.expiry (\x e -> x {ActiveAttackRequirement.expiry = e}) (GameState.attackRequirements gs)
        <*> rows edit ActiveObjectProhibition.source ActiveObjectProhibition.expiry (\x e -> x {ActiveObjectProhibition.expiry = e}) (GameState.objectProhibitions gs)
        <*> rows edit ActiveAttackProhibition.source ActiveAttackProhibition.expiry (\x e -> x {ActiveAttackProhibition.expiry = e}) (GameState.attackProhibitions gs)
        <*> rows edit ActiveEvasion.source ActiveEvasion.expiry (\x e -> x {ActiveEvasion.expiry = e}) (GameState.evasions gs)
        <*> rows edit IgnoredAbility.source IgnoredAbility.expiry (\x e -> x {IgnoredAbility.expiry = e}) (GameState.ignoredAbilities gs)
        <*> fmap (Seq.fromList . Maybe.catMaybes) (traverse delayed (Foldable.toList (GameState.delayedTriggers gs)))
        <*> traverse object (GameState.objects gs)

-- One carrier's list under traverseExpiries' edit, survivors in order.
rows :: (Applicative f) => (ObjectId -> Expiry -> f (Maybe Expiry)) -> (r -> ObjectId) -> (r -> Expiry) -> (r -> Expiry -> r) -> [r] -> f [r]
rows edit source expiry set =
  fmap Maybe.catMaybes . traverse (\x -> fmap (fmap (set x)) (edit (source x) (expiry x)))

-- traverseExpiries under Identity: each row kept under the expiry `edit`
-- answers, or ended.
editExpiries :: (ObjectId -> Expiry -> Maybe Expiry) -> GameState -> GameState
editExpiries edit = Identity.runIdentity . traverseExpiries (\source -> Identity.Identity . edit source)

-- Every stored expiry rewritten in place.
mapExpiries :: (Expiry -> Expiry) -> GameState -> GameState
mapExpiries f = editExpiries (const (Just . f))

-- CR 611.2a: a sweep whose survivors are named by the row's source and expiry.
keepWhere :: (ObjectId -> Expiry -> Bool) -> GameState -> GameState
keepWhere survives = editExpiries (\source expiry -> if survives source expiry then Just expiry else Nothing)

-- CR 500.5's first clause: effects lasting until the end of a step or phase
-- expire as it ends. The window that is ending is passed in, because only the
-- caller knows which one it is -- Engine.runStepThatBegan for a step that ended
-- by itself, and at the end of the last step of a stepped phase there are TWO,
-- the step and the phase, so it calls this twice. Pawl.Engine.Resolve's CR 724.1
-- and CR 724.2 arms are the other callers, for a PHASE ended part-way through,
-- where no last step runs to ask.
--
-- EQUALITY on the selector, not containment: CR 500.5a (repeated by CR 511.2)
-- is precisely the claim that an "until end of combat" effect does NOT expire
-- when the end of combat step ends as a step, and containment would end it at
-- the first combat step it saw. Pawl.Engine.Turn.inWindow is the containment
-- test, and it answers a different question for a different reader.
dropAtEndOf :: PhaseSelector -> GameState -> GameState
dropAtEndOf ending gs =
  let survives expiry = case expiry of
        Expiry.AtEndOf window -> window /= ending
        -- CR 500.8: the FIRST combat phase of that turn to end ends it, so an
        -- added combat phase after it finds nothing left.
        Expiry.AtEndOfCombatOn afterTurn ->
          ending /= PhaseSelector.CombatPhase
            || not (Turn.isActive gs (AfterTurn.player afterTurn))
            || GameState.turnNumber gs <= AfterTurn.turn afterTurn
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        -- Alchemy's "perpetually" lasts for the rest of the game, as Never does.
        Expiry.Perpetual -> True
        Expiry.While {} -> True
        Expiry.AtTurnOf _ -> True
        Expiry.AtUpkeepOf _ -> True
        Expiry.AtEndOfTurnOf _ -> True
        Expiry.DuringTurnOf _ -> True
        Expiry.DuringTurnOfControllerOf _ -> True
        Expiry.DuringExtraTurn _ -> True
        -- CR 116.2c: no window of the turn ends it.
        Expiry.WhenPaid _ -> True
        -- No step or phase ending is a use.
        Expiry.WhenUsed -> True
   in (if ending == PhaseSelector.Step (Phase.Combat CombatStep.DeclareAttackers) then pinAfterDeclareAttackers else id) (keepSurvivors survives gs)

-- CR 503 / 611.2a: "until the beginning of your next upkeep" ends as that
-- upkeep step begins. Engine.runStepThatBegan calls this for every active
-- player (CR 805.4, a shared team turn) before anything triggered then is put on
-- the stack (CR 503.1a), so a permission it ends is gone by the time such a
-- trigger resolves. A skipped upkeep (CR 500.11) never begins, so the duration
-- runs on to the next upkeep that does.
dropAtUpkeepOf :: PlayerId -> GameState -> GameState
dropAtUpkeepOf pid =
  keepSurvivors $ \expiry -> case expiry of
    Expiry.AtUpkeepOf p -> p /= pid
    Expiry.AtCleanup -> True
    Expiry.Never -> True
    Expiry.Perpetual -> True
    Expiry.While {} -> True
    Expiry.AtTurnOf _ -> True
    Expiry.AtEndOfTurnOf _ -> True
    Expiry.DuringTurnOf _ -> True
    Expiry.DuringTurnOfControllerOf _ -> True
    Expiry.DuringExtraTurn _ -> True
    Expiry.AtEndOf _ -> True
    Expiry.AtEndOfCombatOn _ -> True
    Expiry.WhenPaid _ -> True
    Expiry.WhenUsed -> True

-- CR 611.2a: keepWhere for a sweep whose survivors are named by the expiry
-- alone.
keepSurvivors :: (Expiry -> Bool) -> GameState -> GameState
keepSurvivors survives = keepWhere (const survives)

-- CR 116.2c: every offer a payment could end right now, paired with the object
-- that stored it. The one reader of Expiry.WhenPaid outside the sweeps, and the
-- reason that arm is not Expiry.Never -- a duration nothing ends by time still
-- has to be FINDABLE by the player who may end it.
--
-- Every carrier, through sourcedExpiries: Pawl.Types.Duration is one
-- vocabulary and any opcode taking a duration could print this one, so a carrier
-- left out would hold an effect that is offered to nobody and ends never. The
-- pair is the source and the offer -- the price and CR 109.5's seat, which
-- is all CR 116.2c needs. WHICH of the source's effects is not asked, since one
-- printed sentence stores several and the rule ends the sentence.
paidExpiries :: GameState -> [(ObjectId, PaidExpiry.PaidExpiry)]
paidExpiries gs =
  let paid (source, expiry) = case expiry of
        Expiry.WhenPaid paidExpiry -> [(source, paidExpiry)]
        Expiry.AtCleanup -> []
        Expiry.Never -> []
        Expiry.Perpetual -> []
        Expiry.While {} -> []
        Expiry.AtTurnOf _ -> []
        Expiry.AtUpkeepOf _ -> []
        Expiry.AtEndOfTurnOf _ -> []
        Expiry.DuringTurnOf _ -> []
        Expiry.DuringTurnOfControllerOf _ -> []
        Expiry.DuringExtraTurn _ -> []
        Expiry.AtEndOf _ -> []
        Expiry.AtEndOfCombatOn _ -> []
        Expiry.WhenUsed -> []
   in concatMap paid (sourcedExpiries gs)

-- Every stored expiry in the game, paired with the object it came from:
-- traverseExpiries read rather than rebuilt.
sourcedExpiries :: GameState -> [(ObjectId, Expiry)]
sourcedExpiries = Const.getConst . traverseExpiries (\source expiry -> Const.Const [(source, expiry)])

-- CR 116.2c's payment, made: end every effect this object stored under a
-- pay-to-end duration. dropAtEndOf's shape, with the source in the test --
-- delete-and-recompute, so the next projection reverts and nothing is explicitly
-- undone (design.md 2.5).
--
-- ALL of them together, which is the rule's own grain: "that effect" is the
-- printed sentence, and Gliding Licid's one sentence stores four. Two live sets
-- from one object cannot coexist -- a Licid that has activated has lost the
-- ability and stopped being a creature, so it cannot activate a second time --
-- so the source is a sufficient key.
dropWhenPaidBy :: ObjectId -> GameState -> GameState
dropWhenPaidBy oid gs =
  let survives source expiry = case expiry of
        Expiry.WhenPaid _ -> source /= oid
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        -- Alchemy's "perpetually" lasts for the rest of the game, as Never does.
        Expiry.Perpetual -> True
        Expiry.While {} -> True
        Expiry.AtTurnOf _ -> True
        Expiry.AtUpkeepOf _ -> True
        Expiry.AtEndOfTurnOf _ -> True
        Expiry.DuringTurnOf _ -> True
        Expiry.DuringTurnOfControllerOf _ -> True
        Expiry.DuringExtraTurn _ -> True
        Expiry.AtEndOf _ -> True
        Expiry.AtEndOfCombatOn _ -> True
        Expiry.WhenUsed -> True
   in keepWhere survives gs

-- CR 611.2a: does this expiry end when the effect carrying it is exercised,
-- rather than only at a moment the clock or a payment could name? The one
-- reader is Pawl.Engine.PlayerEffect's spentGrants (spentByCast and
-- spentByLandPlay), which needs to know which stored rows are eligible for early removal without
-- themselves casing on Pawl.Types.Expiry -- the standing this module alone
-- holds.
expiresWhenUsed :: Expiry -> Bool
expiresWhenUsed expiry = case expiry of
  Expiry.WhenUsed -> True
  Expiry.AtCleanup -> False
  Expiry.Never -> False
  Expiry.Perpetual -> False
  Expiry.While {} -> False
  Expiry.AtTurnOf _ -> False
  Expiry.AtUpkeepOf _ -> False
  Expiry.AtEndOfTurnOf _ -> False
  Expiry.DuringTurnOf _ -> False
  Expiry.DuringTurnOfControllerOf _ -> False
  Expiry.DuringExtraTurn _ -> False
  Expiry.AtEndOf _ -> False
  Expiry.AtEndOfCombatOn _ -> False
  Expiry.WhenPaid _ -> False
