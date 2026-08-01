-- The event pipeline (CR 603/614). This module owns the single zone-change
-- funnel and the sole casing on TriggerCondition; casing on ReplacementEffect
-- lives in Pawl.Engine.Replacement (CR 616.1's loop), which this module's changeZone
-- calls through rather than cases on directly.
-- changeZone lives here (not in Pawl.Engine.Game) so it can read the projection --
-- Projection imports Game, so a Game.changeZone that read the projection would
-- be an import cycle. See the plan's module dependency note.
module Pawl.Engine.Event where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.Binding (Binding)
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Countering as Countering
import Pawl.Types.DamageEvent (DamageEvent)
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import Pawl.Types.DelayedTrigger (DelayedTrigger)
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DiscardCause as DiscardCause
import Pawl.Types.Game (Game)
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import Pawl.Types.Zone (Zone)
import qualified Pawl.Types.Zone as Zone
import Pawl.Types.ZoneChange (ZoneChange)
import qualified Pawl.Types.ZoneChange as ZoneChange

-- CR 608.2i: append one entry to the turn-scoped log. The single APPEND point --
-- Engine.handoffTurn clears the log at turn end, Setup.emptyGame and the test
-- fixture Support.oneMountainState both seed it empty, and the test helper
-- Support.withEvents sets it directly; none of those append.
recordEvent :: GameEvent -> GameState -> GameState
recordEvent event gs = gs {GameState.events = GameState.events gs Seq.|> event}

-- The zone change an event describes, if it is one.
movedOf :: GameEvent -> Maybe ZoneChange
movedOf event = case event of
  GameEvent.Moved zc _ -> Just zc
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  -- The Moved event emitted by the same discard is the zone change; this one
  -- says the move WAS a discard (CR 701.9a).
  GameEvent.Discarded {} -> Nothing
  -- CR 701.20b: "Revealing a card doesn't cause it to leave the zone it's in."
  -- The rule that makes this arm Nothing rather than an oversight -- a reveal is
  -- never a zone change, even when the card is about to make one.
  GameEvent.Revealed _ _ -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing
  -- The Moved event Event.counter records alongside this one is the zone change
  -- rule 701.6a's last sentence causes; this one says the move WAS a countering
  -- and carries no ZoneChange of its own. Exactly the Discarded arm's case.
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing

-- The damage an event describes, if it is any.
damageOf :: GameEvent -> Maybe DamageEvent
damageOf event = case event of
  GameEvent.DamageDealt ev -> Just ev
  GameEvent.Moved _ _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Revealed _ _ -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing

-- The caster an event describes, if it is a cast (CR 601.2i).
castOf :: GameEvent -> Maybe PlayerId
castOf event = case event of
  GameEvent.SpellCast pid -> Just pid
  GameEvent.Moved _ _ -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Revealed _ _ -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing

-- Who revealed what, if the event is a reveal (CR 701.20a).
revealOf :: GameEvent -> Maybe (PlayerId, PC.ProjectedCharacteristics)
revealOf event = case event of
  GameEvent.Revealed pid snapshot -> Just (pid, snapshot)
  GameEvent.Moved _ _ -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing

-- CR 117.5: the events the trigger scan has not yet consumed.
unscannedEvents :: GameState -> [GameEvent]
unscannedEvents gs =
  Foldable.toList (Seq.drop (Natural.toIntSaturating (GameState.scannedThrough gs)) (GameState.events gs))

-- CR 704.5h: the damage the state-based-action check has not yet consumed.
unscannedDamage :: GameState -> [DamageEvent]
unscannedDamage gs =
  Maybe.mapMaybe damageOf (Foldable.toList (Seq.drop (Natural.toIntSaturating (GameState.damageScannedThrough gs)) (GameState.events gs)))

-- Insert a freshly-built object into `dest` under a new id and timestamp, and
-- return that id. The common tail of changeZone (a moved incarnation) and
-- createTokens (a token from nothing). `mkObj` receives the fresh timestamp so the
-- object records when it entered (CR 613.7d). The Moved event is emitted by the
-- CALLER: only it knows which state the CR 608.2h snapshot must be taken against.
placeObject :: PlayerId -> (Timestamp.Timestamp -> Object.Object) -> Zone -> Game ObjectId
placeObject pid mkObj dest = do
  gs <- State.get
  let (newId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj = mkObj ts
      gs3 = gs2 {GameState.objects = Map.insert newId obj (GameState.objects gs2)}
  State.put (Game.insertIntoZone dest pid newId gs3)
  pure newId

-- The single zone-change primitive (CR 400.7): the source object ceases; a NEW
-- object with a fresh id is created in the destination, carrying owner and
-- source forward and resetting per-incarnation state. No-op if the id is unknown.
-- The Game () wrapper the ~30 existing callers use; changeZoneReturning below
-- carries the same body but hands back the freshly-minted incarnation id, which
-- Resolve's ExileUntilMonarch arm registers for its return sweep.
changeZone :: ObjectId -> Zone -> Game ()
changeZone oid requestedDest = Monad.void (changeZoneReturning oid requestedDest)

-- changeZoneReturning for a move whose effect says how the object ENTERS -- CR
-- 110.5b's "unless a spell or ability says otherwise" -- rather than leaving it
-- to the rule's default. Meandering Towershell's "return it to the battlefield
-- tapped" is the one producer.
--
-- A separate door rather than a fifth parameter on changeZone, exactly as
-- changeZoneInBatch is: the ~30 callers that move an object under the default
-- have no tap state to name, and CR 110.5b is what says the default is theirs.
-- Handed to the funnel rather than applied after it, for the reason
-- createTokens' own comment gives: a permanent an effect says is tapped is never
-- untapped for an instant, and CR 614.1c's entry replacements run inside this
-- call.
changeZoneEntering :: ObjectId -> Zone -> TapState.TapState -> Game (Maybe ObjectId)
changeZoneEntering oid requestedDest = changeZoneAttaching Nothing oid requestedDest Nothing

-- changeZone for one member of a batch of moves that CR 608.2f or CR 704.3
-- processes SIMULTANEOUSLY -- the destroy funnel's graveyard moves below, and CR
-- 704.3's put-into-graveyard batch in Pawl.Engine.Sba. `asOf` is the board the batch
-- began in -- or, when the batch is itself part of a larger simultaneous event,
-- that event's (destroyInBatch below) -- which is what its members' CR 616.1
-- loops collect their replacement candidates from; see Pawl.Engine.Replacement's
-- applyReplacementsIn for why the loop needs a board rather than a filter, and
-- what stays live.
--
-- A separate door rather than a fourth parameter on changeZone: a batch is the
-- rare case, and the ~30 callers that move a single object have no footing to
-- name -- for them the board the move begins on IS the live one.
changeZoneInBatch :: GameState -> ObjectId -> Zone -> Game ()
changeZoneInBatch asOf oid requestedDest = Monad.void (changeZoneAttaching (Just asOf) oid requestedDest Nothing TapState.Untapped)

-- changeZoneReturning's body, returning the destination incarnation's id: Just
-- newId on a completed move (CR 400.7 minted a fresh id), Nothing when the id is
-- unknown or the CR 616.1 replacement loop cancelled the move (`resolved ==
-- Nothing`). changeZoneReturning itself is the `seed = Nothing` case below.
changeZoneReturning :: ObjectId -> Zone -> Game (Maybe ObjectId)
changeZoneReturning oid requestedDest = changeZoneAttaching Nothing oid requestedDest Nothing TapState.Untapped

-- changeZoneReturning with an attachment seed. CR 303.4: "An Aura enters the
-- battlefield attached to an object or player" -- attachment is a property of
-- entering, not a step after it. The entry replacement loop (CR 614.1c) and the
-- Moved event both run before this function returns, so an Aura attached
-- afterward would be unattached during both. No card in this pool can observe
-- the difference today; the seed buys the ordering rather than a passing test.
--
-- Pawl.Engine.Stack's Aura branch is the only caller that supplies a seed; every other
-- door to the battlefield (this function's own changeZoneReturning included)
-- passes Nothing, so an Aura entering the battlefield by any other route enters
-- unattached and is buried on the next SBA pass by CR 704.5m -- where CR 303.4g
-- says such an Aura should instead just stay in its current zone. That
-- divergence is #188.
--
-- `asOf` is the CR 608.2f / 704.3 batch board changeZoneInBatch above supplies,
-- and Nothing for every other caller. `tapped` is CR 110.5b's status as the
-- moving effect states it, and TapState.Untapped -- that rule's default -- for
-- every door but changeZoneEntering.
changeZoneAttaching :: Maybe GameState -> ObjectId -> Zone -> Maybe Recipient.Recipient -> TapState.TapState -> Game (Maybe ObjectId)
changeZoneAttaching asOf oid requestedDest seed tapped = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure Nothing
    Just obj -> do
      let pid = Object.owner obj
          fromZone = Object.zone obj
          -- CR 608.2h: last known information -- the object as it exists in the
          -- zone it is LEAVING, projected against the PRE-MOVE state -- one board
          -- projection per zone change, forced eagerly (GameEvent.Moved's
          -- snapshot field is strict) rather than left as a thunk retaining the
          -- whole pre-move GameState for a turn. Measured on the tasty-bench
          -- suite, pre-log baseline (3cc3ecd) vs. this log with the strict field
          -- (goldfish /casting/fighting, 2p): 10.1/9.30/9.31 ms -> 10.6/9.73/9.72
          -- ms -- a ~4-5% move, within the benchmark's own run-to-run noise
          -- (~800 us stddev on a ~10 ms mean), not the large regression a
          -- captured pre-move GameState would cause. That is the price of an
          -- honest history (a token has no printed card to re-derive from,
          -- CR 111.1).
          snapshot = Projection.project oid gs
          -- CR 613.1b: the OTHER half of last known information, read from the
          -- same pre-move state. Control is not a characteristic (CR 109.3's
          -- list does not include it), so it cannot ride `snapshot`; it is kept
          -- because CR 603.3a asks "who controlled its source at the time it
          -- triggered" about sources that are already gone -- see
          -- eventTriggers below.
          --
          -- The `Object.owner` fallback is unreachable rather than a guess:
          -- Projection.controllerOfGiven's own base case returns
          -- `Just (Object.owner obj)` for any id that resolves, and `oid`
          -- resolves here (this branch matched `Just obj`). It is written as a
          -- fallback only because controllerOf's type is honest about ids that
          -- do not.
          --
          -- A second board walk on the same hot path as `snapshot` above
          -- (controllerOf rebuilds controlGrants and its liveGiven fixpoint).
          -- Measured on the tasty-bench suite, this commit's parent vs. this
          -- change (goldfish / casting / fighting / fighting-aura, 2p):
          -- 15.2/133/24.6/569 ms -> 15.5/134/25.2/575 ms -- every move inside
          -- one run-to-run stddev, so no gate was moved to buy it back.
          lastController = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
      -- CR 614.4: replacements exist before the event, so the loop reads them from
      -- the PRE-MOVE state. CR 614.6: the modified event is what actually happens.
      --
      -- `obj` and `snapshot` are both read from `gs`, BEFORE resolveZoneChange
      -- runs, and both are still used below, AFTER it returns -- only
      -- `ZoneChange.to settled` is read back from the settled event. This is
      -- sound despite Pawl.Engine.Replacement's AsCopy arm calling State.modify' (it
      -- stamps a copy snapshot when a permanent enters as a copy): the loop
      -- resolveZoneChange runs here is a WouldChangeZone loop, which `applies`
      -- restricts to ZoneChangeR candidates -- it cannot reach the EntryR arm
      -- that AsCopy lives under, because EntryR only matches a WouldEnter event
      -- (the entry loop nested inside changeZone below, at
      -- Replacement.runEntry, on the object's NEW id). And `gs` is an immutable
      -- value, not a reference into mutable state, so a State.modify' anywhere
      -- downstream cannot retroactively change what `snapshot` already
      -- captured. A future task extending EITHER loop to mutate state that
      -- `obj` or `snapshot` reads (e.g. a ZoneChangeR arm that edits the
      -- object's own fields) would need to re-derive these bindings from the
      -- state AFTER that loop, not carry the pre-loop ones across unexamined.
      -- Both ids are `oid` in the PROPOSED event: nothing has moved yet, so the
      -- object that will leave is the only one that exists (see
      -- Pawl.Types.ZoneChange).
      resolved <- Replacement.resolveZoneChange asOf (ZoneChange.MkZoneChange oid oid fromZone requestedDest)
      case resolved of
        -- CR 614.6: nothing survived the loop, so no zone change happens. No
        -- producer today -- no card in the pool cancels a zone change outright --
        -- but Maybe is what "the event does not happen" means on this path.
        Nothing -> pure Nothing
        Just settled -> do
          let dest = ZoneChange.to settled
              -- CR 110.5b: untapped unless the moving effect said otherwise
              -- (changeZoneEntering). Meaningful only for a battlefield
              -- destination -- CR 110.5a makes status a property of permanents
              -- -- and every other door passes the default, so nothing else can
              -- put a tapped card in a graveyard.
              mkObj ts = obj {Object.zone = dest, Object.tapped = tapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.attachedTo = seed, Object.timestamp = ts}
          State.modify' $ \g ->
            let g1 = Game.removeFromZones pid oid g
             in g1
                  { GameState.objects = Map.delete oid (GameState.objects g1),
                    -- CR 608.2h: the object ceases here, so this is the last
                    -- moment its information is known. Filed under the id it had
                    -- while it existed -- the id an ability on the stack still
                    -- carries as its source (CR 113.7) -- and from the same
                    -- `snapshot` the Moved event below records, so the two
                    -- readings of "what was it" cannot drift apart.
                    GameState.lastKnown = Map.insert oid (LastKnown.MkLastKnown snapshot lastController (Object.source obj)) (GameState.lastKnown g1)
                  }
          newId <- placeObject pid mkObj dest
          -- CR 614.1c-d: entry replacements apply to BATTLEFIELD entries and
          -- nowhere else. CR 616.1g: this loop is NESTED inside the zone change,
          -- which is how "an effect may apply to an event contained within another
          -- event" is expressed -- as call nesting, not as a field. A lone entry
          -- has no same-batch siblings (CR 614.12a; see Pawl.Engine.Replacement's
          -- applyReplacementsIn for why 614.12a, not 614.13a, is the cite).
          Monad.when (dest == Zone.Battlefield) (Replacement.runEntry Set.empty newId)
          -- CR 603.2g: record the RESOLVED event, carrying the NEW object's id --
          -- what an enters trigger scans -- alongside the id it had while it was
          -- in `fromZone`, which is the key the `lastKnown` entry written just
          -- above is filed under and so the only route back to it once CR 400.7
          -- has minted a new incarnation (CR 603.10a's look-back reads it; see
          -- `leftBattlefield` in eventTriggers). Recorded LAST, so the entry
          -- loop's choices are locked in before any trigger or SBA can observe
          -- the object.
          State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange oid newId fromZone dest) snapshot))
          pure (Just newId)

-- The single destruction funnel (CR 701.8 / 702.12b): every destruction -- the
-- Destroy opcode and the CR 704.5g/h state-based actions -- flows through here.
--
-- Takes the WHOLE BATCH of permanents being destroyed together, not one at a
-- time, because CR 608.2f says the batch happens at once: "Some spells and
-- abilities include actions taken on multiple players and/or objects. In most
-- cases, each such action is processed simultaneously." CR 704.3 says the same
-- of the state-based actions: they are performed "simultaneously as a single
-- event". A lone destruction is the one-element batch.
--
-- CR 702.12b: an indestructible permanent can't be destroyed. The gate is judged
-- for EVERY member of the batch against the state the batch began in, BEFORE any
-- of them is moved -- that is the whole reason this takes a list. A permanent
-- whose static ability grants the others indestructible is still on the
-- battlefield at that moment even when it is itself in the batch, so it dies
-- alone. Judging each member against a board its predecessors had already left
-- would make the answer depend on the batch's order, which CR 608.2f gives
-- nobody the right to decide -- proved by The Walls of Ba Sing Se under Day of
-- Judgment, in Pawl.ResolveSpec's DestroyAll group, in both orders.
--
-- The gate also comes BEFORE the replacement loop, which is CR 614.7: "If a
-- replacement effect would replace an event, but that event never happens, the
-- replacement effect simply doesn't do anything" -- a regeneration shield is
-- neither applied nor consumed. Otherwise the would-be-destroyed event is
-- offered to CR 616.1; if it survives, the permanent is put into its owner's
-- graveyard via changeZone (so Rest in Peace's redirect and a token's CR
-- 704.5d cease-to-exist still compose). Ungated for CR 701.19c "can't be
-- regenerated" (#42).
--
-- This is the door for a batch that is a whole event to itself and whose caller
-- does not care what died. destroyReturning below is that same door for a caller
-- that does, destroyInBatch is the door for a batch nested inside a larger
-- simultaneous event, and destroyIn -- the shared body -- sets out which board
-- each of its three readers gets.
destroy :: Regenerability.Regenerability -> [ObjectId] -> Game ()
destroy regenerability oids = Monad.void (destroyIn Nothing regenerability oids)

-- destroy, answering with the permanents it ACTUALLY destroyed -- CR 701.8b's
-- "destroyed this way", which is emphatically not the batch it was handed. A
-- member with indestructible never reaches the destruction event at all (CR
-- 702.12b, the gate below), and a regenerated one has that event replaced (CR
-- 701.8c, "A regeneration effect replaces a destruction event"), so neither was
-- destroyed and neither is in this answer.
--
-- Each surviving destruction reports the SETTLED object rather than the one
-- asked about, for the same reason the graveyard move follows it: a CR 616.1
-- rewrite may redirect the destruction, and what was destroyed is what the loop
-- handed back. No replacement in this pool redirects one, so the two lists are
-- equal today.
--
-- A second door rather than a return type on `destroy`, the changeZoneReturning
-- posture: the Destroy opcode's bound-count slot (Pawl.Types.Effect) is the only
-- caller that has anything to do with the answer.
destroyReturning :: Regenerability.Regenerability -> [ObjectId] -> Game [ObjectId]
destroyReturning = destroyIn Nothing

-- destroy for a batch that is one PART of a larger simultaneous event, whose
-- board is `asOf`. CR 704.3's state-based-action check is that event -- "the game
-- checks for any of the listed conditions for state-based actions, then performs
-- all applicable state-based actions simultaneously as a single event" -- and
-- Pawl.Engine.Sba is the only caller: it performs CR 704.5f/j/k/m's put-into-graveyard
-- batch and then CR 704.5g/h's destruction batch, which is a sequence only in the
-- implementation. Both halves therefore stand on the board the PASS began in, so
-- an animated Rest in Peace the pass itself buries still exiles the card of the
-- creature the pass destroys.
--
-- A separate door rather than a `Maybe GameState` parameter on `destroy`, for the
-- same reason changeZoneInBatch is one: every other caller -- the Destroy opcode
-- in Pawl.Engine.Resolve and the test suite -- has no larger event to name, and for them
-- the board the batch begins on IS the live one.
destroyInBatch :: GameState -> Regenerability.Regenerability -> [ObjectId] -> Game ()
destroyInBatch asOf regenerability oids = Monad.void (destroyIn (Just asOf) regenerability oids)

-- The shared body. Three separate readers of a board, and they do NOT all get the
-- same one:
--
--   1. The CR 616.1 replacement loops -- the destruction's, and the
--      put-into-graveyard that follows it -- collect from `gs`, the containing
--      event's board. That is the CR 608.2f / 704.3 "single event" reading: the
--      effects in force are the ones that existed before the event, so an effect
--      belonging to a permanent the same event is removing still applies. Both
--      loops get the same board because both are parts of the one event.
--   2. The CR 702.12b gate reads `gs` too. Indestructibility is a fact about the
--      permanent at the moment the event's conditions were judged; letting an
--      earlier part of the same event change the answer would make it depend on
--      an order CR 608.2f gives nobody the right to decide -- the same argument
--      that makes the gate precede the batch's own moves, applied one level out.
--      For the Pawl.Engine.Sba caller this asks the same board `destroyedBySba` asked,
--      rather than second-guessing it from one the buries have already changed; a
--      lone caller's `gs` IS the live board, so The Walls of Ba Sing Se under Day
--      of Judgment is untouched.
--   3. The existence filter reads `live`, NOT `gs`. This is CR 614.7 -- "If a
--      replacement effect would replace an event, but that event never happens,
--      the replacement effect simply doesn't do anything." An object an earlier
--      part of the event has already put into a graveyard is not on the
--      battlefield to be destroyed, so no destruction event happens for it and no
--      regeneration shield may be offered one, let alone spend itself on it. The
--      reachable shape is an Aura named by CR 704.5m and CR 704.5g in the same
--      pass; Pawl.ReplacementSpec's "CR 614.7 an Aura the same pass buries is
--      never offered to a regeneration shield" is the proof. Pawl.Engine.Sba already
--      excludes its CR 704.5j and CR 704.5k victims by name for the same reason,
--      from the other end.
--
-- Only the graveyard move's loop can observe (1) today. The destruction loop's
-- only candidates are DestructionR, and every DestructionR in the pool is a
-- regeneration shield the Replace opcode put in the FLOATING store -- which stays
-- live for CR 614.3's use count -- rather than a permanent's printed ability, so
-- the frozen board holds nothing for it to find. It is passed anyway because the
-- rule, not the pool, is what says the two loops are one event. See
-- Pawl.Engine.Replacement's applyReplacementsIn for what the frozen board covers and
-- what stays live.
--
-- Answers with the members that were actually destroyed, in the order they were
-- handed over; destroyReturning's haddock has what that answer is for and why it
-- is narrower than `oids`.
destroyIn :: Maybe GameState -> Regenerability.Regenerability -> [ObjectId] -> Game [ObjectId]
destroyIn asOf regenerability oids = do
  live <- State.get
  let gs = Maybe.fromMaybe live asOf
      doomed = filter (\oid -> Maybe.isJust (Game.lookupObject oid live) && not (Projection.hasKeyword Keyword.Type.Indestructible oid gs)) oids
  fmap Maybe.catMaybes . Monad.forM doomed $ \oid -> do
    settled <- Replacement.resolveDestruction (Just gs) regenerability oid
    case settled of
      -- CR 701.8c: a regeneration effect REPLACED the destruction, so nothing was
      -- destroyed here and this member is not in the answer.
      Nothing -> pure Nothing
      -- The graveyard move follows the SETTLED object, not the one asked about,
      -- so a rewrite that redirects the destruction is honoured. changeZone is a
      -- no-op for an object that is already gone, which is what makes it safe to
      -- have named the batch's members before any of them moved.
      Just target -> do
        changeZoneInBatch gs target Zone.Graveyard
        pure (Just target)

-- The single spell-countering funnel (CR 701.6 -- not to be confused with
-- Pawl.Engine.Replacement.putCounters, CR 122.6's placement of counter
-- markers). A countered
-- spell is removed from the stack and put into its owner's graveyard (CR
-- 701.6a) through the changeZone funnel -- so Rest in Peace's redirect
-- (graveyard->exile) and CR 400.7's new incarnation still compose, exactly as
-- they do for destroy.
-- Gated on CR 113.6g's "can't be countered", which functions on the stack and so
-- is read off the spell's own card (Card.counterability) rather than through the
-- projection -- there is no battlefield projection of a spell. CR 101.2 is what
-- makes the gate the whole story: the "can't" takes precedence, so the countering
-- effect resolves and simply does nothing. It is NOT targeting immunity -- Cancel
-- still legally targeted the spell (CR 113.6g grants no shroud), which is why
-- this gate lives here at the funnel and not in Pawl.Engine.Target.
--
-- The gate comes before the zone change, the shape Event.destroy's CR 702.12b
-- indestructible gate already has, and for the same CR 614.7 reason: an event
-- that never happens offers nothing for a replacement to intercept.
--
-- Records a GameEvent.SpellCountered of its own, ALONGSIDE the Moved event the
-- zone change files -- never instead of it. The two say different things: the
-- Moved event is the CR 400.7 zone change, and this one is what the change WAS.
-- Keeping them apart is what makes a countered spell distinguishable from one
-- that RESOLVED into the same graveyard by CR 608.2n, and from a discarded or
-- milled card; it is also what survives Rest in Peace redirecting the
-- destination (CR 614), after which no zone pair reads stack-to-graveyard at
-- all.
--
-- Nothing at all is recorded on any of the three paths that DO NOT counter, and
-- CR 603.2g is what makes that mandatory rather than tidy: "an event that's
-- prevented or replaced won't trigger anything."
--
--   * An id with no object -- nothing was on the stack to remove.
--   * The CR 113.6g gate. Read through CR 101.2, a spell that can't be countered
--     was never countered, so there is no event to record -- which is what keeps
--     Baral silent in TriggerSpec's composition case.
--   * A move the CR 616.1 loop cancelled (`Nothing`), which leaves the spell on
--     the stack, so it was never "removed from the stack" and rule 701.6a's
--     countering did not happen. The posture `discard` below takes, and with no
--     producer today: no card in the pool cancels a zone change outright.
--
-- The `source` and `controller` are the countering spell or ability and the
-- player who controlled it (CR 405.4) -- what Baral, Chief of Compliance's
-- "whenever a spell or ability YOU CONTROL counters a spell" reads against CR
-- 109.5's "you". Taken from the caller rather than re-derived here: they are the
-- effect source and the CR 608.2c controller Pawl.Engine.Resolve already holds,
-- and by the time the CR 117.5 trigger scan reads this event the controller can
-- no longer be asked for exactly -- see Pawl.Types.Countering, which sets out
-- the two cases.
counter :: ObjectId -> PlayerId -> ObjectId -> Game ()
counter source controller oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just _ -> case fmap Card.counterability (Game.cardOf oid gs) of
      Just Counterability.CantBeCountered -> pure ()
      _ -> do
        moved <- changeZoneReturning oid Zone.Graveyard
        case moved of
          Nothing -> pure ()
          Just _ ->
            State.modify'
              . recordEvent
              $ GameEvent.SpellCountered
                Countering.MkCountering
                  { Countering.spell = oid,
                    Countering.source = source,
                    Countering.controller = controller
                  }

-- CR 701.21/701.21a: the single sacrifice funnel. The permanent is put into its
-- OWNER's graveyard through changeZone (so Rest in Peace's redirect and a token's
-- CR 704.5d cease-to-exist still compose), and -- unlike Event.destroy -- with no
-- indestructible gate (CR 702.12b) and no regeneration shield consulted (CR
-- 701.19a): CR 701.21a says sacrificing is not destroying. CR 701.21a also
-- restricts it to permanents on the battlefield, so anything else is a no-op.
--
-- CR 701.21a's other clause -- "A player can't sacrifice something that... [is] a
-- permanent they don't control" -- is why this takes the sacrificing player and
-- not just the permanent. Enforced HERE, at the one funnel, rather than trusted
-- from each caller: a cost payment and a triggered ability's own source (CR
-- 113.7's Binding.triggerSource) are controlled by the paying player by
-- construction, but an edict's victim is a permanent a PLAYER named, and only
-- that one could ever be wrong.
sacrifice :: PlayerId -> ObjectId -> Game ()
sacrifice pid oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Object.zone obj of
      -- CR 701.21a: "A player can't sacrifice something that isn't a permanent, or
      -- something that's a permanent they don't control." The zone case below is
      -- the first clause; this is the second. Enforced HERE, at the one funnel,
      -- rather than trusted from each caller -- the callers are a cost payment, a
      -- trigger's own source, and an edict whose victim a player NAMED, and only
      -- the last of those could ever be wrong.
      Zone.Battlefield
        | Projection.controllerOf oid gs /= Just pid -> pure ()
        | otherwise -> changeZone oid Zone.Graveyard
      Zone.Library -> pure ()
      Zone.Hand -> pure ()
      Zone.Graveyard -> pure ()
      Zone.Stack -> pure ()
      Zone.Exile -> pure ()
      -- CR 408.1: a command-zone object is not a permanent, so it is never
      -- sacrificed.
      Zone.Command -> pure ()

-- CR 111.2: create `n` tokens with the given effect-defined characteristics under
-- `controller`'s control (its owner, CR 111.2), summoning-sick (CR 302.6). A token
-- is created from nothing -- it has no prior object to move, so changeZone cannot
-- mint it. Uses from = Battlefield (it appears there; to == from can never read as
-- a leave). Emits the enters event so ETB triggers (CR 603.6a) fire on the same
-- path a resolved permanent uses.
--
-- PLURAL since P5, and that is a rules requirement, not a convenience. CR 614.1
-- replacements scope to the CREATION EVENT, not to each token -- Doubling Season
-- says "if an effect would create ONE OR MORE tokens ... it creates twice that
-- many" (CR 614.16) -- so the count is settled once, up front. Then every token
-- is materialized, and only then does each run its OWN entry loop (CR 616.1g:
-- "one replacement or prevention effect may apply to an event, and another may
-- apply to an event contained within the first event" -- creating a token
-- CONTAINS that token entering); each token's entry loop is handed the whole
-- batch, which EXCLUDES its simultaneously-entering siblings from any copy
-- choice (CR 614.12a; see Pawl.Engine.Replacement's applyReplacementsIn for why
-- 614.12a, not 614.13a, is the cite).
--
-- The call nesting below (runEntry, called once per token, nested inside this
-- function which itself settles the CREATE event) implements CR 616.1g's
-- containment as DESIGN INTENT, but no test exercises it: every token card in
-- the pool has empty `replacementEffects`, so each token's entry loop finds no
-- candidates and returns immediately -- the nesting could be deleted and every
-- scenario in the test pool would still pass. CR 616.1g's own worked example
-- (a token copy of Voice of All) needs a token WITH an entry replacement to
-- exercise, and no such card is in this pool (#73).
-- CR 800.4b, sentence 2: "If a token would be created under the control of a
-- player who has left the game, no token is created." Also CR 800.4d's first
-- sentence ("If an object that would be owned by a player who has left the game
-- would be created in any zone, it isn't created").
--
-- Those two sentences coincide for a token, and by CR 111.2 rather than by any
-- accident of this signature: "The player who creates a token is its owner. The
-- token enters the battlefield under that player's control." Owner and controller
-- are the same player by rule, so one guard on that player satisfies both.
--
-- Before Replacement.resolveTokens, not after. The rule says no token is CREATED,
-- not that one is created and then removed, so nothing may be minted and nothing
-- may be spent getting there -- resolveTokens consumes replacement use counts (CR
-- 614.3), and burning one on a token that the rules say never existed would be a
-- second, quieter violation. Guarding the parameter rather than the resolved
-- owner is exact today because no producer can move a token's controller as it is
-- created (#69); if CR 616.1b's control-modifying entry replacements ever gain
-- one, this check has to move after them and become a re-check.
--
-- Inline rather than a guard delegating to a `createTokensFor` body: the project
-- writes no export lists, so a second top-level name would be a public door
-- straight past the check, and the whole argument for putting the guard here is
-- that this is the ONE door.
createTokens :: PlayerId -> Card -> Natural -> TapState.TapState -> Game [ObjectId]
createTokens controller card n tapped = do
  gs <- State.get
  if List.notElem controller (Game.stillPlaying gs)
    then pure []
    else do
      resolved <- Replacement.resolveTokens controller card n
      case resolved of
        Nothing -> pure []
        Just (owner, tokenCard, count) -> do
          let mkObj ts =
                Object.MkObject
                  { Object.owner = owner,
                    Object.source = Source.OfToken tokenCard,
                    Object.zone = Zone.Battlefield,
                    -- CR 110.5b: "permanents enter the battlefield untapped ...
                    -- unless a spell or ability says otherwise". Untapped for
                    -- every token but the ones an effect says are tapped
                    -- (Hanweir Garrison), which is why the caller supplies it
                    -- rather than this taking the default and tapping after.
                    Object.tapped = tapped,
                    Object.damage = 0,
                    Object.sickness = Sickness.Sick,
                    Object.bindings = Map.empty,
                    Object.counters = Map.empty,
                    Object.attachedTo = Nothing,
                    Object.timestamp = ts
                  }
          ids <- Monad.replicateM (Natural.toIntSaturating count) (placeObject owner mkObj Zone.Battlefield)
          Monad.mapM_ (Replacement.runEntry (Set.fromList ids)) ids
          -- A token is created from nothing, so there is no prior incarnation to
          -- snapshot: its last known information IS what it is now (CR 111.3 makes
          -- the creating effect's stated values functionally printed values).
          -- Recorded AFTER every entry loop, so the events describe settled objects.
          Monad.mapM_ recordTokenEntry ids
          pure ids

-- A token is created from nothing, so nothing departed: `departed` is the token's
-- own id, the same value `object` carries. Harmless rather than a fiction the
-- readers have to know about, because from == to == Battlefield already fails
-- every departure test (CR 603.6c asks for a move "to another zone"), and
-- because a token has no prior incarnation and so no `lastKnown` entry to find.
recordTokenEntry :: ObjectId -> Game ()
recordTokenEntry newId = do
  placed <- State.get
  let snapshot = Projection.project newId placed
  State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId newId Zone.Battlefield Zone.Battlefield) snapshot))

-- CR 121.2/121.3: the single-card draw. Move pid's top library card to their
-- hand; an empty library records the failed draw (CR 704.5b makes it a loss at
-- the next state-based-action check). The primitive shared by the draw step
-- (Engine.runTurnBasedActions), opening hands (Setup.newGame), and the Draw
-- effect (Resolve).
drawCard :: PlayerId -> Game ()
drawCard pid = do
  gs <- State.get
  case Game.zoneMembers Zone.Library pid gs of
    [] -> State.put gs {GameState.drewFromEmpty = Set.insert pid (GameState.drewFromEmpty gs)}
    top : _ -> changeZone top Zone.Hand

-- The single discard funnel (CR 701.9a): "To discard a card, move it from its
-- owner's hand to that player's graveyard." `pid` is the discarding player, whom
-- that rule makes the card's owner either way, and `cause` is why -- see
-- Pawl.Types.DiscardCause.
--
-- The move goes through changeZoneReturning, the CR 400.7 funnel, so a discarded
-- card gets a new incarnation and Rest in Peace's redirect composes. The EVENT is
-- what this function adds over calling that funnel directly, and it is not
-- redundant with the Moved event the move records: CR 701.9c speaks of a card
-- that "is discarded, but an effect causes it to be put into a hidden zone
-- instead of into its owner's graveyard", so a discard the CR 614 loop redirected
-- is still a discard while its Moved event no longer reads hand-to-graveyard. A
-- discard trigger reads this record and never the zone pair.
--
-- Recorded only when the move COMPLETED. `Nothing` is an unknown id or a CR
-- 616.1 loop that cancelled the move, and CR 603.2g is why that must record
-- nothing: "an event that's prevented or replaced won't trigger anything."
--
-- The id recorded is the one the funnel MINTED, not the one that was in hand:
-- CR 702.29c's abilities "trigger from whatever zone the card winds up in after
-- it's cycled", and the graveyard object is the one bearing them.
discard :: DiscardCause.DiscardCause -> PlayerId -> ObjectId -> Game ()
discard cause pid oid = do
  moved <- changeZoneReturning oid Zone.Graveyard
  case moved of
    Nothing -> pure ()
    Just newId -> State.modify' (recordEvent (GameEvent.Discarded pid newId cause))

-- The single reveal funnel (CR 701.20a): `pid` shows the card `oid` to all
-- players, which here means appending what was shown to the public log. No-op
-- for an id with no object, the posture changeZone takes.
--
-- CR 701.20b: nothing moves and nothing about the object changes, so this
-- function's whole effect is the event. That is not a shortcut -- it is the
-- rule.
--
-- The snapshot is Projection.project, the same reading GameEvent.Moved's CR
-- 608.2h last-known information takes -- deliberately NOT the printed-card view
-- (Projection.viewOfCard) that Resolve's search filter matches a library card
-- through. The two ask different questions and really can disagree: CR 604.3
-- says a characteristic-defining ability "function[s] in all zones", so a
-- Tarmogoyf in a library HAS a power, which viewOfCard reports as Nothing and
-- Projection.project computes. A search may ignore that (CR 701.23a's criterion
-- is a description, and viewOfCard's own comment gives its reasons); a reveal
-- may not, because it has to show what a player at the table would see.
--
-- No card in the pool makes the two differ today -- no CDA card is searched for
-- or revealed -- so this is the reading that will still be right rather than a
-- passing test.
reveal :: PlayerId -> ObjectId -> Game ()
reveal pid oid = do
  gs <- State.get
  Monad.when (Maybe.isJust (Game.lookupObject oid gs)) $
    State.modify' (recordEvent (GameEvent.Revealed pid (Projection.project oid gs)))

-- CR 603.2: does this condition fire on this event, for the permanent that bears
-- it? `bearer` is the object whose ability this is and `you` is its controller
-- -- CR 603.3a controls the triggered ability, and CR 109.5 is what makes "your"
-- (as in "your upkeep") mean that controller -- both are part of the match,
-- because the scan below visits EVERY permanent, not only the one an event
-- names. This module is the sole home of casing on TriggerCondition for RULES
-- purposes; Pawl.Codec also cases on every constructor, but only as the JSON
-- data boundary (encode/decode), not to decide game behaviour.
-- CR 508.3a / 608.2i: how many times this object has been declared as an
-- attacker so far this turn, read out of the turn-scoped event log. Only
-- Combat.declareAttackers appends the event, which is what keeps CR 508.4's
-- creature put onto the battlefield attacking -- one that "never attacked" --
-- out of the count.
declarationsOf :: ObjectId -> GameState -> Int
declarationsOf bearer gs =
  let declaredIt event = case event of
        GameEvent.AttackerDeclared oid -> oid == bearer
        _ -> False
   in length (Seq.filter declaredIt (GameState.events gs))

matchesTrigger :: GameState -> ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool
matchesTrigger gs bearer you cond event = case cond of
  -- CR 603.6a: the bearer's own object entered the battlefield.
  TriggerCondition.SelfEnters -> case event of
    GameEvent.Moved zc _ -> ZoneChange.object zc == bearer && ZoneChange.to zc == Zone.Battlefield
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 603.6a's "whenever a [type] enters": a permanent the Filter admits
  -- entered the battlefield. The bearer frames the match rather than being it --
  -- it is the Filter.Context's source (so `Not IsSource` is Soul Warden's
  -- "another"), and its controller is the perspective CR 109.5 gives "you" in
  -- "a creature YOU CONTROL enters".
  TriggerCondition.PermanentEnters f -> case event of
    GameEvent.Moved zc _
      | ZoneChange.to zc == Zone.Battlefield ->
          -- Deliberately NOT the ProjectedCharacteristics the Moved event
          -- carries. That snapshot is the object as it last existed in the zone
          -- it LEFT -- CR 608.2h last known information for the LEAVING side --
          -- and reading it here would answer CR 603.6b backwards: "continuous
          -- effects that modify characteristics of a permanent do so the moment
          -- the permanent is on the battlefield (and not before then)", and CR
          -- 603.6b's own example is a land that an "all lands are creatures"
          -- effect makes trigger a creature-enters ability. The entrant's
          -- characteristics come from the game as it stands, which is what CR
          -- 603.10 asks for in the same sentence that fixes the scan's
          -- candidates: "continuous effects that exist at that time are used to
          -- determine ... what the objects involved in the event look like".
          --
          -- viewWithLastKnown, not viewOfObject, so an entrant that has already
          -- left again -- a creature entering as a 0/0 and buried by CR 704.5f
          -- before the CR 117.5 boundary -- is still read as it was ON THE
          -- BATTLEFIELD (CR 608.2h) instead of vanishing from the match. It
          -- reads LIVE whenever the id still resolves, which is the ordinary
          -- case; the fallback is the same CR 608.2h reading eventTriggers'
          -- `leftBattlefield` and `bystanders` take.
          --
          -- The projection is recomputed for each (bearer carrying THIS
          -- condition, entry event) pair rather than shared across the scan:
          -- `matchesTrigger` is handed the GameState and nothing else to share.
          -- It is forced only inside this arm, so a board with no such ability
          -- pays nothing, and eventTriggers' `projected` hoist is untouched.
          --
          -- Nothing is an entrant that is gone AND filed no last known
          -- information -- Resolve.cease and Departure.objectsLeaveWith remove an
          -- object without a zone change running over it. Nothing is known about
          -- what entered, so no Filter can honestly admit it.
          let entrant = ZoneChange.object zc
           in case Projection.viewWithLastKnown entrant gs entrant of
                Nothing -> False
                Just view -> Filter.matches (Filter.MkContext (Just you) (Just bearer)) view f
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 603.2b: this step began, on a turn the scope admits.
  TriggerCondition.StepBegins wanted scope -> case event of
    GameEvent.StepBegan began active ->
      began == wanted && case scope of
        TurnScope.EachTurn -> True
        TurnScope.ControllersTurn -> active == you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 603.8: a state trigger is not an event trigger. It never matches an entry
  -- in the log; stateTriggers below is its whole story.
  TriggerCondition.StateIs _ -> False
  -- CR 510.1b / 510.2: the bearer dealt COMBAT damage to a PLAYER (the trigger
  -- pattern behind "whenever this creature deals combat damage to a player").
  -- Combat damage already records a DamageDealt event, so the match is a filter
  -- over the log, not new recording.
  TriggerCondition.SelfDealsCombatDamageToPlayer -> case event of
    GameEvent.DamageDealt ev ->
      DamageEvent.source ev == bearer
        && DamageEvent.kind ev == DamageKind.Combat
        && isPlayerRecipient (DamageEvent.target ev)
    GameEvent.Moved _ _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 725.2: never matched via a card's bearer -- the monarch's crown-steal is
  -- an inherent ability of no object, so its real match lives in
  -- Pawl.Engine.Monarch.inherentMatch, not here.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  -- CR 702.29c: the bearer IS the card that was cycled. The event carries the
  -- incarnation the card became (CR 400.7), which is the object the scan offers
  -- as the bearer -- see cycledCard in eventTriggers below.
  --
  -- The CAUSE is what makes this narrower than the discard condition below, and
  -- it is the whole of rule 702.29c's "to pay an activation cost of a cycling
  -- ability": an ordinary discard of a card that HAS cycling -- Cathartic
  -- Reunion pitching a Windcaller Aven -- reaches the same graveyard through the
  -- same funnel and must fire nothing.
  TriggerCondition.SelfCycled -> case event of
    GameEvent.Discarded _ oid DiscardCause.ToPayCyclingCost -> oid == bearer
    GameEvent.Discarded _ _ DiscardCause.Ordinary -> False
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 701.9a: a card was discarded, by a player the relation admits. The
  -- discarding player comes from the event; CR 109.5 fixes "you" as the
  -- ability's controller (CR 603.3a), and Megrim's "an opponent" is every other
  -- player -- CR 806.1 in a free-for-all, CR 102.2 in a two-player game, the
  -- same /= either way. CR 102.3's teams are the one reading it is wrong for,
  -- and pawl has none to express (#175).
  --
  -- The bearer is NOT part of the match, unlike every Self- condition here: the
  -- enchantment watches someone else's hand and has nothing to do with the card
  -- that left it.
  --
  -- CR 702.29d -- "these abilities trigger only once when a card is cycled" --
  -- needs no clause of its own, and the DiscardCause is ignored for that reason
  -- rather than by omission. CR 702.29a makes cycling a discard, so a cycled
  -- card must fire this; the cycle is ONE Discarded event, so it fires it once.
  -- TriggerSpec's "CR 702.29d cycling a card fires the discard trigger exactly
  -- once" is the test that proves it.
  TriggerCondition.PlayerDiscards relation -> case event of
    GameEvent.Discarded discarder _ _ -> case relation of
      PlayerRelation.You -> discarder == you
      PlayerRelation.Opponent -> discarder /= you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 508.3a: the bearer was DECLARED as an attacker. Matched against the
  -- declaration event and not against Combat.attackers, which is what keeps the
  -- same rule's last sentence true -- "such abilities won't trigger if a creature
  -- is put onto the battlefield attacking" -- since a creature that entered
  -- attacking is in that record and has no event here. CombatSpec's "the tokens
  -- are attacking, and the attack trigger fired only for the Garrison" is the
  -- test that proves it.
  TriggerCondition.SelfAttacks frequency -> case event of
    GameEvent.AttackerDeclared oid ->
      oid == bearer && case frequency of
        TriggerFrequency.EveryTime -> True
        -- Aurelia, the Warleader's "for the first time each turn". The
        -- declaration being matched is already in GameState.events when the scan
        -- reaches here, so "the first time" is "this is the only one so far",
        -- and the log being cleared at turn handoff is what makes it "each
        -- turn".
        --
        -- Counted per BEARER, so two creatures declared in the same attack are
        -- each attacking for the first time.
        --
        -- CR 400.7 mints a new object on a zone change, so a creature that left
        -- the battlefield and returned is a different id and attacks for the
        -- first time again -- which is what the rules say happened.
        TriggerFrequency.FirstTimeEachTurn -> declarationsOf bearer gs <= 1
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 603.6: a zone-change trigger, matched on BOTH ends of the move -- library
  -- to graveyard. The bearer is the incarnation the card became on arrival, and
  -- that is CR 400.7e rather than an accident of which id the log happens to
  -- carry: "Abilities that trigger when an object moves from one zone to another
  -- ... can find the new object that it became in the zone it moved to when the
  -- ability triggered, if that zone is a public zone" -- and a graveyard is
  -- public (CR 400.2). It is also the object the graveyard candidates below
  -- offer. The from and to together are what make CR 113.6k put this ability in
  -- the graveyard rather than on the battlefield.
  --
  -- Both ends are load-bearing, and `from` is the half that does the work: the
  -- same card discarded out of a hand or dying off the battlefield reaches the
  -- same graveyard and must not trigger.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> case event of
    GameEvent.Moved zc _ ->
      ZoneChange.object zc == bearer
        && ZoneChange.from zc == Zone.Library
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 603.6c narrowed by CR 700.4 ("the term dies means 'is put into a
  -- graveyard from the battlefield'"): the bearer was put into a graveyard from
  -- the battlefield. Both ends are load-bearing, as they are for
  -- SelfPutIntoGraveyardFromLibrary just above, and for the mirror-image reason:
  -- `from` is what keeps a Doomed Traveler DISCARDED out of a hand silent, and
  -- `to` is what keeps one EXILED off the battlefield silent -- the latter has
  -- left the battlefield (CR 603.6c) without dying (CR 700.4), and the whole
  -- point of naming this condition after the word the card prints is that the
  -- two stay apart. SelfLeavesTheBattlefield below is the other one, and
  -- TriggerSpec's LeavesTheBattlefield group proves the separation from both
  -- sides: a bounced Thragtusk fires, a bounced Doomed Traveler does not.
  --
  -- Matched on `departed`, NOT on `object`. That is CR 603.10a's look-back:
  -- "some zone-change triggers look back in time. These are
  -- leaves-the-battlefield abilities", so the bearer offered here is the
  -- permanent as it was immediately before the event (eventTriggers'
  -- `leftBattlefield`, reading CR 608.2h last known information), never the CR
  -- 400.7 incarnation that arrived in the graveyard.
  TriggerCondition.SelfDies -> case event of
    GameEvent.Moved zc _ ->
      ZoneChange.departed zc == bearer
        && ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 603.6c taken whole: "leaves-the-battlefield abilities trigger when a
  -- permanent moves from the battlefield to another zone". The `from` half is
  -- the same as the SelfDies arm's; the `to` half is where the two part company,
  -- and this one asks only that the destination be ANOTHER zone. Thragtusk's.
  --
  -- The `to /= Battlefield` guard is that rule's own word "another", and it is
  -- load-bearing rather than decorative: recordTokenEntry files a
  -- battlefield-to-battlefield pseudo-move for a newly created token whose
  -- `departed` is the token's own id, so a token bearing this condition would
  -- fire on its own creation without it.
  --
  -- Matched on `departed` for the reason the SelfDies arm gives -- CR 603.10a
  -- names leaves-the-battlefield abilities as its first look-back exception, so
  -- the bearer offered here is the permanent as it was immediately before the
  -- event.
  --
  -- CR 603.6c's other trigger event, "when a phased-in permanent leaves the game
  -- because its owner leaves the game", is not matched (#385).
  TriggerCondition.SelfLeavesTheBattlefield -> case event of
    GameEvent.Moved zc _ ->
      ZoneChange.departed zc == bearer
        && ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc /= Zone.Battlefield
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
  -- CR 701.6a: a spell was countered, by a spell or ability whose controller the
  -- relation admits. The countering source's controller comes from the event --
  -- Countering.controller, captured as the counter happened -- and CR 109.5
  -- fixes "you" as the ability's controller (CR 603.3a). Baral, Chief of
  -- Compliance's is `You`.
  --
  -- The bearer is NOT part of the match, the PlayerDiscards posture rather than
  -- any Self- condition's: Baral is a creature on the battlefield and the
  -- countering is done by an instant somewhere else, so nothing about the bearer
  -- is part of the match.
  --
  -- The CR 113.6g gate needs no clause here, and that is what makes the pair of
  -- proving tests in Pawl.TriggerSpec a composition rather than a coincidence: a
  -- spell that can't be countered is not countered at all (CR 101.2), so
  -- Event.counter records nothing and there is no event for this arm to see.
  TriggerCondition.SpellOrAbilityCounters relation -> case event of
    GameEvent.SpellCountered c -> case relation of
      PlayerRelation.You -> Countering.controller c == you
      PlayerRelation.Opponent -> Countering.controller c /= you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False

-- CR 603.2: the bindings the EVENT contributes to a trigger it has just fired --
-- the environment in which the ability's "that player" / "that creature" is
-- read. Called only for an (ability, event) pair `matchesTrigger` has already
-- said yes to, so an arm here may assume its condition's shape matched; a
-- mismatched pair contributes nothing rather than being an error.
--
-- Separate from `matchesTrigger` rather than folded into a `Maybe bindings`
-- return, because the two have different customers: a DELAYED ability
-- (delayedPending) matches against several events at once and carries the
-- environment CAPTURED when it was armed (CR 603.7c), which is not this.
--
-- The parallel for a SOURCELESS inherent ability is Pawl.Engine.Monarch.inherentMatch,
-- which binds its own event's creature; there is no shared matcher because that
-- one has no bearer to scope the match to.
eventBindings :: TriggerCondition -> GameEvent -> Map.Map SlotName.SlotName Binding
eventBindings cond event = case (cond, event) of
  -- CR 702.70a's "that player": the player the bearer dealt combat damage to.
  (TriggerCondition.SelfDealsCombatDamageToPlayer, GameEvent.DamageDealt ev) ->
    case DamageEvent.target ev of
      Recipient.ToPlayer pid -> Binding.setTriggerPlayer pid Map.empty
      Recipient.ToCreature _ -> Map.empty
      Recipient.ToPlaneswalker _ -> Map.empty
      Recipient.ToObject _ -> Map.empty
  -- CR 400.7e: "Abilities that trigger when an object moves from one zone to
  -- another ... can find the new object that it became in the zone it moved to
  -- when the ability triggered, if that zone is a public zone." CR 603.6c says
  -- it from the other side -- "an ability that attempts to do something to the
  -- card that left the battlefield checks for it only in the first zone that it
  -- went to" -- and CR 603.6e repeats it for an Aura watching its host leave.
  --
  -- ZoneChange.object, NOT ZoneChange.departed. The two are the whole point of
  -- this arm: `departed` is what matchesTrigger just matched the bearer against
  -- (CR 603.10a's look-back reads the permanent as it was on the battlefield),
  -- and it names an id CR 400.7 has already deleted, so an effect handed that
  -- id would move nothing. `object` is the card in the graveyard, which is what
  -- "return it to its owner's hand" has to act on.
  --
  -- Bound ALONGSIDE the source rather than instead of it. Engine.placeBorne
  -- stamps Binding.triggerSource over these, and it must keep stamping the
  -- departed id: that slot is CR 113.7a's source, and it is what
  -- Projection.viewWithLastKnown reads CR 608.2h last known information for at
  -- every quantity-evaluating arm of Pawl.Engine.Resolve. One printed "it", two
  -- objects -- see Pawl.Engine.Binding.became.
  --
  -- CR 400.7e's public-zone proviso is satisfied by construction here and is
  -- therefore not a branch: matchesTrigger's SelfDies arm has already required
  -- `to == Graveyard` (CR 700.4), and CR 400.2 lists the graveyard among the
  -- public zones. The arm below, for the wider condition, is where it becomes a
  -- real test.
  (TriggerCondition.SelfDies, GameEvent.Moved zc _) ->
    Binding.setBecame (ZoneChange.object zc) Map.empty
  -- The same rule, with its proviso doing real work for the first time. CR
  -- 603.6c's wider condition accepts ANY destination, and CR 400.2 makes two of
  -- them hidden -- "library and hand are hidden zones, even if all the cards in
  -- one such zone happen to be revealed" -- so CR 400.7e's "if that zone is a
  -- public zone" is a guard here rather than a fact.
  --
  -- The binding is ABSENT for a hidden destination, not present-but-useless.
  -- ZoneChange.object names a real card sitting in that hand, and stamping it
  -- would hand the ability an object the rule forbids it to find; an effect
  -- reading the slot would then quietly do something the rules deny. Absence is
  -- what CardSpec's slot lint reads (via eventBindingSlots below) and what
  -- Pawl.Engine.Resolve's own arms treat as "nothing to act on".
  --
  -- Classified by the ZONE, through Game.isHiddenZone, never by asking whether
  -- the card is currently visible: CR 400.2's "even if all the cards in one such
  -- zone happen to be revealed" is exactly that distinction.
  (TriggerCondition.SelfLeavesTheBattlefield, GameEvent.Moved zc _)
    | not (Game.isHiddenZone (ZoneChange.to zc)) ->
        Binding.setBecame (ZoneChange.object zc) Map.empty
  -- CR 400.7e again, read in the ENTRY direction: Aether Flash's "whenever a
  -- creature enters, this enchantment deals 2 damage to IT". The object that
  -- moved is the entrant, and "the new object that it became in the zone it
  -- moved to" is the permanent now on the battlefield -- ZoneChange.object, the
  -- same field the SelfDies arm above reads, for the same reason.
  --
  -- The SAME slot as that arm, not a second one, because CR 400.7e is one rule
  -- and this is one of its two readings. What differs between the arms is which
  -- object CR 113.7a's SOURCE happens to be, and that is a fact about the
  -- CONDITION rather than about the slot: SelfDies matches on the departing
  -- incarnation (CR 603.10a's look-back), so `triggerSource` and `became` are
  -- two incarnations of one card, while here the bearer is some other permanent
  -- entirely and `became` is the only name the entrant has. Two slots would
  -- have to be kept apart by every reader for a distinction no rule draws --
  -- and Pawl.Engine.Resolve, which is where the slot is read, cannot draw it: it never
  -- learns which condition placed the ability.
  --
  -- CR 400.7e's public-zone proviso holds by construction here too, and even
  -- more simply than for SelfDies: matchesTrigger's PermanentEnters arm has
  -- already required `to == Battlefield`, and CR 400.2 lists the battlefield
  -- among the public zones.
  --
  -- Bound whatever the Filter admits, creature or not. Whether the entrant can
  -- RECEIVE what the payload does to it is the payload's question -- CR 120.1a
  -- for damage, answered in Pawl.Engine.Damage.damageRecipient -- not this arm's; a
  -- binding that existed only for creatures would make the slot's presence
  -- depend on the entrant, which eventBindingSlots below could not express.
  (TriggerCondition.PermanentEnters _, GameEvent.Moved zc _) ->
    Binding.setBecame (ZoneChange.object zc) Map.empty
  -- Megrim's "that player": the player who discarded, which CR 701.9a's "its
  -- owner's hand ... that player's graveyard" makes one player and the event
  -- carries directly. The same reserved slot CR 702.70a's poisonous uses, for
  -- the same reason -- a player the EVENT names, which CR 109.5's `you` cannot
  -- stand in for.
  (TriggerCondition.PlayerDiscards _, GameEvent.Discarded discarder _ _) ->
    Binding.setTriggerPlayer discarder Map.empty
  _ -> Map.empty

-- Which slots eventBindings above can stamp for a condition, as a set. A
-- CLASSIFICATION of a rule 603 trigger condition -- the sibling of
-- functionsInGraveyard below, which asks the other structural question about the
-- same closed type -- so it never reaches an ability's payload and no reader of
-- it learns what any effect IS.
--
-- Its customer is the card lint (CardSpec's "every slot a triggered ability
-- reads is bound for its condition"): an effect naming CR 400.7e's `became` or
-- CR 702.70a's `thatPlayer` under a condition that binds neither would place its
-- trigger, miss the lookup and silently do nothing, which is the worst failure
-- mode card data has.
--
-- Exhaustive with no wildcard, deliberately unlike eventBindings' own
-- `_ -> Map.empty`: that case is over (condition, event) PAIRS, where a wildcard
-- is the only way to say "this pair does not match", while a new CONDITION here
-- must force a decision rather than defaulting to "binds nothing" -- the default
-- that would silently un-lint whatever slot the new condition binds.
--
-- A PARALLEL STATEMENT, PINNED BY A TEST. This says in one dimension what
-- eventBindings says in two, so the two can drift out of agreement. Deriving
-- this from that would mean fabricating a representative GameEvent per condition
-- inside the rules core, which is fixture work the engine has no other use for;
-- the agreement is therefore pinned from the test side instead, by TriggerSpec's
-- "CR 603.2 eventBindingSlots names exactly the keys eventBindings stamps for
-- EVERY event a condition admits", which runs every condition against the events
-- that genuinely fire it and intersects the Map.keysSet of each result against
-- the answer here.
--
-- Every slot named here is GUARANTEED given a match, which is the only reading
-- that makes a per-CONDITION set sound: the answer must hold for every event the
-- condition admits, because the card lint asking it has no event in hand. For
-- most conditions the two readings coincide -- matchesTrigger's SelfDies and
-- PermanentEnters arms have already required the graveyard and battlefield
-- destinations respectively (so CR 400.7e's public-zone proviso holds by
-- construction for both, CR 400.2), and its SelfDealsCombatDamageToPlayer arm
-- has already required a player recipient (isPlayerRecipient).
-- SelfLeavesTheBattlefield is the one where they come apart, and the floor is
-- what it gets; see its own arm below.
eventBindingSlots :: TriggerCondition -> Set.Set SlotName.SlotName
eventBindingSlots cond = case cond of
  -- CR 603.6a's two written forms differ here, and only because of which object
  -- the bearer is. SelfEnters matches on `ZoneChange.object == bearer`, so the
  -- bearer IS the entrant and CR 113.7a's source slot already names it; binding
  -- it again under `became` would be a second name for one object, exactly the
  -- SelfPutIntoGraveyardFromLibrary case below. "Whenever a [type] enters" has
  -- no such luck: the entrant is some other permanent, so CR 400.7e's slot is
  -- the only name it has (Aether Flash).
  TriggerCondition.SelfEnters -> Set.empty
  TriggerCondition.PermanentEnters _ -> Set.singleton Binding.became
  -- CR 603.2b's step beginning names no object and no player but the active one,
  -- and the active player is not what CR 109.5's `you` means.
  TriggerCondition.StepBegins _ _ -> Set.empty
  -- CR 603.8: a state trigger matches a game STATE rather than an event
  -- (matchesTrigger's StateIs arm answers False for every event), so no event
  -- contributes anything to one.
  TriggerCondition.StateIs _ -> Set.empty
  -- CR 702.70a's "that player": the player the bearer dealt combat damage to.
  TriggerCondition.SelfDealsCombatDamageToPlayer -> Set.singleton Binding.triggerPlayer
  -- CR 725.2's inherent ability is minted by Pawl.Engine.Monarch and borne by no card,
  -- and its bindings come from Monarch.inherentMatch rather than from
  -- eventBindings -- which is why the answer here is eventBindings' own, empty.
  -- A card that declared this condition would get nothing from the event, which
  -- is the honest answer rather than an omission.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> Set.empty
  -- CR 702.29c's cycled card is the bearer itself, already bound as CR 113.7's
  -- source, and rule 508.3a's declared attacker likewise.
  TriggerCondition.SelfCycled -> Set.empty
  -- CR 701.9a's discarding player, which is nobody the bearer already names --
  -- Megrim's "that player" is the opponent whose hand the card left.
  TriggerCondition.PlayerDiscards _ -> Set.singleton Binding.triggerPlayer
  TriggerCondition.SelfAttacks _ -> Set.empty
  -- CR 113.6k: the bearer of a library-to-graveyard trigger IS the arriving
  -- incarnation, so binding it again under `became` would be a second name for
  -- one object. Narcomoeba reads the source slot instead.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Set.empty
  -- CR 400.7e: the incarnation the card became, which CR 603.10a's look-back
  -- keeps out of the source slot.
  TriggerCondition.SelfDies -> Set.singleton Binding.became
  -- The same slot, and the same rule, but bound only for a PUBLIC destination
  -- (CR 400.7e's proviso, over CR 400.2's hidden hand and library) -- so the
  -- guaranteed floor this function answers is empty. The consequence is that a
  -- card whose leaves-the-battlefield payload names `became` is rejected by the
  -- lint (#505).
  TriggerCondition.SelfLeavesTheBattlefield -> Set.empty
  -- CR 701.6a's countering names two objects and a player, and this condition
  -- binds none of them -- eventBindings has no arm for it, and this is that
  -- answer stated in the other dimension. Baral, Chief of Compliance's payload
  -- speaks only about its own controller, which is CR 109.5's `you` slot, bound
  -- for every triggered ability by Engine.placeBorne rather than by the event.
  --
  -- Empty by DECISION rather than by default. The countering source and the
  -- countered spell are both dead ids by the time this trigger resolves (CR
  -- 400.7 for the spell, CR 608.2n for an ability), and CR 400.7e's rescue --
  -- "can find the new object that it became in the zone it moved to ... if that
  -- zone is a public zone" -- would name the countered card in its owner's
  -- graveyard. A card that says "exile it instead" (Dissipate) is the one that
  -- must bind `became` here.
  TriggerCondition.SpellOrAbilityCounters _ -> Set.empty

-- Whether a damage recipient is a player (CR 120.1): a total discriminator over
-- Recipient, so the combat-damage-to-player trigger matcher stays non-partial.
isPlayerRecipient :: Recipient.Recipient -> Bool
isPlayerRecipient r = case r of
  Recipient.ToPlayer _ -> True
  Recipient.ToCreature _ -> False
  Recipient.ToPlaneswalker _ -> False
  Recipient.ToObject _ -> False

-- CR 603.6a: "Each time an event puts one or more permanents onto the
-- battlefield, all permanents on the battlefield (including the newcomers)
-- are checked for any enters-the-battlefield triggers that match the event."
-- This WIDENS M3f's scan, which only ever inspected the object an enters
-- event named: a step trigger belongs to a permanent that has nothing to do
-- with the event at all, so every event is checked against every permanent
-- currently on the battlefield.
--
-- The candidate set -- the current battlefield permanents -- is the same for
-- every event; it is projected ONCE via Projection.projectAll, before the
-- event loop, not once per (event, permanent) pair. The naive alternative --
-- calling Projection.triggeredAbilitiesOf per (event, permanent) pair, as this
-- scan did before this task -- goes through Projection.project, which reruns
-- the whole-board `gather` fold on every call; that made settleForPriority's
-- trigger scan quadratic in board size. Projection.projectAll runs `gather`
-- exactly once and shares the result across the whole scan.
--
-- The battlefield is not the only scanned zone: every GRAVEYARD is scanned too,
-- for the abilities CR 113.6k puts there. The hand, exile, the stack and the
-- command zone are still unscanned (#348).
--
-- CR 603.10, FIRST sentence -- the NORMAL rule, not the "looks back in time"
-- exception list that follows it (603.10a-g are leaves-the-battlefield,
-- sacrifice, phase-out and similar, none of them enters): "objects that exist
-- immediately after an event are checked to see if the event matched any trigger
-- conditions". That is a per-EVENT question, and the live battlefield set this
-- scan walks answers a per-BOUNDARY one: the scan runs once, at CR 117.5, after
-- CR 704.5's state-based actions have already run. Every permanent that left the
-- battlefield anywhere inside the batch is missing from that set, including for
-- the events it was plainly still there for.
--
-- So each event contributes the permanents that left the battlefield LATER in
-- the same batch, read from CR 608.2h last known information
-- (GameState.lastKnown) -- `bystanders` below, the running union of
-- `leftBattlefield` over the events after this one. Four things make that exact
-- rather than approximate:
--
--   * IT IS THE SAME READING, ONE EVENT LATER. A permanent still on the
--     battlefield to be removed by a later event existed immediately after this
--     one, which is precisely what the rule asks about. It reaches the event's
--     own newcomer for free: a creature that enters as a 0/0 and is buried by CR
--     704.5f leaves at a later index than its own entry, so its CR 603.6a entry
--     trigger ("including the newcomers") is recovered by the general rule
--     rather than by a case of its own.
--   * NO DOUBLE FIRE, structurally. GameState.lastKnown is written by the zone
--     change that DELETES an id, and CR 400.7 mints a fresh id per move, so no
--     id is ever in both `lastKnown` and `objects`. A permanent still on the
--     battlefield at the boundary therefore has no lastKnown entry and appears
--     here not at all; the Map.unions below prefers the live reading regardless.
--   * THE RIGHT SNAPSHOT. `lastKnown` holds the permanent as it was in the zone
--     it LEFT -- the battlefield -- so it is read with its continuous effects
--     applied, which is what CR 603.10's same sentence demands ("continuous
--     effects that exist at that time are used to determine what the trigger
--     conditions are").
--   * A CANONICAL PLACE IN THE ORDER. Candidates are a Map keyed by ObjectId and
--     traversed ascending, so the extra candidates sort into the same
--     permanents-inner order every other candidate obeys -- they are not
--     appended.
--
-- CR 603.10a is the OTHER half of that rule, and the exception rather than the
-- normal case: "some zone-change triggers look back in time. These are
-- leaves-the-battlefield abilities ..." So a DEPARTURE event ALSO contributes
-- the permanent it took off the battlefield -- read from the same `lastKnown` --
-- and for that one the last-known reading is not a repair for a boundary the
-- scan arrives at late, it is what the rule asks for. See `leftBattlefield`
-- below, which is both the CR 603.10a source in its own right and the step
-- `bystanders` accumulates.
--
-- The reverse direction is not reconstructed: a permanent that ENTERED later in
-- the batch and left before the boundary is offered as a candidate for the
-- batch's earlier events too, which CR 603.10 would not have it be (#441).
--
-- Events outer, permanents inner (ascending by id): a deterministic canonical
-- order, which is what the CR 603.3b ordering prompt indexes into.
eventTriggers :: [GameEvent] -> GameState -> [PendingTrigger]
eventTriggers events gs =
  let projected = Projection.projectAll gs
      -- The control-grant list is the same for every permanent in this scan
      -- (same reason projected is computed once): Projection.controllerOf
      -- would otherwise rebuild it, and re-run its liveGiven fixpoint, once
      -- per battlefield object.
      grants = Projection.controlGrants gs
      -- CR 702.70a: a keyword can BE a triggered ability, so a permanent's
      -- abilities are its printed-and-granted ones plus the ones rule 702 mints
      -- from its keywords. Derived from the POST-LAYER counts, so Humility takes
      -- them away and an Aura's layer-6 grant adds them without either being
      -- special-cased. Shared by both candidate sources below, so a live
      -- permanent and a last-known one are read the same way.
      abilitiesOf pc = PC.triggeredAbilities pc <> Keyword.triggeredAbilitiesOf (PC.keywords pc)
      onBattlefield =
        Map.fromList
          ( Maybe.mapMaybe
              ( \oid -> case Map.lookup oid projected of
                  -- Unreachable: projected (Projection.projectAll gs) is keyed on
                  -- the same GameState.battlefield set this list walks, so every
                  -- oid drawn from that set has an entry.
                  Nothing -> Nothing
                  -- CR 603.3a: controlled by whoever controls the source when it
                  -- triggers. Projection.controllerOfGiven reads control AT THE SCAN
                  -- BOUNDARY, not at the moment the underlying event fired (#47) --
                  -- unobservable today because nothing changes control between an
                  -- event and the CR 117.5 boundary.
                  Just pc ->
                    fmap
                      (\ctrl -> (oid, (ctrl, abilitiesOf pc)))
                      (Projection.controllerOfGiven grants Set.empty oid gs)
              )
              (Set.toAscList (GameState.battlefield gs))
          )
      -- CR 603.10a: the permanent this event took OFF the battlefield, read from
      -- CR 608.2h last known information. Its look-back list opens with
      -- "leaves-the-battlefield abilities", and CR 603.10's own sentence says
      -- what looking back means: the game uses "the existence of those abilities
      -- and the appearance of objects immediately prior to the event". Both live
      -- in the single `lastKnown` record, written by the very zone change that
      -- deleted the id, from the PRE-MOVE state -- so the ability is read as it
      -- existed on the battlefield (layers applied, CR 613) and CR 603.3a's
      -- controller is the player who controlled the permanent as it left, not
      -- the owner of the card that landed in the graveyard.
      --
      -- It is possible only because GameEvent.Moved names BOTH ids: a departure
      -- event's ZoneChange.object is the new incarnation in the DESTINATION zone
      -- (CR 400.7), which `lastKnown` knows nothing about, while
      -- ZoneChange.departed is exactly the key it files under.
      --
      -- Keyed by that departing id, which by construction no longer exists, so
      -- this source cannot collide with any other: not with `onBattlefield`
      -- (live ids), not with `bystanders` (which unions this same function over
      -- the LATER events only, and an id departs exactly once -- changeZone
      -- deletes it), not with `cycledCard` (a cycled card leaves a HAND), and
      -- not with `inGraveyards` (live graveyard ids). One entry per id means one
      -- pass of `forOne`, without leaning on Map.unions' bias.
      --
      -- EVERY battlefield departure contributes, not only the deaths. Which
      -- destinations a condition accepts is the CONDITION's business --
      -- matchesTrigger's SelfDies arm asks for a graveyard, CR 700.4 -- and
      -- keeping that out of the candidate source is what let CR 603.6c's wider
      -- "leaves the battlefield" arrive as a matcher arm alone, with this
      -- function untouched.
      --
      -- The to /= Battlefield guard is CR 603.6c's own wording, "moves from the
      -- battlefield to ANOTHER zone": the battlefield-to-battlefield pseudo-move
      -- recordTokenEntry emits for a new token is not a departure.
      --
      -- The departing id is also what the placed trigger carries as its SOURCE,
      -- and so what Binding.triggerSource binds -- CR 113.7a, and the id
      -- Projection.viewWithLastKnown answers for. CR 603.6c's "an ability that
      -- attempts to do something to the card that left the battlefield checks
      -- for it only in the first zone that it went to" wants the ARRIVING
      -- incarnation instead, and that is a SECOND slot rather than a different
      -- value in this one: eventBindings binds it under Binding.became.
      --
      -- Empty for a permanent that ceased without a zone change ever running
      -- over it (Resolve.cease, Departure.objectsLeaveWith), which files no last
      -- known information. That hole is `bystanders`' too, since it is built out
      -- of this: such a permanent is recoverable for no event at all.
      leftBattlefield event = case event of
        GameEvent.Moved zc _
          | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc /= Zone.Battlefield ->
              case Map.lookup (ZoneChange.departed zc) (GameState.lastKnown gs) of
                Nothing -> Map.empty
                Just lk ->
                  Map.singleton
                    (ZoneChange.departed zc)
                    (LastKnown.controller lk, abilitiesOf (LastKnown.characteristics lk))
        GameEvent.Moved _ _ -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.StepBegan _ _ -> Map.empty
        GameEvent.SpellCast _ -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.Discarded {} -> Map.empty
        GameEvent.Revealed _ _ -> Map.empty
        GameEvent.AttackerDeclared _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
      -- CR 603.10's first sentence, per EVENT: for each event in the batch, the
      -- permanents that were still on the battlefield when it happened and have
      -- left by the CR 117.5 boundary. One entry per event, aligned with
      -- `events`, so the element at index i is the union of `leftBattlefield`
      -- over the events AFTER i -- a permanent removed by a later event was
      -- there for this one.
      --
      -- STRICTLY LATER, so an event's own departure is not in its own entry.
      -- That is not an optimisation: the departing permanent does NOT exist
      -- immediately after the event that removed it, and it is a candidate for
      -- that one event only through CR 603.10a's look-back, which
      -- `leftBattlefield` supplies separately and under a different rule.
      --
      -- A right scan rather than a lookup table: `List.scanr` shares each
      -- suffix's union with the one before it, so the whole batch costs one pass
      -- and one `leftBattlefield` call per event. Building the union per event
      -- instead would be quadratic in batch size, and a combat damage step's
      -- batch is a whole board's worth of deaths.
      --
      -- `drop 1` is the alignment: scanr yields n+1 entries, the last being the
      -- empty union after the final event, and entry i is the union over the
      -- events from i ONWARD. Dropping the head shifts it to "from i+1 onward",
      -- which is what pairs with events !! i.
      --
      -- CR 603.3a's controller, and the abilities themselves, are the ones the
      -- permanent had as it LEFT -- one moment after the event that triggered
      -- them, not at it. That is #47's elision, on a nearer boundary than the
      -- live path's: `onBattlefield` reads both at the CR 117.5 scan, and this
      -- reads them at the departure, which is somewhere between the event and
      -- the scan. Nothing in this pool moves control or grants an ability in
      -- that window, which is why the two coincide today.
      bystanders = drop 1 (List.scanr (\event acc -> Map.union (leftBattlefield event) acc) Map.empty events)
      -- CR 702.29c: the card that was just cycled, wherever it landed. The
      -- candidate source that is neither on the battlefield nor a permanent that
      -- left it -- which is exactly what that rule asks for:
      -- "these abilities trigger from whatever zone the card winds up in after
      -- it's cycled", the graveyard for every printing today.
      --
      -- Its abilities come from the PRINTED card, not from a projection, for the
      -- reason Keyword.handAbilitiesOf gives about a hand: CR 613's layer system
      -- reaches the battlefield, so there is nothing in a graveyard to project.
      -- Rule 702's own minted triggered abilities are not consulted either --
      -- rule 702.70a's poisonous is a permanent's ability, and no keyword mints
      -- one that functions from a graveyard.
      --
      -- The controller is the OWNER, which is CR 113.8's second clause for a
      -- triggered ability -- "or, if it had no controller, the player who owned
      -- the ability's source when it triggered". A card in a graveyard has no
      -- controller (CR 108.4), the same reason Activate.activatorOf reaches for
      -- the owner of a card in a hand.
      --
      -- Scoped to the CYCLING cause, not to every discard. That is rule 702.29c
      -- speaking about cycling triggers specifically; an ordinary discard's card
      -- reaches the graveyard too, and is offered by `inGraveyards` below under
      -- CR 113.6k -- which admits only a condition that can trigger from there.
      cycledCard event = case event of
        GameEvent.Discarded _ oid DiscardCause.ToPayCyclingCost -> case Game.lookupObject oid gs of
          Nothing -> Map.empty
          Just obj -> case Game.cardOf oid gs of
            Nothing -> Map.empty
            Just card -> Map.singleton oid (Object.owner obj, Card.triggeredAbilities card)
        GameEvent.Discarded _ _ DiscardCause.Ordinary -> Map.empty
        GameEvent.Moved _ _ -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.StepBegan _ _ -> Map.empty
        GameEvent.SpellCast _ -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        -- A reveal offers no candidate source of its own. The event names no
        -- object at all (see GameEvent.Revealed), so there is nothing here to
        -- hang an ability on; a card that triggers on a reveal would need a
        -- TriggerCondition first, and none exists (#322).
        GameEvent.Revealed _ _ -> Map.empty
        GameEvent.AttackerDeclared _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
      -- CR 113.6k: the last candidate source -- every card in every graveyard
      -- that carries at least one ability CR 113.6k puts there. The one source
      -- that widens the SCANNED ZONE rather than recovering an object a single
      -- event names, which is why it is computed ONCE, outside the event loop,
      -- exactly as `onBattlefield` is: the set of graveyard cards is the same
      -- for every event in the batch.
      --
      -- Narrow by construction, which is what keeps a large graveyard cheap.
      -- Membership is decided by `functionsInGraveyard` over each PRINTED
      -- triggered ability's CONDITION -- a total case over a closed type, no
      -- projection, no board walk -- so the whole pass is O(cards in graveyards
      -- x abilities per card), and the common case (a card with no triggered
      -- ability at all, or with only battlefield ones) costs two object-map
      -- lookups and an empty-list check.
      -- Cards contributing nothing are dropped rather than carried as empty
      -- entries, so the candidate map stays proportional to the cards that can
      -- actually fire and not to graveyard size.
      --
      -- Abilities come from the PRINTED card and the controller is the OWNER,
      -- both for the reasons `cycledCard` above spells out (CR 613's layers stop
      -- at the battlefield; CR 108.4 leaves a graveyard card with no controller,
      -- so CR 113.8's second clause names the owner).
      --
      -- CR 603.10a does NOT apply to what this source serves. Its look-back list
      -- is "leaves-the-battlefield abilities, abilities that trigger when a
      -- player sacrifices a permanent, abilities that trigger when a card leaves
      -- a graveyard, and abilities that trigger when an object that all players
      -- can see is put into a hand or library" -- a card ENTERING a graveyard is
      -- none of those, so CR 603.10's normal first sentence governs and the
      -- reading is of the game as it stands, which is what this live read of
      -- GameState.graveyard is. A card that arrives in a graveyard and is gone
      -- again before the CR 117.5 boundary is lost by this reading (#349).
      graveyardCandidate oid = case (Game.lookupObject oid gs, Game.cardOf oid gs) of
        (Just obj, Just card) ->
          case filter (functionsInGraveyard . TriggeredAbility.condition) (Card.triggeredAbilities card) of
            [] -> Nothing
            abilities -> Just (oid, (Object.owner obj, abilities))
        _ -> Nothing
      inGraveyards =
        Map.fromList
          (concatMap (Maybe.mapMaybe graveyardCandidate . Foldable.toList) (Map.elems (GameState.graveyard gs)))
      forOne event (oid, (ctrl, abilities)) =
        let fires ab = matchesTrigger gs oid ctrl (TriggeredAbility.condition ab) event
            pend ab = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) ctrl ab (eventBindings (TriggeredAbility.condition ab) event)
         in fmap pend (filter fires abilities)
      -- Map.unions is left-biased, so a live battlefield reading always wins over
      -- a last-known one, over a cycled card, and over a graveyard reading. That
      -- is what rules out a double fire: one entry per id means one pass of
      -- `forOne` per id, whatever the id's abilities came from.
      --
      -- The first four sets are disjoint by construction -- `leftBattlefield` and
      -- `gone` both file only an id that no longer exists, and they are this
      -- event's departure versus the LATER events' (an id departs exactly once),
      -- while cycledCard files only one the funnel just minted in a graveyard --
      -- and the left-bias is belt and braces over that.
      -- `inGraveyards` genuinely OVERLAPS `cycledCard`, and does so on purpose:
      -- CR 702.29c's "these abilities trigger from whatever zone the card winds up
      -- in after it's cycled" is CR 113.6k for a SelfCycled condition, so a card
      -- cycled into a graveyard is honestly a member of both. The result is the
      -- same either way -- cycledCard's entry, which wins, offers the same card's
      -- printed abilities unfiltered, a superset of what inGraveyards offers for
      -- that id -- but the bias is what makes it one entry rather than two.
      candidates event gone = Map.toAscList (Map.unions [onBattlefield, leftBattlefield event, gone, cycledCard event, inGraveyards])
      scanOne (event, gone) = concatMap (forOne event) (candidates event gone)
   in concatMap scanOne (zip events bystanders)

-- CR 113.6k: "A trigger condition that can't trigger from the battlefield
-- functions in all zones it can trigger from. Other trigger conditions of the
-- same triggered ability may function in different zones." Answered for one
-- zone, the graveyard, which is the only non-battlefield zone eventTriggers
-- scans (#348).
--
-- A CLASSIFICATION of a trigger condition, not of an effect: this asks which
-- zone a rule 603 condition functions in, the same kind of question as "is this
-- a mana ability". It never reaches the ability's payload.
--
-- The default is False, and that is CR 113.6's own default in its second
-- sentence: "Abilities of all other objects usually function only while that
-- object is on the battlefield." Every arm below that answers False is that
-- sentence, not an omission -- a Soul Warden in a graveyard does not see a
-- creature enter.
functionsInGraveyard :: TriggerCondition -> Bool
functionsInGraveyard cond = case cond of
  -- CR 603.6a is an enters-the-battlefield ability; its bearer is on the
  -- battlefield when it fires. Both written forms take CR 113.6's default.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  TriggerCondition.StepBegins _ _ -> False
  -- CR 603.8's state triggers are not event triggers at all -- matchesTrigger's
  -- StateIs arm never matches a log entry -- so this scan is not their reader in
  -- any zone. They are gathered by stateTriggers below, which walks the
  -- battlefield and nothing else.
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  -- CR 302.6 and CR 508.1a: only a permanent on the battlefield can be declared
  -- as an attacker, so this condition triggers from the battlefield and CR
  -- 113.6k never reaches it.
  TriggerCondition.SelfAttacks _ -> False
  -- CR 702.29c: "these abilities trigger from whatever zone the card winds up in
  -- after it's cycled" -- the graveyard for every printing in this pool. A
  -- cycled card cannot be on the battlefield (cycling discards it from a hand),
  -- so CR 113.6k reaches this condition too, and answering honestly is what
  -- keeps the classification true rather than merely convenient. eventTriggers'
  -- own `cycledCard` is the source that actually serves it, and the overlap is
  -- documented at the Map.unions there.
  TriggerCondition.SelfCycled -> True
  -- CR 113.6's default, and False for a reason: Megrim watches from the
  -- battlefield, so "a trigger condition that can't trigger from the
  -- battlefield" (CR 113.6k) never reaches it. A card in a graveyard does not
  -- see an opponent discard.
  TriggerCondition.PlayerDiscards _ -> False
  -- CR 113.6k, the condition this predicate exists for: a card cannot be put
  -- into a graveyard from a library while it is on the battlefield, so this
  -- condition can never trigger from the battlefield, and the one zone it can
  -- trigger from is the graveyard it lands in. Narcomoeba.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> True
  -- The mirror image of the arm above, and False for a reason rather than by
  -- default: a dies trigger CAN trigger from the battlefield, so CR 113.6k's
  -- "can't trigger from the battlefield" never reaches it. CR 603.10a is what
  -- makes that true of a permanent that is, by the time the scan runs, a card in
  -- a graveyard -- the look-back reads it as it was immediately before it left,
  -- which is on the battlefield. eventTriggers' `leftBattlefield` is the source
  -- that serves it, from CR 608.2h last known information; `inGraveyards` must
  -- NOT, or the ability would be read off the graveyard card's printed text and
  -- credited to its owner instead of its last controller.
  TriggerCondition.SelfDies -> False
  -- The same answer as SelfDies just above, and for the same CR 603.10a reason:
  -- a leaves-the-battlefield ability triggers from the battlefield, read
  -- through the look-back, so CR 113.6k never reaches it. This condition makes
  -- the point harder to miss -- its destination may be a hand or a library, and
  -- an ability found in a GRAVEYARD could not be what fired for a permanent that
  -- went somewhere else.
  TriggerCondition.SelfLeavesTheBattlefield -> False
  -- CR 113.6's default again: Baral watches from the battlefield, so CR 113.6k's
  -- "a trigger condition that can't trigger from the battlefield" never reaches
  -- it. A card in a graveyard does not see its controller's counterspell
  -- resolve.
  TriggerCondition.SpellOrAbilityCounters _ -> False

-- CR 603.2b / 109.5: does this condition restrict the turn its event may occur
-- on to the ABILITY'S CONTROLLER's turn? True for "at the beginning of YOUR
-- <step>" and for nothing else.
--
-- A CLASSIFICATION of a trigger condition, the third of the same kind as
-- eventBindingSlots and functionsInGraveyard above -- it asks a structural
-- question about a rule 603 condition and never reaches the ability's payload.
--
-- Its customer is the card lint (CardSpec's "every delayed ability armed for
-- YOUR next turn is controller-scoped"). Pawl.Types.Onset.FromYourNextTurn
-- delivers only the NEXT half of "your next turn": it becomes a turn NUMBER
-- (DelayedTrigger.notBefore) and a number cannot say whose turn it is. The YOUR
-- half is this -- so an onset paired with a condition that answers False here
-- would fire on an intervening opponent's turn, and the lint rejects that
-- pairing rather than leaving the two fields to agree by luck.
--
-- Exhaustive with no wildcard, for eventBindingSlots' reason: a new condition
-- must force a decision rather than defaulting to False, which for the arms that
-- carry no turn scope at all is nonetheless the honest answer -- a condition
-- that says nothing about whose turn it is does not restrict one.
controllerTurnScoped :: TriggerCondition -> Bool
controllerTurnScoped cond = case cond of
  -- The one arm that carries a TurnScope, and the whole content of this
  -- classification. CR 603.3a controls the ability and CR 109.5 is what makes
  -- "your" mean that controller (see Pawl.Types.TurnScope).
  TriggerCondition.StepBegins _ TurnScope.ControllersTurn -> True
  -- "At the beginning of EACH <step>" admits every player's turn, which is
  -- exactly the pairing the lint exists to reject.
  TriggerCondition.StepBegins _ TurnScope.EachTurn -> False
  -- None of the rest is a turn-scoped condition at all: each names an event that
  -- can happen on anybody's turn, so none of them restricts one.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.PlayerDiscards _ -> False
  -- CR 508.1a makes this one the ACTIVE player's turn, which is not the same
  -- thing: CR 109.5's "you" is the ability's controller, and a stolen creature
  -- attacks on its thief's turn. False is the honest answer.
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  TriggerCondition.SelfDies -> False
  TriggerCondition.SpellOrAbilityCounters _ -> False

-- CR 603.8: state triggers. Every battlefield permanent whose StateIs condition
-- is currently TRUE and which has no instance already on the stack.
--
-- Armedness is DERIVED, never stored. CR 603.8's second sentence -- "doesn't
-- trigger again until the ability has resolved, has been countered, or has
-- otherwise left the stack" -- names three outcomes that are all "no longer on
-- the stack", so an instance sitting there is the whole suppression rule and
-- there is no bookkeeping field to leak. There is no triggered-but-not-yet-placed
-- window to worry about: Engine.placePendingTriggers puts them on the stack
-- within the same settle step.
--
-- A trigger whose modes are all unfillable would be removed from the stack (CR
-- 603.3c) and re-trigger on the next settle pass while its condition held, which
-- would not terminate. No card in the pool can do that -- Barbarian Outcast's
-- single mode has no target slots and is always fillable -- and the first card
-- that could is the one that must revisit this.
stateTriggers :: GameState -> [PendingTrigger]
stateTriggers gs =
  let -- The control-grant list is the same for every permanent this scan
      -- walks; computed once here rather than once per oid inside forOne,
      -- the same hoist eventTriggers' `grants` binding makes just above.
      grants = Projection.controlGrants gs
      -- Suppression is scoped to (source, ability): `Object.source obj ==
      -- Source.OfTrigger srcId ab` compares BOTH the source object's id and
      -- the ability, so two permanents bearing the identical triggered
      -- ability (e.g. two Barbarian Outcasts) suppress independently -- one
      -- instance per source, not one for the whole board. A weaker
      -- comparison (ability only, dropping srcId) would wrongly suppress a
      -- second source's identical trigger.
      --
      -- Unreachable caveat: if a SINGLE source ever carried two textually
      -- identical StateIs abilities, this same comparison (Source equality)
      -- would conflate them into one Source.OfTrigger value and suppress the
      -- second as though it were an instance of the first. No card in the
      -- pool has two identical state triggers on one source today; the first
      -- that does must revisit this.
      alreadyOnStack srcId ab =
        let isInstance sid = case Game.lookupObject sid gs of
              -- A stack id whose object can't be found: fail CLOSED (treat
              -- as suppressing), not open. This runs inside the
              -- Engine.settleForPriority fixpoint, so a lost suppression
              -- (failing open) loops forever -- a hang, not a wrong answer --
              -- while failing closed costs at most one settle pass' worth of
              -- a legitimate new instance. Unreachable today: Resolve.cease
              -- removes the stack entry and its object together, so a stack
              -- id is never left dangling.
              Nothing -> True
              -- Source equality, so one source carrying two textually identical
              -- StateIs abilities would conflate them and suppress the second
              -- as though it were an instance of the first (#55).
              Just obj -> Object.source obj == Source.OfTrigger srcId ab
         in any isInstance (GameState.stack gs)
      forOne oid = case Projection.controllerOfGiven grants Set.empty oid gs of
        Nothing -> []
        -- CR 603.3a: a triggered ability is controlled by whoever controls
        -- its source; CR 109.5 is what makes "you" in the condition mean that
        -- same controller. Outside the layer fold, so the ViewOf is the FULL
        -- projection (Pawl.Engine.Condition's spec, unbounded -- unlike the
        -- layer-bounded one Pawl.Engine.Projection hands the fold itself).
        Just ctrl ->
          let live ab = case TriggeredAbility.condition ab of
                TriggerCondition.StateIs cond ->
                  Condition.holds (Projection.fullView gs) (Filter.MkContext (Just ctrl) (Just oid)) gs oid cond
                    && not (alreadyOnStack oid ab)
                TriggerCondition.SelfEnters -> False
                -- CR 603.6a is an EVENT trigger, matched against the log by
                -- matchesTrigger; nothing about it is a CR 603.8 state.
                TriggerCondition.PermanentEnters _ -> False
                TriggerCondition.StepBegins _ _ -> False
                TriggerCondition.SelfDealsCombatDamageToPlayer -> False
                TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
                TriggerCondition.SelfAttacks _ -> False
                TriggerCondition.SelfCycled -> False
                TriggerCondition.PlayerDiscards _ -> False
                TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
                TriggerCondition.SelfDies -> False
                TriggerCondition.SelfLeavesTheBattlefield -> False
                TriggerCondition.SpellOrAbilityCounters _ -> False
              pend ab = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) ctrl ab Map.empty
           in fmap pend (filter live (Projection.triggeredAbilitiesOf oid gs))
   in concatMap forOne (Set.toAscList (GameState.battlefield gs))

-- CR 603.7: delayed abilities whose trigger event is among these events. An
-- entry that fires is REMOVED from the store -- CR 603.7b: "only once, the next
-- time its trigger event occurs" -- UNLESS it carries a stated duration, which
-- is the same rule's own exception ("unless it has a stated duration, such as
-- 'this turn'"). One of Pawl.Engine.Expiry's sweeps ends those instead; CR 514.2's
-- cleanup, for Full Throttle. The survivors are returned so the caller can store
-- them back. CR 603.7d-f: the controller travels with the entry, so a delayed
-- ability resolves under the player who controlled the spell that created it
-- even if that spell's source object is long gone.
--
-- `fires` matches its CONDITION only against EVENTS (`matchesTrigger`), never
-- against live game state -- the live turn number `armed` reads is CR 603.7a's
-- arming gate rather than part of the condition, and it can only ever WITHHOLD a
-- match. So a stored entry whose condition is TriggerCondition.StateIs would
-- never match here -- it would never fire, and unless it states a duration for a
-- Pawl.Engine.Expiry sweep to end, never leave the store either. Not a live gap:
-- TriggerCondition is a closed type (Pawl.Types.TriggerCondition) and no card in
-- this pool arms a delayed ability with a StateIs condition (CR 603.7's few
-- state-triggered delayed abilities, e.g. "at the beginning of the next end
-- step" clauses, are all StepBegins in this pool). Noted because a later P4 task
-- touches state conditions again and should see this before adding one.
--
-- The surviving store this function returns is computed from the EVENT MATCH
-- (`fires`) alone, before gatherTriggers's CR 603.4 intervening-"if" filter
-- (`interveningHolds`) ever runs on the entries it produces -- so an entry whose
-- intervening "if" is false is removed here, spending CR 603.7b's one shot,
-- rather than staying armed for the trigger event's next occurrence (#48). That
-- reaches only an entry with no stated duration: one that has a duration is not
-- spent by firing at all, so a false intervening "if" costs it nothing and it is
-- still armed for the next occurrence.
delayedPending :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
delayedPending events gs =
  let -- CR 603.7a's floor -- "a delayed triggered ability won't trigger until it
      -- has actually been created, even if its trigger event occurred just
      -- beforehand" -- is the watermark's job, and is all an ordinary entry
      -- (notBefore = Nothing) needs. This is the card's OWN further restriction:
      -- an ability printed "on your next turn" (Pawl.Types.Onset) is not armed
      -- on the turn it was created, whatever its condition matches. Read against
      -- the LIVE turn number, so an entry with no onset is untouched.
      armed entry = maybe True (GameState.turnNumber gs >=) (DelayedTrigger.notBefore entry)
      fires entry =
        let cond = TriggeredAbility.condition (DelayedTrigger.ability entry)
         in armed entry && any (matchesTrigger gs (DelayedTrigger.source entry) (DelayedTrigger.controller entry) cond) events
      pend entry =
        PendingTrigger.MkPendingTrigger
          (TriggerSource.OfObject (DelayedTrigger.source entry))
          (DelayedTrigger.controller entry)
          (DelayedTrigger.ability entry)
          (DelayedTrigger.bindings entry)
      store = GameState.delayedTriggers gs
      -- Firing spends the one shot only for an entry with no stated duration.
      spent entry = fires entry && Maybe.isNothing (DelayedTrigger.expiry entry)
   in (fmap pend (Foldable.toList (Seq.filter fires store)), Seq.filter (not . spent) store)

-- Everything that has triggered and is not yet on the stack, from all three
-- sources, plus the delayed store as it stands afterwards. One function, so
-- Pawl.Engine.Engine never needs to know how many sources there are.
gatherTriggers :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
gatherTriggers events gs =
  let (fromDelayed, surviving) = delayedPending events gs
      all_ = eventTriggers events gs <> stateTriggers gs <> fromDelayed
   in (filter (interveningHolds gs) all_, surviving)

-- CR 603.4: "the ability doesn't trigger at all" when its intervening "if" is
-- false as the trigger event occurs. Checked HERE, at the gather -- not at
-- placement -- because "doesn't trigger" must be indistinguishable from "no
-- ability existed", including to the CR 117.5 settle loop's re-run flag.
--
-- A SOURCELESS pending trigger (CR 725.2) never reaches this: gatherTriggers is
-- the only caller, and all three gatherers it draws from hang their triggers on
-- an object. The monarch's inherent pair is gathered by Pawl.Engine.Monarch and merged
-- into the batch by Engine.placePendingTriggers, after this filter has run. The
-- arm is written to be true anyway rather than to fail: CR 725.2 fixes the full
-- text of both inherent abilities and neither has an intervening "if"
-- (Monarch.oneEffect pins `intervening = Nothing` for exactly that reason), so
-- "the ability triggers" is the right answer for every sourceless ability that
-- exists. A sourceless ability that DID carry one would have no subject object
-- for Condition.holds to read, and is the case that must revisit this.
--
-- CR 608.2h supplies the view, not Projection.fullView, and for a look-back
-- trigger that is the difference between reading the clause and reading nothing.
-- CR 603.10a makes a leaves-the-battlefield ability's source the permanent as it
-- was IMMEDIATELY BEFORE the event, so by the time this filter runs that id has
-- been deleted (CR 400.7) -- and fullView describes a deleted id as an object
-- with no characteristics at all, which quietly answers False to every clause
-- that asks about one. viewWithLastKnown is scoped to `oid`, so a source still
-- on the battlefield reads live exactly as before; the fallback is CR 608.2h's,
-- never the default. Pawl.Engine.Stack's CR 608.2a re-check reads the same way, and the
-- two must agree or a trigger would be placed and then removed for disagreeing
-- with itself.
interveningHolds :: GameState -> PendingTrigger -> Bool
interveningHolds gs pending =
  case (TriggeredAbility.intervening (PendingTrigger.ability pending), PendingTrigger.source pending) of
    (Nothing, _) -> True
    (Just _, TriggerSource.Sourceless) -> True
    (Just cond, TriggerSource.OfObject oid) ->
      Condition.holds
        (Projection.viewWithLastKnown oid gs)
        (Filter.MkContext (Just (PendingTrigger.controller pending)) (Just oid))
        gs
        oid
        cond
