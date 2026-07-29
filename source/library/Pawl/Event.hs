-- The event pipeline (CR 603/614). This module owns the single zone-change
-- funnel and the sole casing on TriggerCondition; casing on ReplacementEffect
-- lives in Pawl.Replacement (CR 616.1's loop), which this module's changeZone
-- calls through rather than cases on directly.
-- changeZone lives here (not in Pawl.Game) so it can read the projection --
-- Projection imports Game, so a Game.changeZone that read the projection would
-- be an import cycle. See the plan's module dependency note.
module Pawl.Event where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Binding as Binding
import qualified Pawl.Condition as Condition
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Keyword as Keyword
import qualified Pawl.Projection as Projection
import qualified Pawl.Replacement as Replacement
import Pawl.Type.Binding (Binding)
import Pawl.Type.Card (Card)
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.Counterability as Counterability
import Pawl.Type.DamageEvent (DamageEvent)
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import Pawl.Type.DelayedTrigger (DelayedTrigger)
import qualified Pawl.Type.DelayedTrigger as DelayedTrigger
import Pawl.Type.Game (Game)
import Pawl.Type.GameEvent (GameEvent)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword.Type
import qualified Pawl.Type.LastKnown as LastKnown
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PendingTrigger (PendingTrigger)
import qualified Pawl.Type.PendingTrigger as PendingTrigger
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Regenerability as Regenerability
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Timestamp as Timestamp
import Pawl.Type.TriggerCondition (TriggerCondition)
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggerFrequency as TriggerFrequency
import qualified Pawl.Type.TriggerSource as TriggerSource
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import Pawl.Type.Zone (Zone)
import qualified Pawl.Type.Zone as Zone
import Pawl.Type.ZoneChange (ZoneChange)
import qualified Pawl.Type.ZoneChange as ZoneChange

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
  -- describes why it happened (CR 702.29c).
  GameEvent.Cycled _ -> Nothing
  -- CR 701.20b: "Revealing a card doesn't cause it to leave the zone it's in."
  -- The rule that makes this arm Nothing rather than an oversight -- a reveal is
  -- never a zone change, even when the card is about to make one.
  GameEvent.Revealed _ _ -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing

-- The damage an event describes, if it is any.
damageOf :: GameEvent -> Maybe DamageEvent
damageOf event = case event of
  GameEvent.DamageDealt ev -> Just ev
  GameEvent.Moved _ _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Cycled _ -> Nothing
  GameEvent.Revealed _ _ -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing

-- The caster an event describes, if it is a cast (CR 601.2i).
castOf :: GameEvent -> Maybe PlayerId
castOf event = case event of
  GameEvent.SpellCast pid -> Just pid
  GameEvent.Moved _ _ -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Cycled _ -> Nothing
  GameEvent.Revealed _ _ -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing

-- Who revealed what, if the event is a reveal (CR 701.20a).
revealOf :: GameEvent -> Maybe (PlayerId, PC.ProjectedCharacteristics)
revealOf event = case event of
  GameEvent.Revealed pid snapshot -> Just (pid, snapshot)
  GameEvent.Moved _ _ -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Cycled _ -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing

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

-- changeZoneReturning's body, returning the destination incarnation's id: Just
-- newId on a completed move (CR 400.7 minted a fresh id), Nothing when the id is
-- unknown or the CR 616.1 replacement loop cancelled the move (`resolved ==
-- Nothing`). changeZoneReturning itself is the `seed = Nothing` case below.
changeZoneReturning :: ObjectId -> Zone -> Game (Maybe ObjectId)
changeZoneReturning oid requestedDest = changeZoneAttaching oid requestedDest Nothing

-- changeZoneReturning with an attachment seed. CR 303.4: "An Aura enters the
-- battlefield attached to an object or player" -- attachment is a property of
-- entering, not a step after it. The entry replacement loop (CR 614.1c) and the
-- Moved event both run before this function returns, so an Aura attached
-- afterward would be unattached during both. No card in this pool can observe
-- the difference today; the seed buys the ordering rather than a passing test.
--
-- Pawl.Stack's Aura branch is the only caller that supplies a seed; every other
-- door to the battlefield (this function's own changeZoneReturning included)
-- passes Nothing, so an Aura entering the battlefield by any other route enters
-- unattached and is buried on the next SBA pass by CR 704.5m -- where CR 303.4g
-- says such an Aura should instead just stay in its current zone. That
-- divergence is #188.
changeZoneAttaching :: ObjectId -> Zone -> Maybe ObjectId -> Game (Maybe ObjectId)
changeZoneAttaching oid requestedDest seed = do
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
      -- sound despite Pawl.Replacement's AsCopy arm calling State.modify' (it
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
      -- Pawl.Type.ZoneChange).
      resolved <- Replacement.resolveZoneChange (ZoneChange.MkZoneChange oid oid fromZone requestedDest)
      case resolved of
        -- CR 614.6: nothing survived the loop, so no zone change happens. No
        -- producer today -- no card in the pool cancels a zone change outright --
        -- but Maybe is what "the event does not happen" means on this path.
        Nothing -> pure Nothing
        Just settled -> do
          let dest = ZoneChange.to settled
              mkObj ts = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.attachedTo = seed, Object.timestamp = ts}
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
                    GameState.lastKnown = Map.insert oid (LastKnown.MkLastKnown snapshot lastController) (GameState.lastKnown g1)
                  }
          newId <- placeObject pid mkObj dest
          -- CR 614.1c-d: entry replacements apply to BATTLEFIELD entries and
          -- nowhere else. CR 616.1g: this loop is NESTED inside the zone change,
          -- which is how "an effect may apply to an event contained within another
          -- event" is expressed -- as call nesting, not as a field. A lone entry
          -- has no same-batch siblings (CR 614.12a; see Pawl.Replacement's
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
-- CR 702.12b: an indestructible permanent can't be destroyed, and that gate
-- comes BEFORE the replacement loop, which is CR 614.7: "If a replacement
-- effect would replace an event, but that event never happens, the
-- replacement effect simply doesn't do anything" -- a regeneration shield is
-- neither applied nor consumed. Otherwise the would-be-destroyed event is
-- offered to CR 616.1; if it survives, the permanent is put into its owner's
-- graveyard via changeZone (so Rest in Peace's redirect and a token's CR
-- 704.5d cease-to-exist still compose). Ungated for CR 701.19c "can't be
-- regenerated" (#42).
destroy :: Regenerability.Regenerability -> ObjectId -> Game ()
destroy regenerability oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just _ ->
      if Projection.hasKeyword Keyword.Type.Indestructible oid gs
        then pure ()
        else do
          settled <- Replacement.resolveDestruction regenerability oid
          case settled of
            Nothing -> pure ()
            -- The graveyard move follows the SETTLED object, not the one asked
            -- about, so a rewrite that redirects the destruction is honoured.
            Just target -> changeZone target Zone.Graveyard

-- The single counter-PLACEMENT funnel (CR 122.6: counters as markers on a
-- permanent -- not to be confused with `counter` below, CR 701.6's countering
-- of a spell). Before P5 the PutCounters opcode edited Object.counters in
-- place with no funnel at all, so there was nothing for a replacement to
-- intercept.
--
-- CR 122.6 makes this the right single seam: "Some spells and abilities refer to
-- counters being put on an object. This refers to putting counters on that object
-- while it's on the battlefield and also to an object that's given counters as it
-- enters the battlefield." A zero count after the loop puts nothing on.
putCounters :: ObjectId -> CounterKind.CounterKind -> Natural -> Game ()
putCounters oid kind n = do
  resolved <- Replacement.resolveCounters oid kind n
  case resolved of
    Nothing -> pure ()
    Just (target, settledKind, settledCount) ->
      Monad.when (settledCount > 0)
        . State.modify'
        $ \gs ->
          let bump obj = obj {Object.counters = Map.insertWith (+) settledKind settledCount (Object.counters obj)}
           in gs {GameState.objects = Map.adjust bump target (GameState.objects gs)}

-- The single spell-countering funnel (CR 701.6 -- not to be confused with
-- `putCounters` above, CR 122.6's placement of counter markers). A countered
-- spell is removed from the stack and put into its owner's graveyard (CR
-- 701.6a) via changeZone -- so Rest in Peace's redirect (graveyard->exile) and
-- CR 400.7's new incarnation still compose, exactly as they do for destroy.
-- Gated on CR 113.6g's "can't be countered", which functions on the stack and so
-- is read off the spell's own card (Card.counterability) rather than through the
-- projection -- there is no battlefield projection of a spell. CR 101.2 is what
-- makes the gate the whole story: the "can't" takes precedence, so the countering
-- effect resolves and simply does nothing. It is NOT targeting immunity -- Cancel
-- still legally targeted the spell (CR 113.6g grants no shroud), which is why
-- this gate lives here at the funnel and not in Pawl.Target.
--
-- The gate comes before the zone change, the shape Event.destroy's CR 702.12b
-- indestructible gate already has, and for the same CR 614.7 reason: an event
-- that never happens offers nothing for a replacement to intercept.
--
-- Still emits no distinct "was countered" event for a trigger to read (#43) --
-- no card in the pool has one.
counter :: ObjectId -> Game ()
counter oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just _ -> case fmap Card.counterability (Game.cardOf oid gs) of
      Just Counterability.CantBeCountered -> pure ()
      _ -> changeZone oid Zone.Graveyard

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
-- choice (CR 614.12a; see Pawl.Replacement's applyReplacementsIn for why
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
    GameEvent.Cycled _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
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
          -- case; the fallback mirrors eventTriggers' own `goneEntrant`.
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
    GameEvent.Cycled _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
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
    GameEvent.Cycled _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
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
    GameEvent.Cycled _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
  -- CR 725.2: never matched via a card's bearer -- the monarch's crown-steal is
  -- an inherent ability of no object, so its real match lives in
  -- Pawl.Monarch.inherentMatch, not here.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  -- CR 702.29c: the bearer IS the card that was cycled. The event carries the
  -- incarnation the card became (CR 400.7), which is the object the scan offers
  -- as the bearer -- see cycledCard in eventTriggers below.
  TriggerCondition.SelfCycled -> case event of
    GameEvent.Cycled oid -> oid == bearer
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
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
    GameEvent.Cycled _ -> False
    GameEvent.Revealed _ _ -> False
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
    GameEvent.Cycled _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
  -- CR 603.6c narrowed by CR 700.4 ("the term dies means 'is put into a
  -- graveyard from the battlefield'"): the bearer was put into a graveyard from
  -- the battlefield. Both ends are load-bearing, as they are for
  -- SelfPutIntoGraveyardFromLibrary just above, and for the mirror-image reason:
  -- `from` is what keeps a Doomed Traveler DISCARDED out of a hand silent, and
  -- `to` is what keeps one EXILED off the battlefield silent -- the latter has
  -- left the battlefield (CR 603.6c) without dying (CR 700.4), and the whole
  -- point of naming this condition after the word the card prints is that the
  -- two stay apart (#384).
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
    GameEvent.Cycled _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False

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
-- The parallel for a SOURCELESS inherent ability is Pawl.Monarch.inherentMatch,
-- which binds its own event's creature; there is no shared matcher because that
-- one has no bearer to scope the match to.
--
-- PermanentEnters contributes no binding: the permanent that entered is not
-- named by any slot, so an enters trigger cannot refer back to it (#330).
eventBindings :: TriggerCondition -> GameEvent -> Map.Map SlotName.SlotName Binding
eventBindings cond event = case (cond, event) of
  -- CR 702.70a's "that player": the player the bearer dealt combat damage to.
  (TriggerCondition.SelfDealsCombatDamageToPlayer, GameEvent.DamageDealt ev) ->
    case DamageEvent.target ev of
      Recipient.ToPlayer pid -> Binding.setTriggerPlayer pid Map.empty
      Recipient.ToCreature _ -> Map.empty
      Recipient.ToObject _ -> Map.empty
  _ -> Map.empty

-- Whether a damage recipient is a player (CR 120.1): a total discriminator over
-- Recipient, so the combat-damage-to-player trigger matcher stays non-partial.
isPlayerRecipient :: Recipient.Recipient -> Bool
isPlayerRecipient r = case r of
  Recipient.ToPlayer _ -> True
  Recipient.ToCreature _ -> False
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
-- conditions". That is a per-EVENT question, and the battlefield set this scan
-- walks answers a per-BOUNDARY one: the scan runs once, at CR 117.5, after CR
-- 704.5's state-based actions have already run. A permanent that enters and dies
-- inside one settle -- a creature entering with toughness 0 or less, buried by CR
-- 704.5f -- exists immediately after its own entry event and is gone by the time
-- the scan looks.
--
-- So each entry event contributes ONE extra candidate of its own: the object it
-- names, read from CR 608.2h last known information (GameState.lastKnown), scoped
-- to that event alone. Three things make that exact rather than approximate:
--
--   * NO DOUBLE FIRE, structurally. GameState.lastKnown is written by the zone
--     change that DELETES an id, and CR 400.7 mints a fresh id per move, so no
--     id is ever in both `lastKnown` and `objects`. A newcomer still on the
--     battlefield at the boundary therefore has no lastKnown entry and
--     contributes no extra candidate at all; the Map.union below prefers the
--     live reading regardless.
--   * THE RIGHT SNAPSHOT. `lastKnown` holds the permanent as it was in the zone
--     it LEFT -- the battlefield -- so a creature that entered and died is read
--     with its continuous effects applied, which is what CR 603.10's same
--     sentence demands ("continuous effects that exist at that time are used to
--     determine what the trigger conditions are").
--   * A CANONICAL PLACE IN THE ORDER. Candidates are a Map keyed by ObjectId and
--     traversed ascending, so the extra candidate sorts into the same
--     permanents-inner order every other candidate obeys -- it is not appended.
--
-- CR 603.10a is the OTHER half of that rule, and the exception rather than the
-- normal case: "some zone-change triggers look back in time. These are
-- leaves-the-battlefield abilities ..." So a DEPARTURE event contributes an
-- extra candidate too -- the permanent it took off the battlefield, again read
-- from `lastKnown` -- and for that one the last-known reading is not a repair
-- for a boundary the scan arrives at late, it is what the rule asks for. See
-- `leftBattlefield` below.
--
-- Only the event's own object is recovered, either way. A permanent that was on
-- the battlefield when some OTHER event in the same batch happened and is gone by
-- the boundary still loses that event's trigger (#289).
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
      -- CR 603.10: the newcomer this event put onto the battlefield, IF it no
      -- longer exists. Empty for a newcomer that is still there -- `onBattlefield`
      -- above already carries it, from live state rather than from a snapshot --
      -- and empty for an object that ceased without a zone change ever running
      -- over it (Resolve.cease, Departure.objectsLeaveWith), which files no last
      -- known information.
      --
      -- CR 603.3a's controller comes from that same last known information: the
      -- player who controlled it as it left the battlefield. Within one settle
      -- that IS "the player who controlled its source at the time it triggered",
      -- because the permanent entered and died with nothing in between that could
      -- move control.
      goneEntrant event = case event of
        GameEvent.Moved zc _
          | ZoneChange.to zc == Zone.Battlefield ->
              case Map.lookup (ZoneChange.object zc) (GameState.lastKnown gs) of
                Nothing -> Map.empty
                Just lk ->
                  Map.singleton
                    (ZoneChange.object zc)
                    (LastKnown.controller lk, abilitiesOf (LastKnown.characteristics lk))
        GameEvent.Moved _ _ -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.StepBegan _ _ -> Map.empty
        GameEvent.SpellCast _ -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.Cycled _ -> Map.empty
        GameEvent.Revealed _ _ -> Map.empty
        GameEvent.AttackerDeclared _ -> Map.empty
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
      -- This is the mirror of `goneEntrant` above, and it is possible only
      -- because GameEvent.Moved now names BOTH ids: a departure event's
      -- ZoneChange.object is the new incarnation in the DESTINATION zone (CR
      -- 400.7), which `lastKnown` knows nothing about, while
      -- ZoneChange.departed is exactly the key it files under.
      --
      -- Keyed by that departing id, which by construction no longer exists, so
      -- this source cannot collide with any other: not with `onBattlefield`
      -- (live ids), not with `goneEntrant` (which wants to == Battlefield, and
      -- this wants the opposite), not with `cycledCard` (a cycled card leaves a
      -- HAND), and not with `inGraveyards` (live graveyard ids). One entry per
      -- id means one pass of `forOne`, without leaning on Map.unions' bias.
      --
      -- EVERY battlefield departure contributes, not only the deaths. Which
      -- destinations a condition accepts is the CONDITION's business --
      -- matchesTrigger's SelfDies arm asks for a graveyard, CR 700.4 -- and
      -- keeping that out of the candidate source is what lets CR 603.6c's wider
      -- "leaves the battlefield" (#384) arrive as a matcher arm alone.
      --
      -- The to /= Battlefield guard is CR 603.6c's own wording, "moves from the
      -- battlefield to ANOTHER zone": the battlefield-to-battlefield pseudo-move
      -- recordTokenEntry emits for a new token is not a departure.
      --
      -- The departing id is also what the placed trigger carries as its SOURCE,
      -- and so what Binding.triggerSource binds. CR 603.6c's "an ability that
      -- attempts to do something to the card that left the battlefield checks
      -- for it only in the first zone that it went to" asks for the ARRIVING
      -- incarnation there instead; that split is not made (#387).
      --
      -- Only the departure event's own permanent is recovered. A BYSTANDER that
      -- was on the battlefield when some other event in the same batch happened
      -- and is gone by the boundary still loses that event's trigger (#289) --
      -- though the obstacle that issue names, a departure event not carrying the
      -- departing id, is gone.
      --
      -- Empty for a permanent that ceased without a zone change ever running
      -- over it (Resolve.cease, Departure.objectsLeaveWith), which files no last
      -- known information -- the same hole `goneEntrant` has.
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
        GameEvent.Cycled _ -> Map.empty
        GameEvent.Revealed _ _ -> Map.empty
        GameEvent.AttackerDeclared _ -> Map.empty
      -- CR 702.29c: the card that was just cycled, wherever it landed. The
      -- fourth candidate source, and the first that is neither on the
      -- battlefield nor a permanent that just left it -- which is exactly what
      -- that rule asks for:
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
      cycledCard event = case event of
        GameEvent.Cycled oid -> case Game.lookupObject oid gs of
          Nothing -> Map.empty
          Just obj -> case Game.cardOf oid gs of
            Nothing -> Map.empty
            Just card -> Map.singleton oid (Object.owner obj, Card.triggeredAbilities card)
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
      -- CR 113.6k: the fifth candidate source -- every card in every graveyard
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
      -- The first four sets are disjoint by construction -- goneEntrant and
      -- leftBattlefield both file only an id that no longer exists, and they
      -- disagree about the event's destination (Battlefield versus anything
      -- else), while cycledCard files only one the funnel just minted in a
      -- graveyard -- and the left-bias is belt and braces over that.
      -- `inGraveyards` genuinely OVERLAPS `cycledCard`, and does so on purpose:
      -- CR 702.29c's "these abilities trigger from whatever zone the card winds up
      -- in after it's cycled" is CR 113.6k for a SelfCycled condition, so a card
      -- cycled into a graveyard is honestly a member of both. The result is the
      -- same either way -- cycledCard's entry, which wins, offers the same card's
      -- printed abilities unfiltered, a superset of what inGraveyards offers for
      -- that id -- but the bias is what makes it one entry rather than two.
      candidates event = Map.toAscList (Map.unions [onBattlefield, goneEntrant event, leftBattlefield event, cycledCard event, inGraveyards])
   in concatMap (\event -> concatMap (forOne event) (candidates event)) events

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
        -- projection (Pawl.Condition's spec, unbounded -- unlike the
        -- layer-bounded one Pawl.Projection hands the fold itself).
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
                TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
                TriggerCondition.SelfDies -> False
              pend ab = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) ctrl ab Map.empty
           in fmap pend (filter live (Projection.triggeredAbilitiesOf oid gs))
   in concatMap forOne (Set.toAscList (GameState.battlefield gs))

-- CR 603.7: delayed abilities whose trigger event is among these events. An
-- entry that fires is REMOVED from the store -- CR 603.7b: "only once, the next
-- time its trigger event occurs" -- UNLESS it carries a stated duration, which
-- is the same rule's own exception ("unless it has a stated duration, such as
-- 'this turn'"). One of Pawl.Expiry's sweeps ends those instead; CR 514.2's
-- cleanup, for Full Throttle. The survivors are returned so the caller can store
-- them back. CR 603.7d-f: the controller travels with the entry, so a delayed
-- ability resolves under the player who controlled the spell that created it
-- even if that spell's source object is long gone.
--
-- `fires` matches only against EVENTS (`matchesTrigger`), never against live
-- game state, so a stored entry whose condition is TriggerCondition.StateIs would
-- never match here -- it would never fire, and unless it states a duration for a
-- Pawl.Expiry sweep to end, never leave the store either. Not a live gap:
-- TriggerCondition is a closed type (Pawl.Type.TriggerCondition) and no card in
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
  let fires entry =
        let cond = TriggeredAbility.condition (DelayedTrigger.ability entry)
         in any (matchesTrigger gs (DelayedTrigger.source entry) (DelayedTrigger.controller entry) cond) events
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
-- Pawl.Engine never needs to know how many sources there are.
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
-- an object. The monarch's inherent pair is gathered by Pawl.Monarch and merged
-- into the batch by Engine.placePendingTriggers, after this filter has run. The
-- arm is written to be true anyway rather than to fail: CR 725.2 fixes the full
-- text of both inherent abilities and neither has an intervening "if"
-- (Monarch.oneEffect pins `intervening = Nothing` for exactly that reason), so
-- "the ability triggers" is the right answer for every sourceless ability that
-- exists. A sourceless ability that DID carry one would have no subject object
-- for Condition.holds to read, and is the case that must revisit this.
interveningHolds :: GameState -> PendingTrigger -> Bool
interveningHolds gs pending =
  case (TriggeredAbility.intervening (PendingTrigger.ability pending), PendingTrigger.source pending) of
    (Nothing, _) -> True
    (Just _, TriggerSource.Sourceless) -> True
    (Just cond, TriggerSource.OfObject oid) ->
      Condition.holds
        (Projection.fullView gs)
        (Filter.MkContext (Just (PendingTrigger.controller pending)) (Just oid))
        gs
        oid
        cond
