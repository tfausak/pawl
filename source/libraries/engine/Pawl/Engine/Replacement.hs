-- CR 616.1's loop: the SOLE home of casing on ProposedEvent and
-- ReplacementEffect, a fourth sole-casing home beside Pawl.Engine.Resolve (Effect),
-- Pawl.Engine.Event (TriggerCondition) and Pawl.Engine.Projection
-- (Modification). Pawl.Codec also cases on ReplacementEffect, but only as the
-- JSON data boundary, never to decide game behaviour.
--
-- Read CR 616.1 literally: it is not an ordering prompt, it is a LOOP. Choose one
-- applicable effect from the highest non-empty of five ordered buckets, apply it,
-- then "this process is repeated (taking into account only replacement or
-- prevention effects that would now be applicable) until there are no more left
-- to apply" (616.1f). CR 616.2 adds that an effect can BECOME applicable because
-- another one modified the event. A foldl' over a list computed once is
-- structurally incapable of either.
--
-- This module must NOT import Pawl.Engine.Event: Event raises proposed events through
-- this loop, so the dependency runs one way only. That is also why the entry
-- copy-target legal set lives here rather than in Pawl.Engine.Target (Task 7) -- Target
-- imports Pawl.Engine.Sba, which imports Pawl.Engine.Event.
module Pawl.Engine.Replacement where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import Pawl.Types.CandidateId (CandidateId)
import qualified Pawl.Types.CandidateId as CandidateId
import Pawl.Types.Card (Card)
import Pawl.Types.ControllerRelation (ControllerRelation)
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Expiry as Expiry
import Pawl.Types.ExtraTurn (ExtraTurn)
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PhasePattern as PhasePattern
import Pawl.Types.PhaseSelector (PhaseSelector)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.ProposedEvent (ProposedEvent)
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import Pawl.Types.ReplacementBucket (ReplacementBucket)
import qualified Pawl.Types.ReplacementBucket as ReplacementBucket
import Pawl.Types.ReplacementCandidate (ReplacementCandidate)
import qualified Pawl.Types.ReplacementCandidate as ReplacementCandidate
import Pawl.Types.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.SourceRelation as SourceRelation
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.Uses as Uses
import Pawl.Types.ZoneChange (ZoneChange)
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

-- CR 614: settle a proposed zone change. Nothing means the move does not happen.
-- The typed door Pawl.Engine.Event uses, so Event never cases on a ProposedEvent.
--
-- `asOf` is applyReplacementsIn's: Nothing for a lone move, Just the pre-batch
-- board when this move is one member of a CR 608.2f / 704.3 batch.
resolveZoneChange :: Maybe GameState -> ZoneChange -> Game (Maybe ZoneChange)
resolveZoneChange asOf zc = do
  outcome <- applyReplacementsIn asOf Set.empty (ProposedEvent.WouldChangeZone zc)
  pure (outcome >>= asZoneChange)

asZoneChange :: ProposedEvent -> Maybe ZoneChange
asZoneChange event = case event of
  ProposedEvent.WouldChangeZone zc -> Just zc
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing

-- CR 616.1's loop. `Nothing` means the event DOES NOT HAPPEN -- CR 615.6's
-- prevented damage, CR 701.19a's replaced destruction. A rewrite that cancels an
-- event has already performed its own consequences by the time it returns
-- Nothing.
applyReplacements :: ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacements = applyReplacementsIn Nothing Set.empty

-- CR 608.2f / 704.3: `asOf` is the board a BATCH's candidates are read from --
-- `Just` the state the batch began in, or `Nothing` for the live board. Only the
-- destroy funnel passes `Just` (Event.destroy for its own batch, and
-- Event.destroyInBatch for a batch that is one part of a CR 704.3 pass), together
-- with the graveyard moves both it and Pawl.Engine.Sba's put-into-graveyard batch make
-- through Event.changeZoneInBatch. Everything else is a lone event and wants the
-- live board.
--
-- Two parameters that both name a batch, and they are OPPOSITES: `asOf` widens
-- the candidate set to include effects belonging to permanents the batch is
-- itself removing, while `batch` below NARROWS the copy-target set to exclude
-- permanents entering beside the loop's subject. Deliberately not one parameter:
-- they are about different batches (a simultaneous departure versus a
-- simultaneous entry), they are read by different code (candidate collection
-- versus legalCopyTargets), and no call site ever supplies both.
--
-- CR 608.2f -- "Some spells and abilities include actions taken on multiple
-- players and/or objects. In most cases, each such action is processed
-- simultaneously" -- and CR 704.3 -- state-based actions are performed
-- "simultaneously as a single event" -- make such a batch ONE event, so CR 614.4
-- ("replacement effects must exist before the appropriate event occurs") asks
-- which effects existed before the batch, not before the member being processed.
-- Without this, Rest in Peace animated by Opalescence and swept by Day of
-- Judgment exiles the victims ahead of it in the sweep and buries the ones
-- behind it, an answer that depends on an order CR 608.2f gives nobody the right
-- to decide.
--
-- What it does NOT freeze, and why:
--
--   * The FLOATING store stays live (see `collect`). CR 614.3: a floating
--     replacement "last[s] until [it's] used up", and `consume` spends a
--     one-shot as it applies -- re-reading a frozen store would hand a spent
--     regeneration shield to the next member of the same batch. Freezing it
--     would also buy nothing for this bug: the store is keyed by source id and
--     is not swept when the source leaves the battlefield.
--
--   * The loop still RE-COLLECTS every iteration, so CR 616.1f and CR 616.2 are
--     untouched; only the board those collections read changes.
--
--   * `apply`'s writes and `choose`'s chooser lookup read the LIVE state, since
--     they act on the board as it is now rather than asking what existed.
--
-- A permanent that ENTERED after the batch began therefore contributes nothing,
-- which is the same rule read the other way: CR 614.4 forbids an effect
-- "go[ing] back in time" to change an event that is already under way. No
-- producer today -- nothing enters the battlefield in the middle of a mass
-- destruction or an SBA pass -- so this half is unexercised.
--
-- CR 614.12a: `batch` is the set of ids entering the battlefield AT THE SAME TIME
-- as the object this loop is about -- "If a replacement effect that modifies how
-- a permanent enters the battlefield requires a choice, that choice is made
-- before the permanent enters the battlefield." Clone reads "any creature ON THE
-- BATTLEFIELD"; a sibling entering in the same batch is not there yet at the
-- moment the choice is made, so it is excluded. (CR 614.13a is the wrong cite
-- here: that rule is about an entry effect moving OTHER objects to a different
-- zone, e.g. Sutured Ghoul exiling graveyard cards -- a copy target never
-- changes zones, it just gets copied.) The object THIS loop's WouldEnter event
-- is about is exactly the entering object -- this engine's materialize-first
-- design (see below) already puts it on the battlefield before the loop runs,
-- and legalCopyTargets already excludes it by `self`, so the batch set is
-- never about excluding the loop's own subject.
--
-- `changeZone` still handles one entering object at a time -- its own entry
-- loop completes before the next object's begins, and `batch` is `Set.empty` at
-- that call site. But Event.createTokens (P5) is a second call site, and there
-- `batch` is genuinely non-empty: it materializes every token of a Create
-- BEFORE running any of their entry loops (CR 614.16's doubled count is settled
-- once, up front), so a later token's entry loop finds its siblings already
-- sitting on the battlefield. Without this explicit exclusion they would be
-- visible to legalCopyTargets. Clone's own ruling states the result this batch
-- set exists to reach: "If Clone somehow enters at the same time as another
-- creature, Clone can't become a copy of that creature."
--
-- A simultaneously-entering sibling can reach a later token's entry loop
-- through three channels; only the first needs this explicit exclusion:
--   1. Copy targets -- excluded by `batch`, above.
--   2. Candidate collection -- unreachable regardless of `batch`, though no
--      longer impossible by construction. The entry loop only ever raises a
--      WouldEnter event, and `applies` gates each EntryR candidate by its own
--      Filter; every entry replacement a PERMANENT carries in this pool is CR
--      614.1c's self-only `IsSource` (Clone, Primal Plasma, CR 306.5b's
--      loyalty), which no sibling can satisfy. CR 614.1d's other-objects form
--      exists now -- Gather Specimens -- but it is a FLOATING row rather than a
--      sibling's ability, so it is not what this channel is about. A permanent
--      printing a 614.1d entry replacement (Essence of the Wild) would reach a
--      sibling here, correctly and by the card's own text.
--   3. Projection -- a sibling's STATIC ABILITIES would be visible to a later
--      token's projection (this module's own reads of Projection.controllerOf,
--      Projection.copiableCharacteristics and Projection.isCreatureOf, the last
--      via legalCopyTargets), and nothing in this module excludes them the way
--      `batch` excludes copy targets. CR 614.12's "continuous effects that
--      already exist" does not sanction this: a simultaneously-entering
--      sibling's static abilities do not "already exist" relative to it. NOT
--      IMPLEMENTED AT ALL -- unlike channel 1, below -- and unreached today only
--      because every token card in the pool has empty `staticAbilities`, not
--      fixed (#78).
--
-- Empty for every event class but a nested entry, and empty even for a lone entry
-- (nothing else is entering). Channel 1's exclusion is IMPLEMENTED BUT UNTESTED:
-- no card in the pool puts two copy-choosers onto the battlefield simultaneously
-- (#73).
applyReplacementsIn :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacementsIn asOf batch = loop asOf batch Set.empty

loop :: Maybe GameState -> Set ObjectId -> Set CandidateId -> ProposedEvent -> Game (Maybe ProposedEvent)
loop asOf batch applied event = do
  gs <- State.get
  -- Step 1, from scratch each iteration: collect against the CURRENT state (or,
  -- for a CR 608.2f batch, the state the batch began in), minus CR 614.5's
  -- already-applied set. Re-collecting is what makes CR 616.2 work -- an effect
  -- that only became applicable because of the last application is picked up
  -- here.
  let unused candidate = not (Set.member (ReplacementCandidate.identity candidate) applied)
      fresh = filter unused (applicable asOf gs event)
  case highestBucket fresh of
    -- CR 616.1f: no candidate remains, so the loop ends and the surviving event
    -- is what happens (CR 614.6).
    [] -> pure (Just event)
    bucket -> do
      picked <- choose gs event bucket
      case picked of
        -- Unreachable: highestBucket returns [] for an empty input, so `bucket` is
        -- non-empty and `choose` always picks. Total rather than partial.
        Nothing -> pure (Just event)
        Just candidate -> do
          outcome <- apply batch candidate event
          case outcome of
            Nothing -> pure Nothing
            Just rewritten -> loop asOf batch (Set.insert (ReplacementCandidate.identity candidate) applied) rewritten

-- Every replacement effect instance in the game, in the engine's canonical order.
-- Two segments, concatenated in this order:
--
--   1. PERMANENT abilities (Projection.replacementsAffecting): battlefield
--      permanents ascending by id, each permanent's own effects in printed
--      order. Read from `sources`, which for a CR 608.2f batch is the board the
--      batch began in rather than the live one (see applyReplacementsIn).
--   2. The FLOATING store (GameState.replacements): newest first -- Resolve.hs
--      (the Replace and SkipNextPhase opcodes), Pawl.Engine.Cast (rule 702.34a's
--      flashback exile, armed as the spell goes onto the stack) and
--      `installTurnSkips` below (CR 500.11's turn-scoped skip, armed as the turn
--      it belongs to begins -- the one prepender that is not a resolution) each
--      prepend a new ActiveReplacement onto the front of the list as it is
--      created, so the most recently installed floating replacement is collected
--      before any older one. Always the LIVE store, never a frozen one: CR 614.3
--      spends a one-shot as it is applied, and `consume` writes that back here.
--
-- That concatenated order is what the ChooseReplacement prompt indexes into. The
-- two segments take separate arguments -- rather than one GameState apiece, which
-- would be two interchangeable parameters of the same type -- so the split cannot
-- be got backwards.
collect :: GameState -> [ActiveReplacement.ActiveReplacement] -> [ReplacementCandidate]
collect sources floating =
  let fromPermanent (src, re) =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity = CandidateId.OfPermanent src re,
            ReplacementCandidate.effect = re,
            ReplacementCandidate.source = src,
            -- CR 109.5: "you" is the SOURCE's controller, read live off the
            -- board this segment was gathered from -- a stolen Furnace of Rath's
            -- "you" is whoever holds it now, not whoever printed it.
            ReplacementCandidate.controller = Projection.controllerOf src sources,
            -- CR 614.15: a permanent's replacement ability is a STATIC ability,
            -- and the rule's first sentence puts self-replacement effects
            -- outside that class ("some replacement effects are not continuous
            -- effects"). So this segment is never CR 616.1a's, whatever it
            -- replaces.
            ReplacementCandidate.origin = ReplacementOrigin.Other
          }
      fromFloating active =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity = CandidateId.OfFloating (ActiveReplacement.source active) (ActiveReplacement.timestamp active),
            ReplacementCandidate.effect = ActiveReplacement.effect active,
            ReplacementCandidate.source = ActiveReplacement.source active,
            -- CR 109.5, BAKED at installation rather than derived: this row's
            -- source is a spell CR 608.2n has already put in its owner's
            -- graveyard as a new object with a new id, so `source` names nothing
            -- the board can answer about. See Pawl.Types.ActiveReplacement.
            ReplacementCandidate.controller = Just (ActiveReplacement.controller active),
            -- CR 614.15: "an effect of a resolving spell or ability", which is
            -- what a floating row IS -- so this is the one segment that can
            -- carry a self-replacement, and the row itself says whether it does
            -- (Pawl.Engine.Resolve's Replace arm, from the card).
            ReplacementCandidate.origin = ActiveReplacement.origin active
          }
   in fmap fromPermanent (Projection.replacementsAffecting sources)
        <> fmap fromFloating floating

-- The candidates that apply to this event. `asOf` is Nothing for a lone event
-- and Just the pre-batch board for a CR 608.2f batch (see applyReplacementsIn);
-- `gs` is always the live state.
--
-- `applies` reads the pre-batch board too, not just `collect`. Both ask about
-- the SOURCE -- CR 614.1's "does this instance apply?" reads the source's
-- controller for CR 109.5's "you" (matchesController, matchesZoneOwner, and the
-- TokenR arm) -- and a source the batch has already removed has no controller,
-- so the two have to agree on which board that is. Collecting Leyline of the
-- Void's "an opponent's graveyard" from the frozen board only to have `applies`
-- reject it against the live one would leave the bug exactly where it was.
applicable :: Maybe GameState -> GameState -> ProposedEvent -> [ReplacementCandidate]
applicable asOf gs event =
  let sources = Maybe.fromMaybe gs asOf
   in filter (applies sources event) (collect sources (GameState.replacements gs))

-- CR 614.1: does this instance apply to this proposed event? The arms must agree
-- on the EVENT CLASS -- which the type already rules out for the impossible pairs
-- -- and the pattern must admit the event's subject.
-- CR 701.19c: may this destruction rewrite be applied to this destruction?
--
-- "Effects that say that a permanent can't be regenerated ... cause regeneration
-- shields to not be applied." Asked here, where a candidate is offered the event,
-- so a shield that is refused is also never CONSUMED -- refusing it at
-- application time would spend a shield that the rules say never fired.
--
-- Gates regeneration and nothing else. A destruction replacement that is not a
-- regeneration is untouched by CR 701.19c, which is why this reads the rewrite
-- rather than rejecting the whole DestructionR class.
admits :: Regenerability.Regenerability -> DestructionRewrite.DestructionRewrite -> Bool
admits regenerability rewrite = case rewrite of
  DestructionRewrite.Regenerate -> regenerability == Regenerability.Regenerable

applies :: GameState -> ProposedEvent -> ReplacementCandidate -> Bool
applies gs event candidate =
  let src = ReplacementCandidate.source candidate
   in case (ReplacementCandidate.effect candidate, event) of
        (ReplacementEffect.ZoneChangeR pat _, ProposedEvent.WouldChangeZone zc) ->
          ZoneChange.to zc == ZoneChangePattern.whenDestination pat
            && matchesZoneSubject src (ZoneChangePattern.whichObject pat) (ZoneChange.object zc)
            && matchesZoneOwner gs src (ZoneChangePattern.whoseObject pat) (ZoneChange.object zc)
        -- CR 615.1: a pattern naming no kind admits every damage event. CR
        -- 614.15: one naming TheSource admits only the damage its own source is
        -- dealing.
        (ReplacementEffect.DamageR pat _, ProposedEvent.WouldDealDamage de) ->
          maybe True (== DamageEvent.kind de) (DamagePattern.whichKind pat)
            && matchesDamageSource src (DamagePattern.whichSource pat) de
        -- CR 201.5 / 201.5c / 701.19a: "regenerate THIS creature" names the
        -- ability's own source, so a destruction replacement is self-only.
        -- DestructionR carries no pattern because the only producer in the
        -- card pool is self-regeneration (CR 701.19a).
        (ReplacementEffect.DestructionR rewrite, ProposedEvent.WouldBeDestroyed oid regenerability) ->
          src == oid && admits regenerability rewrite
        (ReplacementEffect.CounterR pat _, ProposedEvent.WouldPutCounters oid kind _) ->
          -- Our own encoding convention, not a rule: `whichKind = Nothing` means
          -- any kind, never no kind.
          maybe True (== kind) (CounterPattern.whichKind pat)
            && matchesController gs src (CounterPattern.whose pat) oid
            && matchesPermanent gs (CounterPattern.onWhat pat) oid
        (ReplacementEffect.TokenR pat _, ProposedEvent.WouldCreateTokens pid _ _) ->
          case TokenPattern.whose pat of
            ControllerRelation.Anyones -> True
            -- CR 109.5: "under YOUR control" -- the tokens' controller is the
            -- effect source's controller.
            ControllerRelation.Yours -> Projection.controllerOf src gs == Just pid
            -- CR 102.2: no producer today -- tokens created under an opponent's
            -- control.
            ControllerRelation.Opponents -> case Projection.controllerOf src gs of
              Just you -> pid /= you
              Nothing -> False
        -- CR 614.1b / 500.11: a skip intercepts a step or phase BEGINNING, and
        -- names exactly which one -- and, for a player-scoped skip, whose.
        --
        -- EQUALITY on the PhaseSelector, so a pattern naming a whole phase
        -- (Stonehorn Dignitary's CombatPhase) matches only the phase question
        -- Engine.runStep raises at that phase's first step, and one naming a step
        -- matches only the step question. Neither can be mistaken for the other,
        -- which is what keeps CR 500.1's "further broken down into steps" out of
        -- this comparison.
        --
        -- The event's PlayerId is the ACTIVE player (Engine.runStep), which is
        -- also whose step this is: every step and phase in a turn belongs to the
        -- player whose turn it is. So Fatigue's "target player skips their next
        -- draw step" is exactly `whosePhase == Just that player` -- it lies
        -- dormant through everyone else's draw steps and takes the named player's
        -- own. Nothing is Eon Hub's symmetric "PLAYERS skip their upkeep steps",
        -- which reads no PlayerId at all.
        --
        -- The SOURCE's controller is not consulted: unlike matchesController's CR
        -- 109.5 "you", the player here was named by the effect, not derived, and
        -- Fatigue's caster is free to name themselves.
        (ReplacementEffect.PhaseR pat, ProposedEvent.WouldBeginPhase selector pid) ->
          PhasePattern.whichPhase pat == selector
            && maybe True (== pid) (PhasePattern.whosePhase pat)
        -- Every row below falls through to False, because an arm ABOVE already
        -- matches every event of that class -- a row below only fires for a
        -- MISMATCHED class (e.g. a DestructionR candidate offered a
        -- WouldChangeZone event), where False is simply the correct answer, not
        -- a stand-in for "not yet implemented".
        -- CR 614.1c-d: which entering permanents this replacement watches, as a
        -- Filter over the entering object (see Pawl.Types.ReplacementEffect).
        -- 614.1c's "as [THIS PERMANENT] enters" is Filter.IsSource, and 614.1d's
        -- "[Objects] enter [the battlefield] . . ." is a characteristic filter.
        (ReplacementEffect.EntryR pat _, ProposedEvent.WouldEnter oid) -> matchesEntering gs candidate pat oid
        (ReplacementEffect.ZoneChangeR _ _, _) -> False
        (ReplacementEffect.EntryR _ _, _) -> False
        (ReplacementEffect.DamageR _ _, _) -> False
        (ReplacementEffect.DestructionR _, _) -> False
        (ReplacementEffect.CounterR _ _, _) -> False
        (ReplacementEffect.TokenR _ _, _) -> False
        (ReplacementEffect.PhaseR _, _) -> False

-- CR 109.5 / 614.1: does `oid` satisfy this pattern's controller relation, read
-- against the controller of the effect's SOURCE? Anyones always does.
matchesController :: GameState -> ObjectId -> ControllerRelation -> ObjectId -> Bool
matchesController gs src rel oid = case rel of
  ControllerRelation.Anyones -> True
  ControllerRelation.Yours -> Projection.controllerOf oid gs == Projection.controllerOf src gs
  -- CR 102.2: no producer today -- a counter or token pattern scoped to an
  -- opponent's permanents. Controller-based, unlike matchesZoneOwner below.
  ControllerRelation.Opponents -> case (Projection.controllerOf oid gs, Projection.controllerOf src gs) of
    (Just theirs, Just yours) -> theirs /= yours
    _ -> False

-- CR 614.1 / 614.15: is this DAMAGE coming from the object the pattern names?
-- AnySource always is; TheSource is the CR 614.15 keying -- the damage this
-- effect's own source is dealing, which for Galvanic Blast's metalcraft clause is
-- the very event its first line proposes.
--
-- The compared id is the DamageEvent's `source`, which Pawl.Engine.Damage.damageEvent
-- sets to the object the damage comes from (CR 113.7a) -- for a resolving spell,
-- the spell on the stack, which is also the object Resolve installs the floating
-- row under. Board-free, so no GameState: an identity test, not a characteristic
-- one, exactly like matchesZoneSubject.
--
-- Not implemented: CR 615.1's shields that name a source by CHARACTERISTIC
-- ("a red source of your choice", Circle of Protection: Red) rather than by
-- identity (#588).
matchesDamageSource :: ObjectId -> SourceRelation.SourceRelation -> DamageEvent.DamageEvent -> Bool
matchesDamageSource src relation de = case relation of
  SourceRelation.AnySource -> True
  SourceRelation.TheSource -> src == DamageEvent.source de

-- CR 614.1: is this ZONE CHANGE about the object the pattern names? AnyObject
-- always is; TheSource is CR 702.34a's "exile THIS card", the self-scoping EntryR
-- (CR 614.1c) and DestructionR (CR 201.5) carry by having no pattern at all.
--
-- The id compared is the event's own subject, which Pawl.Engine.Event proposes as the
-- PRE-move id -- the id the effect's source still has while it sits on the
-- stack. Board-free, so no GameState: this is an identity test, not a
-- characteristic one.
matchesZoneSubject :: ObjectId -> ZoneChangeSubject.ZoneChangeSubject -> ObjectId -> Bool
matchesZoneSubject src subject oid = case subject of
  ZoneChangeSubject.AnyObject -> True
  ZoneChangeSubject.TheSource -> src == oid

-- CR 614.1: does this ZONE CHANGE's object satisfy the pattern's relation?
--
-- The subject is the object's OWNER, not its controller, and that is a rules
-- fact rather than a convenience: CR 400.3 ("if an object would go to any
-- library, graveyard, or hand other than its owner's, it goes to its owner's
-- corresponding zone") and CR 404.1 make the destination zone the owner's, so
-- Leyline of the Void's "an opponent's graveyard" asks who OWNS the card. A
-- creature its controller stole with Act of Treason still dies to its owner's
-- graveyard, which a controller-based test would get backwards.
--
-- Split out of matchesController, which stays controller-based for CR 109.5's
-- "you" on a counter or token pattern. No committed card pairs a ZoneChangeR
-- with anything but Anyones (Rest in Peace), which answers True either way, so
-- the split changed no behavior in the pool when it landed.
matchesZoneOwner :: GameState -> ObjectId -> ControllerRelation -> ObjectId -> Bool
matchesZoneOwner gs src rel oid =
  let ownerOf o = fmap Object.owner (Game.lookupObject o gs)
   in case rel of
        ControllerRelation.Anyones -> True
        ControllerRelation.Yours -> ownerOf oid == Projection.controllerOf src gs
        ControllerRelation.Opponents -> case (ownerOf oid, Projection.controllerOf src gs) of
          (Just owner, Just you) -> owner /= you
          -- An unknown owner or a sourceless effect admits nothing rather than
          -- everything: a redirect with no controller has no opponents.
          _ -> False

-- Which permanents a pattern admits, matched through the lower Pawl.Engine.Filter over
-- the PROJECTED view: creature-ness (CR 205.2b / 300.2 / 613.1d, so an
-- Opalescence'd enchantment counts) and subtype membership (CR 205.3, so Blood
-- Moon is seen) are the projected questions the Filter's atoms already answer. A
-- replacement's pattern frames no player, so the perspective is Nothing.
--
-- Pawl.Engine.Cost narrows its sacrifice candidates through the SAME call, so there is
-- no duplicate matcher to keep in step and no Cost->Replacement cycle to avoid
-- (#111).
matchesPermanent :: GameState -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> Bool
matchesPermanent gs filter_ oid =
  -- No source in scope at this site.
  Filter.matches (Filter.MkContext Nothing Nothing) (Projection.viewOfObject oid gs) filter_

-- CR 614.1c-d: does the entering object satisfy this entry replacement's Filter?
--
-- The same evaluator matchesPermanent uses, over the same projected view, but
-- with a FRAMED Context rather than an empty one, because both of the entry
-- filters in the pool read it: CR 614.1c's `IsSource` asks whether the candidate
-- IS the effect's source (Clone, Primal Plasma, CR 306.5b's loyalty), and CR
-- 614.1d's `ControlledBy Opponent` asks who the candidate's controller is
-- relative to CR 109.5's "you" (Gather Specimens). The perspective is the
-- CANDIDATE's controller, which for a floating row is the baked one -- deriving
-- it from `src` here would answer Nothing for every row whose spell has
-- resolved, and a Nothing perspective makes ControlledBy vacuously False.
--
-- CR 614.12 is why the view is the LIVE projection of the materialized object
-- rather than of the card it came from: "check the characteristics of the
-- permanent AS IT WOULD EXIST ON THE BATTLEFIELD, taking into account
-- replacement effects that have already modified how it enters" -- so a
-- previous iteration's rewrite is visible to this one, including CR 616.1b's own
-- change to who would control it.
matchesEntering :: GameState -> ReplacementCandidate -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> Bool
matchesEntering gs candidate filter_ oid =
  let context = Filter.MkContext (ReplacementCandidate.controller candidate) (Just (ReplacementCandidate.source candidate))
   in Filter.matches context (Projection.viewOfObject oid gs) filter_

-- CR 614.1a: apply a scaling to a number. "That many plus one" and "twice that
-- many" are the same operation with different data, and so is Furnace of Rath's
-- "double that damage" -- which is why CounterR, TokenR (CR 614.16's two shapes)
-- and DamageR all rewrite through this one function.
scale :: Scaling.Scaling -> Natural -> Natural
scale s n = case s of
  Scaling.Multiply m -> n * m
  Scaling.AddMore m -> n + m

-- CR 616.1a-e: take the HIGHEST non-empty bucket. Ord on ReplacementBucket is
-- ascending in the CR's own order, so that is the minimum present; the fold seeds
-- from Other (the largest) so it needs no partial `minimum`.
highestBucket :: [ReplacementCandidate] -> [ReplacementCandidate]
highestBucket candidates =
  let bucketed = fmap (\c -> (bucketOf c, c)) candidates
      best = List.foldl' min ReplacementBucket.Other (fmap fst bucketed)
   in fmap snd (filter (\entry -> fst entry == best) bucketed)

-- CR 616.1a-e: which bucket a candidate falls in.
--
-- CR 616.1a is asked FIRST and is answered by the candidate's ORIGIN, not by its
-- payload: "if any of the replacement and/or prevention effects are
-- self-replacement effects (see rule 614.15), one of them must be chosen." CR
-- 614.15 defines that class by which ability created the effect, so no
-- ReplacementEffect value could answer it -- see Pawl.Types.ReplacementOrigin.
-- Every remaining step reads the payload's SHAPE, never its identity.
bucketOf :: ReplacementCandidate -> ReplacementBucket
bucketOf candidate = case ReplacementCandidate.origin candidate of
  ReplacementOrigin.SelfReplacement -> ReplacementBucket.SelfReplacement
  ReplacementOrigin.Other -> bucketOfEffect (ReplacementCandidate.effect candidate)

-- CR 616.1b-e: which bucket an effect that is NOT CR 614.15's falls in.
bucketOfEffect :: ReplacementEffect -> ReplacementBucket
bucketOfEffect re = case re of
  ReplacementEffect.ZoneChangeR _ _ -> ReplacementBucket.Other
  -- CR 616.1c: "an effect that would cause an object to become a copy of another
  -- object as it enters" is its own, HIGHER bucket. This is NOT what makes the
  -- centerpiece work: on the entering Clone's first iteration, AsCopy is the
  -- ONLY applicable candidate (the copied ChoiceOf does not exist yet, because
  -- nothing has stamped the snapshot), so it is picked because it is the sole
  -- candidate, not because of its bucket -- mapping this arm to Other instead
  -- does not change any of the four centerpiece scenarios' outcomes. What
  -- actually makes the centerpiece work is CR 616.1f's re-collection each
  -- iteration (so the loop finds the ChoiceOf the object did not have before)
  -- together with CR 614.5's identity being keyed on the effect VALUE, which
  -- keeps the newly-acquired ChoiceOf distinct from the already-applied
  -- AsCopy. The split this arm encodes only becomes observable where an AsCopy
  -- races another entry replacement OF NO HIGHER BUCKET in the SAME iteration,
  -- which no card in the pool produces, so THIS bucket's ordering is unexercised
  -- by any test (#73). Gather Specimens racing an entering Clone is a real
  -- same-iteration race, but it does not exercise this arm: CR 616.1b's bucket
  -- outranks this one, so mapping this arm to Other would not change its answer.
  -- CR 616.1a's bucket and CR 616.1b's are both exercised -- Galvanic Blast
  -- racing Furnace of Rath, and that Gather Specimens board.
  ReplacementEffect.EntryR _ EntryRewrite.AsCopy -> ReplacementBucket.CopyOnEntry
  ReplacementEffect.EntryR _ (EntryRewrite.ChoiceOf _) -> ReplacementBucket.Other
  -- CR 616.1a-d name self-replacement, entering under a control effect,
  -- entering as a copy and entering with the back face up; entering with
  -- counters is none of those, so CR 616.1e is what applies.
  ReplacementEffect.EntryR _ (EntryRewrite.WithCounters _ _) -> ReplacementBucket.Other
  -- CR 616.1b: "if any of the replacement and/or prevention effects would modify
  -- under whose control an object would enter the battlefield, one of them must
  -- be chosen." One step ABOVE the copy bucket, and Gather Specimens racing an
  -- entering Clone is the board where the two orders disagree: taking the
  -- control rewrite first hands Clone's own CR 109.5 copy choice to the NEW
  -- controller, and taking the copy first hands it to the old one. ReplacementSpec's
  -- "CR 616.1b before CR 616.1c: the NEW controller chooses the copy" is the
  -- test that pins it, and unlike the copy bucket below this one is exercised.
  ReplacementEffect.EntryR _ EntryRewrite.UnderSourceControl -> ReplacementBucket.ControlOnEntry
  ReplacementEffect.DamageR _ _ -> ReplacementBucket.Other
  ReplacementEffect.DestructionR _ -> ReplacementBucket.Other
  ReplacementEffect.CounterR _ _ -> ReplacementBucket.Other
  ReplacementEffect.TokenR _ _ -> ReplacementBucket.Other
  -- CR 616.1a-d are all about entries and copies (self-replacement, entering
  -- control, entering as a copy, entering back face up); a skip is none of
  -- those, so it falls to CR 616.1e's "any of the applicable ... may be chosen".
  ReplacementEffect.PhaseR _ -> ReplacementBucket.Other

-- CR 616.1: "the affected object's controller (or its owner if it has no
-- controller) or the affected player chooses one to apply."
--
-- TWO ELISIONS, both of them choices the rules make indistinguishable:
--
--   * ONE candidate -- there is nothing to choose, and where the rules leave
--     nothing to ask, don't prompt.
--   * several candidates EQUAL AS VALUES -- each still gets its own CR 614.5
--     opportunity, so every order produces the same board. Only the PROMPT is
--     elided, never an application.
--
-- The second elision's soundness rests on a premise `apply` must keep true:
-- applying a candidate is independent of its `source` field. Candidates equal
-- in `effect` can still differ in `source` (matchesController and the
-- ChooseReplacement payload both read it), so "equal as values" only implies
-- "every order yields the same board" as long as no `apply` arm branches on, or
-- mutates state keyed by, which source is applying.
--
-- Not implemented: that premise is BROKEN, by the EntryRewrite.UnderSourceControl
-- arm -- its whole effect is the candidate's own `controller`, so two Gather
-- Specimens are value-equal here and apply to different boards. Unreachable at
-- two seats (a permanent has one controller, so at most one such row can see it
-- as an opponent's) and unfixed above two (#593).
--
-- Not implemented: the comparison reads `effect` alone, so two floating rows
-- alike in it but differing in `expiry` or `uses` are treated as
-- indistinguishable even though `consume` spends only the one that applied (#490).
--
-- `origin` is NOT such a hole. highestBucket has already partitioned by bucket
-- before this runs, and CR 616.1a's bucket is exactly "origin is
-- SelfReplacement", so every candidate reaching this comparison shares one
-- origin and comparing `effect` alone can never conflate two that differ in it.
--
-- Anything else prompts. The pure fold has silently picked list order since M3f;
-- that is the second-invariant violation this phase exists to retire, and unlike
-- an elision it carried no expiry because nothing detected it.
choose :: GameState -> ProposedEvent -> [ReplacementCandidate] -> Game (Maybe ReplacementCandidate)
choose gs event candidates = case candidates of
  [] -> pure Nothing
  first : rest ->
    if all (\c -> ReplacementCandidate.effect c == ReplacementCandidate.effect first) rest
      then pure (Just first)
      else case chooserOf gs event of
        -- No chooser: the affected object is gone. Apply the canonical first
        -- rather than prompt nobody -- and in particular make no choice on behalf
        -- of a player who is not there to make it.
        Nothing -> pure (Just first)
        Just pid -> do
          let decider = Decide.deciderFor pid gs
          answer <- Trans.lift (Program.prompt (Prompt.ChooseReplacement decider pid (fmap ReplacementCandidate.source candidates)))
          -- Reject-not-repair, as payment and Engine.permute already do: an
          -- out-of-range index leaves the canonical first standing rather than
          -- dropping the event or crashing.
          pure (Just (at candidates answer first))

-- Total index into a list, with a fallback.
at :: [a] -> Natural -> a -> a
at xs i fallback = case List.genericDrop i xs of
  h : _ -> h
  [] -> fallback

-- CR 616.1 / 108.4: who decides. Projection.controllerOf already falls back to
-- the owner (CR 108.4), so "or its owner if it has no controller" is free.
--
-- CR 616.1's APNAP clause -- "If two or more players have to make these choices at
-- the same time, choices are made in APNAP order (see rule 101.4)" -- has no
-- producer: one proposed event has exactly one affected object and therefore one
-- chooser, and the damage batch runs each event's loop independently (#71).
chooserOf :: GameState -> ProposedEvent -> Maybe PlayerId
chooserOf gs event = case event of
  ProposedEvent.WouldChangeZone zc -> Projection.controllerOf (ZoneChange.object zc) gs
  -- CR 616.1's "affected object's controller", read LIVE off the materialized
  -- permanent -- which for an entry is the player it WOULD enter under, and
  -- which a CR 616.1b rewrite may already have changed on an earlier iteration.
  -- So an opponent's entering creature is that opponent's to choose about until
  -- Gather Specimens takes it, and the Gather Specimens controller's afterwards.
  ProposedEvent.WouldEnter oid -> Projection.controllerOf oid gs
  ProposedEvent.WouldDealDamage de -> case DamageEvent.target de of
    Recipient.ToPlayer pid -> Just pid
    Recipient.ToCreature oid -> Projection.controllerOf oid gs
    Recipient.ToPlaneswalker oid -> Projection.controllerOf oid gs
    Recipient.ToObject oid -> Projection.controllerOf oid gs
  ProposedEvent.WouldBeDestroyed oid _ -> Projection.controllerOf oid gs
  ProposedEvent.WouldPutCounters oid _ _ -> Projection.controllerOf oid gs
  ProposedEvent.WouldCreateTokens pid _ _ -> Just pid
  -- CR 616.1's "affected player": a step or phase beginning affects no object,
  -- so the player whose turn it is chooses among applicable skips.
  ProposedEvent.WouldBeginPhase _ pid -> Just pid

-- CR 614.6: apply one chosen effect. Nothing means the event does not happen.
--
-- One arm per ReplacementEffect constructor, same shape as `applies` -- so a new
-- constructor breaks the build HERE too, not just there. A wildcard fallback
-- would defeat that: an author who teaches `applies` a new arm but forgets this
-- one gets a silent no-op replacement (the candidate is consumed into the
-- applied-set, the event passes through unchanged, nothing warns). Every arm
-- below either rewrites its paired event or, for a pair `applies` already
-- excludes, falls through to `pure (Just event)` -- unreachable in practice
-- (nothing reaches `apply` that `applies` rejected) but present so the match
-- stays total per constructor, not merely total by wildcard.
--
-- The same discipline applies one level down, to each arm's inner SUM type --
-- DamageR's DamageRewrite below, DestructionR's DestructionRewrite, EntryR's
-- EntryRewrite (Task 7), and CounterR's and TokenR's shared Scaling -- never to
-- the pattern RECORDS (CounterPattern, TokenPattern, DamagePattern),
-- which are read for their fields rather than cased and so have no constructors
-- to be exhaustive over. An arm must case on the inner sum, not bind it with
-- `_`: binding it with `_` is exhaustive UNCONDITIONALLY -- that is exactly why
-- it raises no build failure and no warning when a new constructor is added,
-- silently treating a real rewrite as a no-op from that day on.
--
-- CounterR's and TokenR's arms below do not case on Scaling themselves -- they
-- delegate it whole to the `scale` helper, which is where the exhaustive case
-- lives. That still satisfies the guarantee above (a new Scaling constructor
-- breaks `scale`'s build, which breaks both arms' build transitively); casing
-- it again inline here would just duplicate `scale`'s match, not strengthen it.
apply :: Set ObjectId -> ReplacementCandidate -> ProposedEvent -> Game (Maybe ProposedEvent)
apply batch candidate event =
  case (ReplacementCandidate.effect candidate, event) of
    (ReplacementEffect.ZoneChangeR _ toDest, ProposedEvent.WouldChangeZone zc) -> do
      consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldChangeZone zc {ZoneChange.to = toDest}))
    -- Unreachable: `applies` admits ZoneChangeR only against WouldChangeZone.
    (ReplacementEffect.ZoneChangeR _ _, _) -> pure (Just event)
    -- CR 707.5 / 614.1c / 614.12a: the entering object's controller chooses a
    -- permanent to copy, and its copiable characteristics are stamped as this
    -- object's copy snapshot. Writing to the COPIABLE layer (CR 613.1a's layer-1
    -- base) is what makes CR 707.2 fall out for free: a later Clone of this object
    -- copies the stamped values with no further machinery.
    --
    -- Clone's "may" is real: Nothing leaves the object as its printed self (a
    -- 0/0 Shapeshifter, which CR 704.5f then buries).
    --
    -- The class match is on the OUTER tuple (EntryR rewrite, WouldEnter oid), same
    -- shape as DamageR/DestructionR below, so the INNER `case rewrite of` is what
    -- carries the exhaustiveness obligation -- not a wildcard-bound `_` on the
    -- outer pattern, which would let a third EntryRewrite constructor fall through
    -- silently whenever it happened to pair with WouldEnter.
    (ReplacementEffect.EntryR _ rewrite, ProposedEvent.WouldEnter oid) -> case rewrite of
      EntryRewrite.AsCopy -> do
        consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable: the object is materialized on the battlefield before this
          -- loop runs, so controllerOf falls back to its owner. Defensive: make no
          -- unprompted copy choice.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            let legal = legalCopyTargets batch oid gs
            answer <- Trans.lift (Program.prompt (Prompt.ChooseCopyTarget decider controller oid legal))
            -- FILTERED, NOT TRUSTED (#222). legalCopyTargets is the ONLY thing
            -- enforcing CR 614.12a's same-batch exclusion -- Clone's own ruling,
            -- "If Clone somehow enters at the same time as another creature, Clone
            -- can't become a copy of that creature" -- so honouring an unoffered
            -- answer would let a Clone copy a sibling token entering beside it.
            let chosen = case answer of
                  Just src | List.elem src legal -> Just src
                  _ -> Nothing
            case chosen of
              Nothing -> pure (Just event)
              Just src2 -> do
                State.modify' $ \g ->
                  let stamp o = o {Object.bindings = Binding.setCopy (Projection.copiableCharacteristics src2 g) (Object.bindings o)}
                   in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
                pure (Just event)
      -- CR 614.1c / 208.2b: Primal Plasma's own choice (which of its three
      -- printed power/toughness-and-keywords options to become -- e.g. a 3/3, a
      -- 2/2 flier, or a 1/6 with defender -- never a creature type). Written
      -- into the COPIABLE snapshot (applyEntryOption), which is what makes CR
      -- 616.2 fall out for free: a Clone that copies Primal Plasma's copiable
      -- values also copies this ability (CR 707.5), and the loop's next
      -- iteration re-collects and finds it newly applicable.
      EntryRewrite.ChoiceOf options -> do
        gs <- State.get
        case options of
          -- Malformed card data: an as-enters choice with nothing to choose
          -- from. No-op rather than a partial function, but still consumed --
          -- a floating one-shot must not survive to apply again, matching the
          -- `first : rest` arm below and AsCopy above.
          [] -> do
            consume (ReplacementCandidate.identity candidate)
            pure (Just event)
          first : rest -> do
            picked <-
              if null rest
                then -- One option is not a choice; where the rules leave
                -- nothing to ask, don't prompt.
                  pure first
                else case Projection.controllerOf oid gs of
                  -- Unreachable: the object is materialized on the
                  -- battlefield before this loop runs, so controllerOf falls
                  -- back to its owner. Defensive: make no unprompted choice.
                  Nothing -> pure first
                  Just controller -> do
                    let decider = Decide.deciderFor controller gs
                    answer <- Trans.lift (Program.prompt (Prompt.ChooseEntryOption decider controller oid options))
                    pure (at options answer first)
            consume (ReplacementCandidate.identity candidate)
            State.modify' (applyEntryOption oid picked)
            pure (Just event)
      -- CR 306.5b via CR 614.1c: "[This permanent] enters with N counters."
      -- Through Event.putCounters, the CR 122.6 funnel, and NOT a direct write to
      -- Object.counters: CR 614.16's second sentence makes a counter-scaling
      -- replacement apply "even if the original event being modified wasn't
      -- itself an effect", so Doubling Season has to see these. That nested CR
      -- 616.1 loop is the reason the counters are placed here rather than folded
      -- into the entry event's own payload.
      --
      -- Consumed like every other arm, so CR 614.5's one-opportunity rule keeps
      -- the loop's next iteration from placing the counters a second time.
      EntryRewrite.WithCounters kind n -> do
        consume (ReplacementCandidate.identity candidate)
        putCounters oid kind n
        pure (Just event)
      -- CR 616.1b / 110.2: Gather Specimens' "it enters under your control
      -- instead". The entering object's CR 110.2 DEFAULT controller becomes CR
      -- 109.5's "you" -- the candidate's controller, baked when the row was
      -- installed -- and that is a permanent change to the permanent, not a
      -- duration-scoped one: the card's "this turn" bounds how long the
      -- REPLACEMENT is around to catch entries, never how long the creature
      -- stays yours.
      --
      -- Written to the object rather than to the surviving ProposedEvent, which
      -- is why ProposedEvent.WouldEnter still carries only an ObjectId. This
      -- engine materializes the entering permanent BEFORE running the entry loop
      -- (see runEntry, and CR 614.12's "as it would exist on the battlefield"),
      -- so the would-be controller is exactly Projection.controllerOf on the
      -- live board and the object IS the event's payload. Keeping it there is
      -- also what makes CR 616.2 fall out: the loop's next iteration re-collects
      -- and re-matches against a board where the control has already changed,
      -- which a value parked on the event would not show it. All three arms
      -- above land on the object for the same reason -- AsCopy and ChoiceOf in
      -- the copiable snapshot, WithCounters through the CR 122.6 funnel.
      --
      -- No prompt, and none is owed: CR 616.1b's rewrite has no choice in it,
      -- and the choice the rule DOES describe -- which of several
      -- control-modifying effects to apply -- is `choose`'s, one level up.
      --
      -- Not implemented: `you` is not checked against CR 800.4a, so this can
      -- hand a permanent to a player who has left the game (#592).
      EntryRewrite.UnderSourceControl -> do
        consume (ReplacementCandidate.identity candidate)
        case ReplacementCandidate.controller candidate of
          -- CR 109.5 has no answer: a permanent-sourced instance whose source
          -- has left the board. Defensive -- no producer, since the one card
          -- with this rewrite is a floating row carrying a baked controller --
          -- and it leaves the entry alone rather than guessing at a player.
          Nothing -> pure (Just event)
          Just you -> do
            State.modify' $ \gs ->
              let claim obj = obj {Object.enteredUnder = Just you}
               in gs {GameState.objects = Map.adjust claim oid (GameState.objects gs)}
            pure (Just event)
    -- Unreachable: `applies` admits EntryR only against WouldEnter.
    (ReplacementEffect.EntryR _ _, _) -> pure (Just event)
    (ReplacementEffect.DamageR _ rewrite, ProposedEvent.WouldDealDamage de) -> case rewrite of
      -- CR 615.6: a prevented event never happens -- it is not marked, not
      -- drained, and never recorded, so no deathtouch bit exists for the CR
      -- 704.5h SBA to read.
      DamageRewrite.PreventAll -> do
        consume (ReplacementCandidate.identity candidate)
        pure Nothing
      -- CR 614.1a's "instead" with a flat amount: Galvanic Blast's "deals 4
      -- damage instead". Only the AMOUNT is rewritten, and that is the rule
      -- rather than economy -- Furnace of Rath's own ruling ("the multiplied
      -- damage counts in all ways as if it came from the original source")
      -- states the general shape: a replaced damage event keeps its source, its
      -- recipient and every deal-time rider it was proposed with.
      DamageRewrite.SetAmount n -> do
        consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = n}))
      -- CR 614.1a: Furnace of Rath's "it deals double that damage ... instead".
      -- Through the same `scale` the counter and token rewrites use, so a
      -- doubling means one thing across every event class.
      DamageRewrite.Scale scaling -> do
        consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = scale scaling (DamageEvent.amount de)}))
    -- Unreachable: `applies` admits DamageR only against WouldDealDamage.
    (ReplacementEffect.DamageR _ _, _) -> pure (Just event)
    -- CR 701.19a: "The next time [permanent] would be destroyed this turn,
    -- instead remove all damage marked on it and its controller taps it. If
    -- it's an attacking or blocking creature, remove it from combat." The
    -- DESTRUCTION does not happen -- so nothing downstream of it (a
    -- put-into-graveyard, and therefore Rest in Peace's redirect) ever runs.
    -- That nesting was hardcoded in Event.destroy before P5; it is structural
    -- now.
    (ReplacementEffect.DestructionR rewrite, ProposedEvent.WouldBeDestroyed oid _) -> case rewrite of
      DestructionRewrite.Regenerate -> do
        consume (ReplacementCandidate.identity candidate)
        State.modify' $ \gs ->
          let healTap obj = obj {Object.damage = 0, Object.tapped = TapState.Tapped}
              healed = gs {GameState.objects = Map.adjust healTap oid (GameState.objects gs)}
           in Game.removeFromCombat oid healed
        pure Nothing
    -- Unreachable: `applies` admits DestructionR only against WouldBeDestroyed.
    (ReplacementEffect.DestructionR _, _) -> pure (Just event)
    -- CR 122.6/614.16: Hardened Scales/Doubling Season scale a counter placement.
    (ReplacementEffect.CounterR _ scaling, ProposedEvent.WouldPutCounters oid kind n) -> do
      consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldPutCounters oid kind (scale scaling n)))
    -- Unreachable: `applies` admits CounterR only against WouldPutCounters.
    (ReplacementEffect.CounterR _ _, _) -> pure (Just event)
    -- CR 614.16: Doubling Season scales token creation ("if an effect would
    -- create one or more tokens ... it creates twice that many").
    (ReplacementEffect.TokenR _ scaling, ProposedEvent.WouldCreateTokens pid card n) -> do
      consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldCreateTokens pid card (scale scaling n)))
    -- Unreachable: `applies` admits TokenR only against WouldCreateTokens.
    (ReplacementEffect.TokenR _ _, _) -> pure (Just event)
    -- CR 614.1b / 614.10: "'skip [something]' is the same as 'instead of doing
    -- [something], do nothing'" -- so the step or phase simply does not begin.
    -- Nothing is done first, unlike DamageRewrite.PreventAll's sibling arm: a
    -- skip has no consequence of its own to perform before it cancels.
    --
    -- The obligation the doc above places on every arm -- case on the inner sum
    -- rather than bind it with `_` -- has nothing to bind here: PhaseR carries a
    -- pattern and no rewrite, because CR 614.1b leaves a skip only one possible
    -- outcome (see Pawl.Types.ReplacementEffect). The day a PhaseRewrite exists,
    -- this arm owes it a case.
    --
    -- CR 614.10a: "if two effects each cause a player to skip their next
    -- occurrence, that player must skip the next two; one effect will be
    -- satisfied in skipping the first occurrence, while the other will remain
    -- until another occurrence can be skipped." Both halves fall out of the
    -- floating store's SHAPE rather than out of care taken here. Two Fatigues
    -- prepend two ActiveReplacements, and a list of instances with distinct
    -- timestamps cannot coalesce the way a Set of patterns or a Boolean flag
    -- would; `consume` below deletes by (source, timestamp), so it spends
    -- exactly the one that applied; and returning Nothing ENDS the CR 616.1
    -- loop, so no second skip can be spent on the same step. One occurrence
    -- skipped, one instance gone, the rest waiting.
    --
    -- "One occurrence" is the occurrence the PATTERN named, which for Stonehorn
    -- Dignitary's PhaseSelector.CombatPhase is a whole combat phase rather than a
    -- step of one. Nothing here has to know that: Engine.runStep raises the phase
    -- question exactly once per phase, so a whole-phase skip gets exactly one
    -- chance to apply and spends itself taking it -- the same arithmetic two
    -- Fatigues do on two draw steps.
    --
    -- Eon Hub's PhaseR reaches the same arm and consumes nothing: it is a
    -- permanent's static ability, so its CandidateId is OfPermanent and `consume`
    -- is a no-op for it. Idempotent and permanent, which is what "players skip
    -- their upkeep steps" means, and it is the store -- not this arm -- that
    -- tells the two apart.
    (ReplacementEffect.PhaseR _, ProposedEvent.WouldBeginPhase _ _) -> do
      consume (ReplacementCandidate.identity candidate)
      pure Nothing
    -- Unreachable: `applies` admits PhaseR only against WouldBeginPhase.
    (ReplacementEffect.PhaseR _, _) -> pure (Just event)

-- CR 208.2b / 707.2: stamp a chosen entry shape into the object's copiable
-- snapshot. Power and toughness are SET; keywords are UNIONED into whatever is
-- already there.
--
-- The union is pinned by Primal Plasma's own Gatherer ruling and is the detail
-- worth stating twice: "a 1/6 creature with flying and defender" is only
-- reachable if the choice ADDS defender to a snapshot that already carries flying
-- from the copy.
applyEntryOption :: ObjectId -> EntryOption.EntryOption -> GameState -> GameState
applyEntryOption oid option gs =
  let base = Projection.copiableCharacteristics oid gs
      stamped =
        base
          { PC.power = Just (EntryOption.power option),
            PC.toughness = Just (EntryOption.toughness option),
            -- Defensive, not load-bearing: a CR 208.2b card has no
            -- characteristic-defining ability by construction (CR 208.2a and
            -- 208.2b are alternatives), so this field is already Nothing on
            -- any object that reaches `applyEntryOption` (it exists only
            -- because the object has a ChoiceOf). Setting it again costs
            -- nothing and keeps this function correct if that invariant ever
            -- changes.
            PC.characteristicPT = Nothing,
            -- The option's keywords are one card's printed text (CR 208.2b), so
            -- each arrives once; unionWith (+) adds them to whatever the copy
            -- snapshot already carries rather than absorbing a repeat.
            PC.keywords = Map.unionWith (+) (PC.keywords base) (Map.fromSet (const 1) (EntryOption.keywords option))
          }
      write o = o {Object.bindings = Binding.setCopy stamped (Object.bindings o)}
   in gs {GameState.objects = Map.adjust write oid (GameState.objects gs)}

-- CR 707.5 / 614.12a: the permanents an entering copy may choose. Battlefield
-- creatures other than itself, minus anything entering in the same batch (see
-- the CR 614.12a note on applyReplacementsIn for why the batch set, not
-- 614.13a, is what excludes them).
legalCopyTargets :: Set ObjectId -> ObjectId -> GameState -> [ObjectId]
legalCopyTargets batch self gs =
  let eligible oid = oid /= self && not (Set.member oid batch) && Projection.isCreatureOf oid gs
   in filter eligible (Set.toAscList (GameState.battlefield gs))

-- CR 614.1c / 614.12: run the entry loop for an object that has just been
-- materialized on the battlefield.
--
-- The object is in GameState.objects and its zone index BEFORE this runs, because
-- CR 614.12 demands it: "check the characteristics of the permanent AS IT WOULD
-- EXIST ON THE BATTLEFIELD, taking into account replacement effects that have
-- already modified how it enters ... and continuous effects that already exist and
-- would apply to the permanent." That is a projection of the object in the state
-- where it has entered, so the cheapest correct implementation is to put it there
-- and project it normally.
--
-- Nothing observes the interim object: this finishes before the Moved event is
-- recorded, so no trigger scan and no state-based action can see it. That is
-- strictly stronger than P2's drain, whose observable-equivalence argument this
-- discharges.
-- `Monad.void` discards the `Nothing` case `apply`'s doc warns means "the event
-- does not happen." Safe here: every EntryR arm (AsCopy, ChoiceOf, WithCounters,
-- UnderSourceControl) always returns `Just`; only DamageR/DestructionR ever
-- return `Nothing`, and neither pairs with WouldEnter, the only event this loop
-- ever proposes.
--
-- Always the LIVE board (`Nothing`), even when the zone change containing this
-- entry belongs to a CR 608.2f batch. Two reasons, both CR: the entering object
-- is not on the pre-batch board at all, and CR 614.12 asks this loop to "check
-- the characteristics of the permanent AS IT WOULD EXIST ON THE BATTLEFIELD" --
-- a question about now, not about when the containing event began. CR 616.1g is
-- the rule that recognizes an entry like this as an event CONTAINED within
-- another rather than a second member of the batch -- though it speaks only to
-- the ORDER the two events' effects are chosen in ("the second effect can't be
-- chosen until after the first effect has been chosen"), not to which board each
-- collects from. That a contained event keeps its own footing is this engine's
-- reading, resting on CR 614.12 above; no rule states it outright.
runEntry :: Set ObjectId -> ObjectId -> Game ()
runEntry batch oid = Monad.void (applyReplacementsIn Nothing batch (ProposedEvent.WouldEnter oid))

-- CR 614.3: a floating replacement whose `uses` is Once is spent by being
-- applied. A permanent's STATIC replacement ability has no use count at all --
-- it is re-derived from the battlefield every iteration -- so only the floating
-- store is touched here.
consume :: CandidateId -> Game ()
consume identity_ = case identity_ of
  CandidateId.OfPermanent _ _ -> pure ()
  CandidateId.OfFloating src ts ->
    State.modify' $ \gs ->
      let spent active =
            ActiveReplacement.source active == src
              && ActiveReplacement.timestamp active == ts
              && ActiveReplacement.uses active == Uses.Once
       in gs {GameState.replacements = filter (not . spent) (GameState.replacements gs)}

-- CR 615: settle one proposed damage event. Nothing means it does not happen.
resolveDamage :: DamageEvent.DamageEvent -> Game (Maybe DamageEvent.DamageEvent)
resolveDamage de = do
  outcome <- applyReplacements (ProposedEvent.WouldDealDamage de)
  pure (outcome >>= asDamageEvent)

asDamageEvent :: ProposedEvent -> Maybe DamageEvent.DamageEvent
asDamageEvent event = case event of
  ProposedEvent.WouldDealDamage de -> Just de
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing

-- CR 701.8 / 614.8: settle a proposed destruction. `Just` is the object actually
-- destroyed -- which need not be the one asked about, since a rewrite may
-- redirect it; `Nothing` means a replacement took the event (regeneration), and
-- that rewrite has already done its own work.
--
-- `asOf` is applyReplacementsIn's, and the destroy funnel always supplies it: a
-- destruction is never lone, since CR 608.2f gives even a single Doom Blade the
-- one-element batch -- and when that batch is itself part of a CR 704.3 pass, the
-- board is the pass's rather than the batch's (Event.destroyInBatch).
resolveDestruction :: Maybe GameState -> Regenerability.Regenerability -> ObjectId -> Game (Maybe ObjectId)
resolveDestruction asOf regenerability oid = do
  outcome <- applyReplacementsIn asOf Set.empty (ProposedEvent.WouldBeDestroyed oid regenerability)
  pure (outcome >>= asDestruction)

asDestruction :: ProposedEvent -> Maybe ObjectId
asDestruction event = case event of
  ProposedEvent.WouldBeDestroyed target _ -> Just target
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing

-- The single counter-PLACEMENT funnel (CR 122.6: counters as markers on a
-- permanent -- not to be confused with Pawl.Engine.Event.counter, CR 701.6's
-- countering of a spell). Before P5 the PutCounters opcode edited Object.counters
-- in place with no funnel at all, so there was nothing for a replacement to
-- intercept.
--
-- CR 122.6 makes this the right single seam: "Some spells and abilities refer to
-- counters being put on an object. This refers to putting counters on that object
-- while it's on the battlefield and also to an object that's given counters as it
-- enters the battlefield." A zero count after the loop puts nothing on.
--
-- It lives HERE rather than beside the other change-and-emit funnels in
-- Pawl.Engine.Event because CR 122.6's second clause -- the object "given counters
-- as it enters the battlefield" -- is served by `apply`'s
-- EntryRewrite.WithCounters arm above, and Pawl.Engine.Event already depends on
-- this module. A copy of the body there would be a second funnel, which is the one
-- thing a funnel must not have.
putCounters :: ObjectId -> CounterKind.CounterKind -> Natural -> Game ()
putCounters oid kind n = do
  resolved <- resolveCounters oid kind n
  case resolved of
    Nothing -> pure ()
    Just (target, settledKind, settledCount) ->
      Monad.when (settledCount > 0)
        . State.modify'
        $ \gs ->
          let bump obj = obj {Object.counters = Map.insertWith (+) settledKind settledCount (Object.counters obj)}
           in gs {GameState.objects = Map.adjust bump target (GameState.objects gs)}

-- CR 122.6: settle a proposed counter placement. Nothing means none are put on.
resolveCounters :: ObjectId -> CounterKind.CounterKind -> Natural -> Game (Maybe (ObjectId, CounterKind.CounterKind, Natural))
resolveCounters oid kind n = do
  outcome <- applyReplacements (ProposedEvent.WouldPutCounters oid kind n)
  pure (outcome >>= asCounters)

asCounters :: ProposedEvent -> Maybe (ObjectId, CounterKind.CounterKind, Natural)
asCounters event = case event of
  ProposedEvent.WouldPutCounters oid kind n -> Just (oid, kind, n)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing

-- CR 111.1: settle a proposed token creation. Nothing means none are created.
resolveTokens :: PlayerId -> Card -> Natural -> Game (Maybe (PlayerId, Card, Natural))
resolveTokens pid card n = do
  outcome <- applyReplacements (ProposedEvent.WouldCreateTokens pid card n)
  pure (outcome >>= asTokens)

asTokens :: ProposedEvent -> Maybe (PlayerId, Card, Natural)
asTokens event = case event of
  ProposedEvent.WouldCreateTokens pid card n -> Just (pid, card, n)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing

-- CR 500.11 / 614.10: settle whether a step or phase begins at all, on the turn
-- of `pid`. False means a skip took it, and CR 500.11's "proceed past it as
-- though it didn't exist" is then the caller's whole obligation -- there is no
-- rewritten event to carry out, because CR 614.1b replaces a skipped step "with
-- nothing". How far "past it" reaches is the caller's too: one schedule entry
-- for a PhaseSelector.Step, the phase's remaining entries for a whole phase
-- (Engine.runStep, Turn.dropRestOfPhase).
--
-- Answers a Bool rather than the settled event, unlike resolveDestruction, whose
-- `Just` had to carry an identity because a rewrite can redirect which object is
-- destroyed. Nothing can rewrite a WouldBeginPhase: PhaseR is the only effect the
-- class admits and it only ever cancels, so a survivor is always the event that
-- was proposed and there is no second identity for the caller to learn.
--
-- The typed door Pawl.Engine.Engine uses, so Engine never cases on a ProposedEvent.
beginsPhase :: PhaseSelector -> PlayerId -> Game Bool
beginsPhase selector pid = do
  outcome <- applyReplacements (ProposedEvent.WouldBeginPhase selector pid)
  pure (Maybe.isJust (outcome >>= asPhaseBegin))

-- CR 500.11 / 614.1b: an extra turn is beginning, so the steps and phases IT
-- skips become floating replacement effects, one per selector. Called by
-- Engine.takeNextTurn at the moment the turn actually begins, and only for a turn
-- that does begin (CR 800.4k).
--
-- Here rather than in Pawl.Engine.Resolve, which installs Effect.SkipNextPhase's
-- rows: what those two opcodes differ on is WHEN the row exists, and this module
-- is the one that reads GameState.replacements. The row itself is the same shape
-- Resolve builds -- PhaseR, scoped to the taker, Uses.Once -- so `beginsPhase`
-- above answers a turn-scoped skip and a "next occurrence" skip through one
-- mechanism, and CR 616.1's loop orders them against each other for free.
--
-- Installed AT THE TURN'S START rather than at the resolution that created the
-- turn, which is the whole point: CR 614.10a's "next" would name whatever step
-- came first in the meantime, and CR 500.7's "the most recently created turn will
-- be taken first" lets that be a different turn entirely.
--
-- CR 614.10: "once a step, phase, or turn has started, it can no longer be
-- skipped". Nothing of this turn has started yet -- Engine.beginTurnOf has only
-- scheduled it, and Engine.runStep asks `beginsPhase` before the untap step's
-- first observable moment.
--
-- Expiry.AtCleanup, not Never. CR 514.2's "until end of turn" is not the reason:
-- the card states no duration. The reason is that the skip names ONE turn and
-- cannot apply to another, so the last moment of that turn is the last moment it
-- could matter, and AtCleanup is the store for exactly that
-- (Pawl.Engine.Expiry.dropAtCleanup). Uses.Once is CR 614.10a's per-occurrence
-- spend, and it is the one that actually fires for the one card in the pool: the
-- untap step is the first step of the turn the skip belongs to, so the row is
-- spent long before any sweep. Not implemented: the sweep does not run at all on
-- a turn whose ending phase was skipped, so the expiry alone would not hold an
-- unspent row to its turn (#491).
installTurnSkips :: ExtraTurn -> GameState -> GameState
installTurnSkips entry gs =
  let install g selector =
        let (ts, g1) = Game.freshTimestamp g
            active =
              ActiveReplacement.MkActiveReplacement
                { ActiveReplacement.effect =
                    ReplacementEffect.PhaseR
                      PhasePattern.MkPhasePattern
                        { PhasePattern.whichPhase = selector,
                          -- The turn's taker, which for a step or phase OF that
                          -- turn is also whose step it is (see PhasePattern).
                          PhasePattern.whosePhase = Just (ExtraTurn.taker entry)
                        },
                  -- CR 113.7: the source of the effect that created the turn.
                  ActiveReplacement.source = ExtraTurn.source entry,
                  -- CR 109.5's "you" for this row. Nothing reads it: a PhaseR
                  -- names its player outright (PhasePattern.whosePhase) and has
                  -- no ControllerRelation to resolve. The TAKER rather than the
                  -- effect's controller, because these skips ride the turn (see
                  -- Pawl.Types.ExtraTurn) and the effect that created it is long
                  -- gone by the time the turn begins -- Projection.controllerOf
                  -- on ExtraTurn.source would answer Nothing here.
                  ActiveReplacement.controller = ExtraTurn.taker entry,
                  ActiveReplacement.timestamp = ts,
                  ActiveReplacement.expiry = Expiry.AtCleanup,
                  ActiveReplacement.uses = Uses.Once,
                  ActiveReplacement.origin = ReplacementOrigin.Other
                }
         in g1 {GameState.replacements = active : GameState.replacements g1}
   in List.foldl' install gs (Set.toAscList (ExtraTurn.skipped entry))

asPhaseBegin :: ProposedEvent -> Maybe (PhaseSelector, PlayerId)
asPhaseBegin event = case event of
  ProposedEvent.WouldBeginPhase selector pid -> Just (selector, pid)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
