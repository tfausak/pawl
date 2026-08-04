-- CR 616.1's loop: the SOLE home of casing on ProposedEvent and
-- ReplacementEffect, beside Pawl.Engine.Resolve (Effect), Pawl.Engine.Event
-- (TriggerCondition) and Pawl.Engine.Projection (Modification). Pawl.Codec also
-- cases on ReplacementEffect, but only as the JSON data boundary, never to
-- decide game behaviour.
--
-- CR 616.1 is a LOOP, not an ordering prompt: choose one applicable effect from
-- the highest non-empty of five ordered buckets, apply it, then repeat over the
-- effects applicable NOW (CR 616.1f) -- and CR 616.2 lets an effect become
-- applicable because another one modified the event. A foldl' over a list
-- computed once is structurally incapable of either.
--
-- This module must NOT import Pawl.Engine.Event: Event raises proposed events
-- through this loop, so the dependency runs one way only. That is also why the
-- entry copy-target legal set lives here rather than in Pawl.Engine.Target --
-- Target imports Pawl.Engine.Sba, which imports Pawl.Engine.Event.
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
import qualified Pawl.Types.Color as Color
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
import qualified Pawl.Types.Subtype as Subtype
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

-- CR 616.1's loop. `Nothing` means the event DOES NOT HAPPEN (CR 615.6, CR
-- 701.19a). A rewrite that cancels an event has already performed its own
-- consequences by the time it returns Nothing.
applyReplacements :: ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacements = applyReplacementsIn Nothing Set.empty

-- CR 608.2f / 704.3: `asOf` is the board a BATCH's candidates are read from --
-- `Just` the state the batch began in, or `Nothing` for the live board. Only the
-- destroy funnel passes `Just` (Event.destroy, Event.destroyInBatch), along with
-- the graveyard moves it and Pawl.Engine.Sba's put-into-graveyard batch make
-- through Event.changeZoneInBatch. Everything else is a lone event and wants the
-- live board. CR 608.2f and CR 704.3 make a batch ONE event, so CR 614.4 asks
-- which effects existed before the BATCH, not before the member being processed
-- -- otherwise Rest in Peace animated by Opalescence and swept by Day of
-- Judgment answers according to an order CR 608.2f gives nobody the right to
-- decide.
--
-- `asOf` and `batch` both name a batch, and they are OPPOSITES: `asOf` widens
-- the candidate set to include effects of permanents the batch is itself
-- removing, while `batch` NARROWS the copy-target set to exclude permanents
-- entering beside the loop's subject. Deliberately not one parameter: different
-- batches, different readers, and no call site ever supplies both.
--
-- What `asOf` does NOT freeze: the FLOATING store stays live (see `collect`),
-- because CR 614.3 has `consume` spend a one-shot as it applies and a frozen
-- store would hand a spent regeneration shield to the next member of the batch;
-- the loop still RE-COLLECTS every iteration, so CR 616.1f and CR 616.2 are
-- untouched; and `apply`'s writes and `choose`'s chooser lookup read the LIVE
-- state. A permanent that ENTERED after the batch began therefore contributes
-- nothing, which is CR 614.4 read the other way. No producer today, so that half
-- is unexercised.
--
-- CR 614.12a: `batch` is the set of ids entering the battlefield AT THE SAME
-- TIME as this loop's subject. Clone may only copy a creature already on the
-- battlefield, and a sibling entering in the same batch is not there yet at the
-- moment the choice is made. (CR 614.13a is the wrong cite: that rule is about
-- an entry effect moving OTHER objects to a different zone; a copy target never
-- changes zones.) The loop's own subject is excluded by legalCopyTargets'
-- `self`, never by this set.
--
-- `changeZone` handles one entering object at a time and passes `Set.empty`. The
-- non-empty case is Event.createTokens, which materializes every token of a
-- Create BEFORE running any of their entry loops (CR 614.16's doubled count is
-- settled once, up front), so a later token's entry loop would otherwise find
-- its siblings already sitting on the battlefield.
--
-- A simultaneously-entering sibling can reach a later token's entry loop through
-- three channels; only the first needs this explicit exclusion:
--   1. Copy targets -- excluded by `batch`. IMPLEMENTED BUT UNTESTED: no card in
--      the pool puts two copy-choosers onto the battlefield at once (#73).
--   2. Candidate collection -- unreachable regardless of `batch`, though no
--      longer impossible by construction. Every entry replacement a PERMANENT
--      carries in this pool is CR 614.1c's self-only `IsSource` (Clone, Primal
--      Plasma, CR 306.5b's loyalty), which no sibling can satisfy; CR 614.1d's
--      other-objects form exists (Gather Specimens) but as a FLOATING row rather
--      than a sibling's ability. A permanent printing a 614.1d entry replacement
--      (Essence of the Wild) would reach a sibling here, correctly and by the
--      card's own text.
--   3. Projection -- a sibling's STATIC ABILITIES would be visible to a later
--      token's projection, and nothing here excludes them the way `batch`
--      excludes copy targets. CR 614.12 does not sanction this: a
--      simultaneously-entering sibling's static abilities do not already exist
--      relative to it. NOT IMPLEMENTED AT ALL, and unreached today only because
--      every token card in the pool has empty `staticAbilities` (#78).
applyReplacementsIn :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacementsIn asOf batch = loop asOf batch Set.empty

loop :: Maybe GameState -> Set ObjectId -> Set CandidateId -> ProposedEvent -> Game (Maybe ProposedEvent)
loop asOf batch applied event = do
  gs <- State.get
  -- From scratch each iteration: collect against the CURRENT state (or, for a
  -- CR 608.2f batch, the state the batch began in), minus CR 614.5's
  -- already-applied set. Re-collecting is what makes CR 616.2 work.
  let unused candidate = not (Set.member (ReplacementCandidate.identity candidate) applied)
      fresh = filter unused (applicable asOf gs event)
  case highestBucket fresh of
    -- CR 616.1f / 614.6: no candidate remains, so the surviving event happens.
    [] -> pure (Just event)
    bucket -> do
      picked <- choose gs event bucket
      case picked of
        -- Unreachable: highestBucket returns [] for an empty input, so `bucket`
        -- is non-empty and `choose` always picks. Total rather than partial.
        Nothing -> pure (Just event)
        Just candidate -> do
          outcome <- apply batch candidate event
          case outcome of
            Nothing -> pure Nothing
            Just rewritten -> loop asOf batch (Set.insert (ReplacementCandidate.identity candidate) applied) rewritten

-- Every replacement effect instance in the game, in the engine's canonical
-- order, which is what the ChooseReplacement prompt indexes into:
--
--   1. PERMANENT abilities (Projection.replacementsAffecting): battlefield
--      permanents ascending by id, each permanent's own effects in printed
--      order. Read from `sources`, which for a CR 608.2f batch is the board the
--      batch began in rather than the live one (see applyReplacementsIn).
--   2. The FLOATING store (GameState.replacements): newest first, since every
--      installer prepends as it creates the row. Always the LIVE store, never a
--      frozen one: CR 614.3 spends a one-shot as it is applied, and `consume`
--      writes that back here.
--
-- The two segments take separate arguments -- rather than one GameState apiece,
-- which would be two interchangeable parameters of the same type -- so the split
-- cannot be got backwards.
collect :: GameState -> [ActiveReplacement.ActiveReplacement] -> [ReplacementCandidate]
collect sources floating =
  let fromPermanent (src, re) =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity = CandidateId.OfPermanent src re,
            ReplacementCandidate.effect = re,
            ReplacementCandidate.source = src,
            -- CR 109.5: "you" is the SOURCE's controller, read live off the
            -- board this segment was gathered from -- a stolen Furnace of Rath
            -- belongs to whoever holds it now.
            ReplacementCandidate.controller = Projection.controllerOf src sources,
            -- CR 614.15: a permanent's replacement ability is a STATIC ability,
            -- which puts it outside the self-replacement class -- so this
            -- segment is never CR 616.1a's, whatever it replaces.
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
            -- CR 614.15: a floating row IS an effect of a resolving spell or
            -- ability, so this is the one segment that can carry a
            -- self-replacement, and the row itself says whether it does.
            ReplacementCandidate.origin = ActiveReplacement.origin active
          }
   in fmap fromPermanent (Projection.replacementsAffecting sources)
        <> fmap fromFloating floating

-- The candidates that apply to this event. `asOf` is Nothing for a lone event
-- and Just the pre-batch board for a CR 608.2f batch (see applyReplacementsIn);
-- `gs` is always the live state.
--
-- `applies` reads the pre-batch board too, not just `collect`: both ask about
-- the SOURCE's controller for CR 109.5's "you" (matchesController,
-- matchesZoneOwner, the TokenR arm), and a source the batch has already removed
-- has no controller, so the two have to agree on which board that is.
applicable :: Maybe GameState -> GameState -> ProposedEvent -> [ReplacementCandidate]
applicable asOf gs event =
  let sources = Maybe.fromMaybe gs asOf
   in filter (applies sources event) (collect sources (GameState.replacements gs))

-- CR 614.1: does this instance apply to this proposed event? The arms must agree
-- on the EVENT CLASS -- which the type already rules out for the impossible
-- pairs -- and the pattern must admit the event's subject.
--
-- CR 701.19c: may this destruction rewrite be applied to this destruction? Asked
-- here, where a candidate is offered the event, so a shield that is refused is
-- also never CONSUMED. Gates regeneration and nothing else, which is why it
-- reads the rewrite rather than rejecting the whole DestructionR class.
admits :: Regenerability.Regenerability -> DestructionRewrite.DestructionRewrite -> Bool
admits regenerability rewrite = case rewrite of
  DestructionRewrite.Regenerate -> regenerability == Regenerability.Regenerable

-- CR 615.7: a spent shield is not an applicable prevention effect at all, and is
-- refused HERE rather than applied for nothing -- as `admits` refuses a
-- regeneration shield CR 701.19c bars, and for the same reason: a candidate
-- refused by `applies` is never spent, and never counts among the applicable
-- sources whose ordering CR 615.7 asks about.
--
-- `setShield` drops a floating row the moment it reaches 0, so the only 0 that
-- can reach this test is one written into card data -- which
-- Pawl.Types.DamageRewrite forbids. Total rather than partial.
unspent :: DamageRewrite.DamageRewrite -> Bool
unspent rewrite = case rewrite of
  DamageRewrite.PreventNext remaining -> remaining > 0
  DamageRewrite.PreventAll -> True
  DamageRewrite.SetAmount _ -> True
  DamageRewrite.Scale _ -> True

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
        -- dealing. CR 615.7: one naming a RECIPIENT admits only the damage
        -- addressed to the permanent or player it shields.
        (ReplacementEffect.DamageR pat rewrite, ProposedEvent.WouldDealDamage de) ->
          maybe True (== DamageEvent.kind de) (DamagePattern.whichKind pat)
            && matchesDamageSource src (DamagePattern.whichSource pat) de
            && maybe True (== DamageEvent.target de) (DamagePattern.whichRecipient pat)
            && unspent rewrite
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
        -- (Stonehorn Dignitary) matches only the phase question Engine.runStep
        -- raises at that phase's first step, and one naming a step matches only
        -- the step question. That is what keeps CR 500.1's decomposition of a
        -- phase into steps out of this comparison.
        --
        -- The event's PlayerId is the ACTIVE player, which is also whose step
        -- this is: every step and phase in a turn belongs to the player whose
        -- turn it is. So Fatigue is `whosePhase == Just that player`, and
        -- Nothing is Eon Hub's symmetric skip, which reads no PlayerId at all.
        -- The SOURCE's controller is not consulted: unlike matchesController's
        -- CR 109.5 "you", the player here was named by the effect, not derived.
        (ReplacementEffect.PhaseR pat, ProposedEvent.WouldBeginPhase selector pid) ->
          PhasePattern.whichPhase pat == selector
            && maybe True (== pid) (PhasePattern.whosePhase pat)
        -- CR 614.1c-d: which entering permanents this replacement watches, as a
        -- Filter over the entering object (see Pawl.Types.ReplacementEffect).
        -- 614.1c's self-scope is Filter.IsSource; 614.1d's is a characteristic
        -- filter.
        (ReplacementEffect.EntryR pat _, ProposedEvent.WouldEnter oid) -> matchesEntering gs candidate pat oid
        -- Every row below falls through to False because an arm ABOVE already
        -- matches every event of that class: a row below fires only for a
        -- MISMATCHED class, where False is the correct answer rather than a
        -- stand-in for "not yet implemented".
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
-- effect's own source is dealing (Galvanic Blast's metalcraft clause).
--
-- The compared id is the DamageEvent's `source` (CR 113.7a) -- for a resolving
-- spell, the spell on the stack, which is also the object Resolve installs the
-- floating row under. Board-free, so no GameState: an identity test, not a
-- characteristic one, like matchesZoneSubject.
--
-- Not implemented: CR 615.1's shields that name a source by CHARACTERISTIC
-- (Circle of Protection: Red) rather than by identity (#588).
matchesDamageSource :: ObjectId -> SourceRelation.SourceRelation -> DamageEvent.DamageEvent -> Bool
matchesDamageSource src relation de = case relation of
  SourceRelation.AnySource -> True
  SourceRelation.TheSource -> src == DamageEvent.source de

-- CR 614.1: is this ZONE CHANGE about the object the pattern names? AnyObject
-- always is; TheSource is CR 702.34a's "exile THIS card", the self-scoping EntryR
-- (CR 614.1c) and DestructionR (CR 201.5) carry by having no pattern at all.
--
-- The id compared is the event's own subject, which Pawl.Engine.Event proposes
-- as the PRE-move id -- the id the effect's source still has while it sits on
-- the stack. Board-free, so no GameState: an identity test, not a characteristic
-- one.
matchesZoneSubject :: ObjectId -> ZoneChangeSubject.ZoneChangeSubject -> ObjectId -> Bool
matchesZoneSubject src subject oid = case subject of
  ZoneChangeSubject.AnyObject -> True
  ZoneChangeSubject.TheSource -> src == oid

-- CR 614.1: does this ZONE CHANGE's object satisfy the pattern's relation?
--
-- The subject is the object's OWNER, not its controller, and that is a rules
-- fact rather than a convenience: CR 400.3 and CR 404.1 make the destination
-- zone the owner's, so Leyline of the Void's "an opponent's graveyard" asks who
-- OWNS the card. A creature stolen with Act of Treason still dies to its owner's
-- graveyard, which a controller-based test would get backwards.
--
-- Split out of matchesController, which stays controller-based for CR 109.5's
-- "you" on a counter or token pattern. No committed card pairs a ZoneChangeR
-- with anything but Anyones, which answers True either way.
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

-- Which permanents a pattern admits, matched through Pawl.Engine.Filter over the
-- PROJECTED view: creature-ness (CR 205.2b / 300.2 / 613.1d, so an Opalescence'd
-- enchantment counts) and subtype membership (CR 205.3, so Blood Moon is seen).
-- A replacement's pattern frames no player, so the perspective is Nothing.
--
-- Pawl.Engine.Cost narrows its sacrifice candidates through the SAME call, so
-- there is no duplicate matcher to keep in step (#111).
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
-- rather than of the card it came from: a previous iteration's rewrite has to be
-- visible to this one, including CR 616.1b's own change to who would control it.
matchesEntering :: GameState -> ReplacementCandidate -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> Bool
matchesEntering gs candidate filter_ oid =
  let context = Filter.MkContext (ReplacementCandidate.controller candidate) (Just (ReplacementCandidate.source candidate))
   in Filter.matches context (Projection.viewOfObject oid gs) filter_

-- CR 614.1a: apply a scaling to a number. "Plus one" and "twice that many" are
-- the same operation with different data, and so is Furnace of Rath's doubling
-- -- which is why CounterR, TokenR (CR 614.16's two shapes) and DamageR all
-- rewrite through this one function.
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
-- payload: CR 614.15 defines the self-replacement class by which ability created
-- the effect, so no ReplacementEffect value could answer it (see
-- Pawl.Types.ReplacementOrigin). Every remaining step reads the payload's SHAPE,
-- never its identity.
bucketOf :: ReplacementCandidate -> ReplacementBucket
bucketOf candidate = case ReplacementCandidate.origin candidate of
  ReplacementOrigin.SelfReplacement -> ReplacementBucket.SelfReplacement
  ReplacementOrigin.Other -> bucketOfEffect (ReplacementCandidate.effect candidate)

-- CR 616.1b-e: which bucket an effect that is NOT CR 614.15's falls in.
bucketOfEffect :: ReplacementEffect -> ReplacementBucket
bucketOfEffect re = case re of
  ReplacementEffect.ZoneChangeR _ _ -> ReplacementBucket.Other
  -- CR 616.1c: entering as a copy is its own, HIGHER bucket. The split only
  -- becomes observable where an AsCopy races another entry replacement of NO
  -- HIGHER bucket in the SAME iteration, which no card in the pool produces, so
  -- this bucket's ordering is unexercised (#73). An entering Clone on its own
  -- does not exercise it: AsCopy is the only applicable candidate on the first
  -- iteration, and what carries the rest is CR 616.1f's re-collection plus CR
  -- 614.5's identity being keyed on the effect VALUE, which keeps the
  -- newly-acquired ChoiceOf distinct from the already-applied AsCopy. Gather
  -- Specimens racing an entering Clone is a real same-iteration race, but CR
  -- 616.1b's bucket outranks this one, so it exercises that arm instead.
  ReplacementEffect.EntryR _ EntryRewrite.AsCopy -> ReplacementBucket.CopyOnEntry
  -- CR 616.1a-d name self-replacement, entering under a control effect, entering
  -- as a copy and entering with the back face up. None of the next four arms is
  -- any of those, so CR 616.1e is what applies to each.
  ReplacementEffect.EntryR _ (EntryRewrite.ChoiceOf _) -> ReplacementBucket.Other
  ReplacementEffect.EntryR _ EntryRewrite.ChooseColor -> ReplacementBucket.Other
  ReplacementEffect.EntryR _ EntryRewrite.ChooseBasicLandType -> ReplacementBucket.Other
  ReplacementEffect.EntryR _ (EntryRewrite.WithCounters _ _) -> ReplacementBucket.Other
  -- CR 616.1b: a control-on-entry rewrite is one step ABOVE the copy bucket, and
  -- Gather Specimens racing an entering Clone is the board where the two orders
  -- disagree: taking the control rewrite first hands Clone's own CR 109.5 copy
  -- choice to the NEW controller, and taking the copy first hands it to the old
  -- one.
  ReplacementEffect.EntryR _ EntryRewrite.UnderSourceControl -> ReplacementBucket.ControlOnEntry
  ReplacementEffect.DamageR _ _ -> ReplacementBucket.Other
  ReplacementEffect.DestructionR _ -> ReplacementBucket.Other
  ReplacementEffect.CounterR _ _ -> ReplacementBucket.Other
  ReplacementEffect.TokenR _ _ -> ReplacementBucket.Other
  -- CR 616.1a-d are all about entries and copies; a skip is none of those, so it
  -- falls to CR 616.1e.
  ReplacementEffect.PhaseR _ -> ReplacementBucket.Other

-- Does applying this effect read the CANDIDATE that is applying it -- something
-- riding the ReplacementCandidate rather than the ReplacementEffect?
--
-- A CLASSIFICATION of effects, the same genre as bucketOf above: what SHAPE an
-- effect has, never which effect it is. Its sole consumer is `choose` below,
-- which folds CR 109.5's "you" into its indistinguishability test exactly for
-- the arms that answer True.
--
-- Consumption is deliberately NOT what this asks about. Every `apply` arm spends
-- its own candidate (CR 614.5), but the loop gives each candidate its own
-- opportunity in any order, so which was spent first is not a board difference.
--
-- One arm per constructor, no wildcard, and the EntryR arms split per
-- EntryRewrite, so a new constructor breaks the build HERE as well as in
-- bucketOfEffect and `apply`. A wildcard defaulting to False would hand an
-- author who teaches `apply` a new controller-reading rewrite an unasked choice
-- instead of a build failure.
readsApplier :: ReplacementEffect -> Bool
readsApplier re = case re of
  -- The destination zone is the effect's own second field, and the pattern is
  -- matched before `apply` runs (Rest in Peace, Leyline of the Void).
  ReplacementEffect.ZoneChangeR _ _ -> False
  -- CR 707.5 / 109.5: Clone's "you" is the ENTERING object's controller, read
  -- live off the board at CR 614.12a's moment, not the candidate's -- so two
  -- such rows offer the same player the same legal set.
  ReplacementEffect.EntryR _ EntryRewrite.AsCopy -> False
  -- Same chooser, and the options ride the effect: CR 614.1c's "enters as"
  -- (Primal Plasma).
  ReplacementEffect.EntryR _ (EntryRewrite.ChoiceOf _) -> False
  -- Same chooser again, with no payload at all: CR 105.1's five colours are the
  -- whole offer whoever's row is applying (Painter's Servant).
  ReplacementEffect.EntryR _ EntryRewrite.ChooseColor -> False
  ReplacementEffect.EntryR _ EntryRewrite.ChooseBasicLandType -> False
  -- CR 614.1c's "enters with": the counter kind and count are the effect's own
  -- fields, and they land on the entering object (CR 306.5b's loyalty included).
  ReplacementEffect.EntryR _ (EntryRewrite.WithCounters _ _) -> False
  -- THE ONE ARM THAT ANSWERS YES. CR 616.1b / 110.2 / 109.5: the rewrite hands
  -- the permanent to the candidate's own `controller`, baked when the row was
  -- installed. Two Gather Specimens are one card, so their `effect` values are
  -- identical while their controllers are not, and applying one puts the
  -- permanent somewhere applying the other does not.
  ReplacementEffect.EntryR _ EntryRewrite.UnderSourceControl -> True
  -- The rewritten amount is the effect's (Galvanic Blast, Furnace of Rath), and
  -- a prevention prevents the same event whoever's row it is (Fog). CR 615.7's
  -- shield is no exception: what makes two shields differ is how much each has
  -- LEFT, which rides the effect value and so is already inside `choose`'s
  -- comparison rather than needing the applier to be read.
  ReplacementEffect.DamageR _ _ -> False
  -- CR 701.19a acts on the creature being destroyed and names no player.
  ReplacementEffect.DestructionR _ -> False
  -- The scaling is the effect's, and it rewrites the count on the object the
  -- event already named (Hardened Scales, Doubling Season).
  ReplacementEffect.CounterR _ _ -> False
  -- CR 614.16, the same shape one event class over: the player the tokens are
  -- created FOR rides the EVENT, not the candidate, so Doubling Season doubles
  -- the same player's tokens whoever's row applies.
  ReplacementEffect.TokenR _ _ -> False
  -- CR 614.10: a skip replaces the step or phase with nothing. The player it is
  -- ABOUT is baked into PhasePattern.whosePhase, on the EFFECT, where this
  -- comparison already sees it.
  ReplacementEffect.PhaseR _ -> False

-- CR 616.1: the affected object's controller (or its owner if it has none), or
-- the affected player, chooses one to apply. Anything not elided below prompts.
--
-- TWO ELISIONS, both of them choices the rules make indistinguishable:
--
--   * ONE candidate -- there is nothing to choose.
--   * several candidates INDISTINGUISHABLE -- each still gets its own CR 614.5
--     opportunity, so every order produces the same board. Only the PROMPT is
--     elided, never an application.
--
-- Indistinguishable is `distinguishing` below: equal in `effect`, plus -- for
-- the effects whose application READS the applying candidate -- equal in CR
-- 109.5's "you". Candidates equal in `effect` can still differ in `source` and
-- in `controller`, so "equal in `effect`" implies "every order yields the same
-- board" only for effects that apply the same way whoever is applying them.
-- readsApplier is the classification that separates those two, and
-- EntryRewrite.UnderSourceControl is the one arm it answers True for.
--
-- Folding `controller` in UNCONDITIONALLY would be sound as well, and wrong the
-- other way: two Rest in Peace under different controllers exile the same card
-- to the same zone whichever applies, so asking about them would raise a
-- question the rules leave nothing to decide. `source` is folded in nowhere for
-- the same reason -- every use of it above is a test run BEFORE `apply`, not a
-- branch inside it.
--
-- Not implemented: two floating rows alike in `effect` and `controller` but
-- differing in `expiry` or `uses` are treated as indistinguishable even though
-- `consume` spends only the one that applied (#490).
--
-- `origin` is NOT such a hole: highestBucket has already partitioned by bucket,
-- and CR 616.1a's bucket is exactly an origin of SelfReplacement, so every
-- candidate reaching this comparison shares one origin.
choose :: GameState -> ProposedEvent -> [ReplacementCandidate] -> Game (Maybe ReplacementCandidate)
choose gs event candidates = case candidates of
  [] -> pure Nothing
  first : rest ->
    if all (\c -> distinguishing c == distinguishing first) rest
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
  where
    -- What two candidates must agree on to be interchangeable here.
    distinguishing c =
      ( ReplacementCandidate.effect c,
        if readsApplier (ReplacementCandidate.effect c)
          then ReplacementCandidate.controller c
          else Nothing
      )

-- Total index into a list, with a fallback.
at :: [a] -> Natural -> a -> a
at xs i fallback = case List.genericDrop i xs of
  h : _ -> h
  [] -> fallback

-- CR 616.1 / 108.4: who decides. Projection.controllerOf already falls back to
-- the owner, so CR 616.1's owner clause is free.
--
-- CR 616.1's APNAP clause has no producer here: one proposed event has exactly
-- one affected object and therefore one chooser, and the damage batch runs each
-- event's loop independently (#71).
chooserOf :: GameState -> ProposedEvent -> Maybe PlayerId
chooserOf gs event = case event of
  ProposedEvent.WouldChangeZone zc -> Projection.controllerOf (ZoneChange.object zc) gs
  -- CR 616.1's affected object's controller, read LIVE off the materialized
  -- permanent -- which for an entry is the player it WOULD enter under, and
  -- which a CR 616.1b rewrite may already have changed on an earlier iteration.
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
-- One arm per ReplacementEffect constructor, same shape as `applies`, so a new
-- constructor breaks the build HERE too. A wildcard fallback would defeat that:
-- an author who teaches `applies` a new arm but forgets this one gets a silent
-- no-op replacement. Every arm below either rewrites its paired event or, for a
-- pair `applies` already excludes, falls through to `pure (Just event)` --
-- unreachable in practice, but present so the match stays total per constructor
-- rather than total by wildcard.
--
-- The same discipline applies one level down, to each arm's inner SUM type
-- (DamageRewrite, DestructionRewrite, EntryRewrite, Scaling), never to the
-- pattern RECORDS, which are read for their fields rather than cased. An arm
-- must CASE on the inner sum, not bind it with `_`: `_` is exhaustive
-- UNCONDITIONALLY, so it raises no build failure and no warning when a new
-- constructor is added, silently treating a real rewrite as a no-op.
--
-- CounterR's and TokenR's arms delegate Scaling whole to `scale`, which is where
-- the exhaustive case lives -- a new Scaling constructor breaks `scale`'s build
-- and both arms' transitively, so casing it again inline would not strengthen
-- anything.
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
    -- object's copy snapshot. Writing to the COPIABLE layer (CR 613.1a) is what
    -- makes CR 707.2 fall out for free: a later Clone of this object copies the
    -- stamped values with no further machinery. Clone's "may" is real: Nothing
    -- leaves the object as its printed self (a 0/0, which CR 704.5f then buries).
    --
    -- The class match is on the OUTER tuple, so the INNER `case rewrite of` is
    -- what carries the exhaustiveness obligation -- a wildcard-bound `_` on the
    -- outer pattern would let a new EntryRewrite constructor fall through
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
            -- enforcing CR 614.12a's same-batch exclusion, so honouring an
            -- unoffered answer would let a Clone copy a sibling token entering
            -- beside it.
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
      -- CR 614.1c / 208.2b: Primal Plasma's choice of which printed
      -- power/toughness-and-keywords option to become. Written into the COPIABLE
      -- snapshot (applyEntryOption), which is what makes CR 616.2 fall out for
      -- free: a Clone that copies Primal Plasma also copies this ability (CR
      -- 707.5), and the loop's next iteration finds it newly applicable.
      EntryRewrite.ChoiceOf options -> do
        gs <- State.get
        case options of
          -- Malformed card data: an as-enters choice with nothing to choose
          -- from. No-op rather than a partial function, but still consumed -- a
          -- floating one-shot must not survive to apply again.
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
      -- CR 614.1c: Painter's Servant's as-enters colour choice. Unlike ChoiceOf
      -- above, this is asked every time the entering object has a controller to
      -- ask: CR 105.1's five colours are always all legal and always
      -- distinguishable, so there is no one-option case to elide.
      --
      -- Written to Object.chosenColor, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseColor.
      EntryRewrite.ChooseColor -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. A WEAKER fallback than
          -- ChoiceOf's, and the one place on this path the engine would decide
          -- something: there is no colour the card named to default to, so white
          -- is conjured. It stands only because the branch cannot be reached.
          Nothing -> pure Color.White
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Trans.lift (Program.prompt (Prompt.ChooseColor decider controller oid))
        consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenColor = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c: Convincing Mirage's as-enters basic land type choice. Asked
      -- every time the entering object has a controller to ask, for
      -- ChooseColor's reason just above: CR 305.6's five basic land types are
      -- always all legal and always distinguishable, so there is no one-option
      -- case to elide.
      --
      -- Written to Object.chosenSubtype, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseBasicLandType.
      EntryRewrite.ChooseBasicLandType -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. The same WEAKER fallback
          -- ChooseColor's arm carries, and for the same reason: no type the card
          -- named to default to, so Mountain is conjured.
          Nothing -> pure Subtype.Mountain
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Trans.lift (Program.prompt (Prompt.ChooseBasicLandType decider controller oid))
        consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenSubtype = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 306.5b via CR 614.1c: this permanent enters with N counters. Through
      -- Event.putCounters, the CR 122.6 funnel, and NOT a direct write to
      -- Object.counters, because CR 614.16 makes a counter-scaling replacement
      -- apply even when the original event was not itself an effect -- so
      -- Doubling Season has to see these. That nested CR 616.1 loop is why the
      -- counters are placed here rather than folded into the entry event's own
      -- payload. Consumed like every other arm, so CR 614.5 keeps the loop's next
      -- iteration from placing them twice.
      EntryRewrite.WithCounters kind n -> do
        consume (ReplacementCandidate.identity candidate)
        putCounters oid kind n
        pure (Just event)
      -- CR 616.1b / 110.2: Gather Specimens. The entering object's CR 110.2
      -- DEFAULT controller becomes CR 109.5's "you" -- the candidate's
      -- controller, baked when the row was installed -- and that is a permanent
      -- change: the card's "this turn" bounds how long the REPLACEMENT is around
      -- to catch entries, never how long the creature stays yours.
      --
      -- Written to the object rather than to the surviving ProposedEvent, which
      -- is why WouldEnter still carries only an ObjectId. This engine
      -- materializes the entering permanent BEFORE running the entry loop (see
      -- runEntry, and CR 614.12), so the would-be controller is exactly
      -- Projection.controllerOf on the live board. That is also what makes CR
      -- 616.2 fall out: the loop's next iteration re-matches against a board
      -- where the control has already changed, which a value parked on the event
      -- would not show it. All five arms above land on the object for the same
      -- reason.
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
          -- has left the board. Defensive, with no producer today, and it leaves
          -- the entry alone rather than guessing at a player.
          Nothing -> pure (Just event)
          Just you -> do
            State.modify' $ \gs ->
              let claim obj = obj {Object.enteredUnder = Just you}
               in gs {GameState.objects = Map.adjust claim oid (GameState.objects gs)}
            pure (Just event)
    -- Unreachable: `applies` admits EntryR only against WouldEnter.
    (ReplacementEffect.EntryR _ _, _) -> pure (Just event)
    (ReplacementEffect.DamageR pat rewrite, ProposedEvent.WouldDealDamage de) -> case rewrite of
      -- CR 615.6: a prevented event never happens -- it is not marked, not
      -- drained, and never recorded, so no deathtouch bit exists for the CR
      -- 704.5h SBA to read.
      DamageRewrite.PreventAll -> do
        consume (ReplacementCandidate.identity candidate)
        pure Nothing
      -- CR 615.7's shield covers as much of THIS event as it has left, and
      -- whatever it could not cover survives as a smaller event of the same
      -- source, recipient and riders. Nothing when it covered all of it, which
      -- is CR 615.6: a prevented event never happens.
      --
      -- No choice is made here, and none is owed: within one event CR 615.7
      -- leaves nothing to decide, since the prevention is neither optional nor
      -- divisible by anyone's say-so. The choice the rule DOES describe -- which
      -- of several simultaneous events the shield covers -- is asked one level
      -- up, in resolveDamageBatch.
      --
      -- NOT `consume`. That spends a row per APPLICATION, while CR 615.7's unit
      -- is the amount of damage rather than the number of events or sources
      -- dealing it. `setShield` writes the remainder back and drops the row at 0.
      DamageRewrite.PreventNext remaining -> do
        let amount = DamageEvent.amount de
            -- Both subtractions below are total on Natural: `prevented` is a min
            -- of the two operands, so it is no greater than either.
            prevented = min remaining amount
        setShield (ReplacementCandidate.identity candidate) pat (remaining - prevented)
        if prevented >= amount
          then pure Nothing
          else pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = amount - prevented}))
      -- CR 614.1a's "instead" with a flat amount (Galvanic Blast). Only the
      -- AMOUNT is rewritten, and that is the rule rather than economy: a
      -- replaced damage event keeps its source, its recipient and every
      -- deal-time rider it was proposed with.
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
    -- CR 701.19a: regeneration removes marked damage, taps the permanent and
    -- removes it from combat. The DESTRUCTION does not happen, so nothing
    -- downstream of it (a put-into-graveyard, and therefore Rest in Peace's
    -- redirect) ever runs.
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
    -- CR 614.16: Doubling Season scales token creation.
    (ReplacementEffect.TokenR _ scaling, ProposedEvent.WouldCreateTokens pid card n) -> do
      consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldCreateTokens pid card (scale scaling n)))
    -- Unreachable: `applies` admits TokenR only against WouldCreateTokens.
    (ReplacementEffect.TokenR _ _, _) -> pure (Just event)
    -- CR 614.1b / 614.10: a skip is "instead of doing X, do nothing", so the
    -- step or phase simply does not begin. Nothing is done first, unlike
    -- DamageRewrite.PreventAll's sibling arm: a skip has no consequence of its
    -- own to perform before it cancels.
    --
    -- The obligation the doc above places on every arm -- case on the inner sum
    -- rather than bind it with `_` -- has nothing to bind here: PhaseR carries a
    -- pattern and no rewrite, because CR 614.1b leaves a skip only one possible
    -- outcome. The day a PhaseRewrite exists, this arm owes it a case.
    --
    -- CR 614.10a's arithmetic -- two skip effects mean two occurrences skipped,
    -- one per instance -- falls out of the floating store's SHAPE rather than
    -- out of care taken here. Two Fatigues prepend two ActiveReplacements, and a
    -- list of instances with distinct timestamps cannot coalesce the way a Set of
    -- patterns or a Boolean flag would; `consume` below deletes by (source,
    -- timestamp), so it spends exactly the one that applied; and returning
    -- Nothing ENDS the CR 616.1 loop, so no second skip can be spent on the same
    -- step.
    --
    -- The occurrence skipped is the one the PATTERN named, which for Stonehorn
    -- Dignitary is a whole combat phase rather than a step of one. Nothing here
    -- has to know that: Engine.runStep raises the phase question exactly once per
    -- phase, so a whole-phase skip gets exactly one chance to apply.
    --
    -- Eon Hub's PhaseR reaches the same arm and consumes nothing: it is a
    -- permanent's static ability, so its CandidateId is OfPermanent and `consume`
    -- is a no-op for it. It is the store, not this arm, that tells the two apart.
    (ReplacementEffect.PhaseR _, ProposedEvent.WouldBeginPhase _ _) -> do
      consume (ReplacementCandidate.identity candidate)
      pure Nothing
    -- Unreachable: `applies` admits PhaseR only against WouldBeginPhase.
    (ReplacementEffect.PhaseR _, _) -> pure (Just event)

-- CR 208.2b / 707.2: stamp a chosen entry shape into the object's copiable
-- snapshot. Power and toughness are SET; keywords are UNIONED into whatever is
-- already there, which is what Primal Plasma's own Gatherer ruling requires -- a
-- 1/6 with flying and defender is only reachable if the choice ADDS defender to
-- a snapshot that already carries flying from the copy.
applyEntryOption :: ObjectId -> EntryOption.EntryOption -> GameState -> GameState
applyEntryOption oid option gs =
  let base = Projection.copiableCharacteristics oid gs
      stamped =
        base
          { PC.power = Just (EntryOption.power option),
            PC.toughness = Just (EntryOption.toughness option),
            -- Defensive, not load-bearing: CR 208.2a and CR 208.2b are
            -- alternatives, so a card with a ChoiceOf has no
            -- characteristic-defining ability and this field is already Nothing.
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
-- The object is in GameState.objects and its zone index BEFORE this runs,
-- because CR 614.12 asks for the permanent's characteristics AS IT WOULD EXIST
-- ON THE BATTLEFIELD -- a projection of the object in the state where it has
-- entered, so the cheapest correct implementation is to put it there and project
-- it normally. Nothing observes the interim object: this finishes before the
-- Moved event is recorded, so no trigger scan and no state-based action can see
-- it.
--
-- `Monad.void` discards the `Nothing` that means the event does not happen. Safe
-- here: every EntryR arm always returns `Just`, and only DamageR/DestructionR
-- ever return `Nothing`, neither of which pairs with WouldEnter -- the only
-- event this loop proposes.
--
-- Always the LIVE board (`Nothing`), even when the zone change containing this
-- entry belongs to a CR 608.2f batch: the entering object is not on the
-- pre-batch board at all, and CR 614.12 asks about now rather than about when
-- the containing event began. CR 616.1g recognizes an entry like this as an
-- event CONTAINED within another rather than a second member of the batch, but
-- speaks only to the ORDER the two events' effects are chosen in, not to which
-- board each collects from. That a contained event keeps its own footing is this
-- engine's reading, resting on CR 614.12; no rule states it outright.
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

-- CR 615.7: write back what is left of a shield after it prevented some damage,
-- and drop the row entirely once nothing is -- a row that can prevent nothing is
-- not a prevention effect (`unspent` above refuses it either way).
--
-- The floating twin of `consume`, and a separate function rather than a case in
-- it, because the two spend a row in different UNITS: `consume` spends CR
-- 614.3's use per application, this spends CR 615.7's shield per point of
-- damage. Both key on (source, timestamp), the row's CR 614.5 identity, which is
-- untouched by rewriting its `effect` -- so a partly-spent shield is the same
-- instance to the applied-set the CR 616.1 loop carries.
--
-- A permanent's static replacement ability is a no-op here, as in `consume`, and
-- for a stronger reason than having no row to write: CR 615.10's static shields
-- are deliberately NOT reduced, since they apply separately to each event. No
-- card can print a PreventNext at all (see Pawl.Types.DamageRewrite), so this
-- arm has no producer.
setShield :: CandidateId -> DamagePattern.DamagePattern -> Natural -> Game ()
setShield identity_ pat left = case identity_ of
  CandidateId.OfPermanent _ _ -> pure ()
  CandidateId.OfFloating src ts ->
    State.modify' $ \gs ->
      let mine active =
            ActiveReplacement.source active == src
              && ActiveReplacement.timestamp active == ts
          rewrite active
            | not (mine active) = Just active
            | left == 0 = Nothing
            | otherwise = Just active {ActiveReplacement.effect = ReplacementEffect.DamageR pat (DamageRewrite.PreventNext left)}
       in gs {GameState.replacements = Maybe.mapMaybe rewrite (GameState.replacements gs)}

-- CR 615: settle one proposed damage event. Nothing means it does not happen.
resolveDamage :: DamageEvent.DamageEvent -> Game (Maybe DamageEvent.DamageEvent)
resolveDamage de = do
  outcome <- applyReplacements (ProposedEvent.WouldDealDamage de)
  pure (outcome >>= asDamageEvent)

-- CR 608.2f / 510.2: settle a whole batch of SIMULTANEOUS damage events, and
-- answer the survivors. The typed door Pawl.Engine.Damage uses, so Damage never
-- cases on a ProposedEvent or on a ReplacementEffect.
--
-- Each event still runs its OWN CR 616.1 loop and the loop's unit is still one
-- event, which is what CR 614.5 and CR 615.10 both describe. The one thing this
-- adds over calling resolveDamage per event is the ORDER those loops run in,
-- because CR 615.7's shield is a single resource allocated across the whole
-- batch and the rule gives that choice to the shielded side, not to the engine.
resolveDamageBatch :: [DamageEvent.DamageEvent] -> Game [DamageEvent.DamageEvent]
resolveDamageBatch events = do
  ordered <- orderForShields events
  fmap Maybe.catMaybes (Monad.mapM resolveDamage ordered)

-- CR 615.7: when two or more applicable sources would deal damage to a shielded
-- recipient at the same time, that recipient chooses which damage the shield
-- prevents.
--
-- Asked as an ORDER over the contested events rather than as a pick, because a
-- pick repeated IS an order: applying a shield to an event covers as much of it
-- as the shield has left and no more, which nobody may decline or divide, so the
-- only freedom the rule grants is which event the shield reaches first.
--
-- CR 616.1's APNAP clause is honoured across choosers here, which is the one
-- place in this module that can honour it: a lone ProposedEvent has exactly one
-- affected object and therefore one chooser (#71).
orderForShields :: [DamageEvent.DamageEvent] -> Game [DamageEvent.DamageEvent]
orderForShields events = do
  gs <- State.get
  Monad.foldM askOne events (contested gs events)

-- Ask one chooser for the order of the positions their shields are contested
-- over, and splice the answer back into the batch.
--
-- The events keep their POSITIONS in the batch and only trade places with each
-- other, so a second chooser's group -- a different recipient's -- is neither
-- moved nor renumbered by the first's answer.
askOne :: [DamageEvent.DamageEvent] -> (PlayerId, [Natural]) -> Game [DamageEvent.DamageEvent]
askOne batch (pid, positions) = do
  gs <- State.get
  let byPosition :: Map.Map Natural DamageEvent.DamageEvent
      byPosition = Map.fromList (zip [0 ..] batch)
      group = Maybe.mapMaybe (\i -> Map.lookup i byPosition) positions
  case group of
    -- Unreachable: `contested` only reports a group of two or more.
    [] -> pure batch
    first : _ -> do
      let decider = Decide.deciderFor pid gs
      answer <- Trans.lift (Program.prompt (Prompt.OrderDamage decider pid group))
      -- Reject-not-repair, as `choose` and Engine's trigger ordering already do:
      -- an answer that is not a permutation of the offered indices leaves the
      -- canonical order standing rather than dropping or duplicating an event.
      let permuted =
            if List.sort answer == zipWith const [0 ..] group
              then fmap (\i -> at group i first) answer
              else group
          moved = Map.fromList (zip positions permuted)
      pure (fmap (\(i, e) -> Map.findWithDefault e i moved) (zip [0 ..] batch))

-- The batch positions one chooser's prevention shields have to be allocated
-- across, one entry per chooser, in CR 616.1's APNAP order.
--
-- A shield is CONTESTED when it admits two or more of the batch's events (CR
-- 615.7) and cannot cover all of them -- the comparison is against the total
-- DAMAGE those events would deal, not against their number, since CR 615.7's
-- unit is the amount. A shield large enough to cover the lot prevents all of it
-- whatever the order, so there is nothing to ask.
--
-- Several shields contribute ONE question per CHOOSER, over the union of what
-- they contest: the order the batch is settled in is a single fact about the
-- batch, and asking twice would ask the same player to state it twice.
--
-- The union is per chooser rather than per recipient, so one player shielding
-- two different creatures orders both creatures' events in one answer. That is a
-- WIDER question than either shield needs -- neither shield can reach the
-- other's events -- but a superset of a question is still the player's answer,
-- and splitting it would ask the same player twice about one batch.
contested :: GameState -> [DamageEvent.DamageEvent] -> [(PlayerId, [Natural])]
contested gs events =
  let indexed :: [(Natural, DamageEvent.DamageEvent)]
      indexed = zip [0 ..] events
      hitsOf candidate = filter (\entry -> applies gs (ProposedEvent.WouldDealDamage (snd entry)) candidate) indexed
      contestedBy candidate = do
        remaining <- shieldRemaining (ReplacementCandidate.effect candidate)
        case hitsOf candidate of
          hits@(firstHit : _ : _)
            | remaining < sum (fmap (DamageEvent.amount . snd) hits) -> do
                -- CR 615.7's chooser is CR 616.1's, read off the shielded
                -- recipient -- and every hit of ONE shield shares that
                -- recipient, since Resolve's PreventNextDamage arm always names
                -- one, so the head is the whole answer. Nothing means the
                -- shielded object has left and no one is there to be asked.
                pid <- chooserOf gs (ProposedEvent.WouldDealDamage (snd firstHit))
                pure (pid, fmap fst hits)
          _ -> Nothing
      groups = Maybe.mapMaybe contestedBy (collect gs (GameState.replacements gs))
      merged = Map.fromListWith (<>) groups
      order = Game.apnapOrder gs
      -- A chooser off the seating roster sorts last, the fallback
      -- Resolve.objectRefObjects takes for the same lookup.
      seated pid = Maybe.fromMaybe (length order) (List.elemIndex pid order)
   in [ (pid, List.sort (List.nub positions))
      | (pid, positions) <- List.sortOn (seated . fst) (Map.toList merged)
      ]

-- CR 615.7: how much of a prevention shield is left, or Nothing for an effect
-- that is not one.
--
-- A CLASSIFICATION of effects -- what SHAPE an effect has, never which effect it
-- is -- in the same genre as bucketOf and readsApplier above. One arm per
-- constructor, no wildcard, so a new rewrite that counts damage down breaks the
-- build here rather than silently going unasked about.
shieldRemaining :: ReplacementEffect -> Maybe Natural
shieldRemaining re = case re of
  ReplacementEffect.DamageR _ rewrite -> case rewrite of
    DamageRewrite.PreventNext remaining -> Just remaining
    -- Fog is unlimited for its duration, so there is nothing to allocate: it
    -- prevents every event it admits and the order cannot matter.
    DamageRewrite.PreventAll -> Nothing
    DamageRewrite.SetAmount _ -> Nothing
    DamageRewrite.Scale _ -> Nothing
  ReplacementEffect.ZoneChangeR _ _ -> Nothing
  ReplacementEffect.EntryR _ _ -> Nothing
  ReplacementEffect.DestructionR _ -> Nothing
  ReplacementEffect.CounterR _ _ -> Nothing
  ReplacementEffect.TokenR _ _ -> Nothing
  ReplacementEffect.PhaseR _ -> Nothing

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
-- `asOf` is applyReplacementsIn's, and the destroy funnel always supplies it: CR
-- 608.2f gives even a single Doom Blade a one-element batch, and when that batch
-- is itself part of a CR 704.3 pass the board is the pass's rather than the
-- batch's (Event.destroyInBatch).
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
-- countering of a spell). CR 122.6 makes this the right single seam, since it
-- covers both counters put on a permanent already on the battlefield and
-- counters an object is given as it enters. A zero count after the loop puts
-- nothing on.
--
-- It lives HERE rather than beside the other change-and-emit funnels in
-- Pawl.Engine.Event because CR 122.6's as-it-enters clause is served by `apply`'s
-- EntryRewrite.WithCounters arm above, and Pawl.Engine.Event already depends on
-- this module. A copy of the body there would be a second funnel, which is the
-- one thing a funnel must not have.
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
-- of `pid`. False means a skip took it, and proceeding past it is then the
-- caller's whole obligation -- CR 614.1b replaces a skipped step with nothing, so
-- there is no rewritten event to carry out. How far past it reaches is the
-- caller's too: one schedule entry for a PhaseSelector.Step, the phase's
-- remaining entries for a whole phase (Engine.runStep, Turn.dropRestOfPhase).
--
-- Answers a Bool rather than the settled event, unlike resolveDestruction, whose
-- `Just` had to carry an identity because a rewrite can redirect which object is
-- destroyed. Nothing can rewrite a WouldBeginPhase: PhaseR is the only effect the
-- class admits and it only ever cancels.
--
-- The typed door Pawl.Engine.Engine uses, so Engine never cases on a
-- ProposedEvent.
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
-- rows: the two differ only on WHEN the row exists, and this module is the one
-- that reads GameState.replacements. The row is the same shape Resolve builds --
-- PhaseR, scoped to the taker, Uses.Once -- so `beginsPhase` answers a
-- turn-scoped skip and a next-occurrence skip through one mechanism, and CR
-- 616.1's loop orders them against each other for free.
--
-- Installed AT THE TURN'S START rather than at the resolution that created the
-- turn, which is the whole point: CR 614.10a's "next" would otherwise name
-- whatever step came first in the meantime, and CR 500.7 lets that be a different
-- turn entirely. CR 614.10 bars skipping a step, phase or turn that has already
-- started; nothing of this turn has started yet, since Engine.beginTurnOf has
-- only scheduled it and Engine.runStep asks `beginsPhase` before the untap step's
-- first observable moment.
--
-- Expiry.AtCleanup, not Never, and not because of CR 514.2 -- the card states no
-- duration. The skip names ONE turn and cannot apply to another, so the last
-- moment of that turn is the last moment it could matter, and AtCleanup is the
-- store for exactly that. Uses.Once is CR 614.10a's per-occurrence spend, and it
-- is what actually fires for the one card in the pool. Not implemented: the
-- sweep does not run at all on a turn whose ending phase was skipped, so the
-- expiry alone would not hold an unspent row to its turn (#491).
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
                  -- names its player outright and has no ControllerRelation to
                  -- resolve. The TAKER rather than the effect's controller,
                  -- because these skips ride the turn (see Pawl.Types.ExtraTurn)
                  -- and the effect that created it is long gone by the time the
                  -- turn begins.
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
