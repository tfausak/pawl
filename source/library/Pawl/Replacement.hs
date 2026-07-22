-- CR 616.1's loop: the SOLE home of casing on ProposedEvent and
-- ReplacementEffect, a fourth sole-casing home beside Pawl.Resolve (Effect),
-- Pawl.Event (TriggerCondition / StateCondition) and Pawl.Projection
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
-- This module must NOT import Pawl.Event: Event raises proposed events through
-- this loop, so the dependency runs one way only. That is also why the entry
-- copy-target legal set lives here rather than in Pawl.Target (Task 7) -- Target
-- imports Pawl.Sba, which imports Pawl.Event.
module Pawl.Replacement where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Decide as Decide
import qualified Pawl.Projection as Projection
import Pawl.Type.CandidateId (CandidateId)
import qualified Pawl.Type.CandidateId as CandidateId
import Pawl.Type.ControllerRelation (ControllerRelation)
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.DamageEvent as DamageEvent
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import Pawl.Type.ProposedEvent (ProposedEvent)
import qualified Pawl.Type.ProposedEvent as ProposedEvent
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.ReplacementBucket (ReplacementBucket)
import qualified Pawl.Type.ReplacementBucket as ReplacementBucket
import Pawl.Type.ReplacementCandidate (ReplacementCandidate)
import qualified Pawl.Type.ReplacementCandidate as ReplacementCandidate
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import Pawl.Type.ZoneChange (ZoneChange)
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern

-- CR 614: settle a proposed zone change. Nothing means the move does not happen.
-- The typed door Pawl.Event uses, so Event never cases on a ProposedEvent.
resolveZoneChange :: ZoneChange -> Game (Maybe ZoneChange)
resolveZoneChange zc = do
  outcome <- applyReplacements (ProposedEvent.WouldChangeZone zc)
  pure (outcome >>= asZoneChange)

asZoneChange :: ProposedEvent -> Maybe ZoneChange
asZoneChange event = case event of
  ProposedEvent.WouldChangeZone zc -> Just zc
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed _ -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing

-- CR 616.1's loop. `Nothing` means the event DOES NOT HAPPEN -- CR 615.6's
-- prevented damage, CR 701.19a's replaced destruction. A rewrite that cancels an
-- event has already performed its own consequences by the time it returns
-- Nothing.
applyReplacements :: ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacements = loop Set.empty

loop :: Set CandidateId -> ProposedEvent -> Game (Maybe ProposedEvent)
loop applied event = do
  gs <- State.get
  -- Step 1, from scratch each iteration: collect against the CURRENT state, minus
  -- CR 614.5's already-applied set. Re-collecting is what makes CR 616.2 work --
  -- an effect that only became applicable because of the last application is
  -- picked up here.
  let unused candidate = not (Set.member (ReplacementCandidate.identity candidate) applied)
      fresh = filter unused (applicable gs event)
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
          outcome <- apply candidate event
          case outcome of
            Nothing -> pure Nothing
            Just rewritten -> loop (Set.insert (ReplacementCandidate.identity candidate) applied) rewritten

-- Every replacement effect instance in the game, in the engine's canonical order:
-- battlefield permanents ascending by id, each permanent's own effects in printed
-- order. That order is what the ChooseReplacement prompt indexes into.
collect :: GameState -> [ReplacementCandidate]
collect gs =
  let fromPermanent (src, re) =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity = CandidateId.OfPermanent src re,
            ReplacementCandidate.effect = re,
            ReplacementCandidate.source = src
          }
   in map fromPermanent (Projection.replacementsAffecting gs)

applicable :: GameState -> ProposedEvent -> [ReplacementCandidate]
applicable gs event = filter (applies gs event) (collect gs)

-- CR 614.1: does this instance apply to this proposed event? The arms must agree
-- on the EVENT CLASS -- which the type already rules out for the impossible pairs
-- -- and the pattern must admit the event's subject.
applies :: GameState -> ProposedEvent -> ReplacementCandidate -> Bool
applies gs event candidate =
  let src = ReplacementCandidate.source candidate
   in case (ReplacementCandidate.effect candidate, event) of
        (ReplacementEffect.ZoneChangeR pat _, ProposedEvent.WouldChangeZone zc) ->
          ZoneChange.to zc == ZoneChangePattern.whenDestination pat
            && matchesController gs src (ZoneChangePattern.whoseObject pat) (ZoneChange.object zc)
        -- Tasks 4, 5, 6, 7 and 9 fill these in as each funnel starts raising its
        -- event; until then no such ProposedEvent is ever constructed.
        (ReplacementEffect.ZoneChangeR _ _, _) -> False
        (ReplacementEffect.EntryR _, _) -> False
        (ReplacementEffect.DamageR _ _, _) -> False
        (ReplacementEffect.DestructionR _, _) -> False
        (ReplacementEffect.CounterR _ _, _) -> False
        (ReplacementEffect.TokenR _ _, _) -> False

-- CR 109.5 / 614.1: does `oid` satisfy this pattern's controller relation, read
-- against the controller of the effect's SOURCE? Anyones always does.
matchesController :: GameState -> ObjectId -> ControllerRelation -> ObjectId -> Bool
matchesController gs src rel oid = case rel of
  ControllerRelation.Anyones -> True
  ControllerRelation.Yours -> Projection.controllerOf oid gs == Projection.controllerOf src gs

-- CR 616.1a-e: take the HIGHEST non-empty bucket. Ord on ReplacementBucket is
-- ascending in the CR's own order, so that is the minimum present; the fold seeds
-- from Other (the largest) so it needs no partial `minimum`.
highestBucket :: [ReplacementCandidate] -> [ReplacementCandidate]
highestBucket candidates =
  let bucketed = map (\c -> (bucketOf (ReplacementCandidate.effect c), c)) candidates
      best = List.foldl' min ReplacementBucket.Other (map fst bucketed)
   in map snd (filter (\entry -> fst entry == best) bucketed)

-- CR 616.1a-e: which bucket an effect falls in.
bucketOf :: ReplacementEffect -> ReplacementBucket
bucketOf re = case re of
  -- CR 616.1e. Every arm lands in Other today -- no arm has a CopyOnEntry (or
  -- any other non-Other) producer yet. EntryR is here too: splitting its AsCopy
  -- case into CopyOnEntry is a later task's job (see ReplacementBucket's
  -- comment), not this one's.
  ReplacementEffect.ZoneChangeR _ _ -> ReplacementBucket.Other
  ReplacementEffect.EntryR _ -> ReplacementBucket.Other
  ReplacementEffect.DamageR _ _ -> ReplacementBucket.Other
  ReplacementEffect.DestructionR _ -> ReplacementBucket.Other
  ReplacementEffect.CounterR _ _ -> ReplacementBucket.Other
  ReplacementEffect.TokenR _ _ -> ReplacementBucket.Other

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
-- mutates state keyed by, which source is applying. Every future `apply` arm
-- must preserve that independence; the day one does not, this elision starts
-- silently choosing between two applications that produce different boards --
-- deciding for a player who was never asked, the second invariant's violation.
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
          answer <- Trans.lift (Program.prompt (Prompt.ChooseReplacement decider pid (map ReplacementCandidate.source candidates)))
          -- Reject-not-repair, as payment and Engine.permute already do: an
          -- out-of-range index leaves the canonical first standing rather than
          -- dropping the event or crashing.
          pure (Just (at candidates answer first))

-- Total index into a list, with a fallback.
at :: [a] -> Natural -> a -> a
at xs i fallback = case drop (fromIntegral i) xs of
  h : _ -> h
  [] -> fallback

-- CR 616.1 / 108.4: who decides. Projection.controllerOf already falls back to
-- the owner (CR 108.4), so "or its owner if it has no controller" is free.
--
-- CR 616.1's APNAP clause -- "If two or more players have to make these choices at
-- the same time, choices are made in APNAP order (see rule 101.4)" -- has no
-- producer: one proposed event has exactly one affected object and therefore one
-- chooser, and the damage batch runs each event's loop independently (#N).
chooserOf :: GameState -> ProposedEvent -> Maybe PlayerId
chooserOf gs event = case event of
  ProposedEvent.WouldChangeZone zc -> Projection.controllerOf (ZoneChange.object zc) gs
  ProposedEvent.WouldEnter oid -> Projection.controllerOf oid gs
  ProposedEvent.WouldDealDamage de -> case DamageEvent.target de of
    Recipient.ToPlayer pid -> Just pid
    Recipient.ToCreature oid -> Projection.controllerOf oid gs
    Recipient.ToObject oid -> Projection.controllerOf oid gs
  ProposedEvent.WouldBeDestroyed oid -> Projection.controllerOf oid gs
  ProposedEvent.WouldPutCounters oid _ _ -> Projection.controllerOf oid gs
  ProposedEvent.WouldCreateTokens pid _ _ -> Just pid

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
apply :: ReplacementCandidate -> ProposedEvent -> Game (Maybe ProposedEvent)
apply candidate event =
  case (ReplacementCandidate.effect candidate, event) of
    (ReplacementEffect.ZoneChangeR _ toDest, ProposedEvent.WouldChangeZone zc) ->
      pure (Just (ProposedEvent.WouldChangeZone zc {ZoneChange.to = toDest}))
    -- Unreachable: `applies` admits ZoneChangeR only against WouldChangeZone.
    (ReplacementEffect.ZoneChangeR _ _, _) -> pure (Just event)
    -- CR 614.1c: Clone (AsCopy) / Primal Plasma (ChoiceOf) rewrite the copiable
    -- snapshot of an entering permanent. Waiting on the WouldEnter funnel.
    (ReplacementEffect.EntryR _, _) -> pure (Just event)
    -- CR 615.1/615.6: a Fog-shaped shield cancels a damage event outright.
    -- Waiting on the WouldDealDamage funnel.
    (ReplacementEffect.DamageR _ _, _) -> pure (Just event)
    -- CR 614.8/701.19a: regeneration rewrites a would-be-destroyed event so the
    -- destruction never happens. Waiting on the WouldBeDestroyed funnel.
    (ReplacementEffect.DestructionR _, _) -> pure (Just event)
    -- CR 122.6: Hardened Scales/Doubling Season scale a counter placement.
    -- Waiting on the WouldPutCounters funnel.
    (ReplacementEffect.CounterR _ _, _) -> pure (Just event)
    -- CR 111.1: Doubling Season scales token creation. Waiting on the
    -- WouldCreateTokens funnel.
    (ReplacementEffect.TokenR _ _, _) -> pure (Just event)
