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

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Binding as Binding
import qualified Pawl.Decide as Decide
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import Pawl.Type.CandidateId (CandidateId)
import qualified Pawl.Type.CandidateId as CandidateId
import Pawl.Type.ControllerRelation (ControllerRelation)
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.CounterPattern as CounterPattern
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamagePattern as DamagePattern
import qualified Pawl.Type.DamageRewrite as DamageRewrite
import qualified Pawl.Type.DestructionRewrite as DestructionRewrite
import qualified Pawl.Type.EntryRewrite as EntryRewrite
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.PermanentCriterion as PermanentCriterion
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
import qualified Pawl.Type.Scaling as Scaling
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Uses as Uses
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
applyReplacements = applyReplacementsIn Set.empty

-- CR 614.13a: `batch` is the set of ids entering the battlefield AT THE SAME TIME
-- as the object this loop is about -- "You can't choose the object that will
-- become that permanent or any other object entering the battlefield at the same
-- time as that object." Clone's own ruling restates it: "If Clone somehow enters
-- at the same time as another creature, Clone can't become a copy of that
-- creature."
--
-- Empty for every event class but a nested entry, and empty even for a lone entry
-- (nothing else is entering). IMPLEMENTED BUT UNTESTED: no real card in reach
-- puts two copy-choosers onto the battlefield simultaneously (#N).
applyReplacementsIn :: Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacementsIn batch = loop batch Set.empty

loop :: Set ObjectId -> Set CandidateId -> ProposedEvent -> Game (Maybe ProposedEvent)
loop batch applied event = do
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
          outcome <- apply batch candidate event
          case outcome of
            Nothing -> pure Nothing
            Just rewritten -> loop batch (Set.insert (ReplacementCandidate.identity candidate) applied) rewritten

-- Every replacement effect instance in the game, in the engine's canonical order.
-- Two segments, concatenated in this order:
--
--   1. PERMANENT abilities (Projection.replacementsAffecting): battlefield
--      permanents ascending by id, each permanent's own effects in printed
--      order.
--   2. The FLOATING store (GameState.replacements): newest first -- Resolve.hs
--      prepends each new ActiveReplacement onto the front of the list as it is
--      created, so the most recently resolved floating replacement is collected
--      before any older one.
--
-- That concatenated order is what the ChooseReplacement prompt indexes into.
collect :: GameState -> [ReplacementCandidate]
collect gs =
  let fromPermanent (src, re) =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity = CandidateId.OfPermanent src re,
            ReplacementCandidate.effect = re,
            ReplacementCandidate.source = src
          }
      fromFloating active =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity = CandidateId.OfFloating (ActiveReplacement.source active) (ActiveReplacement.timestamp active),
            ReplacementCandidate.effect = ActiveReplacement.effect active,
            ReplacementCandidate.source = ActiveReplacement.source active
          }
   in map fromPermanent (Projection.replacementsAffecting gs)
        ++ map fromFloating (GameState.replacements gs)

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
        (ReplacementEffect.DamageR pat _, ProposedEvent.WouldDealDamage de) ->
          case DamagePattern.whichKind pat of
            -- CR 615.1: no kind named means every damage event.
            Nothing -> True
            Just kind -> DamageEvent.kind de == kind
        -- CR 201.5 / 201.5c / 701.19a: "regenerate THIS creature" names the
        -- ability's own source, so a destruction replacement is self-only.
        -- DestructionR carries no pattern because the only producer in the
        -- card pool is self-regeneration (CR 701.19a).
        (ReplacementEffect.DestructionR _, ProposedEvent.WouldBeDestroyed oid) -> src == oid
        (ReplacementEffect.CounterR pat _, ProposedEvent.WouldPutCounters oid kind _) ->
          -- CR 614.16: `whichKind = Nothing` means any kind, never no kind.
          maybe True (== kind) (CounterPattern.whichKind pat)
            && matchesController gs src (CounterPattern.whose pat) oid
            && matchesPermanent gs (CounterPattern.onWhat pat) oid
        -- Every row below falls through to False, but for two different reasons.
        -- ZoneChangeR, DamageR, DestructionR, CounterR and (since Task 7) EntryR
        -- are unreachable HERE because an arm ABOVE already matches every event of
        -- that class -- a row below only fires for a MISMATCHED class (e.g. a
        -- DestructionR candidate offered a WouldChangeZone event), where False is
        -- simply the correct answer, not a stand-in for "not yet implemented".
        -- TokenR is unreachable for the OTHER reason: nothing in the engine raises
        -- the ProposedEvent it would need to match (WouldCreateTokens has no
        -- producer), so every pair naming it is unreachable in practice and False
        -- is correct until one does.
        -- CR 614.1c: "as [this permanent] enters" is the entering object's OWN
        -- ability, so an entry replacement is self-only. CR 614.1d's
        -- other-objects form (Essence of the Wild) has no producer.
        (ReplacementEffect.EntryR _, ProposedEvent.WouldEnter oid) -> src == oid
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

-- CR 614.16: which permanents a pattern admits. P9 generalizes this.
matchesPermanent :: GameState -> PermanentCriterion.PermanentCriterion -> ObjectId -> Bool
matchesPermanent gs crit oid = case crit of
  PermanentCriterion.AnyPermanent -> True
  -- CR 205.2b / 300.2 / 613.1d: creature-ness is the PROJECTED question, so an
  -- Opalescence'd enchantment counts.
  PermanentCriterion.CreaturePermanent -> Projection.isCreatureOf oid gs

-- CR 614.16: apply a scaling to a count. "That many plus one" and "twice that
-- many" are the same operation with different data.
scale :: Scaling.Scaling -> Natural -> Natural
scale s n = case s of
  Scaling.Multiply m -> n * m
  Scaling.AddMore m -> n + m

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
  ReplacementEffect.ZoneChangeR _ _ -> ReplacementBucket.Other
  -- CR 616.1c: "an effect that would cause an object to become a copy of another
  -- object as it enters" is its own, HIGHER bucket. That is what makes the
  -- centerpiece work: a Clone's copy applies before the Primal Plasma choice it
  -- thereby acquires (616.1e), not after.
  ReplacementEffect.EntryR EntryRewrite.AsCopy -> ReplacementBucket.CopyOnEntry
  ReplacementEffect.EntryR (EntryRewrite.ChoiceOf _) -> ReplacementBucket.Other
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
--
-- The same discipline applies one level down, to each arm's inner SUM type --
-- DamageR's DamageRewrite below, DestructionR's DestructionRewrite, EntryR's
-- EntryRewrite (Task 7), and (as later work fills that arm in) TokenR's Scaling
-- -- never to the pattern RECORDS (CounterPattern, TokenPattern, DamagePattern),
-- which are read for their fields rather than cased and so have no constructors
-- to be exhaustive over. An arm must case on the inner sum, not bind it with
-- `_`: binding it with `_` is exhaustive UNCONDITIONALLY -- that is exactly why
-- it raises no build failure and no warning when a new constructor is added,
-- silently treating a real rewrite as a no-op from that day on.
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
    (ReplacementEffect.EntryR rewrite, ProposedEvent.WouldEnter oid) -> case rewrite of
      EntryRewrite.AsCopy -> do
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable: the object is materialized on the battlefield before this
          -- loop runs, so controllerOf falls back to its owner. Defensive: make no
          -- unprompted copy choice.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            chosen <- Trans.lift (Program.prompt (Prompt.ChooseCopyTarget decider controller oid (legalCopyTargets batch oid gs)))
            case chosen of
              Nothing -> pure (Just event)
              Just src2 -> do
                State.modify' $ \g ->
                  let stamp o = o {Object.bindings = Binding.setCopy (Projection.copiableCharacteristics src2 g) (Object.bindings o)}
                   in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
                pure (Just event)
      -- CR 614.1c / 208.2b: Primal Plasma's own choice (which creature type to
      -- become). No producer yet -- the card pool has no ChoiceOf source -- but the
      -- inner sum is cased exhaustively (per the module comment) so a future
      -- producer cannot silently no-op here.
      EntryRewrite.ChoiceOf _ -> pure (Just event)
    -- Unreachable: `applies` admits EntryR only against WouldEnter.
    (ReplacementEffect.EntryR _, _) -> pure (Just event)
    (ReplacementEffect.DamageR _ rewrite, ProposedEvent.WouldDealDamage _) -> case rewrite of
      -- CR 615.6: a prevented event never happens -- it is not marked, not
      -- drained, and never recorded, so no deathtouch bit exists for the CR
      -- 704.5h SBA to read.
      DamageRewrite.PreventAll -> do
        consume (ReplacementCandidate.identity candidate)
        pure Nothing
    -- Unreachable: `applies` admits DamageR only against WouldDealDamage.
    (ReplacementEffect.DamageR _ _, _) -> pure (Just event)
    -- CR 701.19a: "The next time [permanent] would be destroyed this turn,
    -- instead remove all damage marked on it and its controller taps it. If
    -- it's an attacking or blocking creature, remove it from combat." The
    -- DESTRUCTION does not happen -- so nothing downstream of it (a
    -- put-into-graveyard, and therefore Rest in Peace's redirect) ever runs.
    -- That nesting was hardcoded in Event.destroy before P5; it is structural
    -- now.
    (ReplacementEffect.DestructionR rewrite, ProposedEvent.WouldBeDestroyed oid) -> case rewrite of
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
    -- CR 614.16: Doubling Season scales token creation. Waiting on the
    -- WouldCreateTokens funnel.
    (ReplacementEffect.TokenR _ _, _) -> pure (Just event)

-- CR 707.5 / 614.13a: the permanents an entering copy may choose. Battlefield
-- creatures other than itself, minus anything entering in the same batch.
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
runEntry :: Set ObjectId -> ObjectId -> Game ()
runEntry batch oid = Monad.void (applyReplacementsIn batch (ProposedEvent.WouldEnter oid))

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
  ProposedEvent.WouldBeDestroyed _ -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing

-- CR 701.8 / 614.8: settle a proposed destruction. True means the permanent is
-- actually destroyed; False means a replacement took it (regeneration), and that
-- rewrite has already done its own work.
resolveDestruction :: ObjectId -> Game Bool
resolveDestruction oid = do
  outcome <- applyReplacements (ProposedEvent.WouldBeDestroyed oid)
  pure (Maybe.isJust outcome)

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
  ProposedEvent.WouldBeDestroyed _ -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
