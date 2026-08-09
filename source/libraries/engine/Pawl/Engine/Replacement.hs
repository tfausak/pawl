-- CR 616.1's SELECTION half: which replacement effects exist, which of them
-- apply to a proposed event, how they bucket and order, who chooses among them,
-- and how a chosen row is spent. Everything here answers a question ABOUT an
-- event; nothing here performs one.
--
-- The other half -- CR 616.1's loop itself and `apply`, which carries out the
-- chosen rewrite -- lives in Pawl.Engine.Event, next to the CR 400.7 zone-change
-- funnel. That is not a preference: the two are mutually recursive by the rules.
-- A zone change raises its event through the loop, because CR 614.1's replacement
-- effects "watch for a particular event that would happen" and a zone change is
-- one of those, and a chosen rewrite can itself change zones -- CR 614.1c's "as this permanent enters,
-- sacrifice any number of permanents" is a replacement whose application is a CR
-- 701.21a sacrifice. One module has to hold both ends of that cycle, and it is
-- the performing one.
--
-- So this module must NOT import Pawl.Engine.Event, and the classification it
-- exports is what Event calls down into. That is also why the entry copy-target
-- legal set lives here rather than in Pawl.Engine.Target -- Target imports
-- Pawl.Engine.Sba, which imports Pawl.Engine.Event.
--
-- Casing on ProposedEvent and ReplacementEffect is shared with Pawl.Engine.Event
-- and nowhere else, beside Pawl.Engine.Resolve (Effect) and
-- Pawl.Engine.Projection (Modification). Pawl.Codec also cases on
-- ReplacementEffect, but only as the JSON data boundary, never to decide game
-- behaviour.
module Pawl.Engine.Replacement where

import qualified Control.Monad as Monad
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
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.SacrificeRestriction as SacrificeRestriction
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
import Pawl.Types.Prevention (Prevention)
import qualified Pawl.Types.Prevention as Prevention
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
import Pawl.Types.ReplacementEntry (ReplacementEntry)
import qualified Pawl.Types.ReplacementEntry as ReplacementEntry
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.SourceRelation as SourceRelation
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.Uses as Uses
import Pawl.Types.ZoneChange (ZoneChange)
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern

asZoneChange :: ProposedEvent -> Maybe ZoneChange
asZoneChange event = case event of
  ProposedEvent.WouldChangeZone zc -> Just zc
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing

-- Every replacement effect instance in the game, in the engine's canonical
-- order, which is what the ChooseReplacement prompt indexes into:
--
--   1. PERMANENT abilities (Projection.replacementsAffecting): battlefield
--      permanents ascending by id, each permanent's own effects in printed
--      order. Read from `sources`, which for a CR 608.2f batch is the board the
--      batch began in rather than the live one (see Event.applyReplacementsIn).
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
            -- CR 614.3's lifetime, which a static ability does not have: this
            -- segment is re-derived from the battlefield on every iteration, and
            -- `consume` is a no-op for it. So there is no duration to expire and
            -- no use to spend, which is what Nothing says -- not an unknown one.
            ReplacementCandidate.lifetime = Nothing,
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
            -- CR 614.3, read straight off the row: this is the segment that has a
            -- lifetime, and `consume` spends only the row that applied. Two rows
            -- alike in `effect` and unlike here are therefore NOT interchangeable
            -- to `choose` below.
            ReplacementCandidate.lifetime = Just (ActiveReplacement.expiry active, ActiveReplacement.uses active),
            -- CR 614.15: a floating row IS an effect of a resolving spell or
            -- ability, so this is the one segment that can carry a
            -- self-replacement, and the row itself says whether it does.
            ReplacementCandidate.origin = ActiveReplacement.origin active
          }
   in fmap fromPermanent (Projection.replacementsAffecting sources)
        <> fmap fromFloating floating

-- The candidates that apply to this event. `asOf` is Nothing for a lone event
-- and Just the pre-batch board for a CR 608.2f batch (see Event.applyReplacementsIn);
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
  -- CR 614.9's redirection has nothing to spend: it moves the recipient and
  -- leaves the amount alone, so no application can exhaust it.
  DamageRewrite.Redirect _ -> True

applies :: GameState -> ProposedEvent -> ReplacementCandidate -> Bool
applies gs event candidate =
  let src = ReplacementCandidate.source candidate
   in case (ReplacementCandidate.effect candidate, event) of
        -- CR 614.1a: which zone changes this redirect intercepts -- the
        -- destination, the moving object's OWNER, and (Anafenza, the Foremost's
        -- "a nontoken creature") what the moving object IS.
        (ReplacementEffect.ZoneChangeR pat _, ProposedEvent.WouldChangeZone zc) ->
          ZoneChange.to zc == ZoneChangePattern.whenDestination pat
            && matchesZoneOwner gs src (ZoneChangePattern.whoseObject pat) (ZoneChange.object zc)
            && matchesFiltered gs candidate (ZoneChangePattern.whatObject pat) (ZoneChange.object zc)
        -- CR 615.1: which events the pattern admits (see matchesDamagePattern),
        -- plus the one fact about the ROW rather than the event -- a shield
        -- spent to nothing is no longer a prevention effect.
        (ReplacementEffect.DamageR pat rewrite, ProposedEvent.WouldDealDamage de) ->
          matchesDamagePattern (Just src) pat de && unspent rewrite
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
            && matchesPermanent gs Nothing (CounterPattern.onWhat pat) oid
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
        (ReplacementEffect.EntryR pat _, ProposedEvent.WouldEnter oid) -> matchesFiltered gs candidate pat oid
        -- CR 614.1e: which permanents turning face up this replacement watches.
        -- The same Filter language the entry arm above uses, and every producer
        -- writes Filter.IsSource -- CR 614.1e's printed wording is always "as
        -- THIS permanent is turned face up".
        --
        -- Matched against the LIVE projection, which for this event is the
        -- permanent as it is turning over: FaceDown.turnFaceUp has already
        -- written Facing.FaceUp when it raises the event, so CR 708.11's "would
        -- have ... after it's turned face up" is answered by asking about the
        -- board rather than by a counterfactual.
        (ReplacementEffect.TurnUpR pat _, ProposedEvent.WouldTurnFaceUp oid) -> matchesFiltered gs candidate pat oid
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
        (ReplacementEffect.TurnUpR _ _, _) -> False
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
-- (Circle of Protection: Red, Questing Beast's "creatures you control") rather
-- than by identity (#588).
--
-- The source is a MAYBE because CR 615.12's carrier has one: a "damage can't be
-- prevented" effect stored by a resolution has no permanent behind it. Nothing
-- names no object, so TheSource -- which is the "THIS creature" of a printed
-- clause -- cannot be satisfied by it, and no printing writes that pair anyway.
matchesDamageSource :: Maybe ObjectId -> SourceRelation.SourceRelation -> DamageEvent.DamageEvent -> Bool
matchesDamageSource src relation de = case relation of
  SourceRelation.AnySource -> True
  SourceRelation.TheSource -> src == Just (DamageEvent.source de)

-- CR 615.1 / 614.1a: does this damage event have the qualities the pattern
-- names? Three of them, and they are the three a printed clause narrows by: a
-- pattern naming no KIND admits combat and noncombat alike (CR 510.2's dealing
-- versus CR 608's), one naming TheSource admits only the damage its own source
-- is dealing (CR 120.1's "an object that deals damage is the source of that
-- damage", keyed as CR 614.15 keys it), and one naming a RECIPIENT admits only
-- the damage addressed to the permanent or player it names (CR 615.7).
--
-- ONE reading of what a DamagePattern means, shared by the two questions that
-- ask it: `applies` above, for CR 615.1's shields and CR 614.1a's replacements,
-- and `preventable` below, for CR 615.12's narrowed "can't be prevented"
-- (Excruciator). A second reading would let Excruciator's clause and Fog's
-- disagree about what "combat damage from this source" means.
--
-- The rewrite is NOT asked about here, though `applies` asks `unspent` right
-- after: a spent shield is a row that no longer exists as a prevention effect,
-- which is a fact about the ROW and not about which events the pattern admits.
matchesDamagePattern :: Maybe ObjectId -> DamagePattern.DamagePattern -> DamageEvent.DamageEvent -> Bool
matchesDamagePattern src pat de =
  maybe True (== DamageEvent.kind de) (DamagePattern.whichKind pat)
    && matchesDamageSource src (DamagePattern.whichSource pat) de
    && maybe True (== DamageEvent.target de) (DamagePattern.whichRecipient pat)

-- CR 614.1: does this ZONE CHANGE's object satisfy the pattern's relation?
--
-- The subject is the object's OWNER, not its controller, and that is a rules
-- fact rather than a convenience: CR 400.3 and CR 404.1 make the destination
-- zone the owner's, so Leyline of the Void's "an opponent's graveyard" asks who
-- OWNS the card. A creature stolen with Act of Treason still dies to its owner's
-- graveyard, which a controller-based test would get backwards.
--
-- Split out of matchesController, which stays controller-based for CR 109.5's
-- "you" on a counter or token pattern. Leyline of the Void and Anafenza, the
-- Foremost both name Opponents, and Pawl.EventSpec's Anafenza group is what makes
-- the relation observable: her controller's own dying creature reaches her own
-- graveyard where an opponent's is exiled. What is NOT exercised is the owner /
-- controller difference itself -- no test steals a creature and then kills it --
-- so the argument for reading the owner stays the rules one above.
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
-- The SOURCE is a parameter rather than the perspective's fixed Nothing,
-- because the two atoms it decides are not the same question: CR 109.5's "you"
-- is a player and IsSource names an object, and every caller below that knows
-- which object frames the match knows it without knowing a perspective. Nothing
-- where no object frames the match, which leaves IsSource vacuously False the
-- way it was before this took a parameter.
--
-- sacrificeCandidates below is the one caller that narrows a whole battlefield
-- with it, and Pawl.Engine.Cost, Pawl.Engine.Event and Pawl.Engine.Resolve's
-- edict all reach it through that -- so there is no duplicate matcher to keep in
-- step (#111).
matchesPermanent :: GameState -> Maybe ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> Bool
matchesPermanent gs source filter_ oid =
  Filter.matches (Filter.MkContext Nothing source) (Projection.viewOfObject oid gs) filter_

-- CR 701.21a: the permanents this player may sacrifice for a Filter, ascending --
-- the order Prompt.ChooseSacrifices and Prompt.ChooseAnyNumberToSacrifice offer
-- them in, which is what makes both the elision test and the transcript fallback
-- deterministic.
--
-- Here rather than in Pawl.Engine.Cost, which was where it lived while paying a
-- cost was the only way to sacrifice anything: CR 614.1c's as-enters sacrifice
-- (EntryRewrite.SacrificeAnyNumber) asks the same question from Pawl.Engine.Event,
-- which is BELOW Cost. One home keeps CR 701.21a's "a permanent they control"
-- answered once.
--
-- The Maybe ObjectId is what the criterion's `Not IsSource` means by "another"
-- -- the permanent whose cost is being paid, or whose as-enters ability is
-- asking. Gift of Doom is why it is here: its morph cost is "sacrifice ANOTHER
-- creature", and CR 708.2a makes the face-down permanent paying that cost a
-- creature itself, so without the frame it could pay by sacrificing itself.
-- That is the same failure Pawl.Engine.Cost.tapCandidates records for CR
-- 702.122a, where a Vehicle that had become a creature could crew itself.
--
-- CR 101.2 subtracts the permanents an effect in force says CAN'T be sacrificed
-- (Garland, Royal Kidnapper), which is what makes such a permanent unofferable
-- rather than merely unsacrificeable: this one function is what
-- Pawl.Engine.Cost's CR 118.3 payability count, its Prompt.ChooseSacrifices
-- offer, Pawl.Engine.Resolve's edict and Pawl.Engine.Event's as-enters offer all
-- read. The other half of the gate is in Event.sacrifice, for the instructions
-- that name a victim without consulting a candidate list.
sacrificeCandidates :: PlayerId -> Maybe ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
sacrificeCandidates pid source filter_ gs =
  let matching = List.sort (filter (matchesPermanent gs source filter_) (Projection.controls pid gs))
      forbidden = SacrificeRestriction.cantBeSacrificed matching gs
   in filter (\oid -> not (Set.member oid forbidden)) matching

-- CR 614.1a / 614.1c-d: does the event's subject satisfy this replacement's
-- Filter? Both the ENTERING object of an entry replacement and the MOVING object
-- of a zone-change redirect, which is one question asked of one candidate --
-- CR 614.1a puts no restriction on what a replacement may look at, so the two
-- classes narrow by the same predicate language.
--
-- The same evaluator matchesPermanent uses, over the same projected view, but
-- with a FRAMED Context rather than an empty one, because every filter in the
-- pool that reaches here reads it: CR 614.1c's `IsSource` asks whether the
-- candidate IS the effect's source (Clone, Primal Plasma, CR 306.5b's loyalty,
-- and CR 702.34a's "exile THIS card" on the zone-change side), and CR 614.1d's
-- `ControlledBy Opponent` asks who the candidate's controller is relative to CR
-- 109.5's "you" (Gather Specimens). The perspective is the CANDIDATE's
-- controller, which for a floating row is the baked one -- deriving it from the
-- source here would answer Nothing for every row whose spell has resolved, and a
-- Nothing perspective makes ControlledBy vacuously False.
--
-- CR 614.12 is why the view is the LIVE projection of the materialized object
-- rather than of the card it came from: a previous iteration's rewrite has to be
-- visible to this one, including CR 616.1b's own change to who would control it.
--
-- CR 608.2h never fires on the zone-change side, and that is why the plain
-- projection is right for a creature that "would die": Pawl.Engine.Event runs
-- this loop BEFORE the move, so the dying creature is still on the battlefield in
-- `gs` and is read as it last existed there rather than as the card it becomes in
-- the graveyard (CR 400.7). For a CR 608.2f batch `gs` is the pre-batch board
-- Replacement.applicable passed down, which is that same reading.
matchesFiltered :: GameState -> ReplacementCandidate -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> Bool
matchesFiltered gs candidate filter_ oid =
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
  ReplacementEffect.EntryR _ (EntryRewrite.ChooseCardNames _) -> ReplacementBucket.Other
  ReplacementEffect.EntryR _ (EntryRewrite.WithCounters _ _) -> ReplacementBucket.Other
  ReplacementEffect.EntryR _ (EntryRewrite.SacrificeAnyNumber _ _) -> ReplacementBucket.Other
  -- CR 702.136a is none of CR 616.1a-d either: riot rewrites what the permanent
  -- enters WITH, never whose it is, what it copies or which face is up.
  ReplacementEffect.EntryR _ EntryRewrite.Riot -> ReplacementBucket.Other
  -- CR 614.1d is none of CR 616.1a-d either: a tap-state rewrite changes the
  -- STATUS the permanent enters with (CR 110.5b), never whose it is, what it
  -- copies or which face is up. So CR 616.1e.
  ReplacementEffect.EntryR _ EntryRewrite.Tapped -> ReplacementBucket.Other
  -- CR 614.1c's paid variant of the same rewrite is none of CR 616.1a-d either,
  -- and paying life does not make it one: what the rewrite changes is still the
  -- STATUS the permanent enters with (CR 110.5b). So CR 616.1e.
  ReplacementEffect.EntryR _ (EntryRewrite.PayLifeOrTapped _) -> ReplacementBucket.Other
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
  -- CR 616.1a-d are all about entering the battlefield and copying; turning face
  -- up is neither, so CR 616.1e.
  ReplacementEffect.TurnUpR _ _ -> ReplacementBucket.Other
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
-- Consumption is deliberately NOT what this asks about. Every Event.apply arm spends
-- its own candidate (CR 614.5), but the loop gives each candidate its own
-- opportunity in any order, so which was spent first is not a board difference.
--
-- One arm per constructor, no wildcard, and the EntryR and TurnUpR arms split per
-- rewrite, so a new constructor breaks the build HERE as well as in
-- bucketOfEffect and Event.apply. A wildcard defaulting to False would hand an
-- author who teaches Event.apply a new controller-reading rewrite an unasked choice
-- instead of a build failure.
readsApplier :: ReplacementEffect -> Bool
readsApplier re = case re of
  -- The destination zone is the effect's own second field, and the pattern is
  -- matched before Event.apply runs (Rest in Peace, Leyline of the Void).
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
  -- Two choosers rather than one, and neither is the candidate's: the entering
  -- object's controller is read live off the board for ChooseColor's reason, and
  -- CR 102.2's opponent is derived from that same player. The restriction rides
  -- the effect (CR 201.4a).
  ReplacementEffect.EntryR _ (EntryRewrite.ChooseCardNames _) -> False
  -- CR 614.1c's "enters with": the counter kind and count are the effect's own
  -- fields, and they land on the entering object (CR 306.5b's loyalty included).
  ReplacementEffect.EntryR _ (EntryRewrite.WithCounters _ _) -> False
  -- CR 614.1c again, and NO despite performing a sacrifice: the sacrificing
  -- player is the ENTERING object's controller, read live off the board at CR
  -- 614.12a's moment for AsCopy's reason, and the criterion and counter kind ride
  -- the effect. Two such rows would offer the same player the same permanents.
  ReplacementEffect.EntryR _ (EntryRewrite.SacrificeAnyNumber _ _) -> False
  -- CR 702.136a: riot's chooser is the ENTERING object's controller, read live
  -- off the board for AsCopy's reason, and the rewrite carries no payload at all
  -- -- rule 702.136a fixes both halves. Two riot rows are always on the SAME
  -- object, since CR 614.1c's ability is the entering permanent's own, and they
  -- offer that permanent's controller the same two outcomes -- so which applies
  -- first is not a board difference.
  ReplacementEffect.EntryR _ EntryRewrite.Riot -> False
  -- CR 614.1d: no chooser at all, and no payload -- the rewrite sets one status on
  -- the object the event already named (CR 110.5b), so it applies the same way
  -- whoever's row is applying it. Two such rows are the same write twice.
  ReplacementEffect.EntryR _ EntryRewrite.Tapped -> False
  -- CR 614.1c: NO despite spending a resource, for SacrificeAnyNumber's reason.
  -- The payer is the ENTERING object's controller -- "you" in an "as this
  -- permanent enters" ability the permanent prints about itself -- read live off
  -- the board at CR 614.12a's moment rather than off the candidate, and the
  -- amount rides the effect. Two such rows are always on the same object and
  -- would offer that object's controller the same price.
  ReplacementEffect.EntryR _ (EntryRewrite.PayLifeOrTapped _) -> False
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
  -- CR 702.37b via CR 614.1e: the counter kind and count are the effect's own
  -- fields and they land on the object the event already named, which is
  -- WithCounters' answer one event class over. The inner sum is cased so a
  -- second TurnUpRewrite -- CR 208.2b's power-and-toughness setter -- has to be
  -- decided here rather than inheriting this answer.
  ReplacementEffect.TurnUpR _ (TurnUpRewrite.WithCounters _ _) -> False
  -- CR 303.4k: "the AURA's controller" makes the choice, and the Aura is the
  -- object the event already named -- so the player asked is read off the event
  -- rather than off whose row is applying, and two identical rows would put the
  -- same question to the same player. The destination Filter is the effect's own
  -- field, inside `choose`'s comparison already.
  ReplacementEffect.TurnUpR _ (TurnUpRewrite.MayAttachTo _) -> False
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
-- Indistinguishable is `distinguishing` below: equal in `effect` and in CR
-- 614.3's `lifetime`, plus -- for the effects whose application READS the
-- applying candidate -- equal in CR 109.5's "you". Candidates equal in `effect`
-- can still differ in `source` and in `controller`, so "equal in `effect`"
-- implies "every order yields the same board" only for effects that apply the
-- same way whoever is applying them. readsApplier is the classification that
-- separates those two, and EntryRewrite.UnderSourceControl is the one arm it
-- answers True for.
--
-- `lifetime` is folded in UNCONDITIONALLY, and for a reason that has nothing to
-- do with how an effect applies: `consume` spends the row that applied and
-- leaves the rest standing, so two rows differing in when they die or how often
-- they may fire leave DIFFERENT boards behind whichever way round they are
-- taken. CR 614.10a says so outright for the case that reaches it -- "one effect
-- will be satisfied in skipping the first occurrence, while the other will remain
-- until another occurrence can be skipped" -- and Brine Elemental's Expiry.Never
-- skip beside Savor the Moment's Expiry.AtCleanup one is that case, two PhaseR
-- rows equal in every other field. Two rows alike in `lifetime` are still elided:
-- spending either leaves a store of the same shape.
--
-- Folding `controller` in UNCONDITIONALLY would be sound as well, and wrong the
-- other way: two Rest in Peace under different controllers exile the same card
-- to the same zone whichever applies, so asking about them would raise a
-- question the rules leave nothing to decide. `source` is folded in nowhere for
-- the same reason -- every use of it above is a test run BEFORE Event.apply, not a
-- branch inside it.
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
          answer <- Game.choose (Prompt.ChooseReplacement decider pid (fmap entryOf candidates))
          -- Reject-not-repair, as payment and Engine.permute already do: an
          -- out-of-range index leaves the canonical first standing rather than
          -- dropping the event or crashing.
          pure (Just (at candidates answer first))
  where
    -- What two candidates must agree on to be interchangeable here.
    distinguishing c =
      ( ReplacementCandidate.effect c,
        ReplacementCandidate.lifetime c,
        if readsApplier (ReplacementCandidate.effect c)
          then ReplacementCandidate.controller c
          else Nothing
      )

-- What a candidate looks like to the player being asked (#74): its source and
-- the effect that distinguishes it from another of the same source. A
-- projection, not a computation -- the candidate already carries both.
entryOf :: ReplacementCandidate -> ReplacementEntry
entryOf c =
  ReplacementEntry.MkReplacementEntry
    { ReplacementEntry.source = ReplacementCandidate.source c,
      ReplacementEntry.effect = ReplacementCandidate.effect c
    }

-- Total index into a list, with a fallback.
at :: [a] -> Natural -> a -> a
at xs i fallback = case List.genericDrop i xs of
  h : _ -> h
  [] -> fallback

-- CR 616.1 / 108.4: who decides. Projection.controllerOf already falls back to
-- the owner, so CR 616.1's owner clause is free.
--
-- One answer per event, because one proposed event has exactly one affected
-- object. CR 616.1's APNAP clause therefore has nothing to say at this scale and
-- is honoured a level up, where a whole batch of simultaneous events can present
-- choices to two players at once: `orderBatch` sorts the batch by this
-- function's answer before any of it is asked.
chooserOf :: GameState -> ProposedEvent -> Maybe PlayerId
chooserOf gs event = case event of
  ProposedEvent.WouldChangeZone zc -> Projection.controllerOf (ZoneChange.object zc) gs
  -- CR 616.1's affected object's controller, read LIVE off the materialized
  -- permanent -- which for an entry is the player it WOULD enter under, and
  -- which a CR 616.1b rewrite may already have changed on an earlier iteration.
  ProposedEvent.WouldEnter oid -> Projection.controllerOf oid gs
  -- Read LIVE off the event in hand, as the entry arm above is: CR 614.9's
  -- redirection changes the recipient, so an earlier CR 616.1f iteration may
  -- already have moved the question to another player's object. Event.loop
  -- re-enters with the rewritten event, so this is that player.
  ProposedEvent.WouldDealDamage de -> case DamageEvent.target de of
    Recipient.ToPlayer pid -> Just pid
    Recipient.ToCreature oid -> Projection.controllerOf oid gs
    Recipient.ToPlaneswalker oid -> Projection.controllerOf oid gs
    -- The battle's CONTROLLER, not its protector. CR 616.1 asks for the affected
    -- object's controller and CR 310.8d substitutes the protector only for the
    -- "defending player", which rule 616 nowhere says.
    Recipient.ToBattle oid -> Projection.controllerOf oid gs
    Recipient.ToObject oid -> Projection.controllerOf oid gs
  ProposedEvent.WouldBeDestroyed oid _ -> Projection.controllerOf oid gs
  ProposedEvent.WouldPutCounters oid _ _ -> Projection.controllerOf oid gs
  ProposedEvent.WouldCreateTokens pid _ _ -> Just pid
  -- CR 616.1's "affected player": a step or phase beginning affects no object,
  -- so the player whose turn it is chooses among applicable skips.
  ProposedEvent.WouldBeginPhase _ pid -> Just pid
  -- CR 616.1's affected object is the permanent turning over, and its controller
  -- is CR 702.37e's "you" -- the player who took the special action.
  ProposedEvent.WouldTurnFaceUp oid -> Projection.controllerOf oid gs

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
-- the CR 614.12a note on Event.applyReplacementsIn for why the batch set, not
-- 614.13a, is what excludes them).
legalCopyTargets :: Set ObjectId -> ObjectId -> GameState -> [ObjectId]
legalCopyTargets batch self gs =
  let eligible oid = oid /= self && not (Set.member oid batch) && Projection.isCreatureOf oid gs
   in filter eligible (Set.toAscList (GameState.battlefield gs))

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

-- CR 615.1a: is this damage rewrite a PREVENTION effect, rather than one of CR
-- 614.1a's replacements? "Effects that use the word 'prevent' are prevention
-- effects", and that word is what CR 615.13's trigger watches for -- so a
-- SetAmount that cuts an event from 3 to 1 has prevented nothing, though it
-- shrank the event exactly as a shield would.
--
-- A CLASSIFICATION of effects -- what SHAPE a rewrite has, never which effect it
-- is -- in the same genre as bucketOf, readsApplier and shieldRemaining above.
-- One arm per constructor, no wildcard, so a new rewrite that prevents damage
-- breaks the build here rather than silently going unreported.
--
-- A question about the REWRITE alone, and deliberately not about the event: CR
-- 615.12's unpreventable damage is still met by a prevention effect, which is
-- still a prevention effect for having prevented nothing of it. `preventable`
-- below is the other half, and `inertPrevention` asks them together.
prevents :: DamageRewrite.DamageRewrite -> Bool
prevents rewrite = case rewrite of
  DamageRewrite.PreventNext _ -> True
  DamageRewrite.PreventAll -> True
  DamageRewrite.SetAmount _ -> False
  DamageRewrite.Scale _ -> False
  -- CR 614.9's redirection is a rule-614 replacement. Turn the Tables never says
  -- "prevent" -- the damage is still dealt, one recipient over -- so this False
  -- is what keeps `preventionBy`, `inertPrevention` and CR 615.13's trigger away
  -- from it.
  DamageRewrite.Redirect _ -> False

-- CR 614.9: the destination a redirection effect may still use, re-derived
-- against the CURRENT state at redirect time. Nothing is the rule's guard --
-- "if one of those permanents is no longer on the battlefield ... or is no
-- longer a battle, creature, or planeswalker ... the effect does nothing" --
-- and Event.apply then hands the event back UNCHANGED rather than dropping it.
--
-- BOTH of the rule's conditions, though only the card-type one has an observer
-- today: a destination that LEFT is caught by that one already, because CR
-- 400.7 mints a new object on a zone change and the dead id then projects no
-- card types at all. The membership test is what covers the route CR 400.7 does
-- not -- CR 702.26b's phased-out permanent, which keeps its id and its zone and
-- is nonetheless treated as not existing. Deleting it leaves the suite green;
-- it is a fence, not a proven line.
--
-- Damage.damageRecipient asks the card-type half alone, of an object that is on
-- the battlefield by construction. Not shared with it because Pawl.Engine.Damage
-- sits ABOVE this module.
--
-- Re-TAGGED off the live projection rather than merely tested, so a destination
-- that stopped being a creature and became a planeswalker is redirected to as
-- what it now is (CR 613.1d).
--
-- Not implemented, and unreachable: the rule's last sentence, damage redirected
-- to or from a player who has left the game. Pawl has no leave-the-game path, so
-- a ToPlayer destination is always live.
redirectDestination :: GameState -> Recipient.Recipient -> Maybe Recipient.Recipient
redirectDestination gs dest = case Recipient.objectOf dest of
  Nothing -> Just dest
  Just oid
    | not (Set.member oid (GameState.battlefield gs)) -> Nothing
    | Projection.isCreatureOf oid gs -> Just (Recipient.ToCreature oid)
    | Projection.isPlaneswalkerOf oid gs -> Just (Recipient.ToPlaneswalker oid)
    | Projection.isBattleOf oid gs -> Just (Recipient.ToBattle oid)
    | otherwise -> Nothing

-- CR 615.12: could a prevention effect prevent any of this damage event, or is
-- this damage that "can't be prevented" (Spider-Punk)?
--
-- FALSE does not mean the prevention effect is inapplicable, and the rule is
-- emphatic about the difference: "any applicable prevention effects are STILL
-- APPLIED to it. Those effects won't prevent any damage, but any additional
-- effects they have will take place." So `applies` is untouched by this
-- question, the CR 616.1 loop still offers the row and still marks it applied --
-- which is CR 615.12a's "just once", falling straight out of the applied-set the
-- loop already carries -- and what changes is only that the application does
-- nothing and the shield is not spent ("existing damage prevention shields won't
-- be reduced by damage that can't be prevented").
--
-- Asked per EVENT, because that is what the rule's own subject is and what the
-- printed clauses narrow: Spider-Punk's sentence admits every event, and
-- Excruciator's admits only the ones its own 7/7 is the source of, on the same
-- board and in the same batch.
--
-- A DISJUNCTION over the standing effects, for CR 101.2's reason and the shape
-- every prohibition takes: one applicable "can't" is enough and nothing outvotes
-- it. `any` rather than a count because EachPlayer puts one effect on the list
-- once per seat.
--
-- Delegated to Pawl.Engine.PlayerEffect, which owns the CR 613.10/613.11 axis
-- this lives on, so this module never sees a PlayerEffect constructor -- it
-- reads only the DamagePattern each effect hands back, with the same
-- matchesDamagePattern that reads a shield's.
preventable :: GameState -> DamageEvent.DamageEvent -> Bool
preventable gs de =
  not (any (\(src, pat) -> matchesDamagePattern src pat de) (PlayerEffect.unpreventable gs))

-- CR 615.12: is this the pairing the rule describes -- a PREVENTION effect
-- chosen against damage that CAN'T BE PREVENTED? True means the application
-- happens and changes nothing: Pawl.Engine.Event's CR 616.1 loop hands the event
-- back untouched and marks the row applied, spending neither a use nor a point
-- of shield.
--
-- The two halves above, asked together, and asked HERE so that the loop reads
-- one classification rather than composing two. `prevents` is the effect half
-- and refuses CR 614.1a's SetAmount and Scale, which prevent nothing and are not
-- what the rule is about: Furnace of Rath doubles unpreventable damage like any
-- other.
--
-- The wildcard is over (effect, event) PAIRS, where it is the only way to say
-- "this pair is not a prevention of damage" -- preventionBy's arrangement, for
-- preventionBy's reason: the per-constructor obligation is discharged by
-- `prevents`, which this delegates to.
inertPrevention :: GameState -> ReplacementCandidate -> ProposedEvent -> Bool
inertPrevention gs candidate event = case (ReplacementCandidate.effect candidate, event) of
  (ReplacementEffect.DamageR _ rewrite, ProposedEvent.WouldDealDamage de) ->
    prevents rewrite && not (preventable gs de)
  _ -> False

-- CR 615.13: how much of this event the candidate just applied PREVENTED, or
-- Nothing when it prevented nothing.
--
-- `before` is the event as it was offered to Event.apply and `after` what came back,
-- so this is the CR 615.6 arithmetic read off the pair: Nothing means the event
-- does not happen at all, which is the whole of it prevented.
--
-- The wildcard is over (effect, event) PAIRS, where it is the only way to say
-- "this pair is not a prevention of damage"; the per-constructor obligation this
-- module carries is discharged by `prevents` above, which the guard delegates to.
preventionBy :: ReplacementCandidate -> ProposedEvent -> Maybe ProposedEvent -> Maybe Prevention
preventionBy candidate before after = case (ReplacementCandidate.effect candidate, before) of
  (ReplacementEffect.DamageR _ rewrite, ProposedEvent.WouldDealDamage de)
    | prevents rewrite ->
        let was = DamageEvent.amount de
            -- The event that did not happen prevented all of it (CR 615.6).
            now = maybe 0 DamageEvent.amount (after >>= asDamageEvent)
         in if was > now
              then
                Just
                  Prevention.MkPrevention
                    { Prevention.by = ReplacementCandidate.identity candidate,
                      -- The recipient of the event as PROPOSED, which is the
                      -- reading CR 615.13 asks for: its ability watches "damage
                      -- that WOULD be dealt [and] is prevented". Damage CAN now
                      -- be redirected (DamageRewrite.Redirect), and the two
                      -- readings still never diverge, because a redirect
                      -- prevents nothing (CR 615.1a) -- `prevents` refuses it
                      -- above, so no redirect ever reaches here.
                      Prevention.recipient = DamageEvent.target de,
                      Prevention.amount = was - now
                    }
              else Nothing
  _ -> Nothing

-- CR 615.13: collapse a batch's per-event preventions to one entry per applying
-- instance per recipient, carrying the total that instance prevented.
--
-- Keyed on the RECIPIENT as well as the instance, which is narrower than the
-- rule's own unit -- 615.13 counts one application of one prevention effect,
-- whoever the simultaneous events were addressed to. Every prevention the ENGINE
-- bakes names exactly one recipient (Resolve's two prevention arms), and the one
-- card-authored prevention that names none -- Fog's -- has no CR 615.13 trigger
-- paired with it, so the two readings coincide today. Not implemented: a
-- prevention effect naming NO recipient that reaches two recipients in one batch
-- reports two preventions where the rule describes one (#688).
--
-- Ascending by key, so the CR 608.2i record -- and therefore the CR 603.3b order
-- these triggers are offered in -- is canonical rather than gather-dependent.
groupPreventions :: [Prevention] -> [Prevention]
groupPreventions ps =
  let keyed = Map.fromListWith (+) [((Prevention.by p, Prevention.recipient p), Prevention.amount p) | p <- ps]
      rebuild ((by, recipient), amount) = Prevention.MkPrevention {Prevention.by = by, Prevention.recipient = recipient, Prevention.amount = amount}
   in fmap rebuild (Map.toAscList keyed)

-- CR 615.7: when two or more applicable sources would deal damage to a shielded
-- recipient at the same time, that recipient chooses which damage the shield
-- prevents.
--
-- Asked as an ORDER over the contested events rather than as a pick, because a
-- pick repeated IS an order: applying a shield to an event covers as much of it
-- as the shield has left and no more, which nobody may decline or divide, so the
-- only freedom the rule grants is which event the shield reaches first.
--
-- CR 616.1's APNAP clause is honoured here too, and over the WHOLE batch rather
-- than only over the shield questions: `byApnap` groups the batch by chooser
-- before anything is asked, so every question one player is owed -- CR 615.7's
-- allocation and each of their events' CR 616.1 choices alike -- is asked before
-- the next player's. A lone ProposedEvent still has exactly one affected object
-- and therefore one chooser, which is why the batch is the only place the clause
-- can be honoured at all.
--
-- The two rules order DIFFERENT LEVELS and so cannot contend for this list. CR
-- 615.7's freedom is entirely within one chooser: a shield names one recipient,
-- so every event it contests is addressed to one player's object, and that is
-- the same player CR 616.1 asks about those events -- `contested` and `choose`
-- both read the chooser off the recipient through `chooserOf`. `askOne` then
-- permutes only within that player's own positions. CR 101.4c is the rule that
-- licenses it: a player making several simultaneous choices makes them in the
-- order specified, or chooses the order themselves.
orderBatch :: [DamageEvent.DamageEvent] -> Game [DamageEvent.DamageEvent]
orderBatch events = do
  gs <- State.get
  -- Sorted FIRST: `contested` reports batch positions, so it has to see the list
  -- `askOne` will splice into.
  let sorted = byApnap gs events
  Monad.foldM askOne sorted (contested gs sorted)

-- CR 616.1 / 101.4: group a batch by whose CR 616.1 choice each event is, active
-- player first. A stable sort, so events sharing a chooser keep their gather
-- order and CR 615.7's within-chooser permutation is left to `askOne`.
--
-- An event with no chooser -- its affected object has left -- sorts last with
-- the unseated, since `choose` will not prompt for it either.
byApnap :: GameState -> [DamageEvent.DamageEvent] -> [DamageEvent.DamageEvent]
byApnap gs =
  let rank de = maybe (unseated gs) (seatOf gs) (chooserOf gs (ProposedEvent.WouldDealDamage de))
   in List.sortOn rank

-- CR 101.4: how far down APNAP order a player sits. A player off the seating
-- roster sorts last, the fallback Resolve.objectRefObjects takes for the same
-- lookup.
seatOf :: GameState -> PlayerId -> Int
seatOf gs pid = Maybe.fromMaybe (unseated gs) (List.elemIndex pid (Game.apnapOrder gs))

-- The seat index that sorts after every real one.
unseated :: GameState -> Int
unseated = length . Game.apnapOrder

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
      answer <- Game.choose (Prompt.OrderDamage decider pid group)
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
-- Not implemented: a shield is the only contested resource this asks about, so a
-- limited replacement of any other shape that two of one chooser's simultaneous
-- events could each spend is spent by whichever is settled first, rather than by
-- CR 101.4c's answer from that chooser (#839).
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
--
-- CR 615.12's damage is left out of the union, and that is an elision the rule
-- itself licenses rather than a shortcut: a shield prevents none of an
-- unpreventable event and is not reduced by it, so every order of a batch of
-- them leads to the same board and there is nothing for the shielded player to
-- decide. Filtered per EVENT rather than per batch, so a batch mixing
-- preventable and unpreventable damage still asks about the part the shield can
-- reach -- which a narrowed clause reaches: an Excruciator and an ordinary
-- creature hitting one shielded permanent at once is exactly that batch.
contested :: GameState -> [DamageEvent.DamageEvent] -> [(PlayerId, [Natural])]
contested gs events =
  let indexed :: [(Natural, DamageEvent.DamageEvent)]
      indexed = zip [0 ..] events
      hitsOf candidate = filter (\entry -> preventable gs (snd entry) && applies gs (ProposedEvent.WouldDealDamage (snd entry)) candidate) indexed
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
   in [ (pid, List.sort (List.nub positions))
      | (pid, positions) <- List.sortOn (seatOf gs . fst) (Map.toList merged)
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
    DamageRewrite.Redirect _ -> Nothing
  ReplacementEffect.ZoneChangeR _ _ -> Nothing
  ReplacementEffect.EntryR _ _ -> Nothing
  ReplacementEffect.DestructionR _ -> Nothing
  ReplacementEffect.CounterR _ _ -> Nothing
  ReplacementEffect.TokenR _ _ -> Nothing
  ReplacementEffect.TurnUpR _ _ -> Nothing
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
  ProposedEvent.WouldTurnFaceUp {} -> Nothing

asDestruction :: ProposedEvent -> Maybe ObjectId
asDestruction event = case event of
  ProposedEvent.WouldBeDestroyed target _ -> Just target
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing

asCounters :: ProposedEvent -> Maybe (ObjectId, CounterKind.CounterKind, Natural)
asCounters event = case event of
  ProposedEvent.WouldPutCounters oid kind n -> Just (oid, kind, n)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing

asTokens :: ProposedEvent -> Maybe (PlayerId, Card, Natural)
asTokens event = case event of
  ProposedEvent.WouldCreateTokens pid card n -> Just (pid, card, n)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing

-- CR 500.11 / 614.1b: an extra turn is beginning, so the steps and phases IT
-- skips become floating replacement effects, one per selector. Called by
-- Engine.takeNextTurn at the moment the turn actually begins, and only for a turn
-- that does begin (CR 800.4k).
--
-- Here rather than in Pawl.Engine.Resolve, which installs Effect.SkipNextPhase's
-- rows: the two differ only on WHEN the row exists, and this module is the one
-- that reads GameState.replacements. The row is the same shape Resolve builds --
-- PhaseR, scoped to the taker, Uses.Once -- so Event.beginsPhase answers a
-- turn-scoped skip and a next-occurrence skip through one mechanism, and CR
-- 616.1's loop orders them against each other for free.
--
-- Installed AT THE TURN'S START rather than at the resolution that created the
-- turn, which is the whole point: CR 614.10a's "next" would otherwise name
-- whatever step came first in the meantime, and CR 500.7 lets that be a different
-- turn entirely. CR 614.10 bars skipping a step, phase or turn that has already
-- started; nothing of this turn has started yet, since Engine.beginTurnOf has
-- only scheduled it and Engine.runStep asks Event.beginsPhase before the untap step's
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
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
