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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import Pawl.Types.CandidateId (CandidateId)
import qualified Pawl.Types.CandidateId as CandidateId
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Card as Card.Type
import Pawl.Types.ControllerRelation (ControllerRelation)
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.CounterSubject as CounterSubject
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.DestructionCause as DestructionCause
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.DrawCountR as DrawCountR
import qualified Pawl.Types.DrawCountRewrite as DrawCountRewrite
import qualified Pawl.Types.DrawR as DrawR
import qualified Pawl.Types.DrawRewrite as DrawRewrite
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Expiry as Expiry
import Pawl.Types.ExtraTurn (ExtraTurn)
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.FloatingCandidate as FloatingCandidate
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.InstanceOrdinal as InstanceOrdinal
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LifeGainR as LifeGainR
import qualified Pawl.Types.LifeGainRewrite as LifeGainRewrite
import qualified Pawl.Types.LifeLossPattern as LifeLossPattern
import qualified Pawl.Types.LifeLossR as LifeLossR
import qualified Pawl.Types.LifeLossRewrite as LifeLossRewrite
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PermanentCandidate as PermanentCandidate
import qualified Pawl.Types.PhasePattern as PhasePattern
import Pawl.Types.PhaseSelector (PhaseSelector)
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import Pawl.Types.Prevention (Prevention)
import qualified Pawl.Types.Prevention as Prevention
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.ProposedEvent (ProposedEvent)
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import qualified Pawl.Types.Quantity as Quantity.Type
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
import qualified Pawl.Types.ReplacementProvenance as ReplacementProvenance
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.SetPowerToughness as SetPowerToughness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TokenLot as TokenLot
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import Pawl.Types.ZoneChange (ZoneChange)
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

asZoneChange :: ProposedEvent -> Maybe ZoneChange
asZoneChange event = case event of
  ProposedEvent.WouldChangeZone zc -> Just zc
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing

-- Every replacement effect instance in the game, in the engine's canonical
-- order, which is what the ChooseReplacement prompt indexes into:
--
--   1. STATIC abilities (Projection.replacementsAffecting): battlefield
--      permanents ascending by id, then the command zone's emblems and vanguard
--      cards, whose abilities CR 113.6p leaves functioning there, then the rows a card STATES
--      it functions from elsewhere with (CR 113.6b) -- the stack, then the
--      graveyards, then the hands, then the libraries; each object's own effects
--      in printed order. Read from `sources`, which for a CR 608.2f batch is the
--      board the batch began in rather than the live one (see
--      Event.applyReplacementsIn).
--   2. The FLOATING store (GameState.replacements): newest first, since every
--      installer prepends as it creates the row. Always the LIVE store, never a
--      frozen one: CR 614.3 spends a one-shot as it is applied, and `consume`
--      writes that back here.
--   3. The CR 613.11 PLAYER axis (PlayerEffect.protectionCarriers), by seat: rule
--      702.16e's "or player", which no permanent's ability list holds because a
--      player has no keywords and no characteristics. Re-derived from the board
--      like segment 1 and spent like it -- nothing to consume.
--
-- Segments 1 and 2 drop a row whose printed clause is false right now, which is
-- CR 614.1's "they aren't locked in ahead of time": the permanent segment's CR
-- 604.2 clause in Projection.replacementsOf, the floating one's in `lives` below.
-- Segment 3's twin of that is PlayerEffect.applying, whose own CR 604.2 gate
-- drops a carrier that has stopped applying.
--
-- The first two segments take separate arguments -- rather than one GameState
-- apiece, which would be two interchangeable parameters of the same type -- so
-- the split cannot be got backwards. The third reads `sources` for segment 1's
-- reason: it is derived from the board, so it must be derived from the board a CR
-- 608.2f batch began in.
collect :: GameState -> [ActiveReplacement.ActiveReplacement] -> [ReplacementCandidate]
collect sources floating =
  let fromPermanent (instance_, (src, provenance, re)) =
        ReplacementCandidate.MkReplacementCandidate
          { -- CR 614.5 / 702.136b: one identity per INSTANCE, so a permanent
            -- carrying one ability twice gets the rule's two opportunities.
            ReplacementCandidate.identity =
              CandidateId.OfPermanent
                PermanentCandidate.MkPermanentCandidate
                  { PermanentCandidate.source = src,
                    -- CR 615.13's "this way", carried from where the row was
                    -- built rather than inferred from its value here. See
                    -- Pawl.Types.ReplacementProvenance and `printedBy`.
                    PermanentCandidate.provenance = provenance,
                    PermanentCandidate.effect = re,
                    PermanentCandidate.ordinal = instance_
                  },
            ReplacementCandidate.effect = re,
            ReplacementCandidate.source = src,
            -- CR 109.5: "you" is the SOURCE's controller, read live off the
            -- board this segment was gathered from -- a stolen Furnace of Rath
            -- belongs to whoever holds it now.
            ReplacementCandidate.controller = Projection.controllerOf src sources,
            -- CR 614.3's lifetime, which a static ability does not have: this
            -- segment is re-derived from the board on every iteration, and
            -- `consume` is a no-op for it. So there is no duration to expire and no
            -- use to spend, which is what Nothing says -- not an unknown one.
            ReplacementCandidate.lifetime = Nothing,
            -- CR 614.15: a printed replacement ability is a STATIC ability,
            -- which puts it outside the self-replacement class -- so this
            -- segment is never CR 616.1a's, whatever it replaces.
            ReplacementCandidate.origin = ReplacementOrigin.Other,
            -- CR 615.5, built rather than copied: the additional effect is
            -- printed on the ability (DamageR.riders) and the environment it
            -- runs in is read off the live board (see `printedRider`).
            ReplacementCandidate.rider = printedRider src (Projection.controllerOf src sources) re,
            -- No resolution installed this segment, so there is nothing it could
            -- have bound: a printed static ability is re-derived from the board,
            -- and Filter.IsBound is vacuously False in it.
            ReplacementCandidate.slots = Map.empty
          }
      fromFloating active =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity =
              CandidateId.OfFloating
                FloatingCandidate.MkFloatingCandidate
                  { FloatingCandidate.source = ActiveReplacement.source active,
                    FloatingCandidate.timestamp = ActiveReplacement.timestamp active
                  },
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
            ReplacementCandidate.origin = ActiveReplacement.origin active,
            -- CR 615.5, copied off the row rather than looked up later: the row
            -- may be dropped by the very application that fires the rider
            -- (`setShield` drops a shield reduced to 0).
            ReplacementCandidate.rider = ActiveReplacement.rider active,
            -- The installing resolution's bindings, carried through so a row's
            -- own pattern can name one object (see Pawl.Types.ActiveReplacement).
            ReplacementCandidate.slots = ActiveReplacement.slots active
          }
      -- CR 702.16e's PLAYER half: "any damage that would be dealt by sources that
      -- have the stated quality to a permanent or player with protection is
      -- prevented." The permanent half is a row rule 702.16 mints onto the
      -- permanent (Pawl.Engine.Keyword.mintedReplacementsOf); a player has no
      -- keywords, so its half rides the CR 613.11 axis and is minted here, where
      -- the pairs PlayerEffect.protectionCarriers gathers can be read.
      --
      -- CandidateId.OfPermanent with a MINTED provenance rather than a third arm
      -- of that type, and the fit is exact rather than a reuse: CR 614.5's
      -- identity for this row is (carrier, effect value, ordinal), the carrier is
      -- the object whose static ability grants the protection, and CR 615.13's
      -- "prevented this way" must not see it -- which is precisely what Minted
      -- says (see Pawl.Types.ReplacementProvenance). The effect value names the
      -- protected player, so one carrier protecting two seats is two instances,
      -- and the ordinal is 0 because equal rows here are the same (player,
      -- carrier) pair, which CR 702.16m makes redundant anyway.
      --
      -- No collision with segment 1's rows off the same carrier: `whichRecipient`
      -- is engine-baked and no card may write it (Pawl.CardSpec's
      -- engineOnlyOffends), and rule 702.16's keyword row writes `whatRecipient`
      -- instead.
      --
      -- CR 109.5's "you" is the CARRIER's controller, not the protected player --
      -- the two differ the moment a scope wider than PlayerScope.You appears --
      -- and nothing in the row reads it: the pattern's source side is
      -- Filter.HasChosenName, which reads the carrier's own chosen names off the
      -- Context rather than a perspective, and its recipient side is a baked id,
      -- so no atom here is perspective-relative.
      --
      -- That same atom is why the effect value no longer varies with WHICH names
      -- the carrier holds, where the literal disjunction it replaced did. The
      -- identity above is unharmed: the carrier's id is its own component, so two
      -- carriers protecting one seat stay two instances, and this segment is
      -- re-derived from the board every iteration (see below), so a row can never
      -- outlive the names it was built from.
      --
      -- Nothing to consume and no rider: this segment is re-derived from the board
      -- every iteration, exactly as segment 1 is, and CR 615.10 leaves a static
      -- shield unreduced.
      fromProtectedPlayer (pid, src) =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity =
              CandidateId.OfPermanent
                PermanentCandidate.MkPermanentCandidate
                  { PermanentCandidate.source = src,
                    PermanentCandidate.provenance = ReplacementProvenance.Minted,
                    PermanentCandidate.effect = protectionShield pid,
                    PermanentCandidate.ordinal = InstanceOrdinal.MkInstanceOrdinal 0
                  },
            ReplacementCandidate.effect = protectionShield pid,
            ReplacementCandidate.source = src,
            ReplacementCandidate.controller = Projection.controllerOf src sources,
            ReplacementCandidate.lifetime = Nothing,
            ReplacementCandidate.origin = ReplacementOrigin.Other,
            ReplacementCandidate.rider = Nothing,
            ReplacementCandidate.slots = Map.empty
          }
      -- Rule 702.16e's row for one (protected player, carrier) pair.
      --
      -- The quality is CR 201.4's chosen names, spelled as Filter.HasChosenName
      -- -- the same set intersection PlayerEffect.protectedFromGiven asks for the
      -- targeting and Aura bars, so the three consequences of rule 702.16 cannot
      -- disagree about which sources carry the quality. A carrier that has chosen
      -- nothing intersects with nothing and matches no source, rather than
      -- shielding against everything.
      --
      -- The atom answers here because candidateContext fills
      -- Filter.sourceChosenNames off the carrier; it is what makes the read LIVE
      -- at the damage event (CR 609.7b) rather than frozen when this pair was
      -- gathered.
      --
      -- `whichRecipient` and not `whoRecipient`, though both name a player: the
      -- protected seat is an id this segment knows, where a PlayerRelation would
      -- have to be read against a perspective, and CR 109.5's "you" on this row is
      -- the carrier's controller rather than the protected player.
      --
      -- No kind, rule 702.16e saying "any damage", and no printed recipient half
      -- at all -- `whatRecipient` is the permanent half's spelling and this row is
      -- the other one.
      protectionShield pid =
        ReplacementEffect.DamageR
          DamageR.MkDamageR
            { DamageR.matching =
                DamagePattern.MkDamagePattern
                  { DamagePattern.whichKind = Nothing,
                    DamagePattern.whatSource = Filter.Type.HasChosenName,
                    DamagePattern.whatRecipient = Nothing,
                    DamagePattern.whoRecipient = Nothing,
                    DamagePattern.whichRecipient = Just (Recipient.ToPlayer pid),
                    DamagePattern.whichSource = Nothing
                  },
              DamageR.rewrite = DamageRewrite.PreventAll,
              DamageR.riders = Seq.empty
            }
      -- CR 614.1: a replacement effect is not locked in ahead of time, so the
      -- printed "if" the installing clause carried is asked HERE rather than at
      -- installation -- the row stays in GameState.replacements and is asked
      -- again next time. The permanent segment's twin is
      -- Projection.replacementsOf, which drops its own clause the same way --
      -- CR 604.2's there, since a permanent's is a static ability's, and CR
      -- 614.3's here, since a resolution installed this row.
      --
      -- The BOARD is Projection.boardAsEntering's, as replacementsOf's is, and
      -- for a reason neither of those two rules states: CR 614.12a puts the
      -- choice an entry replacement requires "before the permanent enters the
      -- battlefield", so a permanent the CR 616.1 loop is mid-way through has
      -- not entered and is on nobody's battlefield for ANY count made in that
      -- window. That is a claim about zone membership rather than about which
      -- segment asks, which is why the two segments cannot differ on it -- the
      -- same reason Event.apply's EntryRewrite.WithCounters arm reads that board
      -- for a floating row and a printed one alike. The VIEW stays live, as
      -- replacementsOf's does: what the rule takes off the board is membership,
      -- not characteristics.
      --
      -- Proven by Pawl.ReplacementSpec's Synthetic Magnetic Lockdown pair, the
      -- floating twin of its Frontier Mastodon pairs.
      --
      -- Only the CLAUSE narrows, and `applies` below keeps the live board
      -- deliberately: a row's PATTERN is answered against ONE object's view
      -- (matchesFiltered), against controllerOf, and against zone ownership, none
      -- of which folds the battlefield index boardAsEntering subtracts from, and
      -- the object each of them asks about is the entering permanent itself.
      -- `admitsEntry`'s Bloodthirst arm is the one measurement on that path, and
      -- it reads the event log rather than the board. So the two boards cannot
      -- differ there, and passing the narrowed one would say the same thing more
      -- obscurely.
      --
      -- CR 109.5's "you" is the row's BAKED controller, and the reading object is
      -- its source: the clause outlives the resolution that installed it, so
      -- there is nothing live to derive either from. The row's captured slots
      -- ride along, so a clause and its pattern read one context
      -- (candidateContext's).
      lives active = case ActiveReplacement.condition active of
        Nothing -> True
        Just condition ->
          Condition.holds
            (Projection.fullView sources)
            (Filter.contextWithSlots (Game.teams sources) (Just (ActiveReplacement.controller active)) (Just (ActiveReplacement.source active)) (ActiveReplacement.slots active))
            (Projection.boardAsEntering sources)
            (ActiveReplacement.source active)
            condition
   in fmap fromPermanent (numberInstances (Projection.replacementsAffecting sources))
        <> fmap fromFloating (filter lives floating)
        <> fmap fromProtectedPlayer (PlayerEffect.protectionCarriers sources)

-- CR 615.5: the additional effect a PERMANENT's static prevention ability
-- prints, packaged with the environment it will run in.
--
-- Nothing wherever the ability prints none, which is every arm but DamageR and
-- every DamageR whose `riders` is empty -- so the wildcard is over effects that
-- have nowhere to write one, not over effects this function declines to read.
--
-- The environment is BUILT here rather than snapshotted at installation, which
-- is the whole difference between this rider and a floating row's (see
-- Pawl.Types.ActiveReplacement): this ability's source is still an object the
-- board can answer about, so CR 109.5's "you" is whoever controls it now, and a
-- static ability targets nothing (CR 115.10a), so there are no chosen targets to carry -- the slot map
-- is empty. A source the board can no longer answer for has no "you", and gets
-- no rider rather than one performed by nobody.
--
-- `src` is CR 113.7's source, which for a static ability is the object that
-- prints it -- so the rider runs against the same object today's shielded
-- recipient is (Stormwild Capridor shields itself).
--
-- CR 115.1 / 113.7: the RESERVED self slot is bound to that source, which is not
-- a target and so does not contradict the paragraph above -- Pawl.Engine.Resolve
-- .performHandAction binds the same slot the same way for the same reason. It is
-- what lets a rider naming its own permanent by slot say "it": Protean Hydra's
-- "prevent that damage and remove that many +1/+1 counters from it", where
-- Pawl.Types.RemoveCounters names a slot and Pawl.Types.PutCounters (Stormwild
-- Capridor) names an ObjectRef and needs no binding.
printedRider :: ObjectId -> Maybe PlayerId -> ReplacementEffect Card (Effect.Effect Card (GrantedAbility.GrantedAbility Card)) -> Maybe PreventionRider.PreventionRider
printedRider src you re = case re of
  ReplacementEffect.DamageR damageR
    | not (Seq.null (DamageR.riders damageR)),
      Just controller <- you ->
        Just
          PreventionRider.MkPreventionRider
            { PreventionRider.effects = DamageR.riders damageR,
              PreventionRider.targets = Map.singleton Binding.triggerSource (Set.singleton (Recipient.ToObject src)),
              PreventionRider.controller = controller,
              PreventionRider.source = src
            }
  _ -> Nothing

-- CR 614.5 / 702.136b: number each gathered row by how many rows EQUAL to it
-- came before, so a permanent holding one ability twice offers two instances
-- rather than one -- "if a permanent has multiple instances of riot, each works
-- separately".
--
-- An ordinal among EQUALS rather than a position in the list, because the list
-- is re-derived every CR 616.1f iteration and CR 616.2 lets an application
-- change what is in it -- an entry replacement gained or lost anywhere earlier
-- would renumber a surviving row, handing it an identity CR 614.5 has already
-- spent. Counting equals cannot do that: a row is only ever renumbered by
-- another row it is already equal to.
--
-- Swapping this for a plain `zip [0 ..]` leaves the suite green, so that is a
-- fence rather than a proven line: no board in the pool moves a duplicated
-- replacement's position mid-loop. What the ordinal DOES prove is the duplicate
-- itself -- dropping it to a constant reddens Pawl.ReplacementSpec's two
-- "CR 702.136b riot twice" cases.
numberInstances :: [(ObjectId, ReplacementProvenance.ReplacementProvenance, ReplacementEffect Card (Effect.Effect Card (GrantedAbility.GrantedAbility Card)))] -> [(InstanceOrdinal.InstanceOrdinal, (ObjectId, ReplacementProvenance.ReplacementProvenance, ReplacementEffect Card (Effect.Effect Card (GrantedAbility.GrantedAbility Card))))]
numberInstances =
  let step seen row =
        let n = Map.findWithDefault 0 row seen
         in (Map.insert row (n + 1) seen, (InstanceOrdinal.MkInstanceOrdinal n, row))
   in snd . List.mapAccumL step Map.empty

-- The candidates that apply to this event. `asOf` is Nothing for a lone event
-- and Just the pre-batch board for a CR 608.2f batch (see Event.applyReplacementsIn);
-- `gs` is always the live state.
--
-- NOT the last word on which candidates the CR 616.1 loop offers: Event.loop
-- drops every candidate a permanent entering BESIDE this event's subject sources
-- (CR 614.12). That filter needs the batch, which is the loop's parameter rather
-- than this one's.
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
-- CR 701.19c / 122.1c: may this destruction rewrite be applied to this
-- destruction? Asked here, where a candidate is offered the event, so a shield
-- that is refused is also never CONSUMED. Each arm reads the field its own rule
-- names and ignores the other, which is why this reads the rewrite rather than
-- rejecting or admitting the whole DestructionR class:
--
--   * CR 701.19c bars a REGENERATION shield from a destruction that can't be
--     regenerated, and says nothing about what caused the destruction --
--     regeneration replaces CR 704.5g's lethal-damage destruction, which is the
--     shield's whole point.
--   * CR 122.1c's replacement watches only for a destruction "as the result of an
--     EFFECT", and says nothing about regenerability -- "removing a shield counter
--     in this way isn't the same as regenerating a creature", so Terror's clause
--     does not touch it. The rule's own reading of that restriction is the ruling
--     that a shielded creature "may still be destroyed by state-based actions if
--     it has damage marked on it equal to its toughness".
admits :: Regenerability.Regenerability -> DestructionCause.DestructionCause -> DestructionRewrite.DestructionRewrite -> Bool
admits regenerability cause rewrite = case rewrite of
  DestructionRewrite.Regenerate -> regenerability == Regenerability.Regenerable
  DestructionRewrite.RemoveShieldCounter -> cause == DestructionCause.ByEffect

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
  -- CR 615.10's static shield is never used up -- it "will apply separately to
  -- damage from other applicable events" -- so there is nothing to spend and
  -- nothing to refuse.
  DamageRewrite.PreventAllBut _ -> True
  -- CR 122.1c's prevention is spent by the COUNTER going away, not by a number on
  -- the row: Projection.shieldOf mints it only while a counter is there, so a
  -- shield spent to nothing is gone from the gathered list rather than present and
  -- refused here.
  DamageRewrite.PreventRemovingShieldCounter -> True
  DamageRewrite.SetAmount _ -> True
  DamageRewrite.Scale _ -> True
  -- CR 614.9's uncounted redirection has nothing to spend: it moves the
  -- recipient and leaves the amount alone, so no application can exhaust it.
  -- Its printed twin is the same rewrite with the destination described rather
  -- than named.
  DamageRewrite.Redirect _ -> True
  DamageRewrite.RedirectMatching _ -> True
  -- The counted redirection is spent as PreventNext is, in damage.
  DamageRewrite.RedirectNext remaining _ -> remaining > 0

-- CR 122.1c: does this damage rewrite admit the event's RECIPIENT? The shield's
-- prevention says "if damage would be dealt to THIS permanent", and that
-- self-scope is asked here rather than through DamagePattern.whichRecipient for
-- the reason Projection.shieldOf gives: that field is compared to the event's
-- Pawl.Types.Recipient tag, which records how the damage reached its recipient
-- rather than only which permanent it reached.
--
-- Every other rewrite answers True, its scope being the pattern's business alone
-- (Fog shields no one in particular; CR 615.7's row names its recipient in the
-- pattern). Exhaustive, so a later rewrite with a scope of its own is asked here
-- rather than silently given every recipient.
admitsRecipient :: ObjectId -> DamageRewrite.DamageRewrite -> DamageEvent.DamageEvent -> Bool
admitsRecipient src rewrite de = case rewrite of
  DamageRewrite.PreventRemovingShieldCounter -> Recipient.objectOf (DamageEvent.target de) == Just src
  DamageRewrite.PreventAll -> True
  DamageRewrite.PreventNext _ -> True
  DamageRewrite.PreventAllBut _ -> True
  DamageRewrite.SetAmount _ -> True
  DamageRewrite.Scale _ -> True
  DamageRewrite.Redirect _ -> True
  DamageRewrite.RedirectNext _ _ -> True
  DamageRewrite.RedirectMatching _ -> True

applies :: GameState -> ProposedEvent -> ReplacementCandidate -> Bool
applies gs event candidate =
  let src = ReplacementCandidate.source candidate
   in case (ReplacementCandidate.effect candidate, event) of
        -- CR 614.1a: which zone changes this redirect intercepts -- the
        -- destination, the moving object's OWNER, and (Anafenza, the Foremost's
        -- "a nontoken creature") what the moving object IS.
        --
        -- A pattern naming NO destination admits every one, which is CR 702.34a's
        -- "instead of putting it anywhere else"; the destinations are not
        -- enumerated, so this is a Maybe rather than a set of zones.
        (ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR pat _ _ _), ProposedEvent.WouldChangeZone zc) ->
          maybe True (== ZoneChange.to zc) (ZoneChangePattern.whenDestination pat)
            && matchesZoneOwner gs src (ReplacementCandidate.controller candidate) (ZoneChangePattern.whoseObject pat) (ZoneChange.object zc)
            && matchesFiltered gs candidate (ZoneChangePattern.whatObject pat) (ZoneChange.object zc)
        -- CR 615.1: which events the pattern admits (see matchesDamagePattern),
        -- plus the one fact about the ROW rather than the event -- a shield
        -- spent to nothing is no longer a prevention effect.
        --
        -- CR 614.9's prohibition is asked HERE and not in an inert application,
        -- which is the asymmetry `redirectable` below derives: a redirection a
        -- card forbids is not applicable, so it never reaches CR 616.1's choice.
        (ReplacementEffect.DamageR (DamageR.MkDamageR pat rewrite _), ProposedEvent.WouldDealDamage de) ->
          matchesDamagePattern gs (candidateContext gs candidate) pat de
            && unspent rewrite
            && admitsRecipient src rewrite de
            && (not (redirects rewrite) || redirectable gs de)
        -- CR 201.5 / 201.5c / 701.19a: "regenerate THIS creature" names the
        -- ability's own source, so a destruction replacement is self-only. CR
        -- 122.1c's "this permanent" is the same self-scope reached from the other
        -- direction: the effect is minted onto the permanent holding the counters.
        -- DestructionR carries no pattern because both producers are self-scoped.
        (ReplacementEffect.DestructionR rewrite, ProposedEvent.WouldBeDestroyed oid regenerability cause) ->
          src == oid && admits regenerability cause rewrite
        (ReplacementEffect.CounterR (CounterR.MkCounterR pat _), ProposedEvent.WouldPutCounters cause oid kind _) ->
          -- Our own encoding convention, not a rule: `whichKind = Nothing` means
          -- any kind, never no kind.
          maybe True (== kind) (CounterPattern.whichKind pat)
            && matchesPutter gs src (CounterPattern.subject pat) cause
            && matchesController gs src (CounterPattern.whose pat) oid
            && matchesPermanent gs Map.empty Nothing (CounterPattern.onWhat pat) oid
        -- CR 122.1 / 614.1: the same pattern against a PLAYER recipient. A
        -- pattern naming a kind admits none of these: `whichKind` is the object
        -- kinds, and a player can hold no counter of one (see
        -- Pawl.Types.CounterPattern), so Hardened Scales stays off a poison
        -- counter without saying so.
        (ReplacementEffect.CounterR (CounterR.MkCounterR pat _), ProposedEvent.WouldPutPlayerCounters cause pid _ _) ->
          Maybe.isNothing (CounterPattern.whichKind pat)
            && matchesPutter gs src (CounterPattern.subject pat) cause
            && maybe False (\rel -> matchesPlayer gs src rel pid) (CounterPattern.onWho pat)
        -- CR 614.16 at the ENTRY level: the counters this permanent is entering
        -- with are a placement this row may scale, and CR 616.1 orders it against
        -- every other row modifying the same entry. What it scales is the PENDING
        -- count; see GameState.enteringCounters.
        --
        -- The cause is synthesized as the one the entry counters WOULD have been
        -- placed under -- CR 122.6a's default putter, the object's controller --
        -- so matchesPutter still parts Vorinclex's "if you would put" from CR
        -- 614.16's "if an effect would put" (#847). Reading it off the flush
        -- rather than off the event is what keeps the two levels answering alike.
        (ReplacementEffect.CounterR (CounterR.MkCounterR pat _), ProposedEvent.WouldEnter oid) ->
          case Projection.controllerOf oid gs of
            Nothing -> False
            Just putter ->
              not (Map.null (matchingEnteringCounters gs pat oid))
                && matchesPutter gs src (CounterPattern.subject pat) (CounterCause.ByEffect putter)
                && matchesController gs src (CounterPattern.whose pat) oid
                && matchesPermanent gs Map.empty Nothing (CounterPattern.onWhat pat) oid
        -- CR 109.5: "under YOUR control" -- the tokens' controller against the
        -- effect source's controller. CR 102.2's Opponents has no producer today.
        --
        -- And WHAT is being created (Queen Allenal of Ruadach's "creature
        -- tokens"): "one or more" is ANY lot matching, each judged as its token
        -- would exist (CR 614.12) since none is minted yet.
        (ReplacementEffect.TokenR (TokenR.MkTokenR pat _ _), ProposedEvent.WouldCreateTokens pid lots) ->
          matchesPlayer gs src (TokenPattern.whose pat) pid
            && any (matchesTokenLot (candidateContext gs candidate) (TokenPattern.whatToken pat) pid) lots
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
        (ReplacementEffect.EntryR (EntryR.MkEntryR pat rewrite), ProposedEvent.WouldEnter oid) -> matchesFiltered gs candidate pat oid && admitsEntry gs oid rewrite
        -- CR 614.1e: which permanents turning face up this replacement watches.
        -- The same Filter language the entry arm above uses, and every producer
        -- writes Filter.IsSource -- CR 614.1e's printed wording is always "as
        -- THIS permanent is turned face up".
        --
        -- Matched against the LIVE projection, which for this event is the
        -- permanent as it is turning over: FaceDown.performTurnFaceUp has already
        -- written Facing.FaceUp when it raises the event, so CR 708.11's "would
        -- have ... after it's turned face up" is answered by asking about the
        -- board rather than by a counterfactual.
        --
        -- TurnUpR.requiring is the second conjunct, and it is the ROW's own
        -- condition rather than the rewrite class's. CR 702.37b's megamorph
        -- counter is the one row that carries it -- "put a +1/+1 counter on it IF
        -- ITS MEGAMORPH COST WAS PAID to turn it face up" -- so the row is
        -- refused down CR 701.40c's manifest road and off an Effect.TurnFaceUp
        -- that names no procedure at all, neither having paid that cost. Refused
        -- rather than consumed and skipped, which is CR 614.1's own shape for an
        -- ability whose condition is not met.
        --
        -- NOT scoped by rewrite class: CR 614.1e applies down every road, and a
        -- card writing an "as this creature is turned face up, put N counters on
        -- it" clause of its own -- Bubble Smuggler, whose road up is CR 702.168d's
        -- disguise procedure -- states no such condition; see #987. CR 303.4k's
        -- MayAttachTo has none either. Pawl.CardSpec holds that no printing
        -- authors a `requiring` at all, which is what keeps a card's row
        -- unconditional here.
        (ReplacementEffect.TurnUpR turnUpR, ProposedEvent.WouldTurnFaceUp oid procedure) ->
          matchesFiltered gs candidate (TurnUpR.matching turnUpR) oid
            && maybe True (\required -> procedure == Just required) (TurnUpR.requiring turnUpR)
        -- CR 122.1d's "a permanent with a stun counter on it" is the self-scope
        -- DestructionR's arm reads one event class over: the row is minted onto
        -- the permanent holding the counters, so `src` IS the rule's subject and
        -- UntapR needs no pattern. Rule 122.1d states no further restriction on
        -- WHICH untaps it reaches -- unlike CR 122.1c's "as the result of an
        -- effect" -- so there is no `admits` beside this.
        (ReplacementEffect.UntapR _, ProposedEvent.WouldUntap oid) -> src == oid
        -- CR 614.1a / 120.4c: whose life total the row watches (CR 109.5's
        -- "your"), what caused the loss, and whether the loss would actually
        -- carry the player past the total the rewrite leaves them at.
        --
        -- That third conjunct is CR 120.4d's own Worship example, which states
        -- it as APPLICABILITY rather than as a rewrite that changes nothing:
        -- "Worship's effect sees that the damage event would not reduce the
        -- player's life total to less than 1, so Worship's effect is not
        -- applied." So a loss the row would leave alone never reaches CR 616.1's
        -- choice, is never spent under CR 614.5, and prompts nobody.
        (ReplacementEffect.LifeLossR (LifeLossR.MkLifeLossR pat rewrite), ProposedEvent.WouldLoseLife cause pid n) ->
          maybe True (== cause) (LifeLossPattern.whichCause pat)
            && matchesPlayer gs src (LifeLossPattern.whose pat) pid
            && breaches gs (ReplacementCandidate.controller candidate) rewrite pid n
        -- CR 614.1a / 119.10: whose life total the row watches (CR 109.5's
        -- "your"), and whether the rewrite would actually resize the gain. That
        -- second conjunct is LifeLossR's third above, and for its reason: a gain
        -- the row would leave alone never reaches CR 616.1's choice and is never
        -- spent under CR 614.5.
        --
        -- Read off the SOURCE, the LifeLossR arm's posture rather than DrawR's:
        -- every gain producer is a permanent's static ability, whose CR 109.5
        -- "you" is its controller as projected now.
        (ReplacementEffect.LifeGainR (LifeGainR.MkLifeGainR whose rewrite), ProposedEvent.WouldGainLife pid n) ->
          matchesPlayer gs src whose pid
            && resizes rewrite n
        -- CR 121.6 / 614.11: whose card draws the row watches (CR 109.5's "you").
        -- The whole of the pattern, there being nothing else about a draw to
        -- narrow by, and no `admits` beside it: rule 614.11's first sentence
        -- makes the row apply even when the library is empty, so there is no
        -- board on which the rewrite would change nothing.
        --
        -- Read off the CANDIDATE, the ZoneChangeR arm's posture and not the
        -- src-derived arms below it: both producers install a FLOATING row from an
        -- activated ability, so the "you" was baked when the ability resolved and
        -- survives the source leaving the battlefield or changing hands -- which
        -- Ring of Ma'rûf's own cost makes it do. See matchesCandidatePlayer (#2662).
        (ReplacementEffect.DrawR pat, ProposedEvent.WouldDraw pid) ->
          matchesCandidatePlayer gs src (ReplacementCandidate.controller candidate) (DrawR.whose pat) pid
        -- CR 121.2a: whose draw INSTRUCTIONS the row watches (CR 109.5's "you"),
        -- and the number it names. Alms Collector's "two or more" is a condition on
        -- the event rather than a rewrite that changes nothing, LifeLossR's third
        -- conjunct above: an instruction naming one card never reaches CR 616.1's
        -- choice and is never spent under CR 614.5.
        --
        -- Read off the SOURCE, the LifeLossR arm's posture rather than DrawR's: the
        -- only producer is a permanent's static ability, whose CR 109.5 "you" is
        -- its controller as projected now.
        (ReplacementEffect.DrawCountR pat, ProposedEvent.WouldDrawCards pid n) ->
          n >= DrawCountR.atLeast pat
            && matchesPlayer gs src (DrawCountR.whose pat) pid
        -- Every row below falls through to False because an arm ABOVE already
        -- matches every event of that class: a row below fires only for a
        -- MISMATCHED class, where False is the correct answer rather than a
        -- stand-in for "not yet implemented".
        (ReplacementEffect.ZoneChangeR {}, _) -> False
        (ReplacementEffect.EntryR {}, _) -> False
        (ReplacementEffect.DamageR {}, _) -> False
        (ReplacementEffect.DestructionR _, _) -> False
        (ReplacementEffect.CounterR {}, _) -> False
        (ReplacementEffect.TokenR {}, _) -> False
        (ReplacementEffect.TurnUpR {}, _) -> False
        (ReplacementEffect.UntapR _, _) -> False
        (ReplacementEffect.LifeLossR {}, _) -> False
        (ReplacementEffect.LifeGainR {}, _) -> False
        (ReplacementEffect.DrawR {}, _) -> False
        (ReplacementEffect.DrawCountR {}, _) -> False
        (ReplacementEffect.PhaseR _, _) -> False

-- CR 614.1a: would this rewrite actually change the loss? `admits` and `unspent`
-- above, for the life-total class. A row this answers False for never reaches CR
-- 616.1's choice, is never spent under CR 614.5, and prompts nobody.
--
-- CR 120.4d's own Worship example is where the CR states that posture out loud,
-- and it states it as APPLICABILITY rather than as a rewrite that changes nothing:
-- "Worship's effect sees that the damage event would not reduce the player's life
-- total to less than 1, so Worship's effect is not applied."
--
-- `you` is CR 109.5's, the APPLYING row's controller, which is a different seat
-- from `pid` for any pattern but LifeLossPattern's Yours. Nothing where the row
-- has no controller: an ability whose "you" names nobody states no condition it
-- can meet.
--
-- An arm may read the PROPOSED AMOUNT here rather than in the printed CR 604.2
-- condition a Pawl.Types.PrintedReplacement carries, because no
-- Pawl.Types.Condition can see it: a condition is asked of the BOARD, and "at
-- least that many" is asked of the event.
breaches :: GameState -> Maybe PlayerId -> LifeLossRewrite.LifeLossRewrite -> PlayerId -> Natural -> Bool
breaches gs you rewrite pid n = case rewrite of
  -- Read off the LIVE board rather than off the event, because the event carries
  -- only the loss and the rule's threshold is on the resulting TOTAL. A player
  -- already at or below the floor answers True -- the loss would still take them
  -- lower -- and the rewrite then cuts it to nothing.
  --
  -- A player id nothing is filed under answers False: no total to compare, so
  -- nothing to replace.
  LifeLossRewrite.LeaveAtLeast floor_ -> case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Player.life player - toInteger n < toInteger floor_
  -- The board is not consulted: a scaling's whole applicability is arithmetic on
  -- the proposed amount, and rule 119.6's own death check is a state-based action
  -- rather than a bound this may not cross.
  --
  -- `Scaled (Multiply 1)` and `Scaled (AddMore 0)` resize nothing and are refused
  -- here. That is the Worship example's posture carried over rather than a rule of
  -- its own -- the CR states applicability only for the floor case -- and no
  -- printing in data/cards/ scales a life loss by an identity, so the refusal is a
  -- fence rather than a proved behaviour.
  LifeLossRewrite.Scaled scaling -> scale scaling n /= n
  -- Ashiok, Wicked Manipulator's "while your library has at least that many cards
  -- in it". Its own ruling states the other side: "If you would pay life while
  -- you control Ashiok and your library does not have at least that many cards in
  -- it, you'll just pay life as normal."
  --
  -- A COUNT of the library rather than a check that the top `n` exist, which is
  -- the same question asked of a list -- Pawl.Engine.Game.zoneMembers is the
  -- library in top-first order, and Pawl.Engine.Event's arm takes its cards off
  -- the front of that same list.
  LifeLossRewrite.ExileFromTopOfYourLibrary ->
    maybe False (\owner -> Natural.length (Game.zoneMembers Zone.Library owner gs) >= n) you

-- CR 614.1a: would this rewrite actually change the gain? `breaches` above, for
-- the gain class -- and simpler, since every arm of Pawl.Types.LifeGainRewrite
-- is arithmetic on the proposed amount and none reads the board.
--
-- `Scaled (Multiply 1)` and `Scaled (AddMore 0)` resize nothing and are refused,
-- which is `breaches`' Scaled arm carried over word for word: a fence rather than
-- a proved behaviour, no printing in data/cards/ scaling a life gain by an
-- identity.
resizes :: LifeGainRewrite.LifeGainRewrite -> Natural -> Bool
resizes rewrite n = case rewrite of
  LifeGainRewrite.Scaled scaling -> scale scaling n /= n

-- Does the REWRITE itself admit this entry, over and above the pattern matching
-- the entering object? `admits` and `unspent` above, for the entry class.
--
-- Almost every rewrite answers True unconditionally, because almost every
-- producer states its whole applicability in the wording the pattern already
-- carries. An ability that states a further condition of its own is what needs an
-- arm here: rule 702.145b's night-and-double-faced pair, and rule 702.54a's "if
-- an opponent was dealt damage this turn".
--
-- ONLY A RULE'S CONDITION, and every arm below is one. A CARD's condition on its
-- own entry clause -- Dust Animus's "if you control five or more untapped lands",
-- Monstrous War-Leech's "if it was kicked" -- is written where every other
-- card-authored gate on a replacement ability goes, CR 604.2's clause on
-- Pawl.Types.PrintedReplacement, which Pawl.Engine.Projection.replacementsOf has
-- already asked against the entering permanent (CR 614.12) before a row reaches
-- here. One condition mechanism, not two: no arm below reads a card-authored
-- condition, only a rule's.
--
-- One arm per constructor, no wildcard, so a new rewrite with a condition breaks
-- the build here as well as in bucketOfEffect, readsApplier and Event.apply. A
-- wildcard defaulting to True would silently apply such a rewrite always.
--
-- The GameState is `applies`' own, which for a CR 608.2f batch is the PRE-BATCH
-- board rather than the live one (see `applicable`). Immaterial today -- every
-- WouldEnter reaches here from Event.runEntry, which passes no `asOf` -- but a
-- batched entry would read the designation and the card off a frozen board.
admitsEntry :: GameState -> ObjectId -> EntryRewrite.EntryRewrite effect -> Bool
admitsEntry gs oid rewrite = case rewrite of
  EntryRewrite.AsCopy _ -> True
  EntryRewrite.ChoiceOf _ -> True
  EntryRewrite.ChoiceByCoinFlip _ -> True
  EntryRewrite.ChooseColor -> True
  EntryRewrite.ChooseBasicLandType -> True
  EntryRewrite.ChoosePlayer -> True
  EntryRewrite.ChooseCardNames _ -> True
  EntryRewrite.ChooseCardName _ -> True
  EntryRewrite.WithCounters {} -> True
  EntryRewrite.UnderSourceControl -> True
  EntryRewrite.SacrificeAnyNumber {} -> True
  -- CR 702.155b states no condition of its own -- a Saga with read ahead always
  -- has both intrinsic abilities -- so this admits every entry the row is
  -- collected for. CR 702.155a's turn-scoped narrowing is not a condition on the
  -- ENTRY at all; it gates the chapter TRIGGERS, in Pawl.Engine.Saga.
  EntryRewrite.ReadAhead -> True
  EntryRewrite.Riot -> True
  EntryRewrite.Unleash -> True
  -- CR 702.54a's own condition, the ability's rather than the pattern's: "IF AN
  -- OPPONENT WAS DEALT DAMAGE THIS TURN, this permanent enters with N +1/+1
  -- counters on it." Asked here rather than in Event's arm for rule 702.145b's
  -- reason below -- a rewrite that applied and then placed nothing would still
  -- take a CR 616.1e bucket and could be handed the entry ahead of a rewrite with
  -- something to do.
  --
  -- The measurement is Quantity.PlayersDealtDamageThisTurn, which reads CR
  -- 120.3a's damaged-player record (Pawl.Engine.Game.damagedPlayer): damage to
  -- an opponent's creature or to the controller herself is not damage to an
  -- opponent, and CR 119.4's bare life loss is not damage at all. "Was an
  -- opponent dealt damage" is that count compared against zero.
  --
  -- RULE 702.54b'S X FORM STATES NO CONDITION -- "this permanent enters with X
  -- +1/+1 counters on it, where X is the total damage your opponents have been
  -- dealt this turn" -- so Nothing admits the entry whatever the log holds, and X
  -- is read where the row applies (Pawl.Engine.Event). Petrified Wood-Kin with no
  -- damage dealt enters with zero counters because X is zero, not because the row
  -- was refused.
  --
  -- The COUNTERS cannot tell the two readings apart: X is a sum over the same
  -- opponents the arm below counts, so X is greater than zero exactly when that
  -- arm answers True, and admitting unconditionally places the same counters as
  -- admitting under rule 702.54a's condition would. CR 616.1's bucket is where
  -- they diverge, and a board does make that visible -- an opponent's Kismet
  -- (EntryRewrite.Tapped) is a second CR 616.1e candidate for the entering
  -- Petrified Wood-Kin, so with nothing dealt this turn admitting the row leaves
  -- two rows to race and raises Prompt.ChooseReplacement, where refusing it under
  -- rule 702.54a's condition would leave one row and ask nobody. That prompt is
  -- what Pawl.ReplacementSpec's Kismet case asserts.
  --
  -- CR 109.5's "you" is the ENTERING object's controller, read live off the board
  -- rather than off the candidate -- AsCopy's and SacrificeAnyNumber's posture,
  -- and what keeps `readsApplier` honest in answering False for this arm. The two
  -- coincide for a minted row, whose Filter.IsSource makes the entering permanent
  -- its own source; reading the object is what would still be right for a granted
  -- one.
  --
  -- The window is the event log's, which Engine.beginTurnOf clears at the turn
  -- handoff -- so "this turn" costs nothing here.
  EntryRewrite.Bloodthirst Nothing -> True
  EntryRewrite.Bloodthirst (Just _) ->
    let context = Filter.contextFor (Game.teams gs) (Projection.controllerOf oid gs) (Just oid)
     in maybe False (> 0) (Quantity.evaluate (Projection.fullView gs) context gs oid (Quantity.Type.PlayersDealtDamageThisTurn (PlayerRef.Relative PlayerRelation.Opponent)))
  -- CR 702.150a's own first condition, the ability's rather than the pattern's:
  -- "If this permanent WOULD ENTER WITH ONE OR MORE LOYALTY COUNTERS ON IT". The
  -- pending count is what that asks about (GameState.enteringCounters), so the
  -- row is inapplicable until CR 306.5b's has placed something -- which is also
  -- what makes CR 616.2 sequence the two: this row and CR 614.16's multiplier
  -- become applicable together on the pass after the loyalty row applied.
  --
  -- Rule 702.150a's SECOND condition -- "chose to pay life for any part of its
  -- cost" -- is asked where the row is minted instead
  -- (Pawl.Engine.Projection.intrinsicReplacementsOf), since CR 118.13a's
  -- announcement is recorded on the SPELL and this function is handed the
  -- permanent.
  EntryRewrite.Compleated _ -> Map.findWithDefault 0 CounterKind.Loyalty (enteringCountersOf gs oid) > 0
  EntryRewrite.Tapped -> True
  EntryRewrite.PayLifeOrTapped _ -> True
  EntryRewrite.RevealOrTapped _ -> True
  -- CR 614.1c's "as [this permanent] enters, [do something]". No condition here
  -- and Bloodthirst above is the contrast: rule 702.54a's condition is a RULE's,
  -- so it has nowhere else to live, while Monstrous War-Leech's "if it was
  -- kicked" is the CARD's and rides CR 604.2's clause on
  -- Pawl.Types.PrintedReplacement, which Pawl.Engine.Projection.replacementsOf
  -- has already asked before this row was ever collected.
  EntryRewrite.RunEffects _ -> True
  -- CR 702.145b's own two conditions, both of them the ability's rather than the
  -- pattern's: "IF IT IS NIGHT and this permanent is REPRESENTED BY A
  -- DOUBLE-FACED CARD, it enters transformed."
  --
  -- Asked here rather than in Event's arm so that CR 616.1 never offers the row:
  -- a rewrite that applied and then did nothing would still take CR 616.1d's
  -- bucket, and CR 616.1's highest-non-empty rule would hand it the entry ahead
  -- of a rewrite that had something to do.
  --
  -- The layout half is Card.backFace, which is Nothing for exactly the cards CR
  -- 712.1 does not count as double-faced -- the same question CR 712.14a asks one
  -- rule over (Pawl.Engine.Event.changeZoneEntering). It is asked at all because
  -- daybound can be GRANTED: nothing stops a static ability from putting it on a
  -- permanent with no second face, and the rule's condition is what says nothing
  -- happens then.
  EntryRewrite.EntersTransformed ->
    GameState.daytime gs == Just Daytime.Night
      && Maybe.isJust (Game.cardOf oid gs >>= Card.backFace)

-- CR 614.16 versus CR 614.1: does this placement's PROVENANCE satisfy the
-- pattern's subject?
--
-- ByEffect is rule 614.16's own subject, "if an EFFECT would put one or more
-- counters" -- so it admits exactly that rule's two causes and no others. CR 609.1
-- makes CR 714.3c's turn-based action neither, which is what keeps Doubling Season
-- off a Saga's advancing lore counter, and it makes a cost paid under CR 601.2h
-- neither for the same reason, which keeps Doubling Season off Soul Immolation's
-- "blight X".
--
-- ByPlayer is a clause naming a PLAYER instead (Vorinclex, Monstrous Raider's "if
-- you would put", "if an opponent would put"). Rule 714.3c has "that player" put
-- the lore counter, so such a clause reaches it: the two subjects disagree about
-- exactly this case, which is why the cause rides the event to here rather than
-- being spent at the funnel's door (#847).
--
-- ByAnything is a clause naming neither (Hardened Scales, Vizier of Remedies).
-- Rule 614.1 gives it nothing to narrow by, so every cause satisfies it -- CR
-- 120.3d's wither and infect counters included, which is the placement no other
-- subject and no other axis of the pattern can reach.
matchesPutter :: GameState -> ObjectId -> CounterSubject.CounterSubject -> CounterCause.CounterCause -> Bool
matchesPutter gs src subject cause = case (subject, cause) of
  (CounterSubject.ByEffect, CounterCause.ByEffect _) -> True
  (CounterSubject.ByEffect, CounterCause.ByPayment _) -> False
  (CounterSubject.ByEffect, CounterCause.ByRule _) -> False
  -- The three causes coincide here on purpose, and the rows are written out
  -- rather than collapsed through CounterCause.putter: a player clause asks WHO,
  -- and every cause names a player, but a fourth cause must be read against rule
  -- 614.16 above before it is answered here.
  (CounterSubject.ByPlayer rel, CounterCause.ByEffect pid) -> matchesPlayer gs src rel pid
  (CounterSubject.ByPlayer rel, CounterCause.ByPayment pid) -> matchesPlayer gs src rel pid
  (CounterSubject.ByPlayer rel, CounterCause.ByRule pid) -> matchesPlayer gs src rel pid
  -- Written out for the same reason, and answering True for a reason of its own:
  -- the clause narrows by nothing, not by "any player".
  (CounterSubject.ByAnything, CounterCause.ByEffect _) -> True
  (CounterSubject.ByAnything, CounterCause.ByPayment _) -> True
  (CounterSubject.ByAnything, CounterCause.ByRule _) -> True

-- CR 614.1: the ONE judgement of a ControllerRelation. The four readers below
-- each extract the two players their rule names -- CR 109.5's "you" as their
-- segment supplies it, and the event's player, an object's controller or an
-- object's owner -- and ask nothing else, so a new constructor is one arm here
-- that `-Werror` names rather than four it does not (see #2854, where the
-- Opponents arm shipped as `/=` in one reader and CR 102.3 in the other three).
--
-- Nothing matches nothing, on either side: an effect with no controller states
-- no relation it could satisfy and has no opponents, and an unknown player is
-- nobody's. Two absences are NOT a match. CR 108.4a's owner fallback is
-- Projection.controllerOf's already -- an object that exists always has a
-- controller there -- so `theirs` is Nothing only for an id naming no object,
-- which no proposed event carries; matchesController's old `controllerOf oid ==
-- controllerOf src` matched two absences on no board.
--
-- CR 303.4b's enchanted player is read off the SOURCE's attachment rather than
-- supplied, because every reader has the source in hand and no segment bakes
-- one: a floating row's source has left the battlefield, so it enchants nobody
-- and the arm admits nothing.
relationHolds :: GameState -> ObjectId -> Maybe PlayerId -> ControllerRelation -> Maybe PlayerId -> Bool
relationHolds gs src you rel theirs = case rel of
  ControllerRelation.Anyones -> True
  ControllerRelation.Yours -> both (==) you theirs
  ControllerRelation.Opponents -> both (Game.areOpponents gs) you theirs
  ControllerRelation.EnchantedPlayers -> both (==) (Projection.enchantedPlayerOf src gs) theirs
  where
    both f a b = Maybe.fromMaybe False (f <$> a <*> b)

-- CR 109.5 / 614.1: does this PLAYER satisfy a pattern's relation, read against
-- the controller of the effect's SOURCE?
--
-- matchesController's sibling for the players a pattern names outright rather
-- than through an object -- the token's controller (CR 111.2), the player putting
-- counters (CR 122.6a) and the player receiving them (CR 122.1).
matchesPlayer :: GameState -> ObjectId -> ControllerRelation -> PlayerId -> Bool
matchesPlayer gs src rel pid = relationHolds gs src (Projection.controllerOf src gs) rel (Just pid)

-- matchesPlayer's twin for a row whose "you" is the CANDIDATE's rather than a
-- fresh projection of its source -- the shape matchesZoneOwner already takes, and
-- for the argument written out there: a permanent's row derives its controller
-- live, while a FLOATING row baked one at installation because CR 608.2n has
-- already moved its source to another zone as a new object (CR 400.7). Reading
-- the source live instead unscopes such a row the moment the source leaves the
-- battlefield, and hands it to the new controller on a control change.
--
-- The arms still on matchesPlayer -- CounterR, TokenR, LifeLossR, LifeGainR -- are
-- equivalent to this today and are left alone: every producer of theirs is a
-- permanent's static ability, whose source is on the battlefield whenever the row
-- is consulted. DrawR is the first ControllerRelation pattern whose producer
-- installs a floating row, which is where the two readings come apart (#2662).
matchesCandidatePlayer :: GameState -> ObjectId -> Maybe PlayerId -> ControllerRelation -> PlayerId -> Bool
matchesCandidatePlayer gs src you rel pid = relationHolds gs src you rel (Just pid)

-- CR 109.5 / 614.1: does `oid` satisfy this pattern's controller relation, read
-- against the controller of the effect's SOURCE? Controller-based, unlike
-- matchesZoneOwner below. CR 102.2: no producer today for Opponents -- a counter
-- or token pattern scoped to an opponent's permanents.
matchesController :: GameState -> ObjectId -> ControllerRelation -> ObjectId -> Bool
matchesController gs src rel oid = relationHolds gs src (Projection.controllerOf src gs) rel (Projection.controllerOf oid gs)

-- CR 614.1 / 615.1: is this DAMAGE coming from a source the pattern admits?
-- Matched through Pawl.Engine.Filter, against the PROJECTED view of the damage's
-- own source (CR 113.7a) -- for a resolving spell, the spell on the stack, which
-- is also the object Resolve installs a floating row under.
--
-- Two readings ride one Filter. CR 609.7b's characteristic shield (Luminesce's
-- "black sources and red sources") is a predicate over that view, rechecked here
-- at the event exactly as the rule says rather than locked in when the shield was
-- created; CR 614.15's "this way" is `Filter.IsSource`, which compares the
-- candidate's identity to the Context's source and so keys Galvanic Blast's
-- metalcraft clause to the damage its own resolution deals. `And []` admits
-- everything and never forces the view, which is what keeps CR 615.10's "if a
-- source would deal damage" free of a projection.
--
-- The Context's source is a MAYBE because CR 615.12's carrier has one: a "damage
-- can't be prevented" effect stored by a resolution has no permanent behind it.
-- Nothing names no object, so IsSource -- the "THIS creature" of a printed clause
-- -- is vacuously False for it, and no printing writes that pair anyway.
--
-- CR 608.2h decides WHICH view: a source no longer in the zone it was expected to
-- be in is read from its last known information, which is CR 113.7a's own
-- sentence from the shield's side and CR 702.2e's from the keyword's. Ghitu
-- Fire-Eater is the case -- it sacrifices itself to pay for the ability that then
-- deals its damage -- and without the fallback Luminesce's colour disjuncts both
-- answer False on the blank view a dead id projects, so a red source's damage
-- goes unprevented. viewWithLastKnownAnywhere rather than viewWithLastKnown: the
-- id in hand IS the source, so there is no second object for the scoped reader to
-- protect from the substitution.
--
-- Nothing from that reader means the object is gone AND nothing was filed, which
-- falls back to the live blank view rather than to False -- today's behaviour,
-- kept because `And []` above must still admit everything when the view is
-- unavailable. No board separates the two: Pawl.Engine.Event files a record for
-- every object that leaves a zone, and Pawl.Engine.Departure for every object of
-- a departing player.
matchesDamageSource :: GameState -> Filter.Context -> Filter.Type.Filter Keyword.Type.Keyword -> DamageEvent.DamageEvent -> Bool
matchesDamageSource gs context filter_ de =
  let oid = DamageEvent.source de
      view = Maybe.fromMaybe (Projection.viewOfObject oid gs) (Projection.viewWithLastKnownAnywhere gs oid)
   in Filter.matches context view filter_

-- CR 615.1 / 614.1a: does this damage event have the qualities the pattern
-- names? Five of them: a pattern naming no KIND admits combat and noncombat
-- alike (CR 510.2's dealing versus CR 608's), one narrowing the SOURCE admits
-- only the damage a source it describes is dealing (CR 120.1's "an object that
-- deals damage is the source of that damage" -- CR 609.7b's characteristic, or
-- CR 614.15's identity), one DESCRIBING the recipient admits only damage
-- addressed to an object matching it (Stormwild Capridor's "to this creature"),
-- one NAMING a recipient admits only the damage addressed to the permanent or
-- player the engine baked in (CR 615.7), and one NAMING a source admits only the
-- damage the one object a player chose is dealing (CR 609.7a).
--
-- That chosen source is compared by ID and its properties are not re-read here:
-- CR 615.9 splits the question in two, and the property half is the Filter
-- conjunct above, so a shield against "a red source of your choice" whose source
-- has stopped being red matches this conjunct and fails that one.
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
matchesDamagePattern :: GameState -> Filter.Context -> DamagePattern.DamagePattern -> DamageEvent.DamageEvent -> Bool
matchesDamagePattern gs context pat de =
  maybe True (== DamageEvent.kind de) (DamagePattern.whichKind pat)
    && matchesDamageSource gs context (DamagePattern.whatSource pat) de
    && matchesPrintedRecipient gs context de pat
    && maybe True (== DamageEvent.target de) (DamagePattern.whichRecipient pat)
    && maybe True (== DamageEvent.source de) (DamagePattern.whichSource pat)

-- CR 615.1: does the damage's recipient satisfy the pattern's PRINTED recipient
-- side, which is the DISJUNCTION of its object half and its player half?
--
-- Divine Deflection's "you and/or permanents you control" is one CR 615.7 shield
-- over both kinds of recipient, so the two halves are joined by `or` and not by
-- the `and` every other conjunct of matchesDamagePattern uses. Reading them as a
-- conjunction would admit nothing at all: CR 120.3a's two kinds of recipient are
-- disjoint, so no damage event satisfies an object filter and a player relation
-- at once.
--
-- Neither half is a pattern that says nothing about the recipient: that pattern
-- carries Nothing on BOTH halves and admits every recipient, which is the
-- unrestricted reading DamagePattern documents and the empty disjunction would
-- get backwards.
matchesPrintedRecipient :: GameState -> Filter.Context -> DamageEvent.DamageEvent -> DamagePattern.DamagePattern -> Bool
matchesPrintedRecipient gs context de pat = case (DamagePattern.whatRecipient pat, DamagePattern.whoRecipient pat) of
  (Nothing, Nothing) -> True
  (what, who) ->
    maybe False (matchesDamageRecipient gs context de) what
      || maybe False (matchesRecipientPlayer context de) who

-- CR 120.3a / 109.5: is this damage addressed to a PLAYER the pattern's printed
-- clause names -- Divine Deflection's "dealt to you"?
--
-- False for an OBJECT recipient, matchesDamageRecipient's answer with the kinds
-- swapped: the two halves each admit one of CR 120.3a's two kinds and neither
-- guesses about the other.
--
-- "You" is the CONTEXT's perspective, which for a floating row is the controller
-- baked when the row was installed (candidateContext) rather than a controller
-- re-derived from a source the resolution has already left behind. A context with
-- no perspective has no "you" and admits no player, matchesPlayer's posture.
matchesRecipientPlayer :: Filter.Context -> DamageEvent.DamageEvent -> PlayerRelation.PlayerRelation -> Bool
matchesRecipientPlayer context de relation = case Recipient.playerOf (DamageEvent.target de) of
  Nothing -> False
  Just pid -> maybe False (\you -> PlayerRelation.holds (Filter.teams context) relation you pid) (Filter.perspective context)

-- CR 615.1: does the damage's RECIPIENT have the qualities the pattern's PRINTED
-- clause names -- Stormwild Capridor's "if noncombat damage would be dealt to
-- this creature"?
--
-- Read in the candidate's own Context, matchesDamageSource's exactly: the two
-- filters of one pattern must agree on what IsSource and CR 109.5's "you" mean,
-- and for a permanent's static ability both are answered off the permanent.
--
-- False for a PLAYER recipient (CR 120.3a), which is the only answer a Filter can
-- give about something that is not an object. Nothing is lost by it: a pattern
-- saying nothing about the recipient carries Nothing rather than a trivial
-- filter, so this is reached only where a card really did describe an object, and
-- a pattern whose sentence ALSO names a player says so in `whoRecipient`, which
-- matchesPrintedRecipient asks beside this one.
matchesDamageRecipient :: GameState -> Filter.Context -> DamageEvent.DamageEvent -> Filter.Type.Filter Keyword.Type.Keyword -> Bool
matchesDamageRecipient gs context de filter_ = case Recipient.objectOf (DamageEvent.target de) of
  Nothing -> False
  Just oid -> Filter.matches context (Projection.viewOfObject oid gs) filter_

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
--
-- CR 109.5's "you" comes from the CANDIDATE and not from a fresh
-- Projection.controllerOf on the source, because the two segments answer it
-- differently and only the candidate knows which it is: a permanent's row is
-- derived live off the board, while a FLOATING row baked its controller at
-- installation precisely because its source is a spell CR 608.2n has already
-- moved to another zone as a new object. Deriving it here instead silently
-- unscoped every floating "your graveyard" redirect the moment its own source
-- left the stack -- Yawgmoth's Will's second sentence, whose proof is
-- Pawl.PlayerEffectSpec's YawgmothsWill group.
--
-- CR 102.3's opponents and not "not mine": a teammate's card reaches their own
-- graveyard, and Pawl.TeamSpec's "CR 102.3 a teammate's card is not put into an
-- opponent's graveyard" is the test that proves it through this reader.
-- CR 303.4b's enchanted player is Wheel of Sun and Moon's, and
-- Pawl.ZoneChangeSpec's "Wheel of Sun and Moon reroutes only the enchanted
-- player's cards" is the proof.
matchesZoneOwner :: GameState -> ObjectId -> Maybe PlayerId -> ControllerRelation -> ObjectId -> Bool
matchesZoneOwner gs src you rel oid = relationHolds gs src you rel (fmap Object.owner (Game.lookupObject oid gs))

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
--
-- `slots` is what the surrounding announcement or resolution bound, for a
-- criterion that names one of them -- "a creature other than the target",
-- Filter.IsBound. A counter pattern is read off a static ability with nothing
-- announced, so its two callers hand over none.
matchesPermanent :: GameState -> Map.Map SlotName.SlotName (Set ObjectId) -> Maybe ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> Bool
matchesPermanent gs slots source filter_ oid =
  Filter.matches (Filter.contextWithSlots (Game.teams gs) Nothing source slots) (Projection.viewOfObject oid gs) filter_

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
--
-- `slots` is matchesPermanent's: Pawl.Engine.Cost hands over CR 601.2c's targets
-- (Cost.announcedSlots) and Pawl.Engine.Resolve's edict its Context's, while the
-- as-enters offer has nothing to hand, no announcement being in flight there.
sacrificeCandidates :: Map.Map SlotName.SlotName (Set ObjectId) -> PlayerId -> Maybe ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
sacrificeCandidates slots pid source filter_ gs =
  let matching = List.sort (filter (matchesPermanent gs slots source filter_) (Projection.controls pid gs))
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
-- 109.5's "you" (Gather Specimens as a floating row, Kismet as printed static
-- text). The perspective is the CANDIDATE's controller, which for a floating row
-- is the baked one -- deriving it from the
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
  Filter.matches (candidateContext gs candidate) (Projection.viewOfObject oid gs) filter_

-- The Context a candidate's own Filters are read in: CR 109.5's "you" is the
-- ROW's controller -- the baked one for a floating row, since deriving it from
-- the source would answer Nothing once the spell that made it has resolved --
-- and the source is what CR 614.1c's and CR 614.15's `IsSource` compares against.
--
-- Shared by matchesFiltered above and by the damage arm of `applies`, so a
-- shield naming its source by characteristic (Luminesce) and an entry
-- replacement naming its own permanent (Clone) read one context.
--
-- The row's own captured slot bindings ride along, which is what lets a floating
-- pattern's Filter.IsBound name the object the INSTALLING resolution bound
-- (Dire Fleet Daredevil's "that spell"); empty for a permanent's static ability,
-- where the atom is vacuously False.
--
-- CR 303.4b's host rides along too, read LIVE off the board rather than carried:
-- the source of a permanent's static replacement is on the battlefield to be
-- asked what it is attached to, which is what lets Pariah's printed redirect
-- destination say "enchanted creature" (Filter.IsHostOfSource). Nothing for a
-- floating row whose source has left, the atom's ordinary vacuous answer.
--
-- CR 201.4's chosen names and CR 614.1c's chosen player ride along on the same
-- footing, and LIVE for CR 609.7b's reason: a prevention shield's source half is
-- rechecked at the event, so a name or a seat the source chose between the row's
-- installation and the damage is the one this reads. The names go through
-- Pawl.Engine.PlayerEffect.chosenNamesOf, which falls back to CR 608.2h's last
-- known information for a source already in a graveyard; the player has no such
-- fallback because Pawl.Types.LastKnown records no chosen player, and rule
-- 702.16k's carrier is a permanent on the battlefield whenever its own static
-- ability mints a row.
candidateContext :: GameState -> ReplacementCandidate -> Filter.Context
candidateContext gs candidate =
  ( Filter.contextWithSlots
      (Game.teams gs)
      (ReplacementCandidate.controller candidate)
      (Just (ReplacementCandidate.source candidate))
      (ReplacementCandidate.slots candidate)
  )
    { Filter.sourceAttachedTo = Projection.hostOf (ReplacementCandidate.source candidate) gs,
      Filter.sourceChosenNames = PlayerEffect.chosenNamesOf (Just (ReplacementCandidate.source candidate)) gs,
      Filter.carrierChosenPlayer = Game.lookupObject (ReplacementCandidate.source candidate) gs >>= Object.chosenPlayer
    }

-- CR 614.12 for a token that does not exist yet: a lot's token is judged off
-- the characteristics it would have on the battlefield -- its given text, or
-- for CR 707.1's copy token the copiable values it takes (CR 707.2). The
-- controller is the event's player (CR 111.2), which is what lets a lot say
-- "under your control" on the Filter axis too.
matchesTokenLot :: Filter.Context -> Filter.Type.Filter Keyword.Type.Keyword -> PlayerId -> TokenLot.TokenLot -> Bool
matchesTokenLot context filter_ pid lot =
  let view = case TokenLot.copy lot of
        Just snapshot -> Count.viewOfSnapshot (Just pid) True Map.empty snapshot
        Nothing -> (Projection.viewOfCard (NonEmpty.head (Card.Type.faces (TokenLot.card lot)))) {Filter.controller = Just pid}
   in Filter.matches context view filter_

-- The counters `oid` is so far entering with, empty outside an entry (see
-- GameState.enteringCounters).
enteringCountersOf :: GameState -> ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural
enteringCountersOf gs oid = Map.findWithDefault Map.empty oid (GameState.enteringCounters gs)

-- Those of them a pattern's kind admits. CR 614.16's rows name no kind (Doubling
-- Season doubles any counter), and `whichKind = Nothing` is this module's "any
-- kind" for the reason the WouldPutCounters arm gives.
matchingEnteringCounters :: GameState -> CounterPattern.CounterPattern -> ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural
matchingEnteringCounters gs pat oid =
  let pending = enteringCountersOf gs oid
   in case CounterPattern.whichKind pat of
        Nothing -> pending
        Just kind -> Map.filterWithKey (\k _ -> k == kind) pending

-- CR 614.1a: apply a scaling to a number. "Plus one" and "twice that many" are
-- the same operation with different data, and so is Furnace of Rath's doubling
-- -- which is why CounterR, TokenR (CR 614.16's two shapes) and DamageR all
-- rewrite through this one function.
scale :: Scaling.Scaling -> Natural -> Natural
scale s n = case s of
  Scaling.Multiply m -> n * m
  Scaling.AddMore m -> n + m
  -- CR 122.1: saturating, for the reason Pawl.Types.Scaling gives -- and one of
  -- the two scalings that can answer zero.
  Scaling.Subtract m -> Natural.minusSaturating n m
  -- CR 107.1a: "half that many . . . rounded down", which `div` on Natural
  -- already is. One is what makes this answer zero, as Subtract above does.
  Scaling.Halve -> n `div` 2

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
bucketOfEffect :: ReplacementEffect Card (Effect.Effect Card (GrantedAbility.GrantedAbility Card)) -> ReplacementBucket
bucketOfEffect re = case re of
  ReplacementEffect.ZoneChangeR {} -> ReplacementBucket.Other
  -- CR 616.1c: entering as a copy is its own, HIGHER bucket. The split only
  -- becomes observable where an AsCopy races another entry replacement of NO
  -- HIGHER bucket in the SAME iteration, which is an entering Clone under an
  -- opponent's Kismet -- proved by Pawl.ReplacementSpec's "the copy bucket
  -- outranks Kismet's, so no order is asked", on the absence of the prompt, since
  -- CR 616.1f makes the two orders converge on one board. An entering Clone on
  -- its own does not exercise it: AsCopy is the only applicable candidate on the
  -- first iteration, and what carries the rest is CR 616.1f's re-collection plus CR
  -- 614.5's identity being keyed on the effect VALUE rather than a list
  -- position, which keeps the newly-acquired ChoiceOf distinct from the
  -- already-applied AsCopy. Gather Specimens racing an entering Clone is a real
  -- same-iteration race, but CR 616.1b's bucket outranks this one, so it
  -- exercises that arm instead.
  -- CR 707.9's exceptions do not move the bucket: rule 616.1c asks whether the
  -- object is entering as a copy, which an excepted copy still is.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.AsCopy _)) -> ReplacementBucket.CopyOnEntry
  -- CR 616.1a-d name self-replacement, entering under a control effect, entering
  -- as a copy and entering with the back face up. None of the next five arms is
  -- any of those, so CR 616.1e is what applies to each.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.ChoiceOf _)) -> ReplacementBucket.Other
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.ChoiceByCoinFlip _)) -> ReplacementBucket.Other
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.ChooseColor) -> ReplacementBucket.Other
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.ChooseBasicLandType) -> ReplacementBucket.Other
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.ChoosePlayer) -> ReplacementBucket.Other
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.ChooseCardNames _)) -> ReplacementBucket.Other
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.ChooseCardName _)) -> ReplacementBucket.Other
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.WithCounters {})) -> ReplacementBucket.Other
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.SacrificeAnyNumber {})) -> ReplacementBucket.Other
  -- CR 702.136a is none of CR 616.1a-d either: riot rewrites what the permanent
  -- enters WITH, never whose it is, what it copies or which face is up.
  -- CR 616.1e for CR 702.155b's reason: read ahead rewrites how many lore
  -- counters the Saga enters with, never whose it is, what it copies or which
  -- face is up.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.ReadAhead) -> ReplacementBucket.Other
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.Riot) -> ReplacementBucket.Other
  -- CR 702.98a is none of CR 616.1a-d for riot's reason, one keyword over.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.Unleash) -> ReplacementBucket.Other
  -- CR 702.54a is none of CR 616.1a-d for riot's reason too: bloodthirst rewrites
  -- what the permanent enters WITH. Its condition does not change the bucket --
  -- `admitsEntry` has already kept an unsatisfied row out of the collection.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.Bloodthirst _)) -> ReplacementBucket.Other
  -- CR 616.1a-d name self-replacement, control on entry, copy on entry and back
  -- face up; CR 702.150a is none of them -- compleated rewrites HOW MANY loyalty
  -- counters the permanent enters with. So CR 616.1e, the same bucket CR 614.16's
  -- counter multipliers fall in, which is what puts the two rows in one race.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.Compleated _)) -> ReplacementBucket.Other
  -- CR 614.1d is none of CR 616.1a-d either: a tap-state rewrite changes the
  -- STATUS the permanent enters with (CR 110.5b), never whose it is, what it
  -- copies or which face is up. So CR 616.1e.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.Tapped) -> ReplacementBucket.Other
  -- CR 614.1c's paid variant of the same rewrite is none of CR 616.1a-d either,
  -- and paying life does not make it one: what the rewrite changes is still the
  -- STATUS the permanent enters with (CR 110.5b). So CR 616.1e.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.PayLifeOrTapped _)) -> ReplacementBucket.Other
  -- CR 614.1c's revealed variant, PayLifeOrTapped's answer for its reason: what
  -- the rewrite changes is the STATUS the permanent enters with (CR 110.5b), and
  -- CR 701.20b makes the reveal itself change nothing at all. So CR 616.1e.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.RevealOrTapped _)) -> ReplacementBucket.Other
  -- CR 616.1b: a control-on-entry rewrite is one step ABOVE the copy bucket, and
  -- Gather Specimens racing an entering Clone is the board where the two orders
  -- disagree: taking the control rewrite first hands Clone's own CR 109.5 copy
  -- choice to the NEW controller, and taking the copy first hands it to the old
  -- one.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.UnderSourceControl) -> ReplacementBucket.ControlOnEntry
  -- CR 616.1d, and the only arm that is: "replacement and/or prevention effects
  -- that would cause a card to enter the battlefield with its back face up".
  -- That is the whole of what daybound's rewrite does (CR 702.145b), so it ranks
  -- below CR 616.1c's copy bucket and above everything else.
  --
  -- The ordering is proved by Pawl.ReplacementSpec's "the back-face bucket
  -- outranks Kismet's, so no order is asked": a daybound creature entering at
  -- night under an opponent's Kismet, again on the absence of the prompt.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.EntersTransformed) -> ReplacementBucket.BackFaceOnEntry
  -- CR 616.1e: an as-enters rewrite that runs an effect is none of CR 616.1a-d --
  -- it is not a self-replacement, does not change the permanent's controller, is
  -- not a copy and shows no other face -- so it falls to the catch-all bucket
  -- with every other entry rewrite that merely does something.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.RunEffects _)) -> ReplacementBucket.Other
  ReplacementEffect.DamageR {} -> ReplacementBucket.Other
  ReplacementEffect.DestructionR _ -> ReplacementBucket.Other
  ReplacementEffect.CounterR {} -> ReplacementBucket.Other
  ReplacementEffect.TokenR {} -> ReplacementBucket.Other
  -- CR 616.1a-d are all about entering the battlefield and copying; turning face
  -- up is neither, so CR 616.1e.
  ReplacementEffect.TurnUpR {} -> ReplacementBucket.Other
  -- CR 616.1a-d are all about entering the battlefield and copying; becoming
  -- untapped is neither, so CR 616.1e.
  ReplacementEffect.UntapR _ -> ReplacementBucket.Other
  -- CR 616.1a-d are all about entering the battlefield and copying; a life total
  -- is neither, so CR 616.1e.
  ReplacementEffect.LifeLossR {} -> ReplacementBucket.Other
  ReplacementEffect.LifeGainR {} -> ReplacementBucket.Other
  -- CR 616.1a-d are all about entering the battlefield and copying; drawing a
  -- card is neither, so CR 616.1e.
  ReplacementEffect.DrawR {} -> ReplacementBucket.Other
  -- CR 616.1a-d are all about entering the battlefield and copying; an instruction
  -- to draw cards is neither, so CR 616.1e.
  ReplacementEffect.DrawCountR {} -> ReplacementBucket.Other
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
readsApplier :: ReplacementEffect Card (Effect.Effect Card (GrantedAbility.GrantedAbility Card)) -> Bool
readsApplier re = case re of
  -- The destination zone is the effect's own field, and the pattern is matched
  -- before Event.apply runs (Rest in Peace, Leyline of the Void). CR 701.20's
  -- reveal rider does not change the answer: it shows the MOVING object's owner
  -- (CR 400.3), never the row's controller.
  ReplacementEffect.ZoneChangeR {} -> False
  -- CR 707.5 / 109.5: Clone's "you" is the ENTERING object's controller, read
  -- live off the board at CR 614.12a's moment, not the candidate's -- so two
  -- such rows offer the same player the same legal set. Both halves of the
  -- payload ride the effect -- the eligible filter and CR 707.9's exceptions --
  -- so two rows carrying the same ones are still the same offer.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.AsCopy _)) -> False
  -- Same chooser, and the options ride the effect: CR 614.1c's "enters as"
  -- (Primal Plasma).
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.ChoiceOf _)) -> False
  -- NO chooser at all, and the options ride the effect just the same: CR 705.2's
  -- first sentence makes the face nobody's to pick, so two such rows are the same
  -- offer whoever's is applying (Molten Sentry).
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.ChoiceByCoinFlip _)) -> False
  -- Same chooser again, with no payload at all: CR 105.1's five colours are the
  -- whole offer whoever's row is applying (Painter's Servant).
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.ChooseColor) -> False
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.ChooseBasicLandType) -> False
  -- Same chooser again, and the CANDIDATES are the board's rather than the
  -- applier's: CR 102.1's players in the game are the whole offer whoever's row
  -- is applying (Stuffy Doll).
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.ChoosePlayer) -> False
  -- Two choosers rather than one, and neither is the candidate's: the entering
  -- object's controller is read live off the board for ChooseColor's reason, and
  -- CR 102.2's opponent is derived from that same player. The restriction rides
  -- the effect (CR 201.4a).
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.ChooseCardNames _)) -> False
  -- ONE chooser, and still not the applier's: the entering object's controller
  -- is read live off the board for ChooseColor's reason.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.ChooseCardName _)) -> False
  -- CR 614.1c's "enters with": the counter kind and count are the effect's own
  -- fields, and they land on the entering object (CR 306.5b's loyalty included).
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.WithCounters {})) -> False
  -- CR 614.1c again, and NO despite performing a sacrifice: the sacrificing
  -- player is the ENTERING object's controller, read live off the board at CR
  -- 614.12a's moment for AsCopy's reason, and the criterion and counter kind ride
  -- the effect. Two such rows would offer the same player the same permanents.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.SacrificeAnyNumber {})) -> False
  -- CR 702.155b: the chooser is the ENTERING Saga's controller, read live off
  -- the board for riot's reason below, and the bound is the entering Saga's own
  -- final chapter number (CR 714.2d) rather than anything the row carries -- so
  -- nothing about this rewrite is read off the applier.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.ReadAhead) -> False
  -- CR 702.136a: riot's chooser is the ENTERING object's controller, read live
  -- off the board for AsCopy's reason, and the rewrite carries no payload at all
  -- -- rule 702.136a fixes both halves. Two riot rows are always on the SAME
  -- object, since CR 614.1c's ability is the entering permanent's own, and they
  -- offer that permanent's controller the same two outcomes -- so which applies
  -- first is not a board difference.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.Riot) -> False
  -- CR 702.98a: riot's answer, and every word of its reasoning holds -- the
  -- chooser is the entering object's controller and the rewrite carries no
  -- payload, so two unleash rows offer that player the same counter twice.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.Unleash) -> False
  -- CR 702.54a: no chooser at all, and the count rides the effect. The condition
  -- `admitsEntry` asks reads the ENTERING object's controller rather than the
  -- applier, so two bloodthirst rows on one permanent are admitted together and
  -- place the same counters in either order. Rule 702.54b's X is the same answer
  -- for the same reason: Pawl.Engine.Event reads its sum off that controller's
  -- opponents, and two X rows on one permanent read one number.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.Bloodthirst _)) -> False
  -- CR 702.150a: no chooser at all, and the symbol count rides the effect.
  -- Applying it reads the row's payload and the entering object's pending
  -- counters, nothing off the candidate -- so two compleated rows of equal count
  -- subtract the same amount in either order.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.Compleated _)) -> False
  -- CR 614.1d: no chooser at all, and no payload -- the rewrite sets one status on
  -- the object the event already named (CR 110.5b), so it applies the same way
  -- whoever's row is applying it. Two such rows are the same write twice.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.Tapped) -> False
  -- CR 614.1c: NO despite spending a resource, for SacrificeAnyNumber's reason.
  -- The payer is the ENTERING object's controller -- "you" in an "as this
  -- permanent enters" ability the permanent prints about itself -- read live off
  -- the board at CR 614.12a's moment rather than off the candidate, and the
  -- amount rides the effect. Two such rows are always on the same object and
  -- would offer that object's controller the same price.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.PayLifeOrTapped _)) -> False
  -- CR 614.1c: NO, and PayLifeOrTapped's reasoning holds word for word. The
  -- revealer is the ENTERING object's controller -- "your hand" in an ability the
  -- permanent prints about itself -- read live off the board at CR 614.12a's
  -- moment rather than off the candidate, and the criterion rides the effect. Two
  -- such rows are always on the same object and would offer that object's
  -- controller the same cards.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.RevealOrTapped _)) -> False
  -- THE ONE ARM THAT ANSWERS YES. CR 616.1b / 110.2 / 109.5: the rewrite hands
  -- the permanent to the candidate's own `controller`, baked when the row was
  -- installed. Two Gather Specimens are one card, so their `effect` values are
  -- identical while their controllers are not, and applying one puts the
  -- permanent somewhere applying the other does not.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.UnderSourceControl) -> True
  -- CR 702.145b: no chooser at all, and no payload -- the rewrite shows
  -- the back face of the card the event already named, and which face that is
  -- comes from the card. Two such rows are the same write twice, Tapped's answer.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ EntryRewrite.EntersTransformed) -> False
  -- CR 614.1c's as-enters effects, Bloodthirst's and SacrificeAnyNumber's answer
  -- for their reason: Pawl.Engine.Event's arm reads CR 109.5's "you" off the
  -- ENTERING object rather than off the candidate, so two rows alike in `effect`
  -- run the same effects for the same player whoever holds the row.
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.RunEffects _)) -> False
  -- The rewritten amount is the effect's (Galvanic Blast, Furnace of Rath), and
  -- a prevention prevents the same event whoever's row it is (Fog). CR 615.7's
  -- shield is no exception: what makes two shields differ is how much each has
  -- LEFT, which rides the effect value and so is already inside `choose`'s
  -- comparison rather than needing the applier to be read.
  ReplacementEffect.DamageR {} -> False
  -- CR 701.19a acts on the creature being destroyed and names no player.
  ReplacementEffect.DestructionR _ -> False
  -- The scaling is the effect's, and it rewrites the count on the object the
  -- event already named (Hardened Scales, Doubling Season).
  ReplacementEffect.CounterR {} -> False
  -- CR 614.16, the same shape one event class over: the player the tokens are
  -- created FOR rides the EVENT, not the candidate, so Doubling Season doubles
  -- the same player's tokens whoever's row applies.
  ReplacementEffect.TokenR {} -> False
  -- CR 702.37b via CR 614.1e: the counter kind and count are the effect's own
  -- fields and they land on the object the event already named, which is
  -- WithCounters' answer one event class over. The inner sum is cased so a
  -- second TurnUpRewrite -- CR 208.2b's power-and-toughness setter -- has to be
  -- decided here rather than inheriting this answer.
  ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR _ _ (TurnUpRewrite.WithCounters {})) -> False
  -- CR 303.4k: "the AURA's controller" makes the choice, and the Aura is the
  -- object the event already named -- so the player asked is read off the event
  -- rather than off whose row is applying, and two identical rows would put the
  -- same question to the same player. The destination Filter is the effect's own
  -- field, inside `choose`'s comparison already.
  ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR _ _ (TurnUpRewrite.MayAttachTo _)) -> False
  -- CR 122.1d acts on the permanent becoming untapped and names no player -- CR
  -- 701.19a's answer one event class over, and for its reason.
  ReplacementEffect.UntapR _ -> False
  -- The rewrite is the effect's own field and the player is the one the event
  -- already named, so two rows alike in `effect` resize the same loss the same way
  -- whoever holds them. CR 109.5's "your" is read in `applies` rather than in
  -- Event.apply, which is CounterR's split exactly.
  --
  -- The inner sum is cased, TurnUpR's discipline above for its reason: a rewrite
  -- that DOES read the applier has to be decided here rather than inheriting this
  -- answer, and nothing else in this module would break its build.
  ReplacementEffect.LifeLossR (LifeLossR.MkLifeLossR _ (LifeLossRewrite.LeaveAtLeast _)) -> False
  ReplacementEffect.LifeLossR (LifeLossR.MkLifeLossR _ (LifeLossRewrite.Scaled _)) -> False
  -- CR 616.1 / 109.5: the exile comes off the CANDIDATE's own controller's
  -- library, so two rows alike in `effect` and unlike in `you` empty different
  -- libraries -- and `breaches` asks that same seat's count, so one may apply
  -- where the other does not. The other arm that answers True,
  -- EntryRewrite.UnderSourceControl, answers it under a different rule; what the
  -- two share is reading the candidate's own `controller` to apply.
  --
  -- A FENCE rather than a proved behaviour, and provably so: the only printing
  -- with this rewrite pairs it with LifeLossPattern's Yours, so two such rows
  -- match one loss only if their two controllers are the same seat -- which CR
  -- 704.5j's legend rule does not leave standing. No board can put the two
  -- answers apart, and False would still be the wrong classification.
  ReplacementEffect.LifeLossR (LifeLossR.MkLifeLossR _ LifeLossRewrite.ExileFromTopOfYourLibrary) -> True
  -- The gain side's whole rewrite sum, and the two resizing loss arms' answer:
  -- the scaling is the effect's own field and the player is the one the event
  -- named. Cased on the inner sum, LifeLossR's discipline above for its reason.
  ReplacementEffect.LifeGainR (LifeGainR.MkLifeGainR _ (LifeGainRewrite.Scaled _)) -> False
  -- The player who gains the life is the one the EVENT named, and the amount is
  -- the effect's own field, so two rows alike in `effect` gain the same seat the
  -- same life whoever holds them. CR 109.5's "you" is read in `applies` rather
  -- than in Event.apply, which is CounterR's split exactly.
  --
  -- The inner sum is cased, LifeLossR's discipline above for its reason: a
  -- rewrite that DOES read the applier has to be decided here rather than
  -- inheriting this answer.
  ReplacementEffect.DrawR (DrawR.MkDrawR _ (DrawRewrite.GainLife _)) -> False
  -- The card goes to the player the EVENT named and the filter and the reveal
  -- ride the effect, so GainLife's answer above carries over. The one value this
  -- rewrite reads off the CANDIDATE is `source`, which reaches nothing but the
  -- Filter.Context Event.eligible scans the pool under.
  --
  -- Not implemented: two such rows alike in `effect` and unlike in `source` would
  -- offer different cards if the card's filter named its own source, and
  -- `distinguishing` below folds `source` in nowhere, so the choice between them
  -- is elided (#3215).
  ReplacementEffect.DrawR (DrawR.MkDrawR _ (DrawRewrite.FromOutsideTheGame _)) -> False
  -- CR 121.2a: "you and that player each draw a card" -- the "you" is the row's
  -- own controller, so two rows alike in `effect` and unlike in `you` hand the
  -- extra card to different seats. LifeLossRewrite.ExileFromTopOfYourLibrary's
  -- answer, and for its reason. Unlike that one this is a PROVED behaviour rather
  -- than a fence: Alms Collector is not legendary, so two of them under different
  -- controllers stand at once, and Pawl.ReplacementSpec's "CR 616.1 two collectors
  -- under different controllers are told apart, and the drawer chooses" is what
  -- proves the drawer is asked which one applies.
  ReplacementEffect.DrawCountR (DrawCountR.MkDrawCountR _ _ DrawCountRewrite.EachDrawOne) -> True
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
-- the same reason -- every use of it above is a test run BEFORE Event.apply, save
-- the one arm that hands it on as a Filter.Context, which readsApplier's own
-- comment argues (#3215).
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
    -- object's controller and CR 310.9d substitutes the protector only for the
    -- "defending player", which rule 616 nowhere says.
    Recipient.ToBattle oid -> Projection.controllerOf oid gs
    Recipient.ToObject oid -> Projection.controllerOf oid gs
    -- Unreachable: CR 406.4's pile is a candidate at CR 601.2c and never a
    -- target, so nothing deals damage to one.
    Recipient.ToPile _ -> Nothing
  ProposedEvent.WouldBeDestroyed oid _ _ -> Projection.controllerOf oid gs
  ProposedEvent.WouldPutCounters _ oid _ _ -> Projection.controllerOf oid gs
  -- CR 616.1's affected player is the one the event happens to, which for a
  -- counter put on a PLAYER is that player -- not the one putting it.
  ProposedEvent.WouldPutPlayerCounters _ pid _ _ -> Just pid
  ProposedEvent.WouldCreateTokens pid _ -> Just pid
  -- CR 616.1's "affected player": a step or phase beginning affects no object,
  -- so the player whose turn it is chooses among applicable skips.
  ProposedEvent.WouldBeginPhase _ pid -> Just pid
  -- CR 616.1's affected object is the permanent turning over, and its controller
  -- is CR 702.37e's "you" -- the player who took the special action.
  ProposedEvent.WouldTurnFaceUp oid _ -> Projection.controllerOf oid gs
  -- CR 616.1's affected object is the permanent that would become untapped, and
  -- CR 122.1d gives its controller the choice among applicable rows even at CR
  -- 502.3's turn-based action, where that controller is the active player.
  ProposedEvent.WouldUntap oid -> Projection.controllerOf oid gs
  -- CR 616.1's affected player is the one the event happens to, which for a life
  -- loss is the player losing it -- WouldPutPlayerCounters' answer, and for its
  -- reason. Not the damage's source or its controller: by CR 120.4c the damage
  -- is dealt and settled, and what is being replaced is a result of it.
  ProposedEvent.WouldLoseLife _ pid _ -> Just pid
  -- CR 616.1's affected player is the one the event happens to, which for a life
  -- gain is the player gaining it -- WouldLoseLife's answer one direction over.
  -- Not the source causing the gain: CR 119.10 reads the clause as "if a source
  -- would cause [a player] to gain life", and the player is who it happens to.
  ProposedEvent.WouldGainLife pid _ -> Just pid
  -- CR 616.1's affected player is the one the event happens to, which for a draw
  -- is the player who would draw the card -- WouldLoseLife's answer, and for its
  -- reason. There is no affected OBJECT: CR 121.1's card is not chosen until the
  -- draw happens.
  ProposedEvent.WouldDraw pid -> Just pid
  -- CR 616.1 / 121.2a: the instruction is addressed to that same player, so
  -- the seat that chooses among applicable rows is the one it names.
  ProposedEvent.WouldDrawCards pid _ -> Just pid

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

-- CR 707.9: fold a copy effect's "except ..." clauses into the snapshot the copy
-- is about to be stamped with. Left fold, so a later clause overrides an earlier
-- one on the same characteristic -- no printed card writes two that collide, and
-- the printed order is the order the sentence reads in.
--
-- Into the SNAPSHOT and not onto the object, which is CR 707.9b: the excepted
-- value "becomes part of the copiable values of the copy", so a token copy of
-- the copy inherits it (CR 707.2) where a CR 613 layer-7b write would be left
-- behind. Pawl.CopySpec's "a token copy of an excepted copy keeps the exception"
-- is what proves the two apart: it reads 7/7 here and the copied Tarmogoyf's CDA
-- under the layer reading.
applyCopyExceptions :: [CopyException.CopyException] -> PC.ProjectedCharacteristics -> PC.ProjectedCharacteristics
applyCopyExceptions exceptions snapshot = List.foldl' applyCopyException snapshot exceptions

-- One arm per CopyException constructor, no wildcard, for Event.apply's reason: a
-- new exception shape must break the build here rather than silently copy without
-- it.
applyCopyException :: PC.ProjectedCharacteristics -> CopyException.CopyException -> PC.ProjectedCharacteristics
applyCopyException snapshot exception = case exception of
  -- CR 707.9b sets the pair; CR 707.9d is the second write -- an exception that
  -- "provides a specific set of values for a certain characteristic" does not
  -- copy the characteristic-defining ability that defines it, and leaving the CDA
  -- in the snapshot would let layer 7a overwrite the pair
  -- (Projection.applyCharacteristicPT). Not defensive, unlike applyEntryOption's
  -- same write: Quicksilver Gargantuan copying a Tarmogoyf is exactly this case.
  CopyException.SetPowerToughness (SetPowerToughness.MkSetPowerToughness p t) ->
    snapshot
      { PC.power = Just p,
        PC.toughness = Just t,
        PC.characteristicPT = Nothing
      }
  -- CR 707.9a: the gained abilities become part of the copiable values "along
  -- with any other abilities that were copied", so this ADDS rather than sets.
  -- unionWith (+) and not a plain union, applyEntryOption's reason: an instance
  -- gained here is a second instance on a copy that already had the keyword, and
  -- CR 702.105b makes each instance of dethrone trigger separately.
  --
  -- Nothing else moves. CR 604.3a(2) makes an ability acquired through a copy
  -- effect characteristic-defining, and writing it here is what delivers that:
  -- Projection's layer-4 seed reads these keywords, so an excepted changeling
  -- reaches applySubtypeDefining (CR 613.3, CR 702.73a) rather than being
  -- granted in timestamp order. No companion strip either, unlike the arm above
  -- -- CR 707.9d applies to an exception that provides VALUES for a
  -- characteristic, and gaining an ability provides none.
  CopyException.GainKeywords keywords ->
    snapshot {PC.keywords = Map.unionWith (+) (PC.keywords snapshot) (Map.fromSet (const 1) keywords)}

-- CR 707.5 / 614.12a: the permanents an entering copy may choose. Battlefield
-- permanents matching the rewrite's printed noun phrase, other than itself, minus
-- anything entering in the same batch (see the CR 614.12a note on
-- Event.applyReplacementsIn for why the batch set, not 614.13a, is what excludes
-- them).
--
-- The Filter comes off the card (Pawl.Types.AsCopy's `eligible`) rather than
-- being hardcoded here (#1512): Clone writes "any creature" and Copy Enchantment
-- "any enchantment", and this function must not know which. The zone is the one
-- half that is NOT the card's to say -- "on the battlefield" is the domain the
-- walk below supplies.
--
-- Matched through each candidate's own CR 613 projection, revealableFromHand's
-- reading, so a continuous effect that made a permanent an enchantment reaches
-- it. Perspective and source are the ENTERING object's controller and the
-- entering object, so a filter naming "you" or "this" resolves against the
-- player making the choice (CR 109.5); nothing printed today uses either, and
-- supplying Nothing would silently answer False if one did.
legalCopyTargets :: Set ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> GameState -> [ObjectId]
legalCopyTargets batch filter_ self gs =
  let context = Filter.contextFor (Game.teams gs) (Projection.controllerOf self gs) (Just self)
      viewOf = Projection.viewsOf gs
      eligible oid =
        oid /= self
          && not (Set.member oid batch)
          && Filter.matches context (viewOf oid) filter_
   in filter eligible (Set.toAscList (GameState.battlefield gs))

-- CR 614.1c / 701.20a: the cards a player may reveal from their hand to satisfy
-- an "as this enters, you may reveal a [matching] card from your hand" ability
-- (Rustic Clachan). Beside legalCopyTargets for its reason: an entry rewrite's
-- offer is classification, and this module is what Pawl.Engine.Event calls down
-- into for it.
--
-- Matched through each card's own CR 613 projection, the reading Effect.Search's
-- CR 701.23a filter takes one zone over: rule 613.1 starts from the actual object
-- and names no zone, so a continuous effect that made a card a Kithkin card
-- reaches it in a hand.
--
-- No perspective and no source in the context (CR 109.5): the printed filter
-- states a quality of the card ("a Kithkin card") and names no player, so
-- ControlledBy and IsSource are vacuously False. Whose hand is asked by the zone
-- lookup instead, which is the whole of the "from your hand" in the sentence.
revealableFromHand :: PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
revealableFromHand pid filter_ gs =
  let viewOf = Projection.viewsOf gs
      matching oid = Filter.matches (Filter.contextFor (Game.teams gs) Nothing Nothing) (viewOf oid) filter_
   in filter matching (Game.zoneMembers Zone.Hand pid gs)

-- CR 614.3: a floating replacement whose `uses` is Once is spent by being
-- applied. A permanent's STATIC replacement ability has no use count at all --
-- it is re-derived from the battlefield every iteration -- so only the floating
-- store is touched here.
consume :: CandidateId -> Game ()
consume identity_ = case identity_ of
  CandidateId.OfPermanent {} -> pure ()
  CandidateId.OfFloating floating ->
    State.modify' $ \gs ->
      let src = FloatingCandidate.source floating
          ts = FloatingCandidate.timestamp floating
          spent active =
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
--
-- The caller passes the rewrite with the new remainder already written in --
-- PreventNext for a shield, RedirectNext for Harm's Way's counted redirection,
-- which is spent in the same unit -- and the 0 that drops the row is read off
-- `remainingOf`, so the two counted rewrites cannot disagree about when a row
-- is used up.
setShield :: CandidateId -> DamageR.DamageR (Effect.Effect Card (GrantedAbility.GrantedAbility Card)) -> DamageRewrite.DamageRewrite -> Game ()
setShield identity_ damageR spent = case identity_ of
  CandidateId.OfPermanent {} -> pure ()
  CandidateId.OfFloating floating ->
    State.modify' $ \gs ->
      let src = FloatingCandidate.source floating
          ts = FloatingCandidate.timestamp floating
          mine active =
            ActiveReplacement.source active == src
              && ActiveReplacement.timestamp active == ts
          rewrite active
            | not (mine active) = Just active
            | remainingOf spent == Just 0 = Nothing
            | otherwise = Just active {ActiveReplacement.effect = ReplacementEffect.DamageR damageR {DamageR.rewrite = spent}}
       in gs {GameState.replacements = Maybe.mapMaybe rewrite (GameState.replacements gs)}

-- CR 615.7's remaining amount, for the two rewrites that count one down --
-- Mending Hands' shield and Harm's Way's counted redirection -- and Nothing for
-- every rewrite that has no such number. `unspent` and `contestedResource` read
-- the same field through their own arms; this is the write-back's reading.
--
-- A CLASSIFICATION of effects, in `prevents`' genre: one arm per constructor,
-- no wildcard, so a third counted rewrite is asked here rather than never
-- dropped at 0.
remainingOf :: DamageRewrite.DamageRewrite -> Maybe Natural
remainingOf rewrite = case rewrite of
  DamageRewrite.PreventNext remaining -> Just remaining
  DamageRewrite.RedirectNext remaining _ -> Just remaining
  DamageRewrite.PreventAll -> Nothing
  DamageRewrite.PreventAllBut _ -> Nothing
  DamageRewrite.PreventRemovingShieldCounter -> Nothing
  DamageRewrite.SetAmount _ -> Nothing
  DamageRewrite.Scale _ -> Nothing
  DamageRewrite.Redirect _ -> Nothing
  DamageRewrite.RedirectMatching _ -> Nothing

-- CR 614.9 over the counted shape CR 615.7 states for preventions -- the
-- redirect's own countdown rests on Harm's Way's rulings: how much of this
-- damage event the candidate's rewrite will actually MOVE, where that is less
-- than the whole event. Harm's Way's "the next 2 damage ... is dealt to any
-- target instead" meeting a 5-damage event moves 2 and leaves 3 on the original
-- recipient, and that residue is a second EVENT rather than a smaller one --
-- which is what separates this from PreventNext, whose residue is the same
-- event shrunk and is handled inside its own arm of Event.apply. Event.loop
-- splits the event on this answer before applying the candidate to the covered
-- part.
--
-- Nothing for every rewrite that takes the event whole, for a counted redirect
-- large enough to cover all of it, and for one CR 614.9's guard has made inert
-- (a destination that left): that one hands the event back unchanged and
-- spends nothing (CR 609.7b), so there is nothing to split.
--
-- A CLASSIFICATION of effects, in `prevents`' genre: one arm per constructor,
-- no wildcard.
partialCoverage :: GameState -> ReplacementCandidate -> ProposedEvent -> Maybe Natural
partialCoverage gs candidate event = case (ReplacementCandidate.effect candidate, event) of
  (ReplacementEffect.DamageR (DamageR.MkDamageR _ rewrite _), ProposedEvent.WouldDealDamage de) -> case rewrite of
    DamageRewrite.RedirectNext remaining dest
      | Maybe.isJust (redirectDestination gs dest) && remaining < DamageEvent.amount de -> Just remaining
      | otherwise -> Nothing
    DamageRewrite.PreventNext _ -> Nothing
    DamageRewrite.PreventAll -> Nothing
    DamageRewrite.PreventAllBut _ -> Nothing
    DamageRewrite.PreventRemovingShieldCounter -> Nothing
    DamageRewrite.SetAmount _ -> Nothing
    DamageRewrite.Scale _ -> Nothing
    DamageRewrite.Redirect _ -> Nothing
    DamageRewrite.RedirectMatching _ -> Nothing
  _ -> Nothing

-- The two halves of a damage event `partialCoverage` split: the covered part,
-- which the candidate is applied to, and the residue, which continues through
-- the loop as an event of its own with the same source, recipient and riders.
-- Nothing for an event that is not damage or a cover that is not smaller than
-- it; the caller has already asked `partialCoverage`, so neither arises.
splitDamage :: Natural -> ProposedEvent -> Maybe (ProposedEvent, ProposedEvent)
splitDamage covered event = case event of
  ProposedEvent.WouldDealDamage de
    | covered < DamageEvent.amount de ->
        Just
          ( ProposedEvent.WouldDealDamage de {DamageEvent.amount = covered},
            ProposedEvent.WouldDealDamage de {DamageEvent.amount = DamageEvent.amount de - covered}
          )
  _ -> Nothing

-- CR 615.1a: is this damage rewrite a PREVENTION effect, rather than one of CR
-- 614.1a's replacements? "Effects that use the word 'prevent' are prevention
-- effects", and that word is what CR 615.13's trigger watches for -- so a
-- SetAmount that cuts an event from 3 to 1 has prevented nothing, though it
-- shrank the event exactly as a shield would.
--
-- A CLASSIFICATION of effects -- what SHAPE a rewrite has, never which effect it
-- is -- in the same genre as bucketOf and readsApplier above, and
-- contestedResource below. One arm per constructor, no wildcard, so a new
-- rewrite that prevents damage breaks the build here rather than silently going
-- unreported.
--
-- A question about the REWRITE alone, and deliberately not about the event: CR
-- 615.12's unpreventable damage is still met by a prevention effect, which is
-- still a prevention effect for having prevented nothing of it. `preventable`
-- below is the other half, and `inertPrevention` asks them together.
prevents :: DamageRewrite.DamageRewrite -> Bool
prevents rewrite = case rewrite of
  DamageRewrite.PreventNext _ -> True
  DamageRewrite.PreventAll -> True
  -- Temple Altisaur says "prevent all but 1 of that damage", so CR 615.1a makes
  -- this one a prevention too -- which is the whole reason it is not SetAmount.
  DamageRewrite.PreventAllBut _ -> True
  -- CR 122.1c says "prevent that damage", so CR 615.1a makes this one too.
  DamageRewrite.PreventRemovingShieldCounter -> True
  DamageRewrite.SetAmount _ -> False
  DamageRewrite.Scale _ -> False
  -- CR 614.9's redirection is a rule-614 replacement. Turn the Tables never says
  -- "prevent" -- the damage is still dealt, one recipient over -- so this False
  -- is what keeps `preventionBy`, `inertPrevention` and CR 615.13's trigger away
  -- from it.
  DamageRewrite.Redirect _ -> False
  -- Harm's Way counts its damage down as a shield does and still never says
  -- "prevent": the 2 it moves is dealt, one recipient over.
  DamageRewrite.RedirectNext _ _ -> False
  DamageRewrite.RedirectMatching _ -> False

-- CR 615.12: applied to damage that CAN'T be prevented, does this rewrite still
-- spend what `contestedResource` counts? The rule's middle and last sentences
-- disagree by rewrite, which is why this is not `prevents`: CR 122.1c's counter
-- is an ADDITIONAL effect and comes off either way ("any additional effects they
-- have will take place"), where CR 615.7's stored amount is an "existing damage
-- prevention shield" and is explicitly not reduced.
--
-- Must agree arm for arm with Pawl.Engine.Event.applyInertly, which is where the
-- spending actually happens; True here without a corresponding action there would
-- contest a resource nothing consumes.
--
-- A CLASSIFICATION of effects, in `prevents`' genre: one arm per constructor, no
-- wildcard, so a new prevention rewrite with an additional effect breaks the
-- build here.
spentInertly :: DamageRewrite.DamageRewrite -> Bool
spentInertly rewrite = case rewrite of
  DamageRewrite.PreventRemovingShieldCounter -> True
  DamageRewrite.PreventNext _ -> False
  -- CR 615.10's static shield has no stored amount at all, and no additional
  -- effect of its own, so there is nothing for CR 615.12's middle clause to run
  -- and nothing for its last clause to protect. `contestedResource` gives it no
  -- supply either, so it never reaches `hitsOf`.
  DamageRewrite.PreventAllBut _ -> False
  -- Fog has no resource to spend at all, so this could answer either way;
  -- `contestedResource` gives it no supply and it never reaches `hitsOf`.
  DamageRewrite.PreventAll -> False
  -- CR 614.1a's replacements are not preventions, are applied in full to
  -- unpreventable damage, and have no contested resource either.
  DamageRewrite.SetAmount _ -> False
  DamageRewrite.Scale _ -> False
  DamageRewrite.Redirect _ -> False
  DamageRewrite.RedirectNext _ _ -> False
  DamageRewrite.RedirectMatching _ -> False

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
  Just oid -> liveDestination gs oid

-- CR 614.9's two conditions asked of ONE object id, and the CR 613.1d re-tag
-- beside them. Split out of redirectDestination above so the PRINTED
-- destination below reaches the same three lines: a description and a baked id
-- must agree about what a live destination is, or Pariah and Turn the Tables
-- would disagree about a phased-out creature.
liveDestination :: GameState -> ObjectId -> Maybe Recipient.Recipient
liveDestination gs oid
  | not (Set.member oid (GameState.battlefield gs)) = Nothing
  | Projection.isCreatureOf oid gs = Just (Recipient.ToCreature oid)
  | Projection.isPlaneswalkerOf oid gs = Just (Recipient.ToPlaneswalker oid)
  | Projection.isBattleOf oid gs = Just (Recipient.ToBattle oid)
  | otherwise = Nothing

-- CR 614.9: which recipient a PRINTED redirection sends the damage to --
-- Pariah's "is dealt to enchanted creature instead".
--
-- The battlefield's UNIQUE match, read live at the damage event rather than
-- swept when the ability began, for the CR 611.2c reason DamagePattern's
-- printed recipient halves give: a static ability's destination is a
-- description, so an Aura that changes what it enchants redirects somewhere
-- else from then on.
--
-- None is CR 614.9's "the effect does nothing", which Pawl.Engine.Event.apply
-- turns into the event handed back unchanged -- the same answer a baked
-- destination that left the battlefield gets, and for the same sentence.
-- SEVERAL is that answer too, and no card in the pool reaches it: the pool's one
-- printed destination is Filter.IsHostOfSource (Pariah), which names at most one
-- permanent. Nothing in the type stops a card describing several, so this is the
-- answer the rule leaves for that shape -- it redirects to "another battle,
-- creature, planeswalker, or player" and never to a set.
--
-- The battlefield rather than every object, because that is where CR 614.9's
-- own guard puts a destination; liveDestination then answers the rest.
printedDestination :: GameState -> Filter.Context -> Filter.Type.Filter Keyword.Type.Keyword -> Maybe Recipient.Recipient
printedDestination gs context filter_ =
  let viewOf = Projection.viewsOf gs
   in case [oid | oid <- Set.toList (GameState.battlefield gs), Filter.matches context (viewOf oid) filter_] of
        [oid] -> liveDestination gs oid
        _ -> Nothing

-- CR 615.12: could a prevention effect prevent any of this damage event, or is
-- this damage that "can't be prevented" (Spider-Punk)?
--
-- FALSE does not mean the prevention effect is inapplicable, and the rule is
-- emphatic about the difference: "any applicable prevention effects are STILL
-- APPLIED to it. Those effects won't prevent any damage, but any additional
-- effects they have will take place." So `applies` is untouched by this
-- question, the CR 616.1 loop still offers the row and still marks it applied --
-- which is CR 615.12a's "just once", falling straight out of the applied-set the
-- loop already carries -- and what changes is only that the event survives
-- undiminished and the shield is not spent ("existing damage prevention shields
-- won't be reduced by damage that can't be prevented"). The application's
-- ADDITIONAL effect still happens, in Pawl.Engine.Event.applyInertly.
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
--
-- CR 109.5's "you" for one of these patterns is the controller of the ability's
-- own source (Questing Beast's "creatures you control"), derived here rather than
-- carried: unlike a floating shield row, both of CR 615.12's carriers name a
-- source to derive it from -- the printed static ability's permanent, and the
-- object a CR 611.2c stored effect resolved off (Lava Burst's own spell, still on
-- the stack while the damage it deals is proposed).
preventable :: GameState -> DamageEvent.DamageEvent -> Bool
preventable gs de = not (any (\(src, pat) -> matchesDamagePattern gs (patternContext gs src) pat de) (PlayerEffect.unpreventable gs))

-- CR 109.5's Context for a CR 613.10/613.11 pattern: the perspective is the
-- controller of the effect's own source, derived off the board rather than
-- carried. Shared by the two questions of that shape so they cannot disagree
-- about what "you" and IsSource mean.
patternContext :: GameState -> Maybe ObjectId -> Filter.Context
patternContext gs src = Filter.contextFor (Game.teams gs) (src >>= \oid -> Projection.controllerOf oid gs) src

-- CR 614.9: is this rewrite a REDIRECTION -- "dealt instead to another permanent
-- or player"? The classification `redirectable` below is gated on, in the genre
-- of `prevents` and `spentInertly` above: one arm per constructor, no wildcard,
-- so a second rewrite that moves a damage event's recipient is asked here rather
-- than silently escaping a card's prohibition.
redirects :: DamageRewrite.DamageRewrite -> Bool
redirects rewrite = case rewrite of
  DamageRewrite.Redirect _ -> True
  -- The same rule with the destination described rather than named, so a card
  -- forbidding redirection (Lava Burst) stops both.
  DamageRewrite.RedirectMatching _ -> True
  -- And the same rule counted down, so it stops Harm's Way too.
  DamageRewrite.RedirectNext _ _ -> True
  DamageRewrite.PreventNext _ -> False
  DamageRewrite.PreventAll -> False
  DamageRewrite.PreventAllBut _ -> False
  DamageRewrite.PreventRemovingShieldCounter -> False
  -- CR 614.1a's two amount rewrites leave the recipient where it was, so neither
  -- deals the damage "instead to another permanent or player".
  DamageRewrite.SetAmount _ -> False
  DamageRewrite.Scale _ -> False

-- CR 614.9: may a redirection effect move this damage event, or does a card
-- forbid it (Lava Burst)?
--
-- FALSE means the redirection is NOT APPLICABLE, and that is where this parts
-- company with `preventable` beside it. CR 615.12 keeps an inapplicable
-- prevention in the applicable set -- "any applicable prevention effects are
-- still applied to it ... any additional effects they have will take place" --
-- and rule 614.9 states no twin of that sentence; its only "the effect does
-- nothing" case is a destination that left the battlefield or a player who left
-- the game (redirectDestination above). So `applies` drops the row instead, and
-- CR 616.1's loop never offers it as a choice.
--
-- Asked per EVENT for `preventable`'s reason, and read through the same
-- matchesDamagePattern, so one reading of what a DamagePattern means serves CR
-- 615.1's shields, CR 615.12's prohibition and this one.
--
-- Delegated to Pawl.Engine.PlayerEffect, which owns the CR 613.10/613.11 axis:
-- this module sees a DamagePattern and never a PlayerEffect constructor.
redirectable :: GameState -> DamageEvent.DamageEvent -> Bool
redirectable gs de = not (any (\(src, pat) -> matchesDamagePattern gs (patternContext gs src) pat de) (PlayerEffect.unredirectable gs))

-- CR 615.12: is this the pairing the rule describes -- a PREVENTION effect
-- chosen against damage that CAN'T BE PREVENTED? Just means the application
-- happens and prevents nothing: Pawl.Engine.Event's CR 616.1 loop hands the event
-- back undiminished and marks the row applied, spending neither a use nor a point
-- of shield.
--
-- The REWRITE comes back rather than a Bool, because CR 615.12's middle clause --
-- "any additional effects they have will take place" -- makes the inert
-- application's remaining obligation a question about which prevention this is.
-- Pawl.Engine.Event.applyInertly is where that is answered, per constructor, so
-- the classification stays here and the doing stays there.
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
inertPrevention :: GameState -> ReplacementCandidate -> ProposedEvent -> Maybe DamageRewrite.DamageRewrite
inertPrevention gs candidate event = case (ReplacementCandidate.effect candidate, event) of
  (ReplacementEffect.DamageR (DamageR.MkDamageR _ rewrite _), ProposedEvent.WouldDealDamage de)
    | prevents rewrite && not (preventable gs de) ->
        Just rewrite
  _ -> Nothing

-- CR 615.5 / 615.13: the application of a prevention effect that just happened,
-- carrying how much of the event it prevented and the additional effect it owes.
-- Nothing means the pair is not a prevention of damage at all.
--
-- `before` is the event as it was offered to Event.apply and `after` what came back,
-- so the amount is the CR 615.6 arithmetic read off the pair: Nothing means the event
-- does not happen at all, which is the whole of it prevented.
--
-- `inert` is Event.loop's own `inertPrevention` answer, Just when CR 615.12 made
-- this application prevent nothing, and it is what makes an UNDIMINISHED event
-- still report a prevention -- at an amount of 0. The rule is emphatic that such
-- an effect is still applied and that "any additional effects they have will take
-- place", and CR 615.5's rider travels on this record. Passed in rather than
-- re-derived: this function is handed no board, and the board it would have to
-- ask is not the one the classification was made against -- `applyInertly` has
-- run in between.
--
-- ZERO is therefore a load-bearing amount, and the two consumers in
-- Pawl.Engine.Damage read it differently. The CR 615.5 rider queue takes every
-- entry, the rule owing the rider nothing about how much was prevented; CR
-- 615.13's DamagePrevented record is gated above 0, which is that rule's own
-- condition -- such an ability triggers each time a prevention effect is applied
-- "and prevents some or all of that damage".
--
-- The wildcard is over (effect, event) PAIRS, where it is the only way to say
-- "this pair is not a prevention of damage"; the per-constructor obligation this
-- module carries is discharged by `prevents` above, which the guard delegates to.
preventionBy :: Maybe DamageRewrite.DamageRewrite -> ReplacementCandidate -> ProposedEvent -> Maybe ProposedEvent -> Maybe Prevention
preventionBy inert candidate before after = case (ReplacementCandidate.effect candidate, before) of
  (ReplacementEffect.DamageR (DamageR.MkDamageR _ rewrite _), ProposedEvent.WouldDealDamage de)
    | prevents rewrite ->
        let was = DamageEvent.amount de
            -- The event that did not happen prevented all of it (CR 615.6).
            now = maybe 0 DamageEvent.amount (after >>= asDamageEvent)
         in if Maybe.isJust inert || was > now
              then
                Just
                  Prevention.MkPrevention
                    { Prevention.by = ReplacementCandidate.identity candidate,
                      -- CR 120.1's source of the damage that did not happen,
                      -- read off the event as PROPOSED for the recipient's
                      -- reason below.
                      Prevention.source = DamageEvent.source de,
                      -- ONE ENTRY, this event's own: the recipient of the event
                      -- as PROPOSED, which is the reading CR 615.13 asks for --
                      -- its ability watches "damage that WOULD be dealt [and] is
                      -- prevented". Damage CAN now be redirected
                      -- (DamageRewrite.Redirect), and the two readings still
                      -- never diverge, because a redirect prevents nothing (CR
                      -- 615.1a) -- `prevents` refuses it above, so no redirect
                      -- ever reaches here. `groupPreventions` is what widens the
                      -- map, one batch at a time.
                      --
                      -- Zero on the CR 615.12 path, which `applyInertly` makes
                      -- exact: it hands the event back whole, so `now` is `was`.
                      Prevention.amounts = Map.singleton (DamageEvent.target de) (was - now),
                      -- CR 615.5's additional effect, carried out of the loop
                      -- so a caller that CAN run effects finds it. Copied, not
                      -- inspected: this module never asks what the rider is.
                      Prevention.rider = ReplacementCandidate.rider candidate
                    }
              else Nothing
  _ -> Nothing

-- CR 615.13's "prevented THIS WAY": which object's own PRINTED prevention effect
-- an application was, or Nothing when no card printed it. What
-- TriggerCondition.SelfPreventsDamage compares against its bearer.
--
-- A floating row is always a card's: CR 615.3's shields are installed by the
-- resolution of a spell or ability, and `source` is the object whose text said
-- so. Proved by Pawl.ReplacementSpec's "CR 615.9 / 615.13 samite ministration
-- gains life from a black source it named", where the shield and the delayed
-- trigger are both the instant's, so answering Nothing here withholds the life.
--
-- A permanent's is too, unless the row was minted onto it by a rule rather than
-- printed on its card -- CR 122.1c's shield-counter pair and CR 702.16e's
-- protection are the preventions that reach here that way. Neither is text the
-- card set up, so a "prevented this way" trigger on that same permanent is not
-- about either: the counter or the keyword prevented the damage and the printed
-- effect never applied. Rule 702.16e holds even where the card PRINTS the
-- keyword, "this way" pointing at the preceding sentence rather than at the
-- object.
--
-- READ OFF THE CANDIDATE rather than inferred from its effect, because rule
-- 702.16e's row is a plain PreventAll naming the permanent itself -- the same
-- value Phyrexian Vindicator and Stormwild Capridor print, so no reading of the
-- effect can separate them. The mark is made where the row is built and the
-- segments are still apart -- Pawl.Engine.Projection.replacementsOf for the
-- keyword's permanent half, `collect`'s player segment for rule 702.16e's other
-- one; see Pawl.Types.ReplacementProvenance.
printedBy :: CandidateId.CandidateId -> Maybe ObjectId
printedBy candidate = case candidate of
  CandidateId.OfFloating floating -> Just (FloatingCandidate.source floating)
  CandidateId.OfPermanent permanent -> case PermanentCandidate.provenance permanent of
    ReplacementProvenance.Minted -> Nothing
    ReplacementProvenance.Printed -> Just (PermanentCandidate.source permanent)

-- CR 615.13: collapse a batch's per-event preventions to one entry per applying
-- INSTANCE, carrying what that instance prevented for each recipient.
--
-- Keyed on the instance alone, which is the rule's own unit: 615.13 counts one
-- application of one prevention effect "to one or more simultaneous damage
-- events", whoever those events were addressed to. Divine Deflection is the
-- producer that tells the two readings apart -- one shield over a player and the
-- permanents they control -- and a batch spending its pool across both runs CR
-- 615.5's rider ONCE with the total, which Pawl.ReplacementSpec's "the rider
-- deals its 3 in ONE event" proves by counting the rider's damage events rather
-- than summing them.
--
-- The recipients survive as the KEYS of the collapsed amount rather than as part
-- of the key, because a trigger scoped to one of them still has to read its own
-- share -- Selfless Squire's "damage that would be dealt to you", where the same
-- shield also covered a permanent.
--
-- Ascending by key, so the CR 608.2i record -- and therefore the CR 603.3b order
-- these triggers are offered in -- is canonical rather than gather-dependent.
--
-- A CR 615.12 entry, whose amount is 0, merges harmlessly and is the rule: one
-- application of one prevention effect to several simultaneous events reports
-- what it prevented across all of them, which the unpreventable ones added
-- nothing to. So a batch mixing the two still records CR 615.13's trigger --
-- some of the damage WAS prevented -- and a batch of only inert ones sums to 0,
-- which Pawl.Engine.Damage's record then leaves out.
--
-- Only the AMOUNTS are summed. CR 615.5's rider is a property of the INSTANCE,
-- and the key already fixes the instance, so every entry merged here carries the
-- same rider and taking either side is taking the same value -- which is also
-- the rule: one application of one prevention effect runs its additional effect
-- once, with the total it prevented.
--
-- The SOURCE is neither summed nor keyed on, and a merged entry keeps one of the
-- sources merged into it. Samite Ministration -- the one card in data/cards/
-- whose CR 615.13 trigger reads the source -- cannot tell that apart from the
-- rule, its shield naming ONE source under CR 609.7a, so a key already fixed to
-- one instance is fixed to one source too. Not implemented: a shield covering two
-- DIFFERENT sources in one batch, where rule 615.13's one prevention has two
-- sources and this reports one (#2287). Keying on the source instead would report
-- two preventions where that rule counts one, which is the wrong trade and the
-- design call the issue is left open for.
groupPreventions :: [Prevention] -> [Prevention]
groupPreventions ps =
  let merge (a1, s1, r1) (a2, _, _) = (Map.unionWith (+) a1 a2, s1, r1)
      keyed = Map.fromListWith merge [(Prevention.by p, (Prevention.amounts p, Prevention.source p, Prevention.rider p)) | p <- ps]
      rebuild (by, (amounts, source, rider)) =
        Prevention.MkPrevention {Prevention.by = by, Prevention.source = source, Prevention.amounts = amounts, Prevention.rider = rider}
   in fmap rebuild (Map.toAscList keyed)

-- CR 615.7: when two or more applicable sources would deal damage to a shielded
-- recipient at the same time, that recipient chooses which damage the shield
-- prevents. CR 101.4c generalizes it to every prevention a simultaneous batch can
-- exhaust -- CR 122.1c's shield counters are the second such shape -- since the
-- player owes both events a CR 616.1 choice and "if no order is specified, the
-- player chooses the order".
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
-- 615.7's freedom is entirely within one chooser: `contested` splits a shield's
-- contested hits by whoever CR 616.1 asks about each of them -- both read the
-- chooser off the recipient through `chooserOf` -- so a shield covering
-- recipients with two different choosers contributes one group each rather than
-- one group asked of the wrong player. `askOne` then permutes only within that
-- player's own positions. CR 101.4c is the rule that licenses it: a player
-- making several simultaneous choices makes them in the order specified, or
-- chooses the order themselves.
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

-- The batch positions one chooser's countdowns -- prevention shields, and
-- Harm's Way's counted redirection -- have to be allocated across, one entry
-- per chooser, in CR 616.1's APNAP order.
--
-- A countdown is CONTESTED when it admits two or more of the batch's events (CR
-- 615.7) and cannot cover all of them -- the comparison being made in the unit
-- its OWN rule counts, which `contestedResource` below supplies: damage for CR
-- 615.7's counted shield and for the counted redirect, whole events for CR
-- 122.1c's shield counters. One large enough to cover the lot takes all of it
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
--
-- CR 615.12's damage is left out of the union for a shield the rule's last
-- sentence protects, and that is an elision the rule itself licenses rather than
-- a shortcut: a CR 615.7 shield prevents none of an unpreventable event and is
-- not reduced by it, so every order of a batch of them leads to the same board
-- and there is nothing for the shielded player to decide. `spentInertly` is where
-- that stops being true -- CR 122.1c's counter comes off whether or not the
-- damage could be prevented, so an unpreventable event competes for it exactly as
-- a preventable one does. Filtered per EVENT rather than per batch, so a batch
-- mixing preventable and unpreventable damage still asks about the part the
-- shield's own resource is contested over -- which a narrowed clause reaches: an
-- Excruciator and an ordinary creature hitting one shielded permanent at once is
-- exactly that batch.
contested :: GameState -> [DamageEvent.DamageEvent] -> [(PlayerId, [Natural])]
contested gs events =
  let indexed :: [(Natural, DamageEvent.DamageEvent)]
      indexed = zip [0 ..] events
      -- Reached only for a candidate `contestedResource` gave a resource for,
      -- which is a DamageR and nothing else, so the wildcard names no rewrite.
      -- CR 615.12's filter is a PREVENTION's: a redirect never prevents, so
      -- unpreventable damage contends for Harm's Way's 2 exactly as any other
      -- damage does, and `applies` alone says whether the row reaches it.
      contends candidate event = case ReplacementCandidate.effect candidate of
        ReplacementEffect.DamageR (DamageR.MkDamageR _ rewrite _) ->
          not (prevents rewrite) || preventable gs event || spentInertly rewrite
        _ -> False
      hitsOf candidate =
        filter
          ( \entry ->
              contends candidate (snd entry)
                && applies gs (ProposedEvent.WouldDealDamage (snd entry)) candidate
          )
          indexed
      -- CR 615.7's chooser is CR 616.1's, read off each hit's own shielded
      -- recipient and GROUPED, since one shield can cover recipients with
      -- different choosers: Divine Deflection's covers a player and the
      -- permanents they control, and nothing in a DamagePattern's recipient side
      -- confines a described set to one seat. A hit whose recipient has left has
      -- no chooser and is dropped, there being no one to ask about it.
      --
      -- The CONTEST is judged over the whole shield before the split, which is
      -- the rule's own unit: the pool is exhausted by everything it admits, not
      -- by one seat's share of it.
      contestedBy candidate = Maybe.fromMaybe [] $ do
        (left, demand) <- contestedResource gs candidate
        case hitsOf candidate of
          hits@(_ : _ : _)
            | left < demand (fmap snd hits) ->
                pure
                  ( Map.toList
                      ( Map.fromListWith
                          (<>)
                          [ (pid, [position])
                          | (position, event) <- hits,
                            Just pid <- [chooserOf gs (ProposedEvent.WouldDealDamage event)]
                          ]
                      )
                  )
          _ -> Nothing
      groups = concatMap contestedBy (collect gs (GameState.replacements gs))
      merged = Map.fromListWith (<>) groups
   in [ (pid, List.sort (List.nub positions))
      | (pid, positions) <- List.sortOn (seatOf gs . fst) (Map.toList merged)
      ]

-- CR 615.7 / 122.1c: a prevention that a batch can exhaust, as the pair (what it
-- has left, what a set of events would demand of it) -- both in the unit the
-- effect's own rule counts. Nothing for an effect no batch can run out of.
--
-- The two units are the rules' own and not interchangeable. CR 615.7's shield
-- "counts only the amount of damage; the number of events or sources dealing it
-- doesn't matter", so its unit is damage. CR 122.1c's pair prevents a whole
-- event per counter whatever its amount, so its unit is events -- and what it
-- has left is a number on the PERMANENT rather than on the row, which is why
-- this reads the board.
--
-- A CLASSIFICATION of effects -- what SHAPE an effect has, never which effect it
-- is -- in the same genre as bucketOf and readsApplier above. One arm per
-- constructor, no wildcard, so a new rewrite a batch can exhaust breaks the
-- build here rather than silently going unasked about.
contestedResource :: GameState -> ReplacementCandidate -> Maybe (Natural, [DamageEvent.DamageEvent] -> Natural)
contestedResource gs candidate = case ReplacementCandidate.effect candidate of
  ReplacementEffect.DamageR (DamageR.MkDamageR _ rewrite _) -> case rewrite of
    DamageRewrite.PreventNext remaining -> Just (remaining, sum . fmap DamageEvent.amount)
    -- Harm's Way's counted redirection is the same resource in the same unit:
    -- "Harm's Way will redirect just 2 of that damage ... You choose which 2
    -- damage is redirected" (its Oracle rulings), which is CR 615.7's
    -- allocation on a rule-614 rewrite. The CR states the counted allocation
    -- only for preventions; the redirect's rests on those rulings.
    --
    -- Not implemented: splitting the countdown by POINT across two
    -- simultaneous events -- the ruling's "1 damage ... to each of two
    -- different recipients". `orderBatch` asks an order, and the first event
    -- takes what it can (#3188).
    DamageRewrite.RedirectNext remaining _ -> Just (remaining, sum . fmap DamageEvent.amount)
    -- CR 122.1c: one counter per application, so a batch of n events demands n
    -- of them, and the permanent's counters are the supply.
    DamageRewrite.PreventRemovingShieldCounter ->
      Just (Projection.shieldCounters (ReplacementCandidate.source candidate) gs, Int.toNaturalSaturating . length)
    -- Fog is unlimited for its duration, so there is nothing to allocate: it
    -- prevents every event it admits and the order cannot matter.
    DamageRewrite.PreventAll -> Nothing
    -- CR 615.10 states the opposite of CR 615.7 for a static shield: it "will
    -- apply separately to damage from other applicable events that would happen
    -- at the same time", so a batch cannot exhaust it and there is nothing to
    -- allocate.
    DamageRewrite.PreventAllBut _ -> Nothing
    DamageRewrite.SetAmount _ -> Nothing
    DamageRewrite.Scale _ -> Nothing
    DamageRewrite.Redirect _ -> Nothing
    DamageRewrite.RedirectMatching _ -> Nothing
  ReplacementEffect.ZoneChangeR {} -> Nothing
  ReplacementEffect.EntryR {} -> Nothing
  ReplacementEffect.DestructionR _ -> Nothing
  ReplacementEffect.CounterR {} -> Nothing
  ReplacementEffect.TokenR {} -> Nothing
  ReplacementEffect.TurnUpR {} -> Nothing
  ReplacementEffect.UntapR _ -> Nothing
  -- CR 614.1a: neither resizing life-total rewrite is a supply a batch can run out
  -- of. A floor is re-read off the board every application, and once the player is
  -- at it a further loss is cut to nothing rather than exhausting anything; a
  -- scaling is arithmetic on whatever amount arrives.
  --
  -- A LIBRARY is a supply, and a batch could in principle empty one -- but this
  -- classification is only ever asked of a CR 510.2 damage batch (`contested`
  -- above filters by WouldDealDamage), and no LifeLossR row matches one. So the
  -- answer for the exiling rewrite is Nothing because the question never arrives,
  -- not because the resource is inexhaustible; `breaches` is where the count is
  -- actually read, once per proposal.
  ReplacementEffect.LifeLossR {} -> Nothing
  -- A scaling on the gain side is arithmetic on whatever amount arrives, the
  -- resizing loss arms above and for their reason.
  ReplacementEffect.LifeGainR {} -> Nothing
  -- The question never arrives: `contested` above filters by WouldDealDamage,
  -- which no DrawR row matches. A life gain is no supply in any case; CR 400.11b's
  -- pool IS one, and `bringInto` is where its own exhaustion is read.
  ReplacementEffect.DrawR {} -> Nothing
  -- The instruction class one rule up, and the same answer for the same reason:
  -- `contested` asks only about a damage batch.
  ReplacementEffect.DrawCountR {} -> Nothing
  ReplacementEffect.PhaseR _ -> Nothing

asDamageEvent :: ProposedEvent -> Maybe DamageEvent.DamageEvent
asDamageEvent event = case event of
  ProposedEvent.WouldDealDamage de -> Just de
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing

asDestruction :: ProposedEvent -> Maybe ObjectId
asDestruction event = case event of
  ProposedEvent.WouldBeDestroyed target _ _ -> Just target
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing

-- asDestruction's twin one event class over: the permanent that actually becomes
-- untapped, or Nothing when a replacement took the event.
asUntap :: ProposedEvent -> Maybe ObjectId
asUntap event = case event of
  ProposedEvent.WouldUntap oid -> Just oid
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing

-- asUntap's twin one event class over: the player who actually draws a card, or
-- Nothing when a replacement took the event (CR 614.6).
asDraw :: ProposedEvent -> Maybe PlayerId
asDraw event = case event of
  ProposedEvent.WouldDraw pid -> Just pid
  ProposedEvent.WouldDrawCards {} -> Nothing
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing

-- asDraw's twin one event class up (CR 121.2a): who was instructed to draw and how
-- many cards the instruction names once CR 616.1's loop has settled it, or Nothing
-- when a row replaced the instruction outright.
asDrawCount :: ProposedEvent -> Maybe (PlayerId, Natural)
asDrawCount event = case event of
  ProposedEvent.WouldDrawCards pid n -> Just (pid, n)
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing

asCounters :: ProposedEvent -> Maybe (ObjectId, CounterKind.CounterKind Keyword.Type.Keyword, Natural)
asCounters event = case event of
  ProposedEvent.WouldPutCounters _ oid kind n -> Just (oid, kind, n)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing

-- asCounters' player half. The CAUSE is dropped by both, for the same reason: what
-- the funnel needs back is the placement to carry out, and the provenance has
-- already been read by every row that could apply.
asPlayerCounters :: ProposedEvent -> Maybe (PlayerId, PlayerCounterKind.PlayerCounterKind, Natural)
asPlayerCounters event = case event of
  ProposedEvent.WouldPutPlayerCounters _ pid kind n -> Just (pid, kind, n)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing

-- asPlayerCounters' sibling one event class over: the player who would lose life
-- and how much of the loss has survived, or Nothing when a replacement took the
-- event outright. The CAUSE is dropped for that function's reason -- what the
-- funnel needs back is the loss to carry out, and the provenance has already been
-- read by every row that could apply.
asLifeLoss :: ProposedEvent -> Maybe (PlayerId, Natural)
asLifeLoss event = case event of
  ProposedEvent.WouldLoseLife _ pid n -> Just (pid, n)
  ProposedEvent.WouldGainLife {} -> Nothing
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing

-- asLifeLoss the other direction: the player who would gain life and how much of
-- the gain has survived, or Nothing when a replacement took the event outright.
asLifeGain :: ProposedEvent -> Maybe (PlayerId, Natural)
asLifeGain event = case event of
  ProposedEvent.WouldGainLife pid n -> Just (pid, n)
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing

asTokens :: ProposedEvent -> Maybe (PlayerId, Seq.Seq TokenLot.TokenLot)
asTokens event = case event of
  ProposedEvent.WouldCreateTokens pid lots -> Just (pid, lots)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed {} -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldBeginPhase {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing

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
                  ActiveReplacement.origin = ReplacementOrigin.Other,
                  -- No clause: the skip is unconditional (see
                  -- Pawl.Types.ActiveReplacement).
                  ActiveReplacement.condition = Nothing,
                  ActiveReplacement.rider = Nothing,
                  ActiveReplacement.slots = Map.empty
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
  ProposedEvent.WouldPutPlayerCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
  ProposedEvent.WouldTurnFaceUp {} -> Nothing
  ProposedEvent.WouldUntap _ -> Nothing
  ProposedEvent.WouldLoseLife {} -> Nothing
  ProposedEvent.WouldGainLife {} -> Nothing
  ProposedEvent.WouldDraw _ -> Nothing
  ProposedEvent.WouldDrawCards {} -> Nothing
