module Pawl.Types.CounterRestriction where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword

-- | CR 101.2 / CR 122.6: one printed COUNTER PROHIBITION -- an effect saying
-- counters "can't be put on" an object. Solemnity's second sentence ("counters
-- can't be put on artifacts, creatures, enchantments, or lands") and Melira,
-- Sylvok Outcast's second ("creatures you control can't have -1\/-1 counters put
-- on them") are the pool's printings.
--
-- The two rules divide the sentence. CR 101.2 gives the "can't" its force over
-- whatever allowed or directed the placement -- "when a rule or effect allows or
-- directs something to happen, and another effect states that it can't happen,
-- the 'can't' effect takes precedence". CR 122.6 says WHICH placements the
-- printed phrase reaches: counters put on the object while it is on the
-- battlefield, and counters an object is given as it enters. Both roads reach
-- Pawl.Engine.Event.settleCounters, which is where a PLACEMENT consults this;
-- CR 122.5's third impossibility is the second reader, in Pawl.Engine.Resolve's
-- Effect.MoveCounters arm.
--
-- NOT a Pawl.Types.ReplacementEffect, and this type exists to draw that line. CR
-- 614.16 replaces a PLACEMENT -- a row scaling one to zero still describes an
-- event that was possible and was then replaced, with something "put instead" --
-- while these cards say "can't", which is CR 101.2's word. CR 613.11 files the
-- resulting continuous effect outside rule 614 entirely, and CR 101.2 has no
-- "choose which applies" step for CR 616.1 to run.
--
-- Pawl.Types.EntryRestriction's shape and its filing, one game action over: CR
-- 613.11 puts a continuous effect that "affects game rules rather than objects"
-- outside the layer system, CR 101.2a says such an effect is not an ability being
-- added or removed, and Pawl.Engine.Projection sees none of them. Every step of
-- that type's argument for why it cannot be a Pawl.Types.Modification holds here
-- unchanged.
--
-- The PLAYER half of the same two cards is not this type. "Players can't get
-- counters" and "you can't get poison counters" are scoped to a PLAYER and reach
-- Pawl.Types.PlayerCounterKind, a disjoint domain, so they are
-- Pawl.Types.PlayerEffect's CantGetCounters.
--
-- Gathered LIVE from the battlefield on every placement and never captured, the
-- posture every sibling carrier takes: a Solemnity that left the battlefield
-- lifts its prohibition with nothing to unwind.
--
-- Open-half card data, classified rather than identified:
-- Pawl.Engine.CounterRestriction is the only module that may read this type, and
-- it answers a Bool about one object and one kind.
data CounterRestriction = MkCounterRestriction
  { -- | Which objects can't have counters put on them. An Affected, not a bare
    -- ObjectId, so the set is re-derived on every placement -- the field name
    -- every sibling restriction spells, and for its reason: it names the
    -- RESTRICTED objects, never something they act on.
    affected :: Affected.Affected,
    -- | Which KIND of counter the prohibition refuses, or Nothing for every kind.
    -- Solemnity names none ("counters can't be put on ..."); Melira names one
    -- ("can't have -1\/-1 counters put on them").
    --
    -- Maybe rather than a set, mirroring Pawl.Types.MoveCounters' own kind field:
    -- both printings name at most one kind, and an empty set would be a second
    -- spelling of "no prohibition at all". A card naming two kinds prints two
    -- rows, which is what the list on Pawl.Types.Face is for.
    kind :: Maybe (CounterKind.CounterKind Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
