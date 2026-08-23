module Pawl.Types.EntryRestriction where

import qualified Data.Set as Set
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Zone as Zone

-- | CR 101.2 / CR 400.4a: one printed ENTRY PROHIBITION -- an effect saying an
-- object "can't enter the battlefield". Grafdigger's Cage's first sentence
-- ("creature cards in graveyards and libraries can't enter the battlefield") is
-- the pool's printing.
--
-- The two rules divide the sentence. CR 101.2 gives the "can't" its force over
-- whatever allowed or directed the entry. CR 400.4a says what happens instead --
-- "it remains in its previous zone" -- which it states for a CARD TYPE that can't
-- enter rather than for an effect, and which CR 701.40f states again for a
-- prohibited manifest; both readings of "an object that can't enter" get the same
-- answer, which is why this type answers only a Bool.
--
-- Pawl.Types.SacrificeRestriction's shape and its filing, one game action over:
-- CR 613.11 puts a continuous effect that "affects game rules rather than
-- objects" outside the layer system, CR 101.2a says such an effect is not an
-- ability being added or removed, and Pawl.Engine.Projection sees none of them.
-- Every step of that type's argument for why it cannot be a
-- Pawl.Types.Modification holds here unchanged.
--
-- NOT a Pawl.Types.ReplacementEffect, and that is the distinction this type
-- exists to draw. CR 614.1b's "skip" is a replacement with nothing, but the
-- printed cards do not say "instead" or "skip" -- they say "can't", which is CR
-- 101.2's word, and CR 613.11 files the resulting continuous effect outside rule
-- 614 entirely. Widening Pawl.Types.ReplacementEffect's ZoneChangeR arm with a
-- "no destination" destination would be observably wrong rather than merely
-- misfiled: a redirect is a zone change, so CR 400.7 mints a fresh object with no
-- memory of its previous existence, where a refusal leaves the card in its
-- previous zone as the SAME object. Pawl.EntryRestrictionSpec's "each card
-- remains in its previous zone, as the same object" is what separates the two.
-- Building it as a replacement would also put a prohibition inside CR 616.1's
-- ordering loop, where CR 101.2 has no "choose which applies" step.
--
-- NOT a Pawl.Types.PlayerEffect either: every arm of that type is scoped to a
-- PLAYER, and "creature cards in graveyards can't enter the battlefield" is
-- scoped to an OBJECT that need not be the prohibition's controller's.
--
-- CR 101.2 is what gives the prohibition its force over every rule and effect
-- that would put the object onto the battlefield -- a resolving Exhume, a
-- resolving permanent spell, a land play, CR 701.40a's manifest -- since all of
-- them "allow or direct something to happen" and this states it can't.
--
-- Gathered LIVE from the battlefield on every entry and never captured, the
-- posture every sibling carrier takes: a Grafdigger's Cage that left the
-- battlefield lifts its prohibition with nothing to unwind, and CR 613.11 lets it
-- reach cards that were not in a graveyard when it began.
--
-- Open-half card data, classified rather than identified:
-- Pawl.Engine.EntryRestriction is the only module that may read it, and it
-- answers a Bool about one object's move.
data EntryRestriction = MkEntryRestriction
  { -- | Which objects can't enter the battlefield. An Affected, not a bare
    -- ObjectId, so the set is re-derived on every entry -- the field name every
    -- sibling restriction spells, and for its reason: it names the RESTRICTED
    -- objects, never something they act on.
    affected :: Affected.Affected,
    -- | Which zones the object must be LEAVING for the prohibition to reach it.
    -- Grafdigger's Cage names the graveyard and the library; Worms of the Earth's
    -- "lands can't enter the battlefield" names none, i.e. every zone, which is
    -- the full set.
    --
    -- A FIELD rather than an atom of the affected filter above, and this is the
    -- difference between the card and a bug: @Affected.MatchingOffBattlefield@
    -- alone reaches every zone but the battlefield -- INCLUDING THE STACK, where
    -- an ordinary hardcast creature spell sits on its way to becoming a
    -- permanent. Grafdigger's Cage does not touch that spell, and
    -- Pawl.EntryRestrictionSpec's "a hardcast creature spell still enters" is the
    -- control leg that proves it.
    --
    -- Not Filter.IsInZone in the filter either, though that atom exists: it asks
    -- where a candidate IS, and this asks where a MOVE is FROM. The two coincide
    -- only while the object has not yet been re-homed, and CR 400.4a's "remains in
    -- its previous zone" is a sentence about the move.
    --
    -- A Set rather than a Maybe: the pool's printings name two zones and none,
    -- and Pawl.Types.Zone is a small closed enum, so the empty-set case never has
    -- to mean "all".
    --
    -- Read as the zone the object is in when the move is judged, which is CR
    -- 400.4a's "its previous zone" -- the same zone it remains in when the
    -- prohibition applies.
    origins :: Set.Set Zone.Zone
  }
  deriving (Eq, Ord, Show)
