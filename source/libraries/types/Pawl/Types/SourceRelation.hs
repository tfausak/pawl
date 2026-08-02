module Pawl.Types.SourceRelation where

-- | CR 614.1: whose events a pattern admits, read against the SOURCE of the
-- effect's own ability (CR 113.7) -- any source at all, or that one alone.
--
-- TheSource is how CR 614.15's "this way" is keyed. Galvanic Blast's second line
-- replaces the damage its FIRST line deals and nothing else, and the two lines
-- are one spell, so "the damage this ability's source is dealing" names exactly
-- the event the self-replacement may touch. Without it a floating self-
-- replacement would be offered every damage event in the game.
--
-- The damage-side sibling of Pawl.Types.ZoneChangeSubject, whose
-- AnyObject/TheSource asks a shaped-alike question. Separate types rather than
-- one shared enum because the ROLE of the compared id differs -- there it is the
-- event's SUBJECT (which object is moving), here it is the event's SOURCE (which
-- object is dealing) -- and a pattern can scope either without the other.
data SourceRelation
  = AnySource
  | TheSource
  deriving (Eq, Ord, Show)
