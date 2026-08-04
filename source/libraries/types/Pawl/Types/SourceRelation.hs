module Pawl.Types.SourceRelation where

-- | CR 614.1: whose events a pattern admits, read against the SOURCE of the
-- effect's own ability (CR 113.7) -- any source at all, or that one alone.
--
-- TheSource is how CR 614.15's "this way" is keyed: Galvanic Blast's second line
-- replaces the damage its first line deals and nothing else. Without it a
-- floating self-replacement would be offered every damage event in the game.
--
-- The damage-side sibling of Pawl.Types.ZoneChangeSubject, kept a separate type
-- because the ROLE of the compared id differs -- there the event's SUBJECT, here
-- its SOURCE -- and a pattern can scope either without the other.
data SourceRelation
  = AnySource
  | TheSource
  deriving (Eq, Ord, Show)
