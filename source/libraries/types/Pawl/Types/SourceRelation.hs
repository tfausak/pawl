module Pawl.Types.SourceRelation where

-- | CR 614.1: whose events a pattern admits, read against the SOURCE of the
-- effect's own ability (CR 113.7) -- any source at all, or that one alone.
--
-- TheSource is how CR 614.15's "this way" is keyed: Galvanic Blast's second line
-- replaces the damage its first line deals and nothing else. Without it a
-- floating self-replacement would be offered every damage event in the game.
--
-- Not Filter.IsSource, which is how a zone-change or entry pattern spells its
-- own self-scope: the ROLE of the compared id differs. A Filter is a predicate
-- over the event's SUBJECT, and this asks about the damage's SOURCE, which is a
-- different object -- so there is no candidate for a Filter to be evaluated
-- against here.
data SourceRelation
  = AnySource
  | TheSource
  deriving (Eq, Ord, Show)
