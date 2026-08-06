module Pawl.Types.ZoneChangePattern where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Zone as Zone

-- | CR 614.1a: which zone changes a redirect intercepts. Rest in Peace is
-- (Graveyard, Anyones, And []) -- any object that would be put into a graveyard
-- from anywhere. `whenDestination` is compared against the event's CURRENT
-- destination, which is why a redirect whose output no longer matches its own
-- trigger destination cannot re-fire even before CR 614.5 is consulted.
--
-- `whatObject` says WHAT the moving object is, as a Filter over its
-- characteristics -- Anafenza, the Foremost's "a nontoken creature", which is
-- `And [HasCardType Creature, Not IsToken]`. `whoseObject` narrows by the
-- object's OWNER. Orthogonal: the owner relation cannot ride the Filter, because
-- Filter.ControlledBy asks who CONTROLS a candidate, and CR 400.3 sends a stolen
-- creature (Act of Treason) to its OWNER's graveyard rather than its
-- controller's -- so the two answers differ on exactly the case a redirect
-- scoped to "an opponent's graveyard" is about.
--
-- A bare Filter and no separate self-scoping field, which is EntryR's shape (see
-- Pawl.Types.ReplacementEffect) for EntryR's reason: CR 702.34a's "exile THIS
-- card" is `Filter.IsSource`, an identity test the generic matcher already
-- answers off its Context, so a ZoneChangeSubject enum beside the Filter would be
-- a second spelling of one relation (#163). `And []` is the trivial predicate, so
-- a redirect that says nothing about the object needs no "any object" arm.
--
-- `whenDestination` is ONE zone, so CR 702.34a's "instead of putting it anywhere
-- else" is expressible only as the one destination a spell actually leaves the
-- stack for in this pool (#293). Nor is there a FROM-zone field, and no card in
-- the pool needs one: Anafenza's two clauses -- "a nontoken creature would die"
-- and "a creature card not on the battlefield would be put into a graveyard" --
-- union to exactly "a creature CARD would be put into a graveyard from anywhere",
-- since a token is never a card and a battlefield token is outside both clauses.
data ZoneChangePattern = MkZoneChangePattern
  { whenDestination :: Zone.Zone,
    whoseObject :: ControllerRelation.ControllerRelation,
    whatObject :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
