module Pawl.Types.ChosenCardFromAmong where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The printed words "from among them", with a choice: which GROUP the
-- candidates are drawn from, what among it may be picked, HOW MANY, and WHO
-- picks.

-- A SLOT rather than a zone, which is the whole difference from
-- Pawl.Types.ChosenCardInGraveyard and Pawl.Types.ChosenCardInHand: the
-- candidates are the cards an EARLIER effect of this same resolution named --
-- Commune with the Gods' revealed five (CR 701.20a), Carth the Lion's looked-at
-- seven (CR 701.20e) -- and where those cards are is whatever the effect that
-- bound them left them. A library batch is the case no zone-keyed arm can reach:
-- Pawl.Types.ObjectRef.EachCardInYourLibrary's filtered sweep names every match
-- in the whole zone rather than the handful an earlier clause bound, and
-- Pawl.Types.ObjectRef.TopOfLibraryUntil's walk stops at the match that completes
-- its count rather than collecting every one of them.
--
-- The COUNT is a Quantity rather than a Natural, Pawl.Types.TopOfLibrary's reason:
-- Ancestral Memories' printed two sits beside a computed one. CR 609.3 covers the
-- shortfall -- a group holding fewer matches than the count gives what it has, and
-- one holding none yields nothing, that share of the instruction being ignored
-- (CR 101.3).
--
-- The CHOOSER is a PlayerRef naming ONE seat, and CR 608.2c's default is the
-- resolving controller, which is what Relative You spells. Animal Magnetism's "an
-- opponent chooses a creature card from among them" is the other seat, named
-- through the slot a Pawl.Types.Effect ChoosePlayer filled earlier in the same
-- resolution. A PlayerRef rather than Pawl.Types.Chooser: that type's arms fold
-- over a PlayerScope of GRAVEYARDS, one per chooser, where a group is one pile
-- however many seats are in play.
--
-- Nothing here says the chooser has SEEN the group. A reveal is public (CR
-- 701.20a) and a look is not (CR 701.20e), so an opponent-chooser over a
-- looked-at group would show cards that rule hides -- which pawl cannot represent
-- either way (#1412).
--
-- The fields are named rather than positional for ChosenCardInHand's reason.
data ChosenCardFromAmong = MkChosenCardFromAmong
  { slot :: SlotName.SlotName,
    filter :: Filter.Filter Keyword.Keyword,
    count :: Quantity.Quantity,
    chooser :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
