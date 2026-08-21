module Pawl.Types.ChosenCardFromAmong where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SlotName as SlotName

-- | The printed words "from among them", with a choice: which GROUP the
-- candidates are drawn from, and what among it may be picked.

-- A SLOT rather than a zone, which is the whole difference from
-- Pawl.Types.ChosenCardInGraveyard and Pawl.Types.ChosenCardInHand: the
-- candidates are the cards an EARLIER effect of this same resolution named --
-- Commune with the Gods' revealed five (CR 701.20a), Carth the Lion's looked-at
-- seven (CR 701.20e) -- and where those cards are is whatever the effect that
-- bound them left them. A library batch is the case no zone-keyed arm can reach,
-- a library still having no filtered sweep (#1309) -- and
-- Pawl.Types.ObjectRef.TopOfLibraryUntil's walk is not one, since it stops at the
-- match that completes its count rather than collecting every one of them.
--
-- No chooser field: CR 608.2d hands the choice to the player applying the effect,
-- and a group bound by a look is shown to that player alone (CR 701.20e), so the
-- resolving controller is the only seat that can answer. Not implemented: a
-- second player choosing among another's group (#1957).
--
-- ONE card, with no count beside the Filter, and CR 609.3 covers the shortfall:
-- a group holding no matching card yields nothing and that share of the
-- instruction is ignored (CR 101.3). Not implemented: a count above one, Dig
-- Through Time's "put two of them into your hand" (#1956).
--
-- The fields are named rather than positional for ChosenCardInHand's reason.
data ChosenCardFromAmong = MkChosenCardFromAmong
  { slot :: SlotName.SlotName,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
