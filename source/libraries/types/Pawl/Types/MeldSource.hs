module Pawl.Types.MeldSource where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.PrintingId as PrintingId

-- | CR 701.42a: what is behind a melded permanent -- "a single object
-- represented by two cards". The printing the combined back face was interned
-- as, plus the printings of the cards representing it.
--
-- The RESULT is not one of the components and is never a card in anyone's deck:
-- CR 712.4b leaves a meld card's half of the oversized face meaningless on its
-- own, so the combined face is interned by the ability that melds rather than
-- read out of the pool. CR 712.8g is what the field is for -- the object "has
-- only the characteristics of the combined back face" -- so every characteristic
-- read in the engine resolves through it and needs to know nothing about meld.
--
-- The COMPONENTS are read by the rules that look past those characteristics to
-- the cards themselves: CR 202.3c's mana value ("as though it had the combined
-- mana cost of the front faces of each card that represents it"), CR 712.21's
-- departure (one permanent leaves the battlefield and two cards are put into the
-- appropriate zone) and CR 701.27g's exclusion of an object represented by more
-- than one card from being a transformed permanent.
--
-- NonEmpty rather than a pair: CR 701.42a says "two cards" of a meld pair, but
-- nothing in rule 701.42 or rule 712.4 fixes the count for the readers above,
-- which all quantify over "each card that represents it". Pawl.Engine.Game's
-- componentsOf is the shared reader, and CR 730.3's merged permanents will
-- answer it the same way.
--
-- A record rather than positional fields, for Pawl.Types.ActivatedAbilitySource's
-- reason: which printing is which cannot be got wrong at a construction site,
-- and the arm has the one payload a codec needs. Pawl.Types.Source is what this
-- is an arm of.
data MeldSource = MkMeldSource
  { result :: PrintingId.PrintingId,
    components :: NonEmpty.NonEmpty PrintingId.PrintingId
  }
  deriving (Eq, Ord, Show)
