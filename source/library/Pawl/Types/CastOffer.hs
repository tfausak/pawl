module Pawl.Types.CastOffer where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword

-- | What an effect says about a cast IT offers, beyond what casting the card
-- would ordinarily mean -- CR 310.12b's "you may cast it transformed without
-- paying its mana cost", whose two riders are these two fields.
--
-- Carried by the OPCODE (OfferCast) and not by the card, for
-- Pawl.Types.EntryRiders' reason: neither rider is a characteristic of the
-- object (CR 109.3). One printed card can be offered transformed by one effect
-- and normally by another, and CR 118.9's alternative cost is applied to a spell
-- "from another effect" in the rule's own words.
--
-- TWO independent riders, because the rules make them independent. `transformed`
-- is CR 712.11a, which is about which FACE is put onto the stack and says
-- nothing about what is paid; `withoutPayingManaCost` is CR 118.9's alternative
-- cost, which is about payment and says nothing about faces. Cards print each
-- without the other -- a cascade's "you may cast it without paying its mana
-- cost" transforms nothing.
--
-- The first two are Bools rather than richer choices because each is the presence
-- or absence of one printed clause. `payingInstead` is CR 118.9's OTHER wording
-- -- "you may pay [cost] rather than its mana cost" -- and it is a third field
-- rather than another value of the second for exactly the reason this haddock
-- gave before it existed: a stated cost is a different rider, not a different
-- setting of "without paying".
--
-- CR 118.9a allows a spell only ONE alternative cost, so the two cost riders are
-- never both meaningful at once; Pawl.Engine.Resolve.offerCast reads
-- `withoutPayingManaCost` first, and no producer sets both. Rule 702.94a's
-- miracle is what sets this one, carried here rather than on the card for this
-- type's own reason: the cost is not a characteristic, and the same card cast
-- any other way pays what it prints.
data CastOffer = MkCastOffer
  { transformed :: Bool,
    withoutPayingManaCost :: Bool,
    payingInstead :: Maybe (Cost.Cost Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
