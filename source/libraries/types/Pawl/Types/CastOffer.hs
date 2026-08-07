module Pawl.Types.CastOffer where

-- | What an effect says about a cast IT offers, beyond what casting the card
-- would ordinarily mean -- CR 310.11b's "you may cast it transformed without
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
-- Both are Bools rather than richer choices because each is the presence or
-- absence of one printed clause. A second alternative-cost WORDING ("you may pay
-- {2} rather than pay this spell's mana cost") is a different rider and would be
-- a different field, not another value of this one.
data CastOffer = MkCastOffer
  { transformed :: Bool,
    withoutPayingManaCost :: Bool
  }
  deriving (Eq, Ord, Show)
