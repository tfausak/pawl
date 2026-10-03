module Pawl.Types.CastRepetition where

import qualified Pawl.Types.Quantity as Quantity

-- | How many of the cards a CR 608.2g offer names may be cast under it: the one
-- Shell of the Last Kappa's "cast a spell from among cards exiled with Shell of
-- the Last Kappa" allows, or the "any number" Fevered Suspicion's "you may cast
-- any number of spells from among those nonland cards without paying their mana
-- costs" allows.
--
-- @Once@ is a CHOICE among the named cards and a single cast (CR 601.3);
-- @AnyNumber@ repeats that choice over the cards still on offer until the caster
-- declines, which is what Pawl.Engine.Resolve.Effect.offerCast does with it.
--
-- NOT Pawl.Types.CastObligation, which is the other axis: whether the one cast
-- this type admits is a "may" or an "if able". A card prints both -- "you MAY
-- cast ANY NUMBER" -- and neither answers the other's question.
--
-- Not a Bool, for Pawl.Types.CastObligation's reason: @AnyNumber@ names the
-- printed words where @True@ would say nothing.
data CastRepetition
  = Once
  | AnyNumber
  | -- | Rod of Absorption's "cast any number of spells ... with total mana value
    -- X or less": @AnyNumber@ while the spells cast so far leave room for the
    -- next one's mana value (CR 202.3).
    WithinTotalManaValue Quantity.Quantity
  deriving (Eq, Ord, Show)
