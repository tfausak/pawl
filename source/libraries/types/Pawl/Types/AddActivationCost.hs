module Pawl.Types.AddActivationCost where

import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LoyaltyKind as LoyaltyKind

-- | The payload of Pawl.Types.PlayerEffect's AddActivationCost arm (#1305).
--
-- The Filter matches the ability's SOURCE PERMANENT, as
-- Pawl.Types.ReduceActivationCost's does; the components are a list because one
-- sentence can name several actions.
--
-- `whichLoyalty` is the second criterion, and the source filter cannot say it:
-- CR 606.2 classifies the ability BEING ACTIVATED, which is Carth the Lion's
-- "Planeswalkers' LOYALTY abilities you activate". Nothing is every activated
-- ability of a matching source, which is what Brutal Suppression and Drought
-- print; Just a kind is only the abilities on that side of the rule. A Filter
-- atom could never stand in -- that field is asked of the planeswalker, and
-- whether the ability being activated carries a loyalty symbol is a fact about
-- the ABILITY (Pawl.Engine.Cost.isLoyaltyCost). Just NonLoyaltyAbility is
-- expressible and no printing writes it; the field is a classification either
-- way, so admitting the other side costs nothing and asserts nothing.
--
-- Pawl.Types.AbilityKind is a DIFFERENT axis and not this one -- see
-- Pawl.Types.LoyaltyKind for why the two rules cannot share a type. No printing
-- narrows an ADDITION by CR 605.1a's division, so there is no `whichKind` here
-- beside it; Pawl.Types.IncreaseActivationCost carries that one.
--
-- The scale is how many times those components join the cost (#1417): Brutal
-- Suppression writes Once, Drought's "for each black mana symbol in their
-- activation costs" writes PerColoredSymbol. It is a DEFAULTED wire key
-- (Pawl.Codec.AddActivationCost), so an unscaled sentence writes nothing.
data AddActivationCost = MkAddActivationCost
  { whichAbilities :: Filter.Filter Keyword.Keyword,
    whichLoyalty :: Maybe LoyaltyKind.LoyaltyKind,
    components :: [CostComponent.CostComponent Keyword.Keyword],
    scale :: CostScale.CostScale
  }
  deriving (Eq, Ord, Show)
