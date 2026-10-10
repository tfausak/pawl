module Pawl.Types.CostChange where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AppliedReduction as AppliedReduction
import qualified Pawl.Types.CostAddition as CostAddition

-- | CR 601.2f: what a Pawl.Types.CostModifier does to the total cost.
data CostChange
  = -- | CR 601.2f's "cost increases", this much GENERIC mana: no printing taxes
    -- by a coloured symbol (Thalia, Oppressive Rays).
    Increase Natural.Natural
  | -- | CR 601.2f / 118.7's "cost reductions" (Sapphire Medallion, Heartstone).
    Reduce AppliedReduction.AppliedReduction
  | -- | CR 601.2f / 118.8's additional costs applied from another effect
    -- (Drought, Brutal Suppression).
    Add CostAddition.CostAddition
  deriving (Eq, Ord, Show)
