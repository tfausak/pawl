module Pawl.Type.ActivatedAbility where

import Data.Map.Strict (Map)
import Pawl.Type.AbilityCost (AbilityCost)
import Pawl.Type.Effect (Effect)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)

-- CR 602.1: "[cost]: [effect]". Reuses the Effect vocabulary and the slot/target
-- machinery of a spell. An ability is VALUE-typed: two abilities with the same
-- cost and effect are indistinguishable, so Action.Activate carries the value and
-- validates by membership (Projection.abilitiesOf), never an index.
-- Parametric in `card` for the same reason as Effect: its effects are
-- `[Effect card]`, so a concrete `Effect Card` would drag Card into this module
-- and cycle with Card (which embeds [ActivatedAbility Card]). Card ties the knot.
data ActivatedAbility card = MkActivatedAbility
  { cost :: AbilityCost,
    effects :: [Effect card],
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
