{-# LANGUAGE DeriveLift #-}

module Pawl.Type.ActivatedAbility where

import Data.Map.Strict (Map)
import Language.Haskell.TH.Syntax (Lift)
import Pawl.Type.AbilityCost (AbilityCost)
import Pawl.Type.Effect (Effect)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)

-- CR 602.1: "[cost]: [effect]". Reuses the Effect vocabulary and the slot/target
-- machinery of a spell. An ability is VALUE-typed: two abilities with the same
-- cost and effect are indistinguishable, so Action.Activate carries the value and
-- validates by membership (Projection.abilitiesOf), never an index.
data ActivatedAbility = MkActivatedAbility
  { cost :: AbilityCost,
    effects :: [Effect],
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Lift, Ord, Show)
