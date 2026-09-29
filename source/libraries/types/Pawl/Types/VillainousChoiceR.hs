module Pawl.Types.VillainousChoiceR where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.VillainousChoiceRewrite as VillainousChoiceRewrite

-- | The payload of Pawl.Types.ReplacementEffect's VillainousChoiceR arm: whose
-- villainous choices are intercepted (CR 701.55c / 614.1a), and what happens
-- instead. Pawl.Types.ProliferateR's shape and for its reason.
data VillainousChoiceR = MkVillainousChoiceR
  { whose :: ControllerRelation.ControllerRelation,
    rewrite :: VillainousChoiceRewrite.VillainousChoiceRewrite
  }
  deriving (Eq, Ord, Show)
