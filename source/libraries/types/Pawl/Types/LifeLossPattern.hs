module Pawl.Types.LifeLossPattern where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.LifeLossCause as LifeLossCause

-- | CR 614.1a: which life losses a life-total replacement intercepts -- WHOSE
-- life total, and what CAUSED the loss.
--
-- Worship is Yours ("your life total") and ByDamage ("damage that would reduce
-- it"). Nothing in the pool narrows by how much life would be lost: the
-- threshold every such clause states is about the RESULTING TOTAL, which is
-- Pawl.Types.LifeLossRewrite's number rather than a pattern field.
data LifeLossPattern = MkLifeLossPattern
  { -- | CR 109.5's relation, read against the effect source's controller.
    whose :: ControllerRelation.ControllerRelation,
    -- | Our own encoding convention, not a rule: Nothing means any cause, never
    -- no cause.
    whichCause :: Maybe LifeLossCause.LifeLossCause
  }
  deriving (Eq, Ord, Show)
