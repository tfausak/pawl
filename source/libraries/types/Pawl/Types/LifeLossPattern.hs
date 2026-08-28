module Pawl.Types.LifeLossPattern where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.LifeLossCause as LifeLossCause

-- | CR 614.1a: which life losses a life-total replacement intercepts -- WHOSE
-- life total, and what CAUSED the loss.
--
-- Worship is Yours ("your life total") and ByDamage ("damage that would reduce
-- it"); Bloodletter of Aclazotz is Opponents ("an opponent") and no cause at all
-- ("would lose life", which its own reminder text spells out as reaching damage
-- too); Ashiok, Wicked Manipulator is Yours and ByPayment ("if you would pay
-- life").
--
-- No field here is about HOW MUCH life would be lost, and no printing needs one:
-- Worship's threshold is on the RESULTING TOTAL, which is
-- Pawl.Types.LifeLossRewrite's number, and Ashiok's is on the amount against a
-- zone count, which Pawl.Engine.Replacement.breaches asks per rewrite. Both are
-- questions a pattern shared by every rewrite cannot phrase.
data LifeLossPattern = MkLifeLossPattern
  { -- | CR 109.5's relation, read against the effect source's controller.
    whose :: ControllerRelation.ControllerRelation,
    -- | Our own encoding convention, not a rule: Nothing means any cause, never
    -- no cause.
    whichCause :: Maybe LifeLossCause.LifeLossCause
  }
  deriving (Eq, Ord, Show)
