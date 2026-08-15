module Pawl.Types.Replace where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Uses as Uses

-- | CR 614: install a floating replacement effect -- how long it lasts, how many
-- times it applies, what it attaches to, what gates its creation, and what it
-- actually rewrites.
--
-- Parametric in the EFFECT for Pawl.Types.DamageR's reason, and instantiated at
-- `Effect card` where Pawl.Types.Effect declares the arm that holds it.
data Replace effect = MkReplace
  { duration :: Duration.Duration,
    uses :: Uses.Uses,
    origin :: ReplacementOrigin.ReplacementOrigin,
    -- | CR 614.15's gate on whether the row is installed at all, checked on
    -- resolution. Nothing is the unconditional case, which is most of them, so
    -- the key is elided rather than written null.
    condition :: Maybe Condition.Condition,
    effect :: ReplacementEffect.ReplacementEffect effect
  }
  deriving (Eq, Ord, Show)
