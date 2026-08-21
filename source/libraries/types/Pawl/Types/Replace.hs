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
    -- | The clause's printed "if". Nothing is the unconditional case, which is
    -- most of them, so the key is elided rather than written null.
    --
    -- NOT a gate on installation: CR 614.1 makes a replacement effect's
    -- applicability a question asked as the event would happen, so Resolve copies
    -- this onto Pawl.Types.ActiveReplacement.condition and the CR 616.1 loop asks
    -- it there. Galvanic Blast cannot tell the two apart -- a CR 614.15
    -- self-replacement is installed and applied inside one resolution, with no
    -- window for the board to change -- and a row with a stated duration can
    -- (Synthetic Voltaic Surge).
    --
    -- The same posture Pawl.Types.PrintedReplacement.condition takes on a
    -- permanent's static ability, which is CR 604.1's "simply true".
    condition :: Maybe Condition.Condition,
    effect :: ReplacementEffect.ReplacementEffect effect
  }
  deriving (Eq, Ord, Show)
