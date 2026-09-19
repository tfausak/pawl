module Pawl.Types.MillCountRewrite where

import qualified Pawl.Types.Scaling as Scaling

-- | CR 614.1a: how a replacement rewrites an instruction to mill cards (CR
-- 701.17a).
--
-- A type rather than a bare payload-free arm on Pawl.Types.ReplacementEffect for
-- Pawl.Types.LifeGainRewrite's reason: a replacement effect is classified by the
-- event class it intercepts AND the rewrite shape it applies.
newtype MillCountRewrite
  = -- | Bruvac the Grandiloquent's "they mill twice that many cards instead": the
    -- instruction is resized, and the resized count is how many cards move.
    --
    -- Pawl.Types.Scaling rather than a bare multiplier, Pawl.Types.LifeGainRewrite's
    -- Scaled for its reason: that type is already the vocabulary for "twice that
    -- many \/ that many plus four", which is Bruvac and The Water Crystal.
    Scaled Scaling.Scaling
  deriving (Eq, Ord, Show)
