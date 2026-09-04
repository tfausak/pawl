module Pawl.Types.LifeGainRewrite where

import qualified Pawl.Types.Scaling as Scaling

-- | CR 614.1a: how a replacement rewrites a would-gain-life event.
--
-- A type rather than a bare payload-free arm on Pawl.Types.ReplacementEffect for
-- Pawl.Types.UntapRewrite's reason: a replacement effect is classified by the
-- event class it intercepts AND the rewrite shape it applies.
newtype LifeGainRewrite
  = -- | Boon Reflection's "you gain twice that much life instead": the gain is
    -- resized, and the resized amount is what the player's total moves by.
    --
    -- Pawl.Types.Scaling rather than a bare multiplier, Pawl.Types.LifeLossRewrite's
    -- Scaled for its reason: that type is already the vocabulary for "twice that
    -- many \/ that many plus one" and a life-total clause resizes by the same
    -- arithmetic.
    Scaled Scaling.Scaling
  deriving (Eq, Ord, Show)
