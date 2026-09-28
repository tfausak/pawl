module Pawl.Types.ProliferateRewrite where

-- | CR 701.34a / 614.1a: how a replacement rewrites one instruction to
-- proliferate.
--
-- A type rather than a payload-free Pawl.Types.ReplacementEffect arm, for
-- Pawl.Types.CoinFlipRewrite's reason: a replacement effect is classified by the
-- event class it intercepts AND the rewrite shape it applies.
data ProliferateRewrite
  = -- | CR 614.1a: Tekuthal, Inquiry Dominus's "proliferate twice instead" --
    -- twice as many proliferates, so under CR 614.5 a second row doubles again.
    Doubled
  deriving (Bounded, Enum, Eq, Ord, Show)
