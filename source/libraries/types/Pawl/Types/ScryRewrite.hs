module Pawl.Types.ScryRewrite where

-- | CR 701.22a / 614.1a: how a replacement rewrites one player's instruction to
-- scry N.
--
-- A type rather than a payload-free Pawl.Types.ReplacementEffect arm, for
-- Pawl.Types.CoinFlipRewrite's reason: a replacement effect is classified by the
-- event class it intercepts AND the rewrite shape it applies.
data ScryRewrite
  = -- | Kenessos, Priest of Thassa's "scry that many cards plus one instead".
    PlusOne
  | -- | Eligeth, Crossroads Augur's "draw that many cards instead" (CR 614.6).
    DrawInstead
  deriving (Bounded, Enum, Eq, Ord, Show)
