module Pawl.Types.DrawCountRewrite where

-- | CR 121.2a / 614.1a: how a replacement rewrites an instruction to draw cards,
-- applied before any of the individual draws (CR 616.1g).
--
-- A type rather than a payload-free Pawl.Types.ReplacementEffect arm, for
-- Pawl.Types.DrawRewrite's reason: a replacement effect is classified by the
-- event class it intercepts AND the rewrite shape it applies.
data DrawCountRewrite
  = -- | CR 614.1a: Alms Collector's "instead you and that player each draw a
    -- card" -- the row's controller and the instructed player each draw one.
    EachDrawOne
  deriving (Bounded, Enum, Eq, Ord, Show)
