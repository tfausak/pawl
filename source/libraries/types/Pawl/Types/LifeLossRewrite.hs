module Pawl.Types.LifeLossRewrite where

import qualified Numeric.Natural as Natural

-- | CR 614.1a: how a replacement rewrites a would-lose-life event.
--
-- A type rather than a bare payload-free arm on Pawl.Types.ReplacementEffect for
-- Pawl.Types.UntapRewrite's reason: a replacement effect is classified by the
-- event class it intercepts AND the rewrite shape it applies.
newtype LifeLossRewrite
  = -- | Worship's "reduces it to 1 instead": the loss is cut back to whatever
    -- leaves the player at this total, and to nothing at all if they are already
    -- at or below it.
    --
    -- The FLOOR rather than the surviving amount, which is what the printed
    -- template states -- "damage that would reduce your life total to less than
    -- 1 reduces it to 1 instead" names the total, never the loss. Its own
    -- Gatherer ruling insists on the distinction: "It reduces your life total to
    -- 1, not the damage to 1."
    --
    -- Carrying the number rather than fixing it at 1, because the number is
    -- printed on the card and reading it costs nothing.
    LeaveAtLeast Natural.Natural
  deriving (Eq, Ord, Show)
