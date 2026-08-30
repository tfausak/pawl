module Pawl.Types.DrawRewrite where

import qualified Numeric.Natural as Natural

-- | CR 614.1a / 614.11: how a replacement rewrites a would-draw-a-card event.
-- Under its one arm the draw does not happen at all (CR 614.6), so no card leaves
-- the library and CR 121.4's draw-from-an-empty-library is never attempted.
--
-- A type rather than a payload-free Pawl.Types.ReplacementEffect arm, for
-- Pawl.Types.UntapRewrite's reason: a replacement effect is classified by the
-- event class it intercepts AND the rewrite shape it applies.
newtype DrawRewrite
  = -- | CR 614.1a: Words of Worship's "you gain 5 life instead".
    --
    -- The gainer is the player the EVENT names -- the one who would have drawn --
    -- and not the row's controller, whom CR 109.5 would make "you". The two are
    -- one seat on the producer in the pool, whose clause reads "the next time YOU
    -- would draw a card" and so writes Pawl.Types.DrawR's `whose` as Yours.
    -- Reading the event is what keeps Pawl.Engine.Replacement.readsApplier
    -- answering False for this rewrite; a printing whose two halves named
    -- different seats would have to revisit that.
    GainLife Natural.Natural
  deriving (Eq, Ord, Show)
