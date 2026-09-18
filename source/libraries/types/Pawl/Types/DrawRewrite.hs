module Pawl.Types.DrawRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame

-- | CR 614.1a / 614.11: how a replacement rewrites a would-draw-a-card event.
-- Under either arm the draw does not happen at all (CR 614.6), so no card leaves
-- the library and CR 121.4's draw-from-an-empty-library is never attempted.
--
-- A type rather than a payload-free Pawl.Types.ReplacementEffect arm, for
-- Pawl.Types.UntapRewrite's reason: a replacement effect is classified by the
-- event class it intercepts AND the rewrite shape it applies.
--
-- A shape per rewrite rather than a general list of effects: the two printings
-- replace a draw with two different things, and an arm naming which one keeps
-- Pawl.Engine.Replacement.readsApplier and Pawl.Engine.Resolve.Slots.replacementRowReads
-- able to classify a row without running it.
data DrawRewrite
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
  | -- | CR 400.11c: Ring of Ma'rûf's "instead put a card you own from outside the
    -- game into your hand".
    --
    -- The player is the one the EVENT names, GainLife's seat above and for its
    -- reason -- the producer's Pawl.Types.DrawR writes `whose` as Yours, so the
    -- two are one seat on it.
    FromOutsideTheGame FromOutsideTheGame.FromOutsideTheGame
  | -- | CR 702.52a: dredge N -- "you may instead mill N cards and return this
    -- card from your graveyard to your hand".
    --
    -- The card returned is the row's own SOURCE, so the arm carries only the
    -- count: rule 702.52a says "this card", and the row is minted onto the card
    -- in the graveyard (Pawl.Engine.Keyword.graveyardReplacementsOf).
    --
    -- The miller is the player the EVENT names, GainLife's seat above and for
    -- its reason.
    --
    -- Rule 702.52a's "as long as you have at least N cards in your library" is
    -- asked by Pawl.Engine.Replacement.applies rather than written here, which
    -- is also rule 702.52b: a row that cannot mill N is not offered at all, so
    -- CR 616.1's choice never sees it.
    Dredge Natural.Natural
  deriving (Eq, Ord, Show)
