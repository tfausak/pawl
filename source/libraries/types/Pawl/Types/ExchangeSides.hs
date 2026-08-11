module Pawl.Types.ExchangeSides where

import qualified Pawl.Types.SlotName as SlotName

-- | Which two players an exchange runs between (CR 701.12a: an exchange has
-- exactly two sides, and CR 701.12c is the life-total one). Read by
-- Effect.ExchangeLifeTotals.
--
-- Its own sum rather than a pair of Pawl.Types.PlayerRefs: a PlayerRef may name
-- every player at once, which an exchange has nowhere to put, and the two sides
-- of "two target players" come out of ONE instance of the word "target" (CR
-- 601.2c), which no pair of separate references can spell.
--
-- The two arms are the two printed shapes, and they differ in how many
-- recipients the slot holds -- which is why the choice is DATA rather than
-- something resolution could infer from the binding: a slot's count is card
-- data, and a Mirror Universe whose one target had gone illegal would otherwise
-- read as an unfillable Soul Conduit instead of as CR 701.12a's incomplete
-- exchange.
data ExchangeSides
  = -- | Mirror Universe's "exchange life totals with target opponent": the
    -- unstated "you" of CR 109.5 and the one player the slot names.
    WithController SlotName.SlotName
  | -- | Soul Conduit's "two target players exchange life totals": both sides come
    -- out of the one slot, whose count is exactly two (CR 601.2c, which also
    -- makes them distinct -- the same target cannot be chosen twice for one
    -- instance of the word). The controller is a side only if named as one.
    BetweenTargets SlotName.SlotName
  deriving (Eq, Ord, Show)
