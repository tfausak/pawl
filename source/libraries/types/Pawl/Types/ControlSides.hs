module Pawl.Types.ControlSides where

import qualified Pawl.Types.SlotName as SlotName

-- | Which two permanents a control exchange runs between (CR 701.12b, whose
-- subject is control of TWO permanents; CR 701.12a is what makes an exchange
-- left with fewer than two sides do nothing). Read by Effect.ExchangeControl.
--
-- Pawl.Types.ExchangeSides' shape over objects rather than players, and a
-- separate type because the unnamed side is a different thing: a life-total
-- exchange's is CR 109.5's "you", where this one is CR 113.7's source object.
-- Its own sum rather than a pair of Pawl.Types.ObjectRefs for that type's
-- reason -- a ref may name every matching permanent at once, which an exchange
-- has nowhere to put, and the two sides of "two target creatures" come out of
-- ONE instance of the word "target" (CR 601.2c).
data ControlSides
  = -- | Switcheroo's "exchange control of two target creatures": both sides come
    -- out of the one slot, whose count is exactly two (CR 601.2c, which also
    -- makes them distinct). The source is a side only if named as one.
    BetweenTargets SlotName.SlotName
  | -- | Avarice Totem's "exchange control of this artifact and target nonland
    -- permanent": CR 113.7's source object and the one permanent the slot names.
    WithSource SlotName.SlotName
  deriving (Eq, Ord, Show)
