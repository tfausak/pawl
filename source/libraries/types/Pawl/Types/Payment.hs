module Pawl.Types.Payment where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Pawl.Types.Recipient (Recipient)
import Pawl.Types.SlotName (SlotName)

-- | Whether a cost was paid. CR 601.2h allows no partial payments, so the answer
-- is genuinely two-valued, and a sum type rather than a Bool.
--
-- Runtime-only: a Payment is never card data and never serialized.
--
-- Unpaid is always a COMPLETE no-op: Pawl.Engine.Cost.pay restores the state it was
-- entered with before returning it, so a caller never has to unwind a partial
-- payment (mana spent, one component paid, the next one rejected).
--
-- Paid carries the slots the payment BOUND -- CR 608.2h's "the sacrificed
-- creature", whose power Jarad, Golgari Lich Lord reads after the payment put it
-- in a graveyard. Shaped as the recipient map CR 601.2c's targets ride in
-- (Pawl.Engine.Binding.targetsOf) rather than as a bare id list, so the caller
-- that merges it into an object's bindings writes one field and the readers that
-- already answer "which object does this slot name" need no second shape. Empty
-- for every component that binds nothing, which is all of them but the two that
-- reserve a name (Pawl.Engine.Cost's Sacrifice and TapPermanents arms).
data Payment
  = Paid (Map SlotName (Set Recipient))
  | Unpaid
  deriving (Eq, Ord, Show)
