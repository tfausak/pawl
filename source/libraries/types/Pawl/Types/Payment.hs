module Pawl.Types.Payment where

-- Whether a cost was paid. CR 601.2h: "Partial payments are not allowed.
-- Unpayable costs can't be paid." -- so the answer is genuinely two-valued, and
-- a sum type rather than a Bool per the house rule against boolean blindness.
--
-- Runtime-only: a Payment is never card data and never serialized.
--
-- Unpaid is always a COMPLETE no-op: Pawl.Engine.Cost.pay restores the state it was
-- entered with before returning it, so a caller never has to unwind a partial
-- payment (mana spent, one component paid, the next one rejected).
data Payment
  = Paid
  | Unpaid
  deriving (Eq, Show)
