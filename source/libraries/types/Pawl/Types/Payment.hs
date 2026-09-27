module Pawl.Types.Payment where

import Data.Map.Strict (Map)
import Pawl.Types.Binding (Binding)
import Pawl.Types.SlotName (SlotName)

-- | Whether a cost was paid. CR 601.2h allows no partial payments, so the answer
-- is genuinely two-valued, and a sum type rather than a Bool.
--
-- Runtime-only: a Payment is never card data and never serialized.
--
-- Unpaid unwinds the whole ACTION: Pawl.Engine.Cost.pay puts back the state the
-- caller says the action began in before returning it, so a caller never has to
-- unwind a partial payment (mana spent, one component paid, the next one
-- rejected) or its own announcement. It is not a complete no-op, CR 733.1
-- leaving the mana abilities the payer activated in the CR 605.3a window to that
-- player -- one who keeps them keeps the mana, the taps and CR 405.6c's other
-- effects (Cost.reverseIllegal).
--
-- Paid carries the slots the payment BOUND -- CR 608.2h's "the sacrificed
-- creature", whose power Jarad, Golgari Lich Lord reads after the payment put it
-- in a graveyard, and Ooze Flux's "the number of +1\/+1 counters removed this
-- way". Shaped as the binding environment it is folded into
-- (Pawl.Engine.Binding.setPaid), so an object slot and an amount slot ride the
-- same map. Empty for every component that binds nothing, which is all of them
-- but the ones Pawl.Engine.Cost's payComponent gives a reserved name.
data Payment
  = Paid (Map SlotName Binding)
  | Unpaid
  deriving (Eq, Ord, Show)
