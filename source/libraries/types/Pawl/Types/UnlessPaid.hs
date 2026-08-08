module Pawl.Types.UnlessPaid where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SlotName as SlotName

-- | CR 118.12a: the "unless" a clause's instructions hang on -- Mana Leak's
-- "Counter target spell UNLESS ITS CONTROLLER PAYS {3}". That rule is the whole
-- semantics, and it is a rewriting rather than a concept of its own: "'[Do
-- something] unless [a player does something else]' ... means the same thing as
-- '[A player may do something else]. If [that player doesn't], [do something].'"
-- So the clause's effect list is the "if they don't" branch, and this is the
-- offer that precedes it.
--
-- CR 118.12 puts the payment at RESOLUTION -- "the action [do something] is a
-- cost, paid when the spell or ability resolves" -- which is the whole reason
-- this cannot ride Pawl.Types.Face.additionalCosts or .alternativeCosts. Those
-- are announced and paid at CR 601.2f-h, as the spell is cast, by its caster.
--
-- A field on Pawl.Types.Clause rather than an arm of Pawl.Types.Effect, for
-- Pawl.Types.Optionality's reason carried over: it gates an instruction list on
-- an answer given as the effect is applied, and an Effect arm holding other
-- effects would put a BRANCH inside the ISA, which design.md section 1 keeps
-- out.
--
-- The clause is CR 608.2e's span, so an "unless" governing only some of an
-- ability's instructions is now representable -- Condescend's "counter target
-- spell unless its controller pays {X}. Scry 2", where the scry happens either
-- way. No card in the pool prints that shape yet, and Condescend itself also
-- wants {X} in a resolution cost (CR 107.3, CR 118.4), so #703 stays open on
-- the card rather than on the carrier.
--
-- NEGATIVE only: the effects run when the cost was NOT paid. CR 118.12's other
-- half -- "[Do something]. If [a player] does, [effect]", where the effects run
-- when it WAS paid (Standstill) -- has no producer and no representation (#701).
data UnlessPaid = MkUnlessPaid
  { -- | Which player is offered the cost. A SLOT rather than a
    -- Pawl.Types.PlayerRef, because the answer Mana Leak needs is "the
    -- controller of the object bound here" and PlayerRef.InSlot reads a slot
    -- bound to a PLAYER. Pawl.Engine.Resolve.payerOf takes both readings off one
    -- slot -- a slot bound to a player IS that player, and a slot bound to an
    -- object names whoever controls it (CR 109.4, and CR 405.4 for a spell) --
    -- so a future "unless that player pays" needs no second field.
    --
    -- A slot and not the resolving controller, which is the question this card
    -- family forces: Mana Leak's payer is the TARGETED spell's controller, who
    -- controls nothing about the resolution. Pawl.Types.MonarchTarget's InSlot
    -- arm is the same call made at a different opcode: pawl has no general
    -- "which player" spec for effects, and a slot name is what a spell's own
    -- target namespace already offers.
    payer :: SlotName.SlotName,
    -- | What that player is offered the chance to pay. A whole Cost and not a
    -- bare ManaCost, so Pawl.Engine.Cost.canPay and .pay are the one payment
    -- path, and so a non-mana cost of this family (CR 118.12's own "sacrifice
    -- this enchantment") needs no second field. Mana Leak's is {3}.
    --
    -- NOT routed through CR 601.2f's totalling: that rule totals the cost of a
    -- spell being CAST or an ability being ACTIVATED, and a cost paid during
    -- resolution is neither, so no cost increase or reduction applies to it.
    cost :: Cost.Cost Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
