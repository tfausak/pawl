module Pawl.Types.PayGate where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.SlotName as SlotName

-- | CR 118.12's cost, offered to a player as the spell or ability RESOLVES, plus
-- which of that rule's two branches the clause's instructions are.
--
-- Both branches ride one type because the offer is one offer. CR 118.12a's
-- rewriting reaches the negative one -- "'[Do something] unless [a player does
-- something else]' ... means the same thing as '[A player may do something
-- else]. If [that player doesn't], [do something]'" -- so Mana Leak's "counter
-- target spell unless its controller pays {3}" is the clause's effect list under
-- @IfNotPaid@. Merfolk Seer's "you may pay {1}{U}. If you do, draw a card" is
-- the same offer under @IfPaid@. The "something else" need not spend a resource:
-- CR 701.63a's endure offers putting +1/+1 counters on the permanent (Fortress
-- Kin-Guard), and CR 702.123a's fabricate prints the negative shape with the
-- rewriting already done.
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
-- The clause is CR 608.2e's span, so a gate governing only some of an ability's
-- instructions is representable -- Condescend's "counter target spell unless its
-- controller pays {X}. Scry 2", where the scry happens either way. No card in the
-- pool prints that shape yet, and Condescend itself also wants {X} in a
-- resolution cost (CR 107.3, CR 118.4), so #703 stays open on the card rather
-- than on the carrier.
--
-- Not implemented: two clauses hanging off ONE payment -- Divert Disaster's
-- "counter target spell unless its controller pays {2}. If they do, you create a
-- Lander token". A gate per clause is a prompt per clause, so that card would be
-- asked twice and charged twice (#1555).
data PayGate = MkPayGate
  { -- | Which player is offered the cost. A SLOT rather than a
    -- Pawl.Types.PlayerRef, because the answer Mana Leak needs is "the
    -- controller of the object bound here" and PlayerRef.InSlot reads a slot
    -- bound to a PLAYER. Pawl.Engine.Resolve.payerOf takes both readings off one
    -- slot -- a slot bound to a player IS that player, and a slot bound to an
    -- object names whoever controls it (CR 109.4, and CR 405.4 for a spell) --
    -- so "unless that player pays" needs no second field.
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
    -- this enchantment") needs no second field. Mana Leak's is {3}, Merfolk
    -- Seer's {1}{U}.
    --
    -- NOT routed through CR 601.2f's totalling: that rule totals the cost of a
    -- spell being CAST or an ability being ACTIVATED, and a cost paid during
    -- resolution is neither, so no cost increase or reduction applies to it.
    cost :: Cost.Cost Keyword.Keyword,
    -- | Which of CR 118.12's branches this clause's instructions are. See
    -- Pawl.Types.PayBranch, and Pawl.Engine.Resolve.payGateAdmits for where the
    -- comparison is made.
    branch :: PayBranch.PayBranch
  }
  deriving (Eq, Ord, Show)
