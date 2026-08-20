module Pawl.Types.PayGate where

import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.PayObligation as PayObligation
import qualified Pawl.Types.PlayerRef as PlayerRef

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
-- instructions is representable -- Stymied Hopes' "counter target spell unless
-- its controller pays {1}. Scry 1", where the scry happens either way and is a
-- clause carrying no gate at all.
--
-- Two clauses hanging off ONE payment are `offeredAt` below: Don't Make a
-- Sound's "counter target spell unless its controller pays {2}. If they do,
-- surveil 2" is an IfNotPaid clause and an IfPaid clause over one offer.
data PayGate = MkPayGate
  { -- | Which players are offered the cost. A Pawl.Types.PlayerRef and not a
    -- slot, because CR 118.12a's rewriting is per player and a card may name
    -- several at once: Rishadan Cutpurse's "each opponent sacrifices a permanent
    -- of their choice unless they pay {1}" is one offer to each opponent, and
    -- CR 101.4 puts them in APNAP order. Every single-payer card in the pool
    -- writes one of the two slot-reading arms -- Whipstitched Zombie's own
    -- controller is InSlot "you", Mana Leak's targeted spell's controller is
    -- ControllerOfBound "spell" (CR 109.4, CR 405.4 for a spell).
    --
    -- Not the resolving controller, which is the question this card family
    -- forces: Mana Leak's payer is the TARGETED spell's controller, who controls
    -- nothing about the resolution.
    --
    -- Whichever players the gate's answer selects are bound under
    -- Pawl.Engine.Binding.gatePlayers, so the clause's own instructions can say
    -- "they" -- see Pawl.Engine.Resolve.payGateAdmits.
    payer :: PlayerRef.PlayerRef,
    -- | What that player is offered the chance to pay. A whole Cost and not a
    -- bare ManaCost, so Pawl.Engine.Cost.canPay and .pay are the one payment
    -- path, and so a non-mana cost of this family (CR 118.12's own "sacrifice
    -- this enchantment") needs no second field. Mana Leak's is {3}, Merfolk
    -- Seer's {1}{U}.
    --
    -- NOT routed through CR 601.2f's totalling: that rule totals the cost of a
    -- spell being CAST or an ability being ACTIVATED, and a cost paid during
    -- resolution is neither, so no cost increase or reduction applies to it.
    --
    -- An {X} in it IS resolved, which is a different rule: CR 118.4 sends X to
    -- CR 107.3a, whose value is the one the object's controller announced at
    -- CR 601.2b. Clash of Wills' is {X}; Pawl.Engine.Resolve.announcedXOn is
    -- where it is substituted in.
    cost :: Cost.Cost Keyword.Keyword,
    -- | Which of CR 118.12's branches this clause's instructions are. See
    -- Pawl.Types.PayBranch, and Pawl.Engine.Resolve.payGateAdmits for where the
    -- comparison is made.
    branch :: PayBranch.PayBranch,
    -- | CR 118.12's other axis: is this a cost the payer may decline, or one
    -- they must pay if able? See Pawl.Types.PayObligation. Mana Leak's is
    -- Optional (CR 118.12a's rewriting prints the "may"), Standstill's
    -- Mandatory.
    obligation :: PayObligation.PayObligation,
    -- | Which clause of this mode MAKES the offer, when it is not this one --
    -- CR 118.12 offers a resolution cost once and reads the one answer, so
    -- Don't Make a Sound's second clause names its first rather than asking
    -- again. Nothing is the unmarked case and means this clause is the offer.
    --
    -- An explicit ordinal (CR 608.2e's, Pawl.Types.ClauseIndex) rather than an
    -- equality over gates: two adjacent clauses that coincidentally state the
    -- same cost are two offers and must stay spellable apart from two clauses
    -- sharing one.
    --
    -- The gate a sharing clause carries is a RESTATEMENT of the one it names,
    -- and it is read rather than ignored: Pawl.Engine.Resolve.payGateAdmits
    -- reuses the recorded answer when there is one and falls back to offering
    -- this gate when there is not -- which is the case where the named clause
    -- never reached its own gate, because its CR 701.46a "if" or its CR 603.5
    -- "may" said no.
    offeredAt :: Maybe ClauseIndex.ClauseIndex
  }
  deriving (Eq, Ord, Show)
