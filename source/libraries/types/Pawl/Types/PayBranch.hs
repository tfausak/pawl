module Pawl.Types.PayBranch where

-- | CR 118.12's two halves: which side of "[Do something]. If [a player] [does,
-- doesn't, or can't], [effect]" a clause's instructions are.
--
-- @IfNotPaid@ is Mana Leak's, reached through CR 118.12a's rewriting of "[Do
-- something] unless [a player does something else]"; @IfPaid@ is the rule's own
-- first reading, Merfolk Seer's "you may pay {1}{U}. If you do, draw a card".
-- One carrier and not two, because the offer is the same offer: CR 118.12 puts
-- the payment at resolution and reads the ANSWER either way, and only the side
-- the instructions hang off differs.
--
-- Not a Bool, for Pawl.Types.Optionality's reason: @IfPaid@ says which half of
-- the rule is in play where @True@ would say nothing.
--
-- Orthogonal to CR 118.12's OTHER axis, Pawl.Types.PayObligation: which branch
-- the instructions are is a separate question from whether the payer had a
-- choice. Standstill is IfPaid and Mandatory; Merfolk Seer is IfPaid and
-- Optional.
--
-- The "may" is not a separate question in either half. CR 118.12a's rewriting
-- makes the "unless" cost an offer -- "[A player may do something else]" -- and
-- the positive half generally prints that "may" itself, so Pawl.Types.PayGate's
-- own Prompt.ChooseToPay IS the printed "may" and
-- Pawl.Types.Clause.optionality stays Mandatory on both. A card setting both
-- would ask twice.
data PayBranch
  = -- | The instructions run when the cost WAS paid -- CR 118.12's "if [a
    -- player] does".
    IfPaid
  | -- | The instructions run when the cost was NOT paid, which covers both of
    -- the rule's other answers at once: declined, and unpayable (CR 118.3).
    --
    -- A player the gate's reference never NAMED is not one of them and gets no
    -- answer at all: CR 118.12a's rewriting is per player, so a gate naming
    -- nobody selects nobody and its clause is skipped -- see
    -- Pawl.Engine.Resolve.payGateAdmits.
    IfNotPaid
  deriving (Bounded, Enum, Eq, Ord, Show)
