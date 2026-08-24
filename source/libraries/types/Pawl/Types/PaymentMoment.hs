module Pawl.Types.PaymentMoment where

-- | WHEN a cost is being paid, at the one grain the rules make anything depend
-- on: is there a resolution for the payment's own events to be the effect of?
--
-- CR 609.1 makes an effect "something that happens in the game as a result of a
-- spell or ability" resolving, so a cost paid under CR 601.2h -- while a spell is
-- being cast or, through CR 602.2b, an ability activated -- produces no effect,
-- and neither does CR 508.1i's or CR 509.1e's combat toll. A cost paid under CR
-- 118.12, as the spell or ability RESOLVES, does.
--
-- Carried into Pawl.Engine.Cost.pay rather than derived there, because nothing
-- about a cost says which moment it is being paid at: Soul Immolation's "blight
-- X" and Boggart Mischief's "unless you blight 1" are the same
-- Pawl.Types.CostComponent, and only the caller knows which door it came in by.
--
-- Read by Pawl.Engine.Cost.counterCause, and by nothing else -- CR 614.16 is the
-- only rule in the vocabulary that asks.
data PaymentMoment
  = -- | CR 601.2h's payment, and CR 508.1i \/ 509.1e's combat toll: no spell or
    -- ability is resolving, so CR 609.1 gives the payment's events no effect to
    -- be the result of.
    OutsideResolution
  | -- | CR 118.12's payment, made as the spell or ability resolves, which is
    -- exactly CR 609.1's "when a spell, activated ability, or triggered ability
    -- resolves".
    DuringResolution
  deriving (Eq, Ord, Show)
