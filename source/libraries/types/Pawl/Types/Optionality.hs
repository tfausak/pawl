module Pawl.Types.Optionality where

import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 603.5: whether a clause's instructions are OPTIONAL -- the printed "may".
-- The ability goes on the stack regardless, and the choice is made as it
-- resolves.
--
-- That timing is why this rides Pawl.Types.Clause and is not a
-- ModeSelection.ChooseBetween 0 1 over a one-mode payload. Modes and targets are
-- chosen as the spell is cast (CR 601.2b/601.2c, CR 700.2b, CR 603.3d); a "may" is
-- decided strictly later, while the effect is applied (CR 608.2d). As a mode
-- selection, an ability the player declines would leave the stack with no legal
-- mode instead of resolving and doing nothing.
--
-- Not a Bool, for the reason Regenerability and TapState are not: `Optional` says
-- which rule is in play where `True` would say nothing.
--
-- Scoped to a CLAUSE (CR 608.2e) where Pawl.Types.Clause carries it, which is
-- the span one printed "may" governs. A "may" over two instructions is one
-- clause and so one question, as the printed English says; two adjacent printed
-- "may"s are two clauses and two questions. Shed Weakness is the card that
-- separates the two readings.
--
-- CR 608.2g's own may/must axis ("instructs or allows") is a DIFFERENT question,
-- asked after this one over the span of one cast: Pawl.Types.CastObligation
-- carries it, and Wild Evocation's mandatory cast sits in a clause whose own
-- instructions are mandatory. Two independent readings rather than nested ones,
-- which is why they are two types.
data Optionality
  = Mandatory
  | -- | WHO is asked. CR 603.5 says only that the choice is made on resolution;
    -- the printed sentence says whose it is, and it is not always the resolving
    -- controller -- Jungle Wayfinder's "EACH PLAYER may search their library"
    -- asks the whole table, one question each. CR 608.2e then orders them: every
    -- choice for the action is made in APNAP order before the action is taken.
    --
    -- Pawl.Types.OfferCast.caster and Pawl.Types.PayGate.payer are the same field
    -- on the neighbouring two questions, and the reason it rides HERE rather than
    -- on Pawl.Types.Clause is that a mandatory clause has nobody to name: the
    -- pairing "Mandatory, asked of each player" is one this type cannot express
    -- and a Clause field would admit.
    --
    -- Relative You is the unmarked value -- what every printed "you may" means
    -- (CR 405.4 for a spell, CR 113.8 for an ability) -- and the codec writes it
    -- as a bare tag, so a card says nothing unless it means somebody else.
    --
    -- The seats that ACCEPT are bound under Binding.mayPlayers, which is how the
    -- clause's own instructions say "they" -- Binding.gatePlayers one question
    -- over.
    Optional PlayerRef.PlayerRef
  deriving (Eq, Ord, Show)
