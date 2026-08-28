module Pawl.Types.LifeLossCause where

-- | CR 120.4c / 119.3: how a life loss came about -- the one grain a replacement
-- effect that watches a life total narrows by.
--
-- It matters because the printed clauses split on it. Worship's is "DAMAGE that
-- would reduce your life total to less than 1", and its own ruling says the
-- other half out loud: "Worship does not prevent loss of life, so loss of life
-- bypasses Worship". CR 120.4c is where the damage half happens -- damage that
-- has been dealt is "processed into its results, as modified by replacement
-- effects that interact with those results (such as life loss or counters)" --
-- and CR 119.3 is where the other half does.
--
-- Carried on Pawl.Types.ProposedEvent's WouldLoseLife rather than derived from
-- it, for the reason Pawl.Types.CounterCause gives one event class over: nothing
-- about a proposed loss says where it came from, the player and the amount being
-- identical either way. Only the caller knows.
--
-- Read by Pawl.Engine.Replacement.applies, and by nothing else.
data LifeLossCause
  = -- | CR 120.3a: the life a damage event causes its player to lose, proposed by
    -- Pawl.Engine.Damage.applyDamage at CR 120.4c's result-processing step. The
    -- DAMAGE itself is settled by then and is not what this rewrites -- which is
    -- why a lifelink source still gains its controller the whole amount dealt.
    ByDamage
  | -- | CR 119.3: the life an effect causes a player to lose --
    -- Pawl.Engine.Resolve's Effect.LoseLife arm, and its Effect.SetLifeTotal arm
    -- where the new total is lower, CR 119.5 spelling that as the player losing
    -- "the necessary amount of life".
    --
    -- Not implemented: CR 701.12c's exchange and CR 119.7's redistribution move a
    -- life total without proposing a loss of their own, so no replacement reaches
    -- either (#2544).
    ByEffect
  | -- | CR 119.4: the life a PAYMENT costs its payer -- Pawl.Engine.Event.payLife,
    -- reached from a cost component, from CR 107.4f's Phyrexian symbol and from
    -- CR 614.1c's pay-or-enter-tapped rewrite.
    --
    -- Its own arm rather than ByEffect's, because rule 119.4 arrives at the loss
    -- by a different road than rule 119.3 does -- a cost the player chooses to
    -- pay, not an effect instructing them -- and a printed clause narrows by
    -- exactly that grain: Ashiok, Wicked Manipulator says "if you would pay life",
    -- where Zof Consumption's loss is an effect's.
    --
    -- Not implemented: no card in data/cards/ narrows a
    -- Pawl.Types.LifeLossPattern to this cause; Ashiok, Wicked Manipulator would
    -- be the producer, and its rewrite (exile that many cards instead) is a shape
    -- Pawl.Types.LifeLossRewrite has no arm for (gap #2549).
    ByPayment
  deriving (Bounded, Enum, Eq, Ord, Show)
