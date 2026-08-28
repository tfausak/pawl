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
    -- Pawl.Engine.Resolve's Effect.LoseLife arm.
    --
    -- Not implemented: CR 118.3b's life PAYMENT, CR 119.5's set-a-total downward
    -- and CR 701.12c's exchange raise no event of their own, so no replacement
    -- reaches any of them (#2544).
    ByEffect
  deriving (Bounded, Enum, Eq, Ord, Show)
