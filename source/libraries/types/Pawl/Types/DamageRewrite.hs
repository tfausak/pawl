module Pawl.Types.DamageRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Scaling as Scaling

-- | CR 614.1a / 615.1: how a replacement or prevention effect rewrites a damage
-- event. PreventAll cancels it outright (Fog) -- CR 615.6, a prevented event
-- never happens.
--
-- PreventNext is CR 615.7's shield -- "Prevent the next 4 damage that would be
-- dealt to any target this turn" (Mending Hands) -- and its Natural is the
-- REMAINING amount rather than the printed one. That is CR 615.7's own
-- arithmetic ("preventing 1 damage reduces the remaining shield by 1"), so the
-- number here is rewritten in place on the row that carries it, and the row is
-- dropped once it reaches 0 (Pawl.Engine.Replacement.reduceShield).
--
-- The remaining amount lives HERE rather than as a counted arm of
-- Pawl.Types.Uses, and CR 615.7's last sentence is the reason: "such effects
-- count only the amount of damage; the number of events or sources dealing it
-- doesn't matter." Uses counts APPLICATIONS -- one per event -- so a counted
-- Uses would spend the shield in the unit the rule says does not matter, and a
-- shield partially covering a 5-damage event would be spent as though it had
-- covered the whole of it. Uses.Unlimited is what a shield's row carries, and
-- this number is its whole terminator.
--
-- Engine-baked, never authored on a card, for the reason
-- Pawl.Types.DamagePattern.whichRecipient gives: a shield names the permanent or
-- player it shields, which is chosen at resolution. Effect.PreventNextDamage is
-- the one producer.
--
-- SetAmount is CR 614.1a's "instead" with a flat number: Galvanic Blast's "deals
-- 4 damage instead". A Natural rather than a Pawl.Types.Quantity because every
-- printed instead-amount in the pool is a literal, and a variable one would need
-- the whole quantity-evaluation environment (a resolving object, an announced X)
-- inside the CR 616.1 loop, which nothing asks for yet.
--
-- Scale is Furnace of Rath's "it deals double that damage ... instead", and it
-- reuses Pawl.Types.Scaling rather than adding a Double arm for the reason that
-- type's own comment gives: the difference between doubling and tripling is a
-- NUMBER. Same vocabulary CounterR and TokenR rewrite their counts with, and the
-- same Pawl.Engine.Replacement.scale evaluates it.
data DamageRewrite
  = PreventAll
  | PreventNext Natural.Natural
  | SetAmount Natural.Natural
  | Scale Scaling.Scaling
  deriving (Eq, Ord, Show)
