module Pawl.Types.DamageRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Scaling as Scaling

-- | CR 614.1a / 615.1: how a replacement or prevention effect rewrites a damage
-- event. PreventAll cancels it outright (Fog) -- CR 615.6, a prevented event
-- never happens. CR 615.7's shared N-damage shield and the prevent-the-next-N
-- shape are card-driven, not structure-blocked (#58).
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
  | SetAmount Natural.Natural
  | Scale Scaling.Scaling
  deriving (Eq, Ord, Show)
