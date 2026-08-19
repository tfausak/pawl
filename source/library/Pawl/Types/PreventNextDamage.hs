module Pawl.Types.PreventNextDamage where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity

-- | CR 615.7's prevention SHIELD: over whom, of what kind, how much, for how
-- long, and CR 615.5's additional effect riding it.

-- Parametric in the EFFECT rather than in the card, which is what keeps this out
-- of a module cycle: Pawl.Types.Effect holds this record and this record holds
-- effects, so naming Effect here would need an hs-boot file. The parameter is
-- instantiated at `Effect card` where the arm is declared.
data PreventNextDamage effect = MkPreventNextDamage
  { duration :: Duration.Duration,
    -- | PRINTED, not assumed -- Decorated Griffin says "the next 1 COMBAT
    -- damage", where Mending Hands says only "damage". Nothing is a shield
    -- naming no kind, taking combat and noncombat alike, and is elided rather
    -- than written null.
    kind :: Maybe DamageKind.DamageKind,
    ref :: ObjectRef.ObjectRef,
    quantity :: Quantity.Quantity,
    -- | CR 615.5's additional effect -- Test of Faith's "for each 1 damage
    -- prevented this way, put a +1/+1 counter on that creature". Empty for a
    -- shield with no such clause, which is every other prevention in the pool,
    -- so the key is elided rather than written as an empty array.
    riders :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
