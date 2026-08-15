module Pawl.Types.DamageR where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite

-- | The payload of Pawl.Types.ReplacementEffect's DamageR arm (#1305): which
-- damage events are intercepted, how each is rewritten, and CR 615.5's
-- additional effect riding the prevention.
--
-- Parametric in the EFFECT rather than in the card, which is what keeps this out
-- of a module cycle: Pawl.Types.Effect's Replace arm holds a
-- Pawl.Types.ReplacementEffect, which holds this, so naming Effect here would
-- need an hs-boot file. Pawl.Types.PreventNextDamage is the same trick for the
-- same reason, and the parameter is instantiated at `Effect card` where
-- Pawl.Types.Face declares the field.
data DamageR effect = MkDamageR
  { matching :: DamagePattern.DamagePattern,
    rewrite :: DamageRewrite.DamageRewrite,
    -- | CR 615.5's additional effect -- Stormwild Capridor's "put a +1/+1
    -- counter on this creature for each 1 damage prevented this way". Empty for
    -- every other replacement in the pool, so the key is elided rather than
    -- written as an empty array.
    --
    -- Here rather than on a floating row's carrier because THIS is the one a
    -- card writes: Pawl.Types.ActiveReplacement.rider carries the shield an
    -- Effect.PreventNextDamage installed, whose targets and CR 109.5 "you" had
    -- to be snapshotted at resolution (see Pawl.Types.PreventionRider). A
    -- permanent's static ability snapshots neither -- its source is on the
    -- battlefield to be asked -- so it needs only the program, and
    -- Pawl.Engine.Replacement.collect supplies the environment live.
    --
    -- Meaningful only beside a preventing `rewrite` (CR 615.5 is about
    -- prevention effects); Pawl.CardSpec's lint is what holds card data to that,
    -- since the type cannot.
    riders :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
