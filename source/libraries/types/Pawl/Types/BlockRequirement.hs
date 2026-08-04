module Pawl.Types.BlockRequirement where

import qualified Pawl.Types.Affected as Affected

-- | CR 509.1c: one printed BLOCKING REQUIREMENT -- an effect saying a creature
-- must block, or must block if some condition is met. Lure, and Nemesis Mask's
-- identical line on an Equipment.
--
-- The THIRD carrier of a printed static ability, alongside
-- Pawl.Types.StaticAbility and Pawl.Types.PlayerStaticAbility, and neither of
-- them can hold this:
--
--   * StaticAbility is Affected x Modification, and CR 613.1's layers compute an
--     OBJECT's characteristics. A requirement is not a characteristic, so no
--     Modification could express it and Pawl.Engine.Projection would have to
--     carry something it can never apply.
--   * PlayerStaticAbility is PlayerScope x PlayerEffect, CR 611.1's clause for an
--     effect that affects players or the rules of the game. This IS that clause
--     -- CR 613.11's own example is a creature that must attack -- but
--     PlayerScope names PLAYERS, and a blocking requirement names an ATTACKING
--     CREATURE. Widening PlayerScope to objects would make "which players is this
--     about" answer "an object", which every existing reader would then have to
--     defend against.
--
-- So this is its own carrier on its own axis, and it lands where CR 613.11 puts
-- it: after the layer system has run, never inside it. Pawl.Engine.BlockRequirement is
-- the only module that reads it; Pawl.Engine.Projection never sees it.
--
-- ONE requirement shape, which is the one the pool prints: "all creatures able to
-- block X do so". The two axes CR 509.1c implies -- WHICH creatures are required,
-- and WHAT they must block -- are collapsed to the second, because "all
-- creatures" is the only subject any printing here has. A requirement with a
-- narrower subject, and the subjectless "blocks each combat if able" shape, are
-- unrepresentable (#341).
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture both siblings take -- so a Lure leaving the battlefield lifts its
-- requirement with nothing to unwind.
newtype BlockRequirement = MkBlockRequirement
  { -- | Which attacking creature every able creature must block. An Affected, and
    -- not a bare ObjectId, because two printings in the pool already name that
    -- creature two different ways: an Aura's requirement names Affected.Attached
    -- (CR 303.4m) -- Lure -- while a creature's names its own source,
    -- Affected.Matching Filter.IsSource -- Prized Unicorn.
    --
    -- Neither is ever frozen, and CR 611.2c is why a resolution-created
    -- requirement would not be either: a continuous effect that modifies the
    -- rules rather than any object's characteristics can affect objects that were
    -- not affected when it began, and CR 613.11 classifies a requirement as
    -- exactly that. So Affected.TheseObjects is the one arm this field has no use
    -- for.
    attacker :: Affected.Affected
  }
  deriving (Eq, Ord, Show)
