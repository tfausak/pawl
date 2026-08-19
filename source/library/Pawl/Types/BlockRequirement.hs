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
-- BOTH axes CR 509.1c implies -- WHICH creatures are required, and WHAT they
-- must block -- each optional, Nothing being "unrestricted on that axis". Lure
-- and Nemesis Mask print the object alone ("all creatures able to block
-- enchanted creature do so"); Razorgrass Screen prints the subject alone ("this
-- creature blocks each combat if able"), naming no attacker at all.
--
-- Pawl.Types.ActiveBlockRequirement is the sibling that carries both axes as
-- bare ObjectIds, and it is still not a widening of this one: it is the
-- RESOLUTION-created carrier (CR 702.39a's provoke), where the creature is named
-- by targeting and so is one object rather than an affected set. Reshaping this
-- carrier to ObjectIds would freeze a set CR 611.2c keeps dynamic; reshaping
-- that one to Affecteds would fabricate a set where targeting chose one object.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture both siblings take -- so a Lure leaving the battlefield lifts its
-- requirement with nothing to unwind.
data BlockRequirement = MkBlockRequirement
  { -- | CR 509.1c's SUBJECT axis: which creatures are required to block. Nothing
    -- is "all creatures able to", which is what Lure, Nemesis Mask, Alluring
    -- Scent and Prized Unicorn print. Just names a narrower set -- Razorgrass
    -- Screen's requirement is on ITSELF, Affected.Matching Filter.IsSource.
    subject :: Maybe Affected.Affected,
    -- | CR 509.1c's OBJECT axis: which attacking creature they must block.
    -- Nothing is "each combat", naming no attacker: the requirement instantiates
    -- one pair per attacker the subject may legally block, and CR 509.1a is what
    -- makes obeying ONE of them the maximum, since a creature blocks one
    -- attacker unless something says otherwise.
    --
    -- An Affected, and not a bare ObjectId, because two printings in the pool
    -- already name that creature two different ways: an Aura's requirement names
    -- Affected.Attached (CR 303.4m) -- Lure -- while a creature's names its own
    -- source, Affected.Matching Filter.IsSource -- Prized Unicorn.
    --
    -- Neither axis is ever frozen, and CR 611.2c is why a resolution-created
    -- requirement would not be either: a continuous effect that modifies the
    -- rules rather than any object's characteristics can affect objects that were
    -- not affected when it began, and CR 613.11 classifies a requirement as
    -- exactly that. So Affected.TheseObjects is the one arm these fields have no
    -- use for.
    attacker :: Maybe Affected.Affected
  }
  deriving (Eq, Ord, Show)
