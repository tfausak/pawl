module Pawl.Types.ManaUnit where

import qualified Data.Set as Set
import qualified Pawl.Types.ManaRestriction as ManaRestriction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaRider as ManaRider
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.Subtype as Subtype

-- | One unit of mana in a pool.
--
-- SIX axes, and two of them are facts about how the mana was made: the tags, and
-- the subtype its source had chosen (`sourceChosenSubtype` below).
-- Pawl.Types.ProductionTag is the CLOSED half -- snow-ness, "this activation
-- caused you to lose life" -- observable facts about the production event that
-- the engine determines with no card knowledge.
--
-- Pawl.Types.ManaRetention is a DURATION something must end (CR 514.2, CR
-- 500.5a) rather
-- than such a fact, and it comes from the wording of the effect that added the
-- mana rather than from any property of its source -- which is why it is a field
-- of its own and not a production tag.
--
-- The RESTRICTION is the OPEN half, and conflating it with the tags would be a
-- mistake: Geosurge's "spend this mana only to cast artifact or creature spells"
-- is a predicate over the object being paid for, not a fact about this mana, so
-- payment evaluates it through the one generic matcher
-- (Pawl.Engine.Filter.matches) and never cases on what it says.
--
-- Deliberately no source ObjectId, and `sourceChosenSubtype` below is that rule
-- kept rather than broken: it is a VALUE read off the source at production, which
-- is the only way a CR 106.6 restriction can ask about the thing that made the
-- mana. Snow cares about a PROPERTY of the source, not
-- its identity, and a reference would dangle by construction: mana outlives its
-- source, and CR 400.7 mints a fresh id on every zone change. Properties are
-- stamped at production time, by whichever of the two producers is adding the
-- mana: Pawl.Engine.Mana.manaOptionsOfGiven for a mana ability paid inline (CR
-- 605.3b), and Pawl.Engine.Resolve's Effect.AddMana arm for an ability that
-- resolves off the stack (CR 605.1b). Both read them from the same decider,
-- Pawl.Engine.Mana.productionTagsGiven. CR 106.13's transfer stamps nothing: it
-- moves whole units between pools (Pawl.Engine.Mana.moveMana), which is how that
-- rule's second sentence keeps every field below unchanged.
data ManaUnit = MkManaUnit
  { manaType :: ManaType.ManaType,
    tags :: Set.Set ProductionTag.ProductionTag,
    -- | CR 106.4: whether the player loses this mana as a step or phase ends.
    -- Read off the ADDITION (Pawl.Types.ManaAddition) rather than off the
    -- source, and by BOTH producers -- Pawl.Engine.Mana.manaOptionsOfGiven for
    -- a mana ability paid inline (Synthetic Lasting Spring) and
    -- Pawl.Engine.Resolve's Effect.AddMana arm for one that resolves off the
    -- stack (Shizuko, Caller of Autumn).
    retention :: ManaRetention.ManaRetention,
    -- | CR 106.6: what this mana may be spent on -- Nothing for mana that may
    -- be spent on anything, which is almost every mana. @Just r@ names the
    -- payment kinds it admits and the predicate each of them owes
    -- (Pawl.Types.ManaRestriction), read by
    -- Pawl.Engine.Mana.admitsUnder against the payment's subject.
    --
    -- Stamped off the ADDITION (Pawl.Types.ManaAddition) rather than off the
    -- source, for CR 106.6a's reason: the restriction belongs to the spell or
    -- ability that produced the mana and so applies to every mana it produced.
    -- BOTH producers read it -- Pawl.Engine.Mana.manaOptionsOfGiven for a mana
    -- ability paid inline (Mishra's Workshop) and Pawl.Engine.Resolve's
    -- Effect.AddMana arm for one that resolves off the stack (Geosurge).
    restriction :: Maybe ManaRestriction.ManaRestriction,
    -- | CR 106.6's second shape: what this mana DOES to the spell it is spent
    -- on -- Nothing for mana that does nothing to it, which is almost every
    -- mana. @Just r@ names the objects the clause is about and what happens to
    -- one (Pawl.Types.ManaRider).
    --
    -- Stamped off the ADDITION (Pawl.Types.ManaAddition) exactly as the
    -- restriction beside it is, and for CR 106.6a's same sentence. BOTH
    -- producers stamp it -- Pawl.Engine.Mana.manaOptionsOfGiven for a mana
    -- ability paid inline (Boseiju, Who Shelters All) and Pawl.Engine.Resolve's
    -- Effect.AddMana arm for one that resolves off the stack.
    --
    -- READ off the spent units and not off this pool: CR 400.7d keeps the
    -- payment as Pawl.Types.Object.manaSpent, and Pawl.Engine.ManaRider is the
    -- one reader. Deliberately NOT read by Pawl.Engine.Mana.admitsUnder: a
    -- rider narrows nothing about what the mana may pay for, so folding it in
    -- there would turn an unconditional rider into an unconditional restriction.
    --
    -- Not implemented: CR 106.6's THIRD shape -- a delayed triggered ability
    -- (CR 603.7a) that triggers when the mana is spent, which Pyromancer's
    -- Goggles and Path of Ancestry print. It carries a whole ability rather
    -- than the closed word Pawl.Types.ManaRiderEffect holds, so it is a
    -- different carrier and not a further arm (#2248).
    rider :: Maybe ManaRider.ManaRider,
    -- | CR 607.2d: the subtype this mana's SOURCE had chosen as it entered (CR
    -- 614.1c), baked in here at production so that a CR 106.6 restriction can
    -- read it -- Pillar of Origins' "creature spell of the chosen type". Nothing
    -- for mana from a source that chose none, which is almost every mana.
    --
    -- Read by Pawl.Engine.Mana.admitsUnder, which hands it to
    -- Pawl.Engine.Filter's sourceChosenSubtype for Filter.HasChosenSubtype to
    -- ask. The atom's other position, a static ability's affected set (Obelisk of
    -- Urd), fills that field off the board instead and never reaches this one.
    -- BOTH producers stamp it -- Pawl.Engine.Mana.manaOptionsOfGiven for a
    -- mana ability paid inline (Pillar of Origins) and Pawl.Engine.Resolve's
    -- Effect.AddMana arm for one that resolves off the stack.
    --
    -- BAKED and not looked up, which is what this type's own haddock requires: there
    -- is no source id to look anything up by, the source may have left the
    -- battlefield by the time the mana is spent, and CR 106.6a makes the
    -- restriction the ABILITY's rather than the permanent's.
    --
    -- Not implemented: a restriction reading the source's IDENTITY rather than a
    -- value it chose -- Ice Cauldron's "only to cast the last card exiled with
    -- this artifact", which would want the id this type does not carry (#1978).
    sourceChosenSubtype :: Maybe Subtype.Subtype
  }
  deriving (Eq, Ord, Show)
