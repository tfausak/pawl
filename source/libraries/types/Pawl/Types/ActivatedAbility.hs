module Pawl.Types.ActivatedAbility where

import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal

-- | CR 602.1 / 700.2 / 602.2b: "[cost]: [effect]", modal-capable. VALUE-typed:
-- Action.Activate carries the value and validates by membership
-- (Projection.abilitiesOf), never an index. Parametric in `card` because a
-- concrete Modal Card would cycle with Card, which ties the knot at Modal Card.
--
-- An activation cost is a Pawl.Types.Cost, the same type a spell's cost takes
-- (CR 118.1).
data ActivatedAbility card = MkActivatedAbility
  { cost :: Cost.Cost Keyword.Keyword,
    modal :: Modal.Modal card,
    -- | CR 602.5: every clause of the "activate only ..." rider the ability
    -- prints, ALL of which must hold. Empty for an ability without one, which is
    -- all of them but equip and a handful of printed windows.
    restrictions :: [ActivationRestriction.ActivationRestriction],
    -- | The "as long as" clause of a static ability that GRANTS this one, or
    -- Nothing for an ability the object simply has, which is nearly all of them.
    --
    -- CR 702.178a is one producer: "Max speed — [Ability]" means "as long as your
    -- speed is 4, this object has '[Ability]'". A printed clause is the other, and
    -- Villainous Ogre's "as long as you control a Demon, this creature has '{B}:
    -- Regenerate this creature'" is the pool's -- unlike max speed's, its clause
    -- reads the BOARD, so the reader it is judged through has to be the one at the
    -- caller's own layer depth (Pawl.Engine.Projection.abilitiesFromCharacteristics).
    -- A grant whose grantee is the granting object itself and whose granted
    -- ability is printed right there is indistinguishable from the printed ability
    -- plus its own gate, so pawl carries the gate on the ability rather than
    -- minting a layer-6 grant.
    -- Pawl.Types.Modification's GainAbility is the layer-6 grant, and
    -- it is for the OTHER case: an ability handed to a DIFFERENT object, which
    -- is not indistinguishable from anything -- CR 113.7 moves the source and
    -- CR 303.4e the player who may activate it.
    --
    -- The SAME shape as Pawl.Types.StaticAbility.condition, which spells CR
    -- 604.2's "as long as" for a continuous effect, and the same rule about what
    -- it is not: not a Pawl.Types.Duration, because CR 611.2c's "for as long as"
    -- ends a stored effect once, while this is re-asked on every read
    -- (Pawl.Engine.Projection.abilitiesGiven) and so takes the ability away and
    -- gives it back as the board moves, with no resolution in between.
    --
    -- Read AFTER the layer fold rather than at the projection seed: the seed can
    -- describe no object, so its view determines nothing, and CR 604.2's own gate -- the object being
    -- on the battlefield with the ability -- is the fold's job, which this only
    -- narrows.
    --
    -- CR 702.178b's zone clause is why abilitiesGiven is not the only reader: "if
    -- an ability granted by a max speed ability states which zones it functions
    -- from, the max speed ability that grants that ability functions from those
    -- zones". Pawl.Engine.Activate.zoneAbilitiesOf asks the same gate of a
    -- card in a GRAVEYARD, for an ability whose cost or effect names that zone
    -- (CR 113.6m).
    condition :: Maybe Condition.Condition,
    -- | The name another clause of the SAME card uses to refer to this ability,
    -- or Nothing for an ability nothing refers to, which is nearly all of them.
    --
    -- The one producer is CR 613.1f's named removal: a Licid's "this creature
    -- loses this ability" is a layer-6 removal of the very ability that created
    -- it, and Modification.LoseNamedAbility carries the name back.
    --
    -- A MAYBE field rather than a second Modification arm reading a positional
    -- index, and rather than a name every ability must invent. This module's own
    -- header rules out the index -- Action.Activate carries the ability by VALUE
    -- and validates by membership, so no index into a face's list is a reference
    -- the rules could follow -- and a required name would put a made-up string on
    -- every ability in data/cards/ to serve the handful that are referred to.
    -- Pawl.Types.Search.count takes the same fork one type over: absent means "no
    -- such clause was printed", which is a different fact from any name.
    --
    -- NOT an identity: two abilities may share a name, and a removal naming it
    -- removes both. Nothing in the rules gives an ability an identity distinct
    -- from its text, and CR 613.1f removes abilities rather than one instance of
    -- one, so a name is a REFERENCE the card's own author writes and pawl's card
    -- lint checks, never a key the engine assumes unique.
    name :: Maybe AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
