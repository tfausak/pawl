module Pawl.Type.StaticAbility where

import Pawl.Type.Affected (Affected)
import Pawl.Type.Modification (Modification)

-- A card's printed static continuous ability (CR 604.1/604.2: a static ability
-- creates a continuous effect active while its permanent is on the battlefield).
-- Gathered live from every battlefield permanent by the projection, with the
-- permanent's own timestamp (CR 613.7a: a static ability's continuous effect has
-- the same timestamp as the object it is on).
--
-- One affected set, MANY modifications, because CR 613.6 makes that the unit the
-- layer system reasons about: "If an effect should be applied in different layers
-- and/or sublayers, the parts of the effect each apply in their appropriate ones.
-- If an effect starts to apply in one layer ... it will continue to be applied to
-- the same set of objects in each other applicable layer." Humility's "All
-- creatures lose all abilities and have base power and toughness 1/1" is ONE
-- ability whose parts land in layers 6 and 7b, and the set it applies to is
-- chosen once. Declaring it as two abilities -- which this type used to force --
-- let the projection ask the filter once per layer and get two different answers
-- (#233).
--
-- A list, like the neighbouring Card.activatedAbilities and
-- Card.staticAbilities: it is built once by the codec and only ever walked in
-- order. Its order is the card's PRINTED order, not the application order --
-- Projection.layer decides that, per CR 613.1.
data StaticAbility = MkStaticAbility
  { affected :: Affected,
    modifications :: [Modification]
  }
  deriving (Eq, Ord, Show)
