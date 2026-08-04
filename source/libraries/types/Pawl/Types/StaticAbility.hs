module Pawl.Types.StaticAbility where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Modification as Modification

-- | A card's printed static continuous ability (CR 604.1/604.2: a static ability
-- creates a continuous effect active while its permanent is on the battlefield).
-- Gathered live from every battlefield permanent by the projection, with the
-- permanent's own timestamp (CR 613.7a: a static ability's continuous effect has
-- the same timestamp as the object it is on).
--
-- One affected set, MANY modifications, because CR 613.6 makes that the unit the
-- layer system reasons about: an effect applying in several layers keeps applying
-- to the same set of objects in each. Humility is ONE ability whose parts land in
-- layers 6 and 7b, and its set is chosen once; declaring it as two abilities let
-- the projection ask the filter once per layer and get two answers (#233).
--
-- NonEmpty rather than a list validated at the boundary: an ability with no parts
-- does nothing, which no card means, and a malformed `"modifications": []` then
-- fails to decode rather than quietly producing a permanent that under-performs
-- its own text.
--
-- Its order is the card's PRINTED order, not the application order --
-- Projection.layer decides that, per CR 613.1.
data StaticAbility = MkStaticAbility
  { affected :: Affected.Affected,
    modifications :: NonEmpty.NonEmpty Modification.Modification
  }
  deriving (Eq, Ord, Show)
