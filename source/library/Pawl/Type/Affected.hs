module Pawl.Type.Affected where

import Data.Set (Set)
import Pawl.Type.Color (Color)
import Pawl.Type.ObjectId (ObjectId)

-- What a continuous effect applies to. CR 611.2c: a resolution effect's set is
-- LOCKED when it begins (TheseObjects -- a bounced-and-returned creature is a new
-- id the effect no longer names). A static ability's set is dynamic (AllCreatures
-- -- any creature currently on the battlefield), which is why static effects are
-- re-derived each projection, never captured once.
data Affected
  = TheseObjects (Set ObjectId)
  | AllCreatures
  | AllLands -- Urborg
  | AllNonbasicLands -- Blood Moon
  | OtherNonAuraEnchantments -- Opalescence ("each other"); self excluded by the effect's source at fold time
  | -- Bad Moon: "Black creatures get +1/+1". A DYNAMIC set, re-derived every
    -- projection like AllCreatures -- and evaluated against the partial
    -- projection, so a CR 613.1e layer-5 colour change is visible to this
    -- layer-7c effect (CR 613: layers apply in order).
    CreaturesOfColor Color
  deriving (Eq, Ord, Show)
