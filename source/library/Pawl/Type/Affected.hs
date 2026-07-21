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
    -- projection like AllCreatures -- and evaluated against the PARTIAL
    -- projection accumulated so far, so it sees colour as of whichever layers
    -- have already applied (CR 613: layers apply in order). For a layer-7c
    -- modification (Bad Moon) that partial projection already includes layer 5,
    -- so a CR 613.1e colour change is visible to it -- but this is the general
    -- rule, not something specific to layer 7c: pairing this affected set with a
    -- modification BELOW layer 5 (a layer-2/3/4 op) is the untested case that
    -- Projection.baseColorsOf's devoid-seed comment names as the shortcut's
    -- named expiry.
    CreaturesOfColor Color
  deriving (Eq, Ord, Show)
