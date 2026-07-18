module Pawl.Type.Layer where

-- CR 613.1: the layers a continuous effect can apply in, ordered by rule number
-- so the DERIVED Ord IS the application order -- the sole thing the projection
-- sorts on. Complete for diffability against CR 613 (the Keyword posture); only
-- Ability (6), SetPT (7b), and ModifyPT (7c) have producers at M3b. No
-- Enum/Bounded -- nothing enumerates layers or asks for bounds.
data Layer
  = Copy -- 613.1a, layer 1
  | Control -- 613.1b, layer 2
  | Text -- 613.1c, layer 3
  | Type -- 613.1d, layer 4
  | Color -- 613.1e, layer 5
  | Ability -- 613.1f, layer 6
  | CharacteristicPT -- 613.1g / 613.3, layer 7a (characteristic-defining)
  | SetPT -- layer 7b
  | ModifyPT -- layer 7c
  | SwitchPT -- layer 7d
  deriving (Eq, Ord, Show)
