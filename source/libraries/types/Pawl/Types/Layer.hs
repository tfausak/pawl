module Pawl.Types.Layer where

-- | CR 613.1: the layers a continuous effect can apply in, ordered by rule number
-- so the DERIVED Ord IS the application order -- the sole thing the projection
-- sorts on. Complete for diffability against CR 613 (the Keyword posture).
-- Producers as of M4.5 P3b: Control (2, P1), Text (3, M3d), Type (4, M3c),
-- Color (5, P3a), Ability (6, M3b), CharacteristicPT (7a), SetPT (7b),
-- ModifyPT (7c) and SwitchPT (7d). Copy (1) has no Modification producer -- a
-- copy is seeded into the fold by Projection.copiableCharacteristics (P2), not
-- applied as a continuous effect. No Enum/Bounded -- nothing enumerates layers
-- or asks for bounds.
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
