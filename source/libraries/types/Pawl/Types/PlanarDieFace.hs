module Pawl.Types.PlanarDieFace where

-- | CR 901.3a: what the planar die shows. Four of its six faces are blank.
data PlanarDieFace
  = -- | CR 901.9a: nothing happens.
    Blank
  | -- | CR 901.9b: chaos ensues (CR 311.7).
    Chaos
  | -- | CR 901.9c: the planeswalking ability triggers (CR 901.8).
    Planeswalker
  deriving (Bounded, Enum, Eq, Ord, Show)
