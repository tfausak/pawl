module Pawl.Types.TapState where

data TapState
  = Untapped
  | Tapped
  deriving (Bounded, Enum, Eq, Ord, Show)
