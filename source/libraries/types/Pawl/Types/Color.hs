module Pawl.Types.Color where

-- | CR 105.1: there are five colors. Closed and finite -- this enumeration is
-- complete and will never grow.
data Color
  = White
  | Blue
  | Black
  | Red
  | Green
  deriving (Bounded, Enum, Eq, Ord, Show)
