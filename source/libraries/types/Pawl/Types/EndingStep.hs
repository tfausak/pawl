module Pawl.Types.EndingStep where

data EndingStep
  = EndStep
  | Cleanup
  deriving (Bounded, Enum, Eq, Ord, Show)
