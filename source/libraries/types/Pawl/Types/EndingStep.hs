module Pawl.Types.EndingStep where

data EndingStep
  = EndStep
  | Cleanup
  deriving (Eq, Ord, Show)
