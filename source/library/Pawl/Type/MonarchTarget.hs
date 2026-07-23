module Pawl.Type.MonarchTarget where

-- Which player an Effect.BecomeMonarch names. pawl has no general "which player"
-- spec for effects yet (#120 tracks the targeted case); these two cases are the
-- entire need this phase, so a minimal two-constructor sum is the honest shape.
data MonarchTarget
  = -- "you become the monarch" (Palace Jailer's ETB): the resolving controller.
    TheController
  | -- CR 725.2: "its controller becomes the monarch" (the steal): the controller
    -- of the object bound as the ability's source (the damaging creature).
    ControllerOfSource
  deriving (Eq, Ord, Show)
