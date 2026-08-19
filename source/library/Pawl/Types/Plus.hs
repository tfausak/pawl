module Pawl.Types.Plus where

-- | The payload of Pawl.Types.Quantity's Plus arm (#1305): CR 208.2's
-- composition, so a printed 1+* needs no constructor of its own.
--
-- PARAMETRIC in the quantity, and that is what keeps this out of a module cycle
-- rather than a stylistic choice: both halves ARE a Quantity and Quantity names
-- this record, so a concrete field here would need an hs-boot file. The
-- parameter is instantiated at Quantity where the arm is declared, the technique
-- Pawl.Types.Count already uses one level down.
--
-- Addition commutes, so nothing about the RULES distinguishes the two halves.
-- They are still separate fields rather than a list: CR 208.2's printed form is
-- binary, and a list would make the empty and singleton cases expressible with
-- nothing to mean.
data Plus quantity = MkPlus
  { left :: quantity,
    right :: quantity
  }
  deriving (Eq, Ord, Show)
