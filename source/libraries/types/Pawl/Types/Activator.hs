module Pawl.Types.Activator where

-- | CR 602.1b: which players an activated ability's own activation instructions
-- let activate it.
--
-- CR 602.2 states the default and its exception in one sentence -- only the
-- object's controller, or its owner if it has none, "unless the object
-- specifically says otherwise" -- and CR 602.1b is what an object says otherwise
-- WITH: an activation instruction that "may state which players can activate
-- that ability". This type is that instruction, and nothing else.
--
-- A separate axis from Pawl.Types.ActivationRestriction, which is CR 602.5's
-- "activate only ..." rider. The two are printed in the same sentence often
-- enough -- Endbringer's Revel's "Any player may activate this ability but only
-- as a sorcery" -- and they compose rather than merge: the rider narrows WHEN,
-- this widens WHO, and every clause of both must hold.
--
-- The reader is Pawl.Engine.Activate.mayActivateGiven, which is CR 602.2's whole
-- permission conjunct. Nothing here says who PAYS: CR 602.1a gives the cost to
-- the player activating the ability whichever arm this is.
data Activator
  = -- | CR 602.2's default: the object's controller, or its owner if it has none.
    Controller
  | -- | "Any player may activate this ability" (Glittering Lion, Aether Storm).
    AnyPlayer
  deriving (Bounded, Enum, Eq, Ord, Show)
