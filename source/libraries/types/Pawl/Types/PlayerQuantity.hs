module Pawl.Types.PlayerQuantity where

import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | "These players, this many" -- the payload of every Pawl.Types.Effect arm
-- whose whole instruction is a PlayerRef and a Quantity: Draw, Scry, Surveil,
-- Fateseal, LoseLife, GainLife and IncreaseSpeed.
--
-- SHARED FOR EXPEDIENCY, and not because those seven mean the same thing. The
-- name says the shape rather than a concept precisely because there is no shared
-- concept: drawing and losing life coincide in what they need to be told, and
-- nothing more. A domain noun here would claim an equality that does not hold.
--
-- The invariant that keeps the sharing honest, and the reason it is written
-- here rather than left to judgement:
--
-- WHEN ONE SHARER NEEDS A FIELD THE OTHERS DO NOT, SPIN OUT A SEPARATE TYPE FOR
-- IT. Never bolt an optional field onto this record for one arm's sake.
--
-- A record carrying a Maybe for exactly one of its users has quietly become the
-- untagged union #1304 removed: the field's absence would once again be how a
-- reader tells which arm it is looking at. Pawl.Types.Mill is what spinning out
-- looks like -- a mill is "these players, this many" plus CR 728.1's tally, so
-- it has its own record rather than a nullable field here.
data PlayerQuantity = MkPlayerQuantity
  { player :: PlayerRef.PlayerRef,
    quantity :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
