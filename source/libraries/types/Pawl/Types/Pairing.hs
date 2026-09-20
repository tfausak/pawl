module Pawl.Types.Pairing where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 702.95b: one creature's half of a soulbond pairing -- which creature it is
-- paired with, and under whose control the pairing was made.

-- BOTH HALVES ARE STORED, one on each creature, because CR 702.95b lets an
-- ability ask the question from either side ("a paired creature", "the creature
-- another creature is paired with"), and Pawl.Engine.Filter's View reads the
-- CANDIDATE. Keeping the two rows in step is Pawl.Engine.Soulbond's job: it is
-- the only writer, and its sweep is symmetric, so neither row outlives the other.
--
-- `under` is the PLAYER and not a derived "both have the same controller",
-- because CR 702.95e ends the pairing when "another player gains control of it or
-- the creature it's paired with" -- an effect that takes BOTH at once
-- (Insurrection) leaves the controllers equal and the pairing ended, which only a
-- remembered seat can tell. Pawl.Types.Object's `ringBearerFor` remembers a player
-- for the same rule's sake.
data Pairing = MkPairing
  { partner :: ObjectId.ObjectId,
    under :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
