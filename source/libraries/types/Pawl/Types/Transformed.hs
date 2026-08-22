module Pawl.Types.Transformed where

import Data.Set (Set)
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 701.27a: a permanent turned over -- which permanent, and what it turned
-- into.

-- The names are the object's PROJECTED names (CR 201.1 / 707.2), sampled the
-- instant the turn finished, which is what CR 701.27e's "has the specified
-- characteristic immediately after it does so" asks for. A set for
-- Pawl.Engine.Projection.namesOf's reasons: CR 709 gives a Room several and CR
-- 708.2a gives a face-down permanent none.
--
-- Carried on the event rather than re-derived when a trigger is matched, for the
-- reason Pawl.Types.HalfUnlocked carries its flag: the CR 117.5 scan runs after
-- the board has moved on, and a permanent that turned twice in one resolution
-- would answer about the second turn on both events.
data Transformed = MkTransformed
  { object :: ObjectId.ObjectId,
    names :: Set CardName.CardName
  }
  deriving (Eq, Ord, Show)
