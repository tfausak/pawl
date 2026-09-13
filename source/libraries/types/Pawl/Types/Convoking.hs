module Pawl.Types.Convoking where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 702.51c's "convoked" relation for one cast: the spell, and the creatures
-- tapped to pay for mana in its total cost that way.
--
-- Pawl.Types.Crewing's shape, whose two fields are the same two questions asked
-- of rule 702.122c's relation, and a record of its own for that type's reason:
-- the ends are named rather than positional, and one end here is a SPELL where
-- that one's is a Vehicle.
data Convoking = MkConvoking
  { spell :: ObjectId.ObjectId,
    -- | CR 702.51a: the creatures tapped rather than paying mana as that spell's
    -- total cost was paid. Never empty -- Pawl.Engine.Cast records nothing where
    -- the payer substituted nothing.
    convokedBy :: Set.Set ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
