module Pawl.Types.DamagePart where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity

-- | One clause of a damage instruction (Pawl.Types.DealDamage): a recipient
-- description and the amount each of its recipients gets. CR 608.2f makes every
-- clause of one instruction a single action, and the amount sits here because
-- one such sentence may deal different amounts to different recipients -- Char's
-- "4 damage to any target and 2 damage to you", which Pawl.ReplacementSpec's
-- Char case proves is one batch.
data DamagePart = MkDamagePart
  { -- | WHICH RECIPIENTS this clause names -- one description, whose set may
    -- hold objects or players (CR 120.3a).
    ref :: ObjectRef.ObjectRef,
    -- | HOW MUCH, read once per recipient rather than once for the set: Acidic
    -- Soil's "each player equal to the number of lands they control" is a
    -- different number per seat. Still one CR 608.2f batch -- see
    -- Pawl.Engine.Resolve's arm, which reads every recipient's amount off the
    -- same pre-effect state.
    quantity :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
