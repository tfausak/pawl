module Pawl.Types.SacrificeEffect where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Sacrificer as Sacrificer

-- | The payload of Pawl.Types.Effect's Sacrifice arm (CR 701.21): which
-- permanents are sacrificed, and which player is instructed to sacrifice each.
--
-- Distinct from Pawl.Types.Sacrifice, which is the same keyword action as a COST
-- -- that one counts matching permanents its payer chooses, where this one names
-- the permanents outright and nobody chooses anything.
data SacrificeEffect = MkSacrificeEffect
  { -- | ObjectRef and not a bare SlotName so a filtered sweep of the battlefield
    -- reaches CR 701.21a -- Golgothian Sylex's "each nontoken permanent with a
    -- name originally printed in the Antiquities expansion". InSlot is the arm
    -- every other card in the pool writes.
    ref :: ObjectRef.ObjectRef,
    -- | CR 701.21a: whom the printed sentence addresses.
    sacrificer :: Sacrificer.Sacrificer
  }
  deriving (Eq, Ord, Show)
