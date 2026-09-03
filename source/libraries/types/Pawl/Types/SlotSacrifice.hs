module Pawl.Types.SlotSacrifice where

import qualified Pawl.Types.Sacrificer as Sacrificer
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Sacrifice arm (CR 701.21): the permanent
-- a slot names, and which player is instructed to sacrifice it.
--
-- Distinct from Pawl.Types.Sacrifice, which is the same keyword action as a COST
-- -- that one counts matching permanents its payer chooses, where this one names
-- one already-bound permanent and nobody chooses anything.
data SlotSacrifice = MkSlotSacrifice
  { slot :: SlotName.SlotName,
    -- | CR 701.21a: whom the printed sentence addresses. EffectController is
    -- every card in the pool, so the codec elides it.
    sacrificer :: Sacrificer.Sacrificer
  }
  deriving (Eq, Ord, Show)
