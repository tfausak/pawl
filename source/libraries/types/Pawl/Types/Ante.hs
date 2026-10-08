module Pawl.Types.Ante where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName

-- | CR 407.4: the players `player` names each ante the objects `ref` names
-- that they own. An object none of them owns stays where it is.
data Ante = MkAnte
  { -- | CR 407.4: who antes.
    player :: PlayerRef.PlayerRef,
    -- | What is anted.
    ref :: ObjectRef.ObjectRef,
    -- | CR 400.7: where the anted objects are bound for a later instruction.
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
