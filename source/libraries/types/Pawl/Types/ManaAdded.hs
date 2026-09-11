module Pawl.Types.ManaAdded where

import qualified Data.Set as Set
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 605.1b's "mana being added to a player's mana pool": which player added
-- it, whose ability made them, and which types of mana they added. The payload
-- of Pawl.Types.GameEvent's arm of the same name.
--
-- One per PLAYER per activation: CR 106.4 fills the pool each addition names,
-- and "causes you to add one or more mana" (Caged Sun) asks once of each
-- player's share however much of it there was. A SET of types for
-- Pawl.Types.TappedForMana's reason: the rule asks which types were added.
data ManaAdded = MkManaAdded
  { player :: PlayerId.PlayerId,
    source :: ObjectId.ObjectId,
    mana :: Set.Set ManaType.ManaType
  }
  deriving (Eq, Ord, Show)
