module Pawl.Types.AttackTarget where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | What an attacking creature is attacking (CR 508.1b), which CR 506.3 limits to
-- a player, a planeswalker or a battle. These arms are that enumeration minus the
-- battle, the one card type pawl has no way to print; it grows an OfBattle arm
-- (CR 310.5, #302).
--
-- Ord because Pawl.Types.Combat's `attacked` is a Set of these.
data AttackTarget
  = OfPlayer PlayerId.PlayerId
  | -- | CR 306.6. Named by id and nothing else: CR 506.4c can stop it being
    -- attacked while the creature stays in combat, so the readers of this arm
    -- re-ask whether it is still an attackable planeswalker as they read it
    -- (Pawl.Engine.Combat.stillAttacked).
    OfPlaneswalker ObjectId.ObjectId
  deriving (Eq, Ord, Show)
