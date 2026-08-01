module Pawl.Types.AttackTarget where

import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)

-- What an attacking creature is attacking (CR 508.1b): "the active player
-- announces which player, planeswalker, or battle each of the chosen creatures
-- is attacking". CR 506.3's second sentence is the same list from the other end
-- -- "Only a player, a planeswalker, or a battle can be attacked" -- so these
-- arms are that rule's enumeration, minus the one card type pawl has no way to
-- print.
--
-- Grows: OfBattle (CR 310.5, #302), the last of CR 506.3's three.
--
-- Ord because Pawl.Types.Combat's `attacked` is a Set of these.
data AttackTarget
  = OfPlayer PlayerId
  | -- CR 306.6: "Planeswalkers can be attacked." The planeswalker is named by
    -- id and nothing else: CR 506.4 can stop it being attacked while the
    -- creature stays in combat (CR 506.4c), and the only readers of this arm
    -- re-ask whether it is still an attackable planeswalker at the moment they
    -- read it -- see Pawl.Engine.Combat.stillAttacked.
    OfPlaneswalker ObjectId
  deriving (Eq, Ord, Show)
