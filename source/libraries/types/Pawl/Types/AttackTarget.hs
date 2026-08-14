module Pawl.Types.AttackTarget where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | What an attacking creature is attacking (CR 508.1b), which CR 506.3 limits to
-- a player, a planeswalker or a battle. These arms are that enumeration, and the
-- rulebook's own: nothing here asks which EFFECT an object carries.
--
-- Ord because Pawl.Types.Combat's `attacked` is a Set of these.
data AttackTarget
  = OfPlayer PlayerId.PlayerId
  | -- | CR 306.6. Named by id and nothing else: CR 506.4c can stop it being
    -- attacked while the creature stays in combat, so the readers of this arm
    -- re-ask whether it is still an attackable planeswalker as they read it
    -- (Pawl.Engine.Combat.stillAttacked).
    OfPlaneswalker ObjectId.ObjectId
  | -- | CR 310.5. Named by id for OfPlaneswalker's reason, and the id is the
    -- battle's rather than its protector's: CR 310.9f lets the designation move
    -- while the attack stands, so who is being attacked THROUGH is re-read from
    -- the battle at every use (Pawl.Engine.Defender.playerOf, CR 310.9d).
    --
    -- Unlike OfPlaneswalker, this arm does NOT imply the target belongs to the
    -- defending player: CR 310.9b makes a battle attackable by anyone for whom
    -- its protector is a defending player, so a Siege's own controller can attack
    -- it (CR 310.12a puts the protector among the controller's opponents).
    OfBattle ObjectId.ObjectId
  deriving (Eq, Ord, Show)
