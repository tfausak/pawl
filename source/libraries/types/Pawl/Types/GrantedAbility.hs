module Pawl.Types.GrantedAbility where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 613.1f layer 6: one whole quoted ability that a continuous effect hands
-- to another object -- Presence of Gond's "'{T}: Create a 1/1 green Elf Warrior
-- creature token'", Sixth Sense's "'Whenever this creature deals combat damage
-- to a player, you may draw a card'".
--
-- The KIND of ability is CR 113.3's classification, so casing on this
-- constructor is the closed half reading a rulebook category and not the open
-- half's identity -- the same standing Pawl.Types.Keyword has. Which arm it is
-- decides only which ProjectedCharacteristics list the projection appends to;
-- nothing downstream learns the ability came from a grant.
--
-- Parametric in `card` for the reason ActivatedAbility and TriggeredAbility are,
-- and it is what Pawl.Types.Modification's own variable is instantiated at:
-- naming either ability type there would close the module cycle that variable
-- exists to open.
--
-- CR 113.3a's static and CR 113.3d's replacement abilities have no arm. Not
-- implemented: a granted STATIC or REPLACEMENT ability (#1942).
data GrantedAbility card
  = Activated (ActivatedAbility.ActivatedAbility card)
  | Triggered (TriggeredAbility.TriggeredAbility card)
  deriving (Eq, Ord, Show)
