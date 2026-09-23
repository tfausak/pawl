module Pawl.Types.GrantedAbility where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.StaticAbility as StaticAbility
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
-- and it is what Pawl.Types.Modification's own variable is instantiated at.
--
-- THIS is where the ability knot is tied, the way Pawl.Types.Card ties the card
-- one: every arm instantiates its ability variable at this very type, so an
-- ability granted by a continuous effect may itself grant an ability. Every
-- module on the path -- Effect, ModifyTarget, Clause, Mode, Modal,
-- ActivatedAbility, TriggeredAbility, StaticAbility -- stays parametric so that
-- none of them has to import this one, which is the cycle the variable exists
-- to open.
--
-- Not implemented: a granted REPLACEMENT ability, the other half of CR 614.1
-- (#1942).
data GrantedAbility card
  = Activated (ActivatedAbility.ActivatedAbility card (GrantedAbility card))
  | Triggered (TriggeredAbility.TriggeredAbility card (GrantedAbility card))
  | -- | CR 113.3d.
    Static (StaticAbility.StaticAbility (GrantedAbility card))
  deriving (Eq, Ord, Show)
