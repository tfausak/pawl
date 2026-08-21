module Pawl.Types.Destroy where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Destroy arm (#1305).
--
-- Three independent back-references, because "destroyed this way" and "put into
-- a graveyard this way" are read three ways and the three answers are different
-- kinds.
--
-- `slot` is where the count of what CR 701.8 actually destroyed is written, for
-- a later effect of the same resolution to read as Quantity.InSlot -- Builder's
-- Bane's "the number of artifacts they controlled that were put into a graveyard
-- this way".
--
-- `buried` is where the CARDS the destruction put into a graveyard are written,
-- for a later effect to NAME -- Come Back Wrong's "if a creature card is put
-- into a graveyard this way, return it to the battlefield". Those are the CR
-- 400.7 incarnations minted in the graveyard, never the permanents destroyed:
-- the permanent is gone by the time the naming clause runs, and CR 400.7 gives
-- the card in the graveyard a fresh id.
--
-- `permanents` is where the PERMANENTS CR 701.8 destroyed are written, under the
-- ids they held while they were on the battlefield, for a later effect to walk
-- one at a time -- Rampage of the Clans' "for each permanent destroyed this way,
-- ITS CONTROLLER creates a 3/3 green Centaur creature token". Neither of the
-- other two answers that question: a count has no controller to read, and the CR
-- 400.7 incarnation `buried` names is a card in a graveyard, which CR 108.4
-- leaves with no controller at all. What a reader of this slot gets is CR
-- 608.2h's last known information, which is what the rule asks for -- the
-- permanent is gone by the time the rider runs.
--
-- Any of the three is absent for a destruction that is not looked back at in
-- that way, which is every destruction in the pool but the three that are.
data Destroy = MkDestroy
  { ref :: ObjectRef.ObjectRef,
    regenerability :: Regenerability.Regenerability,
    slot :: Maybe SlotName.SlotName,
    buried :: Maybe SlotName.SlotName,
    permanents :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
