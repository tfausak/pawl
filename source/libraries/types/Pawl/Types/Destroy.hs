module Pawl.Types.Destroy where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Destroy arm (#1305).
--
-- Two independent back-references, because the printed phrase "put into a
-- graveyard this way" is read two ways and the two answers are different kinds.
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
-- Either is absent for a destruction that is not looked back at, which is every
-- destruction in the pool but the two that are.
data Destroy = MkDestroy
  { ref :: ObjectRef.ObjectRef,
    regenerability :: Regenerability.Regenerability,
    slot :: Maybe SlotName.SlotName,
    buried :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
