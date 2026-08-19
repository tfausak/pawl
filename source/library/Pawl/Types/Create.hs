module Pawl.Types.Create where

import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | CR 111.1's token creation: how many, of what card, entering how, and under
-- what name the rest of the instruction list may read them.
--
-- Parametric in @card@ for 'Pawl.Types.Effect.Effect'\'s reason: the token's
-- card is card DATA nested inside card data, and the parameter is what keeps
-- 'Pawl.Types.Effect' from naming a concrete card type.
data Create card = MkCreate
  { quantity :: Quantity.Quantity,
    card :: card,
    -- | CR 110.5b's default is no riders at all, which is most tokens, so the
    -- key is elided rather than written.
    riders :: EntryRiders.EntryRiders,
    -- | The slot the created tokens are bound to, when a later effect in the
    -- same list reads them. Absent when nothing does.
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
