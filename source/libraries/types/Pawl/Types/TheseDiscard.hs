module Pawl.Types.TheseDiscard where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | The discard of a set the card names, so no discarding player chooses: CR
-- 701.9b's two exceptions -- Hymn to Tourach at random, Duress by another
-- player's choice -- and Amnesia's sweep.
data TheseDiscard = MkTheseDiscard
  { cards :: ObjectRef.ObjectRef,
    -- | Where the cards this discard moved are written, CountedDiscard's
    -- `discarded` -- Aether Rift's "if you discard a creature card this way,
    -- return it".
    discarded :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
