module Pawl.Types.CreateCopy where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Effect's CreateCopy arm (#1305).
--
-- The quantity is how many copies. Rite of Replication is the only producer
-- above one, and its five enter SIMULTANEOUSLY, which is what CR 614.12's entry
-- loop and CR 616.1g's containment are asked about.
data CreateCopy = MkCreateCopy
  { quantity :: Quantity.Quantity,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)

-- | What a card minting a single copy writes, and the value the codec elides.
defaultQuantity :: Quantity.Quantity
defaultQuantity = Quantity.Literal 1
