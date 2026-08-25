module Pawl.Types.CreateCopy where

import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Effect's CreateCopy arm (#1305).
--
-- The quantity is how many copies. Rite of Replication is the only producer
-- above one, and its five enter SIMULTANEOUSLY, which is what CR 614.12's entry
-- loop and CR 616.1g's containment are asked about.
data CreateCopy = MkCreateCopy
  { quantity :: Quantity.Quantity,
    ref :: ObjectRef.ObjectRef,
    -- | CR 110.5b's default is no riders at all, which is every copy token in
    -- data/cards but Littjara Mirrorlake's, so the key is elided rather than
    -- written.
    --
    -- The SAME record Create and MoveToZone carry, rather than a bare counter
    -- map of this opcode's own: CR 122.6 does not care which door an object
    -- arrives by, and Littjara Mirrorlake's "except it enters with an additional
    -- +1/+1 counter on it" is the same sentence Eyes of Gitaxias writes over a
    -- Create. Only `counters` is read here -- Pawl.CardSpec lints that no
    -- CreateCopy in the pool sets any of the others (gap #2302).
    riders :: EntryRiders.EntryRiders Quantity.Quantity
  }
  deriving (Eq, Ord, Show)

-- | What a card minting a single copy writes, and the value the codec elides.
defaultQuantity :: Quantity.Quantity
defaultQuantity = Quantity.Literal 1
