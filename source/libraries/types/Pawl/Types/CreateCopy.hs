module Pawl.Types.CreateCopy where

import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's CreateCopy arm (#1305).
--
-- The quantity is how many copies. Rite of Replication is the only producer
-- above one, and its five enter SIMULTANEOUSLY, which is what CR 614.12's entry
-- loop and CR 616.1g's containment are asked about.
data CreateCopy ability = MkCreateCopy
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
    -- Create. `counters`, `tapped` and `attacking` are read here; Pawl.EffectLintSpec
    -- lints that no CreateCopy in the pool sets any of the others.
    riders :: EntryRiders.EntryRiders Quantity.Quantity,
    -- | CR 603.7c: the slot the minted token is bound under, Create.slot's
    -- shape -- Flamerush Rider's "Exile the token at end of combat".
    slot :: Maybe SlotName.SlotName,
    -- | CR 707.9's "except ..." clause, empty for a copy effect that states none.
    -- The SAME list EntryRewrite.AsCopy and Pawl.Types.BecomeCopy carry, applied
    -- by the same fold (Pawl.Engine.Replacement.applyCopyExceptions) into the
    -- snapshot the token is minted from, which is CR 707.9b's requirement that
    -- the excepted value join the copiable values -- Multiversal Recruitment's
    -- "except it isn't legendary".
    --
    -- NOT the same clause as `riders` above, though both reach the token: an
    -- exception modifies the copiable values (CR 707.2), while an entry rider is
    -- CR 122.6's counters written onto the object. Littjara Mirrorlake's "except
    -- it enters with an additional +1/+1 counter on it" prints as an exception
    -- and is a rider, which is CR 707.9e's distinction rather than a spelling
    -- choice.
    exceptions :: [CopyException.CopyException ability]
  }
  deriving (Eq, Ord, Show)

-- | What a card minting a single copy writes, and the value the codec elides.
defaultQuantity :: Quantity.Quantity
defaultQuantity = Quantity.Literal 1
