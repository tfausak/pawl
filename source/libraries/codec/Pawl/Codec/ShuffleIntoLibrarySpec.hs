module Pawl.Codec.ShuffleIntoLibrarySpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Pawl.Codec.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ShuffleIntoLibrary" $ do
  -- Riftsweeper's "its owner shuffles it into their library": the library is
  -- derived from the objects (CR 400.3), so there is no key to write.
  Spec.it s "MkShuffleIntoLibrary, library elided" $
    Common.assertCodec
      s
      ShuffleIntoLibrary.codec
      ( ShuffleIntoLibrary.MkShuffleIntoLibrary
          { ShuffleIntoLibrary.library = Nothing,
            ShuffleIntoLibrary.refs = NonEmpty.singleton (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
          }
      )
      " {\"refs\":[{\"type\":\"InSlot\",\"value\":\"target\"}]} "
  -- CR 701.24c's named library -- Dwell on the Past's "their library", which
  -- is shuffled whether or not any of the named cards arrive.
  Spec.it s "MkShuffleIntoLibrary, library named" $
    Common.assertCodec
      s
      ShuffleIntoLibrary.codec
      ( ShuffleIntoLibrary.MkShuffleIntoLibrary
          { ShuffleIntoLibrary.library = Just (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "player"))),
            ShuffleIntoLibrary.refs = NonEmpty.singleton (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "cards")))
          }
      )
      " {\"library\":{\"type\":\"InSlot\",\"value\":\"player\"},\"refs\":[{\"type\":\"InSlot\",\"value\":\"cards\"}]} "
  -- Timetwister's "their hand and graveyard": two sets in one instruction.
  Spec.it s "MkShuffleIntoLibrary, two refs" $
    Common.assertCodec
      s
      ShuffleIntoLibrary.codec
      ( ShuffleIntoLibrary.MkShuffleIntoLibrary
          { ShuffleIntoLibrary.library = Nothing,
            ShuffleIntoLibrary.refs = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "hand")) NonEmpty.:| [ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "graveyard"))]
          }
      )
      " {\"refs\":[{\"type\":\"InSlot\",\"value\":\"hand\"},{\"type\":\"InSlot\",\"value\":\"graveyard\"}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ShuffleIntoLibrary.codec
