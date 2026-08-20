module Pawl.Codec.DestroySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Destroy as Destroy
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Destroy" $ do
  -- CR 701.19c's count is bound into a slot only when a later effect of the
  -- same resolution reads it, so the key is absent here and present below.
  Spec.it s "MkDestroy, no bound slot: the key is omitted" $
    Common.assertCodec
      s
      Destroy.codec
      (Destroy.MkDestroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing Nothing)
      " {\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"regenerability\":{\"type\":\"Regenerable\"}} "
  Spec.it s "MkDestroy, a bound slot: the key is written" $
    Common.assertCodec
      s
      Destroy.codec
      (Destroy.MkDestroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Artifact)) Regenerability.Regenerable (Just (SlotName.MkSlotName (Text.pack "destroyed"))) Nothing)
      " {\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}},\"regenerability\":{\"type\":\"Regenerable\"},\"slot\":\"destroyed\"} "
  -- The other slot, and the other reading of "put into a graveyard this way":
  -- the CARDS the destruction buried rather than how many it destroyed. An
  -- independent key, so a card may write either, both or neither.
  Spec.it s "MkDestroy, a buried slot: the key is written, and independently of the count" $
    Common.assertCodec
      s
      Destroy.codec
      (Destroy.MkDestroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Regenerability.Regenerable Nothing (Just (SlotName.MkSlotName (Text.pack "buried"))))
      " {\"buried\":\"buried\",\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"regenerability\":{\"type\":\"Regenerable\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Destroy.codec
