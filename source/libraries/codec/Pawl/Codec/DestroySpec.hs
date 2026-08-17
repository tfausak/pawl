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
      (Destroy.MkDestroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing)
      " {\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"regenerability\":{\"type\":\"Regenerable\"}} "
  Spec.it s "MkDestroy, a bound slot: the key is written" $
    Common.assertCodec
      s
      Destroy.codec
      (Destroy.MkDestroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Artifact)) Regenerability.Regenerable (Just (SlotName.MkSlotName (Text.pack "destroyed"))))
      " {\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}},\"regenerability\":{\"type\":\"Regenerable\"},\"slot\":\"destroyed\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Destroy.codec
