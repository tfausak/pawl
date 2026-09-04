module Pawl.Codec.TriggerSourceSpec where

import qualified Pawl.Codec.TriggerSource as TriggerSource
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.TriggerSource as TriggerSource

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TriggerSource" $ do
  -- CR 113.7: the object whose ability triggered.
  Spec.it s "OfObject" $
    Common.assertCodec
      s
      TriggerSource.codec
      (TriggerSource.OfObject (ObjectId.MkObjectId 4))
      " {\"type\":\"OfObject\",\"value\":4} "
  -- CR 725.2 / CR 702.179d: no object to name.
  Spec.it s "Sourceless" $
    Common.assertCodec
      s
      TriggerSource.codec
      TriggerSource.Sourceless
      " {\"type\":\"Sourceless\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TriggerSource.codec
