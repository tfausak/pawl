module Pawl.Codec.BlockProducerSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.BlockProducer as BlockProducer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BlockProducer as BlockProducer
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BlockProducer" $ do
  Spec.it s "Declared" $
    Common.assertCodec s BlockProducer.codec BlockProducer.Declared " {\"type\":\"Declared\"} "
  Spec.it s "PutOntoBattlefield" $
    Common.assertCodec s BlockProducer.codec BlockProducer.PutOntoBattlefield " {\"type\":\"PutOntoBattlefield\"} "
  Spec.it s "ByEffect" $
    Common.assertCodec
      s
      BlockProducer.codec
      (BlockProducer.ByEffect (Set.fromList [ObjectId.MkObjectId 3, ObjectId.MkObjectId 4]))
      " {\"type\":\"ByEffect\",\"value\":[3,4]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BlockProducer.codec
