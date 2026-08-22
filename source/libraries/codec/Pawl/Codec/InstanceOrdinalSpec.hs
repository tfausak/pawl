module Pawl.Codec.InstanceOrdinalSpec where

import qualified Pawl.Codec.InstanceOrdinal as InstanceOrdinal
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.InstanceOrdinal as InstanceOrdinal

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.InstanceOrdinal" $ do
  Spec.it s "the first instance" $
    Common.assertCodec s InstanceOrdinal.codec (InstanceOrdinal.MkInstanceOrdinal 0) " 0 "
  -- CR 702.136b's second instance of riot, which is the whole reason the ordinal
  -- exists.
  Spec.it s "the second instance" $
    Common.assertCodec s InstanceOrdinal.codec (InstanceOrdinal.MkInstanceOrdinal 1) " 1 "
  Spec.it s "has a schema" $
    Common.assertHasSchema s InstanceOrdinal.codec
