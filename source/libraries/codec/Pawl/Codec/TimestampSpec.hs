module Pawl.Codec.TimestampSpec where

import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Timestamp" $ do
  Spec.it s "zero" $
    Common.assertCodec s Timestamp.codec (Timestamp.MkTimestamp 0) " 0 "
  Spec.it s "a later stamp" $
    Common.assertCodec s Timestamp.codec (Timestamp.MkTimestamp 7) " 7 "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Timestamp.codec
