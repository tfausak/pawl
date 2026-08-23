module Pawl.Codec.EventGroupSpec where

import qualified Pawl.Codec.EventGroup as EventGroup
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EventGroup as EventGroup

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EventGroup" $ do
  Spec.it s "the first group" $
    Common.assertCodec s EventGroup.codec EventGroup.first " 0 "
  -- CR 608.2f's groups are minted in order, and the ordering is what makes the
  -- log's "non-decreasing along the log" invariant checkable.
  Spec.it s "the group after it" $
    Common.assertCodec s EventGroup.codec (EventGroup.next EventGroup.first) " 1 "
  Spec.it s "has a schema" $
    Common.assertHasSchema s EventGroup.codec
