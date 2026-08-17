module Pawl.Codec.PlayerQuantitySpec where

import qualified Pawl.Codec.PlayerQuantity as PlayerQuantity
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerQuantity" $ do
  -- Both keys are required: this payload has no optional part, which is what
  -- makes it shareable at all. An arm needing anything else spins out its own
  -- record rather than adding a nullable key here -- see Pawl.Types.Mill.
  Spec.it s "MkPlayerQuantity, both keys written" $
    Common.assertCodec
      s
      PlayerQuantity.codec
      (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerQuantity.codec
