module Pawl.Codec.PlusSpec where

import qualified Pawl.Codec.Plus as Plus
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Quantity as Quantity

-- | Instantiated at 'Quantity.Quantity', the only concrete instantiation
-- anywhere: 'Pawl.Codec.Quantity' passes its own recursive codec in.
codec :: Codec.Codec (Plus.Plus Quantity.Quantity)
codec = Plus.codec Quantity.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Plus" $ do
  -- CR 208.2: a printed 1+*. Asymmetric on purpose -- both halves are a Quantity, so a swapped codec would round-trip an equal pair.
  Spec.it s "MkPlus" $
    Common.assertCodec
      s
      codec
      ( Plus.MkPlus
          { Plus.left = Quantity.Literal 1,
            Plus.right = Quantity.Star
          }
      )
      " {\"left\":{\"type\":\"Literal\",\"value\":1},\"right\":{\"type\":\"Star\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
