module Pawl.Codec.TypeSwapSpec where

import qualified Pawl.Codec.TypeSwap as TypeSwap
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Subtype as Subtype.Type
import qualified Pawl.Types.TypeSwap as TypeSwap.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TypeSwap" $ do
  Spec.it s "the word replaced and its replacement" $
    Common.assertCodec s TypeSwap.codec (TypeSwap.Type.MkTypeSwap Subtype.Type.Island Subtype.Type.Swamp) " {\"from\":{\"type\":\"Island\"},\"to\":{\"type\":\"Swamp\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s TypeSwap.codec
