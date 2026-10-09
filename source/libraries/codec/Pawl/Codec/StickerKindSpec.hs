module Pawl.Codec.StickerKindSpec where

import qualified Pawl.Codec.StickerKind as StickerKind
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.StickerKind as StickerKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.StickerKind" $ do
  Spec.it s "Art" $ Common.assertCodec s StickerKind.codec StickerKind.Art " {\"type\":\"Art\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s StickerKind.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s StickerKind.codec
