module Pawl.Codec.StickerKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.StickerKind as StickerKind

codec :: Codec.Codec StickerKind.StickerKind
codec = Arm.enum
