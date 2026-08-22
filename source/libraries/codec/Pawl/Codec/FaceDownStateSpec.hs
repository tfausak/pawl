module Pawl.Codec.FaceDownStateSpec where

import qualified Pawl.Codec.FaceDownState as FaceDownState
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.FaceDownState as FaceDownState

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.FaceDownState" $ do
  -- Morph lists nothing, so CR 708.2a's defaults write no keys and the reason is
  -- the whole of the payload.
  Spec.it s "a morphed object lists nothing" $
    Common.assertCodec
      s
      FaceDownState.codec
      FaceDownState.MkFaceDownState
        { FaceDownState.reason = FaceDownReason.Morphed,
          FaceDownState.listed = FaceDownCharacteristics.defaultValue
        }
      " {\"reason\":{\"type\":\"Morphed\"},\"listed\":{}} "
  -- Disguise lists ward {2} (CR 702.168b), so the two fields differ and neither
  -- can stand in for the other.
  Spec.it s "a disguised object carries the listing its allower made" $
    Common.assertCodec
      s
      FaceDownState.codec
      FaceDownState.MkFaceDownState
        { FaceDownState.reason = FaceDownReason.Disguised,
          FaceDownState.listed = FaceDownCharacteristics.disguisedValue
        }
      " {\"reason\":{\"type\":\"Disguised\"},\"listed\":{\"keywords\":[{\"type\":\"Ward\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}}]}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s FaceDownState.codec
