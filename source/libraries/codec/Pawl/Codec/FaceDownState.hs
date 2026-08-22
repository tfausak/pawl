{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FaceDownState where

import qualified Pawl.Codec.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Codec.FaceDownReason as FaceDownReason
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FaceDownState as FaceDownState

codec :: Codec.Codec FaceDownState.FaceDownState
codec = Fields.object $ do
  reason <- Fields.required "reason" FaceDownReason.codec FaceDownState.reason
  listed <- Fields.required "listed" FaceDownCharacteristics.codec FaceDownState.listed
  pure FaceDownState.MkFaceDownState {FaceDownState.reason = reason, FaceDownState.listed = listed}
