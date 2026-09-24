{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FaceDownState where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Codec.FaceDownReason as FaceDownReason
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FaceDownState as FaceDownState

codec :: (Typeable.Typeable ability, Eq ability) => Codec.Codec ability -> Codec.Codec (FaceDownState.FaceDownState ability)
codec abilityCodec = Fields.object $ do
  reason <- Fields.required "reason" FaceDownReason.codec FaceDownState.reason
  listed <- Fields.required "listed" (FaceDownCharacteristics.codec abilityCodec) FaceDownState.listed
  pure FaceDownState.MkFaceDownState {FaceDownState.reason = reason, FaceDownState.listed = listed}
