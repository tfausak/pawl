{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TurnFaceDown where

import qualified Pawl.Codec.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown

-- | The listed characteristics are ELIDED when they are CR 708.2a's, so
-- Backslide -- which lists none -- writes only the ref.
codec :: Codec.Codec TurnFaceDown.TurnFaceDown
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec TurnFaceDown.ref
  characteristics <- Fields.defaulted "characteristics" FaceDownCharacteristics.defaultValue FaceDownCharacteristics.codec TurnFaceDown.characteristics
  pure
    TurnFaceDown.MkTurnFaceDown
      { TurnFaceDown.ref = ref,
        TurnFaceDown.characteristics = characteristics
      }
