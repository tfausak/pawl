{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ObjectSnapshot where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ObjectSnapshot as ObjectSnapshot

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec ObjectSnapshot.ObjectSnapshot
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec ObjectSnapshot.object
  characteristics <- Fields.required "characteristics" ProjectedCharacteristics.codec ObjectSnapshot.characteristics
  controller <- Fields.required "controller" (Common.maybe PlayerId.codec) ObjectSnapshot.controller
  owner <- Fields.required "owner" PlayerId.codec ObjectSnapshot.owner
  pure
    ObjectSnapshot.MkObjectSnapshot
      { ObjectSnapshot.object = object,
        ObjectSnapshot.characteristics = characteristics,
        ObjectSnapshot.controller = controller,
        ObjectSnapshot.owner = owner
      }
