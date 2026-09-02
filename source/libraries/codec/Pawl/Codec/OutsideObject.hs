{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.OutsideObject where

import qualified Pawl.Codec.Facing as Facing
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Facing as Facing.Type
import qualified Pawl.Types.OutsideObject as OutsideObject

-- | A bare object keyed by the record's field names, mirroring
-- Pawl.Codec.Amass's shape.
codec :: Codec.Codec OutsideObject.OutsideObject
codec = Fields.object $ do
  owner <- Fields.required "owner" PlayerId.codec OutsideObject.owner
  printing <- Fields.required "printing" PrintingId.codec OutsideObject.printing
  facing <- Fields.defaulted "facing" Facing.Type.FaceUp Facing.codec OutsideObject.facing
  pure
    OutsideObject.MkOutsideObject
      { OutsideObject.owner = owner,
        OutsideObject.printing = printing,
        OutsideObject.facing = facing
      }
