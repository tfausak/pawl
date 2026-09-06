{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Earthbend where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Earthbend as Earthbend

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's Earthbend arm.
codec :: Codec.Codec Earthbend.Earthbend
codec = Fields.object $ do
  quantity <- Fields.required "quantity" Quantity.codec Earthbend.quantity
  ref <- Fields.required "ref" ObjectRef.codec Earthbend.ref
  pure
    Earthbend.MkEarthbend
      { Earthbend.quantity = quantity,
        Earthbend.ref = ref
      }
