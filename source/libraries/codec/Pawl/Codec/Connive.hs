{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Connive where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Connive as Connive

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's Connive arm.
codec :: Codec.Codec Connive.Connive
codec = Fields.object $ do
  quantity <- Fields.required "quantity" Quantity.codec Connive.quantity
  ref <- Fields.required "ref" ObjectRef.codec Connive.ref
  pure
    Connive.MkConnive
      { Connive.quantity = quantity,
        Connive.ref = ref
      }
