{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Saddling where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Saddling as Saddling

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec Saddling.Saddling
codec = Fields.object $ do
  mount <- Fields.required "mount" ObjectId.codec Saddling.mount
  saddledBy <- Fields.required "saddledBy" (Common.set ObjectId.codec) Saddling.saddledBy
  pure
    Saddling.MkSaddling
      { Saddling.mount = mount,
        Saddling.saddledBy = saddledBy
      }
