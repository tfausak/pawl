{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Transformed where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Transformed as Transformed

-- | Pawl.Codec.HalfUnlocked's shape: a bare object keyed by the record's field
-- names. Runtime-only, GameEvent serialising transcripts rather than card data.
codec :: Codec.Codec Transformed.Transformed
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec Transformed.object
  names <- Fields.required "names" (Common.set CardName.codec) Transformed.names
  pure
    Transformed.MkTransformed
      { Transformed.object = object,
        Transformed.names = names
      }
