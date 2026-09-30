{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TurnedFaceUp where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TurnedFaceUp as TurnedFaceUp

-- | Pawl.Codec.Transformed's shape: a bare object keyed by the record's field
-- names. Runtime-only, GameEvent serialising transcripts rather than card data.
codec :: Codec.Codec TurnedFaceUp.TurnedFaceUp
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec TurnedFaceUp.object
  announcedX <- Fields.defaulted "announcedX" Nothing (Common.maybe Common.natural) TurnedFaceUp.announcedX
  pure
    TurnedFaceUp.MkTurnedFaceUp
      { TurnedFaceUp.object = object,
        TurnedFaceUp.announcedX = announcedX
      }
