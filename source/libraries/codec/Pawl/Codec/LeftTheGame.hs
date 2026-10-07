{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LeftTheGame where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LeftTheGame as LeftTheGame

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PermanentSacrificed is. Both keys are required.
codec :: Codec.Codec LeftTheGame.LeftTheGame
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec LeftTheGame.object
  from <- Fields.required "from" Zone.codec LeftTheGame.from
  pure LeftTheGame.MkLeftTheGame {LeftTheGame.object = object, LeftTheGame.from = from}
