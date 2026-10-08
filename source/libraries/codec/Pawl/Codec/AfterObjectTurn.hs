{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AfterObjectTurn where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AfterObjectTurn as AfterObjectTurn

-- | A bare object keyed by the record's field names, like Pawl.Codec.AfterTurn.
codec :: Codec.Codec AfterObjectTurn.AfterObjectTurn
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec AfterObjectTurn.object
  turn <- Fields.required "turn" Common.natural AfterObjectTurn.turn
  pure AfterObjectTurn.MkAfterObjectTurn {AfterObjectTurn.object = object, AfterObjectTurn.turn = turn}
