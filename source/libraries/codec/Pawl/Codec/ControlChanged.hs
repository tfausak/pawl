{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ControlChanged where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ControlChanged as ControlChanged

-- | A bare object keyed by the record's field names, replacing the positional
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec ControlChanged.ControlChanged
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec ControlChanged.object
  before <- Fields.required "before" PlayerId.codec ControlChanged.before
  after <- Fields.required "after" PlayerId.codec ControlChanged.after
  pure
    ControlChanged.MkControlChanged
      { ControlChanged.object = object,
        ControlChanged.before = before,
        ControlChanged.after = after
      }
