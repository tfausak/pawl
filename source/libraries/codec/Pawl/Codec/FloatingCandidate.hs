{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FloatingCandidate where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FloatingCandidate as FloatingCandidate

codec :: Codec.Codec FloatingCandidate.FloatingCandidate
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec FloatingCandidate.source
  timestamp <- Fields.required "timestamp" Timestamp.codec FloatingCandidate.timestamp
  pure FloatingCandidate.MkFloatingCandidate {FloatingCandidate.source = source, FloatingCandidate.timestamp = timestamp}
