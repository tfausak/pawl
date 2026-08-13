{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MovedBetween where

import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MovedBetween as MovedBetween

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec MovedBetween.MovedBetween
codec = Fields.object $ do
  from <- Fields.required "from" Zone.codec MovedBetween.from
  to <- Fields.required "to" Zone.codec MovedBetween.to
  pure MovedBetween.MkMovedBetween {MovedBetween.from = from, MovedBetween.to = to}
