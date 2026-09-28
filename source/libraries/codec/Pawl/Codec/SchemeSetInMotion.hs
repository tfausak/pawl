{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SchemeSetInMotion where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SchemeSetInMotion as SchemeSetInMotion

codec :: Codec.Codec SchemeSetInMotion.SchemeSetInMotion
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec SchemeSetInMotion.player
  scheme <- Fields.required "scheme" ObjectId.codec SchemeSetInMotion.scheme
  pure SchemeSetInMotion.MkSchemeSetInMotion {SchemeSetInMotion.player = player, SchemeSetInMotion.scheme = scheme}
