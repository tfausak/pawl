{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ViewIs where

import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.Codec.Reply as Reply
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ViewIs as ViewIs

codec :: Codec.Codec ViewIs.ViewIs
codec = Fields.object $ do
  view <- Fields.required "of" Arm.keyedEnum ViewIs.view
  player <- Fields.defaulted "player" Nothing (Common.maybe Label.codec) ViewIs.player
  object <- Fields.defaulted "object" Nothing (Common.maybe Reference.codec) ViewIs.object
  zone <- Fields.defaulted "zone" Nothing (Common.maybe Arm.keyedEnum) ViewIs.zone
  is <- Fields.required "is" Reply.codec ViewIs.is
  pure ViewIs.MkViewIs {ViewIs.view = view, ViewIs.player = player, ViewIs.object = object, ViewIs.zone = zone, ViewIs.is = is}
