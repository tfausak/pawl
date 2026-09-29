{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MonarchIs where

import qualified Pawl.Codec.Label as Label
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MonarchIs as MonarchIs

-- | @"player": null@ when nobody is the monarch.
codec :: Codec.Codec MonarchIs.MonarchIs
codec = Fields.object $ do
  player <- Fields.required "player" (Common.maybe Label.codec) MonarchIs.player
  pure MonarchIs.MkMonarchIs {MonarchIs.player = player}
