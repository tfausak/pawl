{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MonarchWatch where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MonarchWatch as MonarchWatch

codec :: Codec.Codec MonarchWatch.MonarchWatch
codec = Fields.object $ do
  controller <- Fields.required "controller" PlayerId.codec MonarchWatch.controller
  due <- Fields.required "due" Common.boolean MonarchWatch.due
  pure
    MonarchWatch.MkMonarchWatch
      { MonarchWatch.controller = controller,
        MonarchWatch.due = due
      }
