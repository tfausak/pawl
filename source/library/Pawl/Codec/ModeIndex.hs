module Pawl.Codec.ModeIndex where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ModeIndex as ModeIndex

codec :: Codec.Codec ModeIndex.ModeIndex
codec = Common.wrapper Common.natural ModeIndex.MkModeIndex ModeIndex.unwrap
