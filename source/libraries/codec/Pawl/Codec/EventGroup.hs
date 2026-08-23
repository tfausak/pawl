module Pawl.Codec.EventGroup where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EventGroup as EventGroup

codec :: Codec.Codec EventGroup.EventGroup
codec = Common.wrapper Common.natural EventGroup.MkEventGroup EventGroup.unwrap
