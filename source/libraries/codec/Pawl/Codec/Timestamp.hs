module Pawl.Codec.Timestamp where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Timestamp as Timestamp

codec :: Codec.Codec Timestamp.Timestamp
codec = Common.wrapper Common.natural Timestamp.MkTimestamp Timestamp.unwrap
