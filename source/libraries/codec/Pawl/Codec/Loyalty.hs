module Pawl.Codec.Loyalty where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Loyalty as Loyalty

codec :: Codec.Codec Loyalty.Loyalty
codec = Common.wrapper Common.natural Loyalty.MkLoyalty Loyalty.unwrap
