module Pawl.Codec.Decider where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Decider as Decider

codec :: Codec.Codec Decider.Decider
codec = Common.wrapper PlayerId.codec Decider.MkDecider Decider.unwrap
