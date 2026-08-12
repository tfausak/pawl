module Pawl.Codec.Defense where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Defense as Defense

codec :: Codec.Codec Defense.Defense
codec = Common.wrapper Common.natural Defense.MkDefense Defense.unwrap
