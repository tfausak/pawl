module Pawl.Codec.ClassLevel where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ClassLevel as ClassLevel

codec :: Codec.Codec ClassLevel.ClassLevel
codec = Common.wrapper Common.natural ClassLevel.MkClassLevel ClassLevel.unwrap
