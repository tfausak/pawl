module Pawl.Codec.Label where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Label as Label

codec :: Codec.Codec Label.Label
codec = Common.wrapper Common.text Label.MkLabel Label.unwrap
