module Pawl.Codec.AbilityName where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.AbilityName as AbilityName

codec :: Codec.Codec AbilityName.AbilityName
codec = Common.wrapper Common.text AbilityName.MkAbilityName AbilityName.unwrap
