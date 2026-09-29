module Pawl.Codec.VillainousChoiceRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.VillainousChoiceRewrite as VillainousChoiceRewrite

codec :: Codec.Codec VillainousChoiceRewrite.VillainousChoiceRewrite
codec = Arm.enum
