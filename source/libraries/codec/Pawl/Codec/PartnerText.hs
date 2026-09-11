module Pawl.Codec.PartnerText where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PartnerText as PartnerText

-- | Nullary tags, Pawl.Codec.AbilityKind's shape: Arm.enum derives the arm
-- list from the type.
codec :: Codec.Codec PartnerText.PartnerText
codec = Arm.enum
