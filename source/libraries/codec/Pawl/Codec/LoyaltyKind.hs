module Pawl.Codec.LoyaltyKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.LoyaltyKind as LoyaltyKind

-- | Nullary tags, Pawl.Codec.AbilityKind's shape and for its reason: the type is
-- payload-free, so `{"type":"LoyaltyAbility"}` is the whole of it, and Arm.enum
-- derives the arm list from the type rather than leaving a new constructor to
-- compile with no arm.
codec :: Codec.Codec LoyaltyKind.LoyaltyKind
codec = Arm.enum
