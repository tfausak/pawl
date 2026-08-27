module Pawl.Codec.AbilityKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.AbilityKind as AbilityKind

-- | Nullary tags, Pawl.Codec.KeywordFamily's shape and for its reason: the type
-- is payload-free, so `{"type":"ManaAbility"}` is the whole of it, and Arm.enum
-- derives the arm list from the type rather than leaving a new constructor to
-- compile with no arm.
codec :: Codec.Codec AbilityKind.AbilityKind
codec = Arm.enum
