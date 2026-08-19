module Pawl.Codec.KeywordFamily where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.KeywordFamily as KeywordFamily

-- | Nullary tags, unlike Pawl.Codec.Keyword's tagged pairs: the type is
-- payload-free, so `{"type":"Toxic"}` names the family and
-- `{"type":"Toxic","value":2}` the written instance. The two never collide,
-- because they sit under different Filter tags -- HasKeywordFamily and
-- HasKeyword.
codec :: Codec.Codec KeywordFamily.KeywordFamily
codec = Arm.enum
