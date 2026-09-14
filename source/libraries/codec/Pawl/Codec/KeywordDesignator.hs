module Pawl.Codec.KeywordDesignator where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.KeywordFamily as KeywordFamily
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.KeywordDesignator as KeywordDesignator

-- | Tagged, so `{"type":"OfFamily","value":{"type":"Cycling"}}` names CR 702.29a's
-- family and `{"type":"OfNullary","value":{"type":"Exhaust"}}` names CR 702.177a's
-- keyword. Arm.tagged carries its own fallthrough, so a new arm here ships with no
-- wire format until one is written (#2262).
codec :: Codec.Codec KeywordDesignator.KeywordDesignator
codec =
  Arm.tagged
    [ Arm.payload "OfFamily" KeywordFamily.codec KeywordDesignator.OfFamily (\x -> case x of KeywordDesignator.OfFamily y -> Just y; _ -> Nothing),
      Arm.payload "OfNullary" Keyword.codec KeywordDesignator.OfNullary (\x -> case x of KeywordDesignator.OfNullary y -> Just y; _ -> Nothing)
    ]
