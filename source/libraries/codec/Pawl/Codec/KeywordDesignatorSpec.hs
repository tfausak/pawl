module Pawl.Codec.KeywordDesignatorSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.KeywordDesignator as KeywordDesignator
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordDesignator as KeywordDesignator
import qualified Pawl.Types.KeywordFamily as KeywordFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.KeywordDesignator" $ do
  let codec = KeywordDesignator.codec Keyword.codec
  -- CR 702.29a: the family, which is what Fluctuator's "cycling abilities you
  -- activate" names.
  Spec.it s "OfFamily" $
    Common.assertCodec
      s
      codec
      (KeywordDesignator.OfFamily KeywordFamily.Cycling)
      " {\"type\":\"OfFamily\",\"value\":{\"type\":\"Cycling\"}} "

  -- CR 702.177a: the keyword itself, which is what Boom Scholar's "exhaust
  -- abilities of other permanents you control" names.
  Spec.it s "OfKeyword" $
    Common.assertCodec
      s
      codec
      (KeywordDesignator.OfKeyword Keyword.Exhaust)
      " {\"type\":\"OfKeyword\",\"value\":{\"type\":\"Exhaust\"}} "

  -- CR 702.33e: Faerie Squadron's "if this spell was kicked".
  Spec.it s "PrintedKicker" $
    Common.assertCodec
      s
      codec
      KeywordDesignator.PrintedKicker
      " {\"type\":\"PrintedKicker\"} "

  -- The arms must stay distinguishable on the wire: a family and a keyword can
  -- share a spelling (`{"type":"Cycling"}` under either tag), so the outer tag
  -- is the whole of the difference.
  Spec.it s "the arms do not collide" $
    Spec.assertBool
      s
      (Codec.encode codec (KeywordDesignator.OfFamily KeywordFamily.Cycling) /= Codec.encode codec (KeywordDesignator.OfKeyword Keyword.Exhaust))
      "a family is not a keyword"

  Spec.it s "has a schema" $
    Common.assertHasSchema s codec
