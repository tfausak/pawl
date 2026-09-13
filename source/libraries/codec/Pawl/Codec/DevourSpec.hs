module Pawl.Codec.DevourSpec where

import qualified Pawl.Codec.Devour as Devour
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Devour as Devour
import qualified Pawl.Types.DevourCount as DevourCount
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Devour.Devour Keyword.Keyword)
codec = Devour.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Devour" $ do
  -- CR 702.82a: devour 3 (Thunder-Thrash Elder), whose "creatures" is the rule's
  -- rather than the card's, so no quality is written.
  Spec.it s "MkDevour, no quality" $
    Common.assertCodec
      s
      codec
      (Devour.MkDevour {Devour.quality = Nothing, Devour.count = DevourCount.Fixed 3})
      " {\"count\":{\"type\":\"Fixed\",\"value\":3}} "
  -- CR 702.82c: devour artifact 1 (Caprichrome).
  Spec.it s "MkDevour, a quality" $
    Common.assertCodec
      s
      codec
      (Devour.MkDevour {Devour.quality = Just (Filter.HasCardType CardType.Artifact), Devour.count = DevourCount.Fixed 1})
      " {\"count\":{\"type\":\"Fixed\",\"value\":1},\"quality\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
