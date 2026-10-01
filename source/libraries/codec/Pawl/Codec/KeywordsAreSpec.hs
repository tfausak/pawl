module Pawl.Codec.KeywordsAreSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.KeywordsAre as KeywordsAre
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.KeywordsAre as KeywordsAre.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Reference as Reference.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.KeywordsAre" $ do
  Spec.it s "an object, a keyword and a count" $
    Common.assertCodec s KeywordsAre.codec (KeywordsAre.Type.MkKeywordsAre (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "bird"))) Keyword.Type.Flying 1) " {\"object\":\"@bird\",\"keyword\":{\"type\":\"Flying\"},\"count\":1} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s KeywordsAre.codec
