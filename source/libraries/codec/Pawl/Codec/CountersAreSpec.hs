module Pawl.Codec.CountersAreSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CountersAre as CountersAre
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind.Type
import qualified Pawl.Types.CountersAre as CountersAre.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Reference as Reference.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CountersAre" $ do
  Spec.it s "the kind is tagged as a placement's is" $
    Common.assertCodec
      s
      CountersAre.codec
      (CountersAre.Type.MkCountersAre (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "jace"))) CounterKind.Type.Loyalty 3)
      " {\"object\":\"$jace\",\"kind\":{\"type\":\"Loyalty\"},\"count\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CountersAre.codec
