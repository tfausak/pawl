module Pawl.Codec.DefendersAreSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.DefendersAre as DefendersAre
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DefendersAre as DefendersAre.Type
import qualified Pawl.Types.Label as Label.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DefendersAre" $ do
  Spec.it s "in order" $
    Common.assertCodec
      s
      DefendersAre.codec
      (DefendersAre.Type.MkDefendersAre (fmap (Label.Type.MkLabel . Text.pack) ["carol", "bob"]))
      " {\"players\":[\"carol\",\"bob\"]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s DefendersAre.codec
