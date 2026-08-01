module Pawl.Codec.AbilityNameSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AbilityName" $ do
  Spec.describe s "fromJson" $ do
    Spec.it s "works" $ do
      Common.assertFromJson s AbilityName.fromJson "\"a\"" (AbilityName.MkAbilityName $ Text.pack "a")

  Spec.describe s "toJson" $ do
    Spec.it s "works" $ do
      Common.assertToJson s AbilityName.toJson (AbilityName.MkAbilityName $ Text.pack "b") "\"b\""
