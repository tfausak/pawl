module Pawl.Codec.AbilityNameSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AbilityName" $ do
  Spec.it s "MkAbilityName \"a\"" $
    Common.assertJsonCodec
      s
      AbilityName.toJson
      AbilityName.fromJson
      (AbilityName.MkAbilityName $ Text.pack "a")
      "\"a\""
  Spec.it s "MkAbilityName \"b\"" $
    Common.assertJsonCodec
      s
      AbilityName.toJson
      AbilityName.fromJson
      (AbilityName.MkAbilityName $ Text.pack "b")
      "\"b\""
