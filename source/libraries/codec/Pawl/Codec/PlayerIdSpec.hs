module Pawl.Codec.PlayerIdSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerId" $ do
  Spec.it s "MkPlayerId" $
    Common.assertCodec
      s
      PlayerId.codec
      (PlayerId.MkPlayerId 1)
      " 1 "

  Spec.it s "has a schema" $
    Common.assertHasSchema s PlayerId.codec

  Spec.it s "rejects a negative id" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " -1 ") >>= Codec.decode PlayerId.codec))
      "expected a decode failure"
