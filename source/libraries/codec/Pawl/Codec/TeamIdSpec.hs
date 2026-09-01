module Pawl.Codec.TeamIdSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.TeamId as TeamId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TeamId as TeamId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TeamId" $ do
  Spec.it s "MkTeamId" $
    Common.assertCodec
      s
      TeamId.codec
      (TeamId.MkTeamId 1)
      " 1 "

  Spec.it s "has a schema" $
    Common.assertHasSchema s TeamId.codec

  Spec.it s "rejects a negative id" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " -1 ") >>= Codec.decode TeamId.codec))
      "expected a decode failure"
