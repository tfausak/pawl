module Pawl.Codec.DefenseSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Defense as Defense
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Defense as Defense

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Defense" $ do
  Spec.it s "MkDefense" $
    Common.assertCodec
      s
      Defense.codec
      (Defense.MkDefense 5)
      " 5 "

  Spec.it s "has a schema" $
    Common.assertHasSchema s Defense.codec

  -- The wrapped type is Natural, so a negative number is a decode failure
  -- rather than a wrapped negative going through a partial fromInteger.
  Spec.it s "rejects a negative number" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " -1 ") >>= Codec.decode Defense.codec))
      "expected a decode failure"
