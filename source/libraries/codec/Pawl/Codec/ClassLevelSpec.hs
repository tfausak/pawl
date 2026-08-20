module Pawl.Codec.ClassLevelSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.ClassLevel as ClassLevel
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ClassLevel as ClassLevel

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ClassLevel" $ do
  -- CR 716.2b: the level designation, as a class level bar prints its number.
  Spec.it s "MkClassLevel" $
    Common.assertCodec
      s
      ClassLevel.codec
      (ClassLevel.MkClassLevel 2)
      " 2 "

  Spec.it s "has a schema" $
    Common.assertHasSchema s ClassLevel.codec

  -- The wrapped type is Natural, so a negative number is a decode failure rather
  -- than a wrapped negative going through a partial fromInteger.
  Spec.it s "rejects a negative number" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " -1 ") >>= Codec.decode ClassLevel.codec))
      "expected a decode failure"
