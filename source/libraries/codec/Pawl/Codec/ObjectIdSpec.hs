module Pawl.Codec.ObjectIdSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ObjectId" $ do
  Spec.it s "MkObjectId" $
    Common.assertCodec
      s
      ObjectId.codec
      (ObjectId.MkObjectId 7)
      " 7 "

  Spec.it s "has a schema" $
    Common.assertHasSchema s ObjectId.codec

  -- The wrapped type is Natural, so a negative number is a decode failure
  -- rather than a wrapped negative going through a partial fromInteger.
  Spec.it s "rejects a negative number" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " -1 ") >>= Codec.decode ObjectId.codec))
      "expected a decode failure"
