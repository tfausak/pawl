module Pawl.Codec.ClauseIndexSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.ClauseIndex as ClauseIndex
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ClauseIndex as ClauseIndex

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ClauseIndex" $ do
  Spec.it s "MkClauseIndex" $
    Common.assertCodec
      s
      ClauseIndex.codec
      (ClauseIndex.MkClauseIndex 2)
      " 2 "

  Spec.it s "has a schema" $
    Common.assertHasSchema s ClauseIndex.codec

  -- The wrapped type is Natural, so a negative number is a decode failure
  -- rather than a wrapped negative going through a partial fromInteger.
  Spec.it s "rejects a negative number" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " -1 ") >>= Codec.decode ClauseIndex.codec))
      "expected a decode failure"
