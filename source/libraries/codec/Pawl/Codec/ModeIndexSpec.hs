{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModeIndexSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.ModeIndex as ModeIndex
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ModeIndex as ModeIndex

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ModeIndex" $ do
  Spec.it s "MkModeIndex" $
    Common.assertCodec
      s
      ModeIndex.codec
      (ModeIndex.MkModeIndex 2)
      """ 2 """

  Spec.it s "has a schema" $
    Common.assertHasSchema s ModeIndex.codec

  -- The wrapped type is Natural, so a negative number is a decode failure
  -- rather than a wrapped negative going through a partial fromInteger.
  Spec.it s "rejects a negative number" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ -1 """) >>= Codec.decode ModeIndex.codec))
      "expected a decode failure"
