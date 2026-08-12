{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CardNameSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardName" $ do
  Spec.it s "MkCardName" $
    Common.assertCodec
      s
      CardName.codec
      (CardName.MkCardName (Text.pack "a"))
      """ "a" """

  Spec.it s "has a schema" $
    Common.assertHasSchema s CardName.codec

  Spec.it s "rejects a non-string" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ 1 """) >>= Codec.decode CardName.codec))
      "expected a decode failure"
