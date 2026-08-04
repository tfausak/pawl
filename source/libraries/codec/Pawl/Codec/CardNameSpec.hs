{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CardNameSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardName" $ do
  Spec.it s "round trips through JSON" $
    Common.assertJsonCodec
      s
      CardName.toJson
      CardName.fromJson
      (CardName.MkCardName $ Text.pack "a")
      """ "a" """
