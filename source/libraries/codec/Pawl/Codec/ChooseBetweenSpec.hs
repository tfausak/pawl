{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ChooseBetweenSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.ChooseBetween as ChooseBetween
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ChooseBetween as ChooseBetween

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChooseBetween" $ do
  -- CR 700.2's "Choose one or both --" (data/cards/vandalize.json). Both fields
  -- are a Natural, so the fixture uses different numbers on purpose: only an
  -- asymmetric case catches a codec that swapped the bounds.
  Spec.it s "MkChooseBetween, both keys" $
    Common.assertCodec
      s
      ChooseBetween.codec
      (ChooseBetween.MkChooseBetween 1 2)
      """ {"least":1,"most":2} """
  -- The one invariant Pawl.Types.ChooseBetween states and this decoder keeps.
  -- Equal bounds are legal; only a minimum ABOVE the maximum is not.
  Spec.it s "accepts equal bounds" $
    Common.assertCodec
      s
      ChooseBetween.codec
      (ChooseBetween.MkChooseBetween 2 2)
      """ {"least":2,"most":2} """
  Spec.it s "rejects a minimum above the maximum" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"least":2,"most":1} """) >>= Codec.decode ChooseBetween.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s ChooseBetween.codec
