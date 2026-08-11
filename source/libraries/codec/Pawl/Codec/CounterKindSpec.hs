{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CounterKindSpec where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterKind" $ do
  Spec.it s "PlusOnePlusOne" $
    Common.assertCodec
      s
      (CounterKind.codec Keyword.codec)
      CounterKind.PlusOnePlusOne
      """ {"type":"PlusOnePlusOne"} """
  Spec.it s "MinusOneMinusOne" $
    Common.assertCodec
      s
      (CounterKind.codec Keyword.codec)
      CounterKind.MinusOneMinusOne
      """ {"type":"MinusOneMinusOne"} """
  -- CR 122.1b's keyword counter carries the keyword it grants.
  Spec.it s "Keyword carries its keyword" $
    Common.assertCodec
      s
      (CounterKind.codec Keyword.codec)
      (CounterKind.Keyword Keyword.Flying)
      """ {"type":"Keyword","value":{"type":"Flying"}} """
  -- CR 122.1e, the first kind that modifies no characteristic.
  Spec.it s "Loyalty" $
    Common.assertCodec
      s
      (CounterKind.codec Keyword.codec)
      CounterKind.Loyalty
      """ {"type":"Loyalty"} """
  -- CR 714.3, the kind rule 122.1 never lists.
  Spec.it s "Lore" $
    Common.assertCodec
      s
      (CounterKind.codec Keyword.codec)
      CounterKind.Lore
      """ {"type":"Lore"} """
  Spec.it s "Defense" $
    Common.assertCodec
      s
      (CounterKind.codec Keyword.codec)
      CounterKind.Defense
      """ {"type":"Defense"} """
  Spec.it s "Time" $
    Common.assertCodec
      s
      (CounterKind.codec Keyword.codec)
      CounterKind.Time
      """ {"type":"Time"} """
  -- CR 122.1c, the kind whose count is how many events its pair may still
  -- replace.
  Spec.it s "Shield" $
    Common.assertCodec
      s
      (CounterKind.codec Keyword.codec)
      CounterKind.Shield
      """ {"type":"Shield"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s (CounterKind.codec Keyword.codec)
