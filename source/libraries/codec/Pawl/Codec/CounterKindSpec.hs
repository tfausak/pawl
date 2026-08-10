{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CounterKindSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterKind" $ do
  Spec.it s "PlusOnePlusOne" $
    Common.assertJsonCodec
      s
      (CounterKind.toJson Keyword.toJson)
      (CounterKind.fromJson Keyword.fromJson)
      CounterKind.PlusOnePlusOne
      """ {"type":"PlusOnePlusOne"} """
  Spec.it s "MinusOneMinusOne" $
    Common.assertJsonCodec
      s
      (CounterKind.toJson Keyword.toJson)
      (CounterKind.fromJson Keyword.fromJson)
      CounterKind.MinusOneMinusOne
      """ {"type":"MinusOneMinusOne"} """
  -- CR 122.1b's keyword counter carries the keyword it grants.
  Spec.it s "Keyword carries its keyword" $
    Common.assertJsonCodec
      s
      (CounterKind.toJson Keyword.toJson)
      (CounterKind.fromJson Keyword.fromJson)
      (CounterKind.Keyword Keyword.Flying)
      """ {"type":"Keyword","value":{"type":"Flying"}} """
  -- CR 122.1e, the first kind that modifies no characteristic.
  Spec.it s "Loyalty" $
    Common.assertJsonCodec
      s
      (CounterKind.toJson Keyword.toJson)
      (CounterKind.fromJson Keyword.fromJson)
      CounterKind.Loyalty
      """ {"type":"Loyalty"} """
  -- CR 714.3, the kind rule 122.1 never lists.
  Spec.it s "Lore" $
    Common.assertJsonCodec
      s
      (CounterKind.toJson Keyword.toJson)
      (CounterKind.fromJson Keyword.fromJson)
      CounterKind.Lore
      """ {"type":"Lore"} """
  Spec.it s "Defense" $
    Common.assertJsonCodec
      s
      (CounterKind.toJson Keyword.toJson)
      (CounterKind.fromJson Keyword.fromJson)
      CounterKind.Defense
      """ {"type":"Defense"} """
