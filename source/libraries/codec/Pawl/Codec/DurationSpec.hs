{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DurationSpec where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Duration" $ do
  Spec.it s "UntilEndOfTurn" $
    Common.assertJsonCodec
      s
      Duration.toJson
      Duration.fromJson
      Duration.UntilEndOfTurn
      """ {"type":"UntilEndOfTurn"} """
  -- CR 611.2a: for the rest of the game.
  Spec.it s "Indefinite" $
    Common.assertJsonCodec
      s
      Duration.toJson
      Duration.fromJson
      Duration.Indefinite
      """ {"type":"Indefinite"} """
  -- CR 611.2a: until your next turn.
  Spec.it s "UntilYourNextTurn" $
    Common.assertJsonCodec
      s
      Duration.toJson
      Duration.fromJson
      Duration.UntilYourNextTurn
      """ {"type":"UntilYourNextTurn"} """
  -- CR 611.2b, carrying its Condition.
  Spec.it s "ForAsLongAs carries its condition" $
    Common.assertJsonCodec
      s
      Duration.toJson
      Duration.fromJson
      (Duration.ForAsLongAs (Condition.Compares (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0)))
      """ {"type":"ForAsLongAs","value":{"type":"Compares","value":{"measured":{"type":"Literal","value":0},"comparison":{"type":"Exactly"},"threshold":{"type":"Literal","value":0}}}} """
  -- CR 500.5a: until end of combat.
  Spec.it s "UntilEndOfCombat" $
    Common.assertJsonCodec
      s
      Duration.toJson
      Duration.fromJson
      Duration.UntilEndOfCombat
      """ {"type":"UntilEndOfCombat"} """
