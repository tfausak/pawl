{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CardTypeSpec where

import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardType" $ do
  Spec.it s "Artifact" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Artifact
      """ {"type":"Artifact"} """

  Spec.it s "Battle" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Battle
      """ {"type":"Battle"} """

  Spec.it s "Conspiracy" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Conspiracy
      """ {"type":"Conspiracy"} """

  Spec.it s "Creature" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Creature
      """ {"type":"Creature"} """

  Spec.it s "Dungeon" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Dungeon
      """ {"type":"Dungeon"} """

  Spec.it s "Enchantment" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Enchantment
      """ {"type":"Enchantment"} """

  Spec.it s "Instant" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Instant
      """ {"type":"Instant"} """

  Spec.it s "Kindred" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Kindred
      """ {"type":"Kindred"} """

  Spec.it s "Land" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Land
      """ {"type":"Land"} """

  Spec.it s "Phenomenon" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Phenomenon
      """ {"type":"Phenomenon"} """

  Spec.it s "Plane" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Plane
      """ {"type":"Plane"} """

  Spec.it s "Planeswalker" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Planeswalker
      """ {"type":"Planeswalker"} """

  Spec.it s "Scheme" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Scheme
      """ {"type":"Scheme"} """

  Spec.it s "Sorcery" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Sorcery
      """ {"type":"Sorcery"} """

  Spec.it s "Vanguard" $ do
    Common.assertCodec
      s
      CardType.codec
      CardType.Vanguard
      """ {"type":"Vanguard"} """

  Spec.it s "has a schema" $
    Common.assertHasSchema s CardType.codec
