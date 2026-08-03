module Pawl.Codec.CardTypeSpec where

import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardType" $ do
  Spec.it s "Artifact" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Artifact
      "{\"type\":\"Artifact\"}"

  Spec.it s "Battle" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Battle
      "{\"type\":\"Battle\"}"

  Spec.it s "Conspiracy" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Conspiracy
      "{\"type\":\"Conspiracy\"}"

  Spec.it s "Creature" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Creature
      "{\"type\":\"Creature\"}"

  Spec.it s "Dungeon" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Dungeon
      "{\"type\":\"Dungeon\"}"

  Spec.it s "Enchantment" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Enchantment
      "{\"type\":\"Enchantment\"}"

  Spec.it s "Instant" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Instant
      "{\"type\":\"Instant\"}"

  Spec.it s "Kindred" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Kindred
      "{\"type\":\"Kindred\"}"

  Spec.it s "Land" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Land
      "{\"type\":\"Land\"}"

  Spec.it s "Phenomenon" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Phenomenon
      "{\"type\":\"Phenomenon\"}"

  Spec.it s "Plane" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Plane
      "{\"type\":\"Plane\"}"

  Spec.it s "Planeswalker" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Planeswalker
      "{\"type\":\"Planeswalker\"}"

  Spec.it s "Scheme" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Scheme
      "{\"type\":\"Scheme\"}"

  Spec.it s "Sorcery" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Sorcery
      "{\"type\":\"Sorcery\"}"

  Spec.it s "Vanguard" $ do
    Common.assertJsonCodec
      s
      CardType.toJson
      CardType.fromJson
      CardType.Vanguard
      "{\"type\":\"Vanguard\"}"
