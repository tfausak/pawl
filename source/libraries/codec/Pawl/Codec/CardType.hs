module Pawl.Codec.CardType where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CardType as CardType

toJson :: CardType.CardType -> Value.Value
toJson c = Common.nullary $ case c of
  CardType.Artifact -> "Artifact"
  CardType.Battle -> "Battle"
  CardType.Conspiracy -> "Conspiracy"
  CardType.Creature -> "Creature"
  CardType.Dungeon -> "Dungeon"
  CardType.Enchantment -> "Enchantment"
  CardType.Instant -> "Instant"
  CardType.Kindred -> "Kindred"
  CardType.Land -> "Land"
  CardType.Phenomenon -> "Phenomenon"
  CardType.Plane -> "Plane"
  CardType.Planeswalker -> "Planeswalker"
  CardType.Scheme -> "Scheme"
  CardType.Sorcery -> "Sorcery"
  CardType.Vanguard -> "Vanguard"

fromJson :: Value.Value -> Either Text.Text CardType.CardType
fromJson =
  Common.decodeNullary
    "CardType"
    [ ("Artifact", CardType.Artifact),
      ("Battle", CardType.Battle),
      ("Conspiracy", CardType.Conspiracy),
      ("Creature", CardType.Creature),
      ("Dungeon", CardType.Dungeon),
      ("Enchantment", CardType.Enchantment),
      ("Instant", CardType.Instant),
      ("Kindred", CardType.Kindred),
      ("Land", CardType.Land),
      ("Phenomenon", CardType.Phenomenon),
      ("Plane", CardType.Plane),
      ("Planeswalker", CardType.Planeswalker),
      ("Scheme", CardType.Scheme),
      ("Sorcery", CardType.Sorcery),
      ("Vanguard", CardType.Vanguard)
    ]
