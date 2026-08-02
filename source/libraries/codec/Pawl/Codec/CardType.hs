module Pawl.Codec.CardType where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.CardType as CardType

toJson :: CardType.CardType -> Value.Value
toJson c = Common.nullary $ case c of
  CardType.Land -> "Land"
  CardType.Creature -> "Creature"
  CardType.Instant -> "Instant"
  CardType.Enchantment -> "Enchantment"
  CardType.Artifact -> "Artifact"
  CardType.Sorcery -> "Sorcery"
  CardType.Kindred -> "Kindred"
  CardType.Planeswalker -> "Planeswalker"

fromJson :: Value.Value -> Either Text.Text CardType.CardType
fromJson =
  Common.decodeNullary
    "CardType"
    [ ("Land", CardType.Land),
      ("Creature", CardType.Creature),
      ("Instant", CardType.Instant),
      ("Enchantment", CardType.Enchantment),
      ("Artifact", CardType.Artifact),
      ("Sorcery", CardType.Sorcery),
      ("Kindred", CardType.Kindred),
      ("Planeswalker", CardType.Planeswalker)
    ]
