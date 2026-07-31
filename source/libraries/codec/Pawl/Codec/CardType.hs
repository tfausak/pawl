-- | The @CardType ⇆ Json@ codec (#481).
module Pawl.Codec.CardType where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.CardType as CardType

cardTypeToJson :: CardType.CardType -> Value
cardTypeToJson c = Json.nullary . Text.pack $ case c of
  CardType.Land -> "Land"
  CardType.Creature -> "Creature"
  CardType.Instant -> "Instant"
  CardType.Enchantment -> "Enchantment"
  CardType.Artifact -> "Artifact"
  CardType.Sorcery -> "Sorcery"
  CardType.Kindred -> "Kindred"
  CardType.Planeswalker -> "Planeswalker"

jsonToCardType :: Value -> Either Text CardType.CardType
jsonToCardType =
  Json.decodeNullary
    (Text.pack "CardType")
    [ (Text.pack "Land", CardType.Land),
      (Text.pack "Creature", CardType.Creature),
      (Text.pack "Instant", CardType.Instant),
      (Text.pack "Enchantment", CardType.Enchantment),
      (Text.pack "Artifact", CardType.Artifact),
      (Text.pack "Sorcery", CardType.Sorcery),
      (Text.pack "Kindred", CardType.Kindred),
      (Text.pack "Planeswalker", CardType.Planeswalker)
    ]
