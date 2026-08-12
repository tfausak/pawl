module Pawl.Codec.CardType where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CardType as CardType

codec :: Codec.Codec CardType.CardType
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Artifact" CardType.Artifact,
      Arm.nullary "Battle" CardType.Battle,
      Arm.nullary "Conspiracy" CardType.Conspiracy,
      Arm.nullary "Creature" CardType.Creature,
      Arm.nullary "Dungeon" CardType.Dungeon,
      Arm.nullary "Enchantment" CardType.Enchantment,
      Arm.nullary "Instant" CardType.Instant,
      Arm.nullary "Kindred" CardType.Kindred,
      Arm.nullary "Land" CardType.Land,
      Arm.nullary "Phenomenon" CardType.Phenomenon,
      Arm.nullary "Plane" CardType.Plane,
      Arm.nullary "Planeswalker" CardType.Planeswalker,
      Arm.nullary "Scheme" CardType.Scheme,
      Arm.nullary "Sorcery" CardType.Sorcery,
      Arm.nullary "Vanguard" CardType.Vanguard
    ]
  where
    encode c = Common.nullary $ case c of
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
