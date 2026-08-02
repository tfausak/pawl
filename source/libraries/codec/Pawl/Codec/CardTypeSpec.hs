module Pawl.Codec.CardTypeSpec where

import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardType" $ do
  Spec.it s "Land" $
    Common.assertJsonCodec s CardType.toJson CardType.fromJson CardType.Land "{\"type\":\"Land\"}"
  Spec.it s "Creature" $
    Common.assertJsonCodec s CardType.toJson CardType.fromJson CardType.Creature "{\"type\":\"Creature\"}"
  Spec.it s "Instant" $
    Common.assertJsonCodec s CardType.toJson CardType.fromJson CardType.Instant "{\"type\":\"Instant\"}"
  Spec.it s "Enchantment" $
    Common.assertJsonCodec s CardType.toJson CardType.fromJson CardType.Enchantment "{\"type\":\"Enchantment\"}"
  Spec.it s "Artifact" $
    Common.assertJsonCodec s CardType.toJson CardType.fromJson CardType.Artifact "{\"type\":\"Artifact\"}"
  Spec.it s "Sorcery" $
    Common.assertJsonCodec s CardType.toJson CardType.fromJson CardType.Sorcery "{\"type\":\"Sorcery\"}"
  Spec.it s "Kindred" $
    Common.assertJsonCodec s CardType.toJson CardType.fromJson CardType.Kindred "{\"type\":\"Kindred\"}"
  Spec.it s "Planeswalker" $
    Common.assertJsonCodec s CardType.toJson CardType.fromJson CardType.Planeswalker "{\"type\":\"Planeswalker\"}"
