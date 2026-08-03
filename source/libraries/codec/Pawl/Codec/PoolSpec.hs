module Pawl.Codec.PoolSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Pool as Pool
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Pool as Pool

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Pool" $ do
  Spec.it s "Creatures" $
    Common.assertJsonCodec
      s
      Pool.toJson
      Pool.fromJson
      Pool.Creatures
      "{\"type\":\"Creatures\"}"
  Spec.it s "Players" $
    Common.assertJsonCodec
      s
      Pool.toJson
      Pool.fromJson
      Pool.Players
      "{\"type\":\"Players\"}"
  Spec.it s "AnyTarget" $
    Common.assertJsonCodec
      s
      Pool.toJson
      Pool.fromJson
      Pool.AnyTarget
      "{\"type\":\"AnyTarget\"}"
  Spec.it s "Permanents" $
    Common.assertJsonCodec
      s
      Pool.toJson
      Pool.fromJson
      Pool.Permanents
      "{\"type\":\"Permanents\"}"
  Spec.it s "Spells" $
    Common.assertJsonCodec
      s
      Pool.toJson
      Pool.fromJson
      Pool.Spells
      "{\"type\":\"Spells\"}"
  -- CR 113.9: activated and triggered abilities on the stack -- Stifle's pool,
  -- disjoint from Spells (Pawl.Types.Pool's header).
  Spec.it s "Abilities" $
    Common.assertJsonCodec
      s
      Pool.toJson
      Pool.fromJson
      Pool.Abilities
      "{\"type\":\"Abilities\"}"
  Spec.it s "SpellsAndPermanents" $
    Common.assertJsonCodec
      s
      Pool.toJson
      Pool.fromJson
      Pool.SpellsAndPermanents
      "{\"type\":\"SpellsAndPermanents\"}"
  -- CR 404.1: the cards in a graveyard, tagged with WHOSE -- Raise Dead's "target
  -- creature card in your graveyard".
  Spec.it s "CardsInGraveyard" $
    Common.assertJsonCodec
      s
      Pool.toJson
      Pool.fromJson
      (Pool.CardsInGraveyard PlayerScope.You)
      "{\"type\":\"CardsInGraveyard\",\"value\":{\"type\":\"You\"}}"
  -- CR 406.1: the cards in the exile zone -- Riftsweeper's "choose target
  -- face-up exiled card". Nullary: exile has no per-player copy (Pawl.Types.Pool's
  -- header).
  Spec.it s "CardsInExile" $
    Common.assertJsonCodec
      s
      Pool.toJson
      Pool.fromJson
      Pool.CardsInExile
      "{\"type\":\"CardsInExile\"}"
