{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PoolSpec where

import qualified Pawl.Codec.Pool as Pool
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Pool as Pool

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Pool" $ do
  Spec.it s "Creatures" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Creatures
      """ {"type":"Creatures"} """
  Spec.it s "Players" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Players
      """ {"type":"Players"} """
  Spec.it s "AnyTarget" $
    Common.assertCodec
      s
      Pool.codec
      Pool.AnyTarget
      """ {"type":"AnyTarget"} """
  Spec.it s "Permanents" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Permanents
      """ {"type":"Permanents"} """
  Spec.it s "Spells" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Spells
      """ {"type":"Spells"} """
  -- CR 113.9: activated and triggered abilities on the stack, disjoint from
  -- Spells.
  Spec.it s "Abilities" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Abilities
      """ {"type":"Abilities"} """
  Spec.it s "SpellsAndPermanents" $
    Common.assertCodec
      s
      Pool.codec
      Pool.SpellsAndPermanents
      """ {"type":"SpellsAndPermanents"} """
  -- CR 404.1: the cards in a graveyard, tagged with WHOSE.
  Spec.it s "CardsInGraveyard" $
    Common.assertCodec
      s
      Pool.codec
      (Pool.CardsInGraveyard (GraveyardScope.Scoped PlayerScope.You))
      """ {"type":"CardsInGraveyard","value":{"type":"Scoped","value":{"type":"You"}}} """
  -- CR 406.1: the cards in the exile zone. Nullary, since exile has no
  -- per-player copy.
  Spec.it s "CardsInExile" $
    Common.assertCodec
      s
      Pool.codec
      Pool.CardsInExile
      """ {"type":"CardsInExile"} """
  -- CR 115.1a and CR 404.1 in one slot, so the graveyard half's scope is what the
  -- payload carries.
  Spec.it s "CreaturesAndCardsInGraveyard" $
    Common.assertCodec
      s
      Pool.codec
      (Pool.CreaturesAndCardsInGraveyard (GraveyardScope.Scoped PlayerScope.EachPlayer))
      """ {"type":"CreaturesAndCardsInGraveyard","value":{"type":"Scoped","value":{"type":"EachPlayer"}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s Pool.codec
