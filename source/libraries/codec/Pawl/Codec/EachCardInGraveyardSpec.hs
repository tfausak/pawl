{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EachCardInGraveyardSpec where

import qualified Pawl.Codec.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerScope as PlayerScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EachCardInGraveyard" $ do
  -- CR 404.1, Gaea's Blessing's shape.
  Spec.it s "MkEachCardInGraveyard, both keys" $
    Common.assertCodec
      s
      EachCardInGraveyard.codec
      ( EachCardInGraveyard.MkEachCardInGraveyard
          { EachCardInGraveyard.players = PlayerScope.You,
            EachCardInGraveyard.filter = Filter.HasCardType CardType.Creature
          }
      )
      """ {"players":{"type":"You"},"filter":{"type":"HasCardType","value":{"type":"Creature"}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s EachCardInGraveyard.codec
