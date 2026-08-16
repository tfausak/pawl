{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DiscardCardsSpec where

import qualified Pawl.Codec.DiscardCards as DiscardCards
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (DiscardCards.DiscardCards Keyword.Keyword)
codec = DiscardCards.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DiscardCards" $ do
  -- CR 601.2f as a cost that names a quality: Magmatic Insight discards one
  -- land card.
  Spec.it s "MkDiscardCards, a filter that names a quality" $
    Common.assertCodec
      s
      codec
      ( DiscardCards.MkDiscardCards
          { DiscardCards.count = 1,
            DiscardCards.whichCards = Filter.HasCardType CardType.Land
          }
      )
      """ {"count":1,"whichCards":{"type":"HasCardType","value":{"type":"Land"}}} """
  -- Cathartic Reunion's "discard two cards", which names none.
  Spec.it s "MkDiscardCards, a filter that admits everything" $
    Common.assertCodec
      s
      codec
      ( DiscardCards.MkDiscardCards
          { DiscardCards.count = 2,
            DiscardCards.whichCards = Filter.And []
          }
      )
      """ {"count":2,"whichCards":{"type":"And","value":[]}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
