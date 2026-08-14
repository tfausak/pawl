{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ExileCardsFromGraveyardSpec where

import qualified Pawl.Codec.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (ExileCardsFromGraveyard.ExileCardsFromGraveyard Keyword.Keyword)
codec = ExileCardsFromGraveyard.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExileCardsFromGraveyard" $ do
  -- CR 406.2 as a cost: Headless Skaab exiles one creature card from your graveyard.
  Spec.it s "MkExileCardsFromGraveyard" $
    Common.assertCodec
      s
      codec
      ( ExileCardsFromGraveyard.MkExileCardsFromGraveyard
          { ExileCardsFromGraveyard.count = 1,
            ExileCardsFromGraveyard.whichCards = Filter.HasCardType CardType.Creature
          }
      )
      """ {"count":1,"whichCards":{"type":"HasCardType","value":{"type":"Creature"}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
