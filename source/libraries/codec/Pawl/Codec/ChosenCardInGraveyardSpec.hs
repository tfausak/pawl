{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ChosenCardInGraveyardSpec where

import qualified Pawl.Codec.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerScope as PlayerScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChosenCardInGraveyard" $ do
  -- CR 404.1 with a choice. The chooser and the graveyards' owners are
  -- separate questions -- "target opponent chooses a card in YOUR graveyard"
  -- names two seats -- which is the shape a positional payload could state
  -- backwards.
  Spec.it s "MkChosenCardInGraveyard, every key" $
    Common.assertCodec
      s
      ChosenCardInGraveyard.codec
      ( ChosenCardInGraveyard.MkChosenCardInGraveyard
          { ChosenCardInGraveyard.chooser = Chooser.TheController,
            ChosenCardInGraveyard.players = PlayerScope.You,
            ChosenCardInGraveyard.filter = Filter.HasCardType CardType.Creature
          }
      )
      """ {"chooser":{"type":"TheController"},"players":{"type":"You"},"filter":{"type":"HasCardType","value":{"type":"Creature"}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s ChosenCardInGraveyard.codec
