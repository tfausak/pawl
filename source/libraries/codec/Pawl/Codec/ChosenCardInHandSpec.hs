module Pawl.Codec.ChosenCardInHandSpec where

import qualified Pawl.Codec.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChosenCardInHand" $ do
  -- Elvish Piper's "a creature card from your hand": CR 402.3 makes the one
  -- PlayerRef both the chooser and the hand, and the Filter is what narrows the
  -- offer.
  Spec.it s "MkChosenCardInHand, both keys" $
    Common.assertCodec
      s
      ChosenCardInHand.codec
      ( ChosenCardInHand.MkChosenCardInHand
          { ChosenCardInHand.player = PlayerRef.Relative PlayerRelation.You,
            ChosenCardInHand.filter = Filter.HasCardType CardType.Creature
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChosenCardInHand.codec
