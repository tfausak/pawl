module Pawl.Codec.PlayerDrawsNthCardSpec where

import qualified Pawl.Codec.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerDrawsNthCard" $ do
  -- Underworld Dreams' shape read from the other side: WHICH draw of the turn
  -- fires the ability, not how many cards are drawn.
  Spec.it s "MkPlayerDrawsNthCard, both keys" $
    Common.assertCodec
      s
      PlayerDrawsNthCard.codec
      ( PlayerDrawsNthCard.MkPlayerDrawsNthCard
          { PlayerDrawsNthCard.player = PlayerRelation.You,
            PlayerDrawsNthCard.nth = 2
          }
      )
      " {\"player\":{\"type\":\"You\"},\"nth\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerDrawsNthCard.codec
