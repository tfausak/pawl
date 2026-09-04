module Pawl.Codec.CardLeavesGraveyardSpec where

import qualified Pawl.Codec.CardLeavesGraveyard as CardLeavesGraveyard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardLeavesGraveyard as CardLeavesGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardLeavesGraveyard" $ do
  -- Kishla Skimmer's payload: whose graveyard the card left, and whose turn it
  -- has to be.
  Spec.it s "MkCardLeavesGraveyard, both keys" $
    Common.assertCodec
      s
      CardLeavesGraveyard.codec
      ( CardLeavesGraveyard.MkCardLeavesGraveyard
          { CardLeavesGraveyard.filter = Filter.OwnedBy PlayerRelation.You,
            CardLeavesGraveyard.scope = TurnScope.ControllersTurn
          }
      )
      " {\"filter\":{\"type\":\"OwnedBy\",\"value\":{\"type\":\"You\"}},\"scope\":{\"type\":\"ControllersTurn\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CardLeavesGraveyard.codec
