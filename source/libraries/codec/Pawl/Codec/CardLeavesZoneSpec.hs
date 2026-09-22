module Pawl.Codec.CardLeavesZoneSpec where

import qualified Pawl.Codec.CardLeavesZone as CardLeavesZone
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardLeavesZone as CardLeavesZone
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardLeavesZone" $ do
  -- Kishla Skimmer's payload: whose graveyard the card left, and whose turn it
  -- has to be.
  Spec.it s "MkCardLeavesZone, no destination" $
    Common.assertCodec
      s
      CardLeavesZone.codec
      ( CardLeavesZone.MkCardLeavesZone
          { CardLeavesZone.filter = Filter.OwnedBy PlayerRelation.You,
            CardLeavesZone.scope = TurnScope.ControllersTurn,
            CardLeavesZone.from = Zone.Graveyard,
            CardLeavesZone.to = Nothing
          }
      )
      " {\"filter\":{\"type\":\"OwnedBy\",\"value\":{\"type\":\"You\"}},\"scope\":{\"type\":\"ControllersTurn\"},\"from\":{\"type\":\"Graveyard\"}} "
  -- Rakshasa Vizier's: "put into exile from your graveyard".
  Spec.it s "MkCardLeavesZone, with a destination" $
    Common.assertCodec
      s
      CardLeavesZone.codec
      ( CardLeavesZone.MkCardLeavesZone
          { CardLeavesZone.filter = Filter.OwnedBy PlayerRelation.You,
            CardLeavesZone.scope = TurnScope.EachTurn,
            CardLeavesZone.from = Zone.Graveyard,
            CardLeavesZone.to = Just Zone.Exile
          }
      )
      " {\"filter\":{\"type\":\"OwnedBy\",\"value\":{\"type\":\"You\"}},\"scope\":{\"type\":\"EachTurn\"},\"from\":{\"type\":\"Graveyard\"},\"to\":{\"type\":\"Exile\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CardLeavesZone.codec
