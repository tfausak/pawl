module Pawl.Codec.EachCardInGraveyardSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EachCardInGraveyard" $ do
  -- CR 404.1, Gaea's Blessing's shape.
  Spec.it s "MkEachCardInGraveyard, both keys" $
    Common.assertCodec
      s
      EachCardInGraveyard.codec
      ( EachCardInGraveyard.MkEachCardInGraveyard
          { EachCardInGraveyard.graveyards = GraveyardScope.Scoped PlayerScope.You,
            EachCardInGraveyard.filter = Filter.HasCardType CardType.Creature
          }
      )
      " {\"graveyards\":{\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- The arm the PlayerScope this field used to hold could not say: Angel of
  -- Finality's "target player's graveyard" (#1310).
  Spec.it s "MkEachCardInGraveyard, a slot-named graveyard" $
    Common.assertCodec
      s
      EachCardInGraveyard.codec
      ( EachCardInGraveyard.MkEachCardInGraveyard
          { EachCardInGraveyard.graveyards = GraveyardScope.InSlot (SlotName.MkSlotName (Text.pack "player")),
            EachCardInGraveyard.filter = Filter.HasCardType CardType.Creature
          }
      )
      " {\"graveyards\":{\"type\":\"InSlot\",\"value\":\"player\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s EachCardInGraveyard.codec
