module Pawl.Codec.ChosenCardInGraveyardSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.ZoneScope as ZoneScope

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
            ChosenCardInGraveyard.players = ZoneScope.Scoped PlayerScope.You,
            ChosenCardInGraveyard.filter = Filter.HasCardType CardType.Creature,
            ChosenCardInGraveyard.count = Quantity.Literal 1
          }
      )
      " {\"chooser\":{\"type\":\"TheController\"},\"players\":{\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- The scope's other arm, which is what Grasping Tentacles' "that player's
  -- graveyard" needs: whose is the slot the spell's own target filled, not a
  -- reading CR 109.5 answers.
  Spec.it s "MkChosenCardInGraveyard, a slot-named scope" $
    Common.assertCodec
      s
      ChosenCardInGraveyard.codec
      ( ChosenCardInGraveyard.MkChosenCardInGraveyard
          { ChosenCardInGraveyard.chooser = Chooser.TheController,
            ChosenCardInGraveyard.players = ZoneScope.InSlot (SlotName.MkSlotName (Text.pack "opponent")),
            ChosenCardInGraveyard.filter = Filter.HasCardType CardType.Artifact,
            ChosenCardInGraveyard.count = Quantity.Literal 1
          }
      )
      " {\"chooser\":{\"type\":\"TheController\"},\"players\":{\"type\":\"InSlot\",\"value\":\"opponent\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
  -- The count Fall of the Thran's "two land cards" states, which is the only
  -- payload in the pool writing this key at all: defaulted, so the printed
  -- singular above encodes without it.
  Spec.it s "MkChosenCardInGraveyard, a count above one" $
    Common.assertCodec
      s
      ChosenCardInGraveyard.codec
      ( ChosenCardInGraveyard.MkChosenCardInGraveyard
          { ChosenCardInGraveyard.chooser = Chooser.EachInScope,
            ChosenCardInGraveyard.players = ZoneScope.Scoped PlayerScope.EachPlayer,
            ChosenCardInGraveyard.filter = Filter.HasCardType CardType.Land,
            ChosenCardInGraveyard.count = Quantity.Literal 2
          }
      )
      " {\"chooser\":{\"type\":\"EachInScope\"},\"players\":{\"type\":\"Scoped\",\"value\":{\"type\":\"EachPlayer\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"count\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChosenCardInGraveyard.codec
