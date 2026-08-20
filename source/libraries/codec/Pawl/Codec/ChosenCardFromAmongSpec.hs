module Pawl.Codec.ChosenCardFromAmongSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChosenCardFromAmong" $ do
  -- Commune with the Gods' "a creature or enchantment card from among them": the
  -- slot names the group an earlier clause bound, and the Filter narrows what may
  -- be picked out of it.
  Spec.it s "MkChosenCardFromAmong, both keys" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "revealed"),
            ChosenCardFromAmong.filter = Filter.HasCardType CardType.Creature
          }
      )
      " {\"slot\":\"revealed\",\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChosenCardFromAmong.codec
