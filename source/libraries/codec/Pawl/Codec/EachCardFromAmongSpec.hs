module Pawl.Codec.EachCardFromAmongSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EachCardFromAmong" $ do
  -- Mulch's "all land cards revealed this way": the slot names the group an
  -- earlier clause bound, and the Filter says which of its members are taken --
  -- every one of them, where ChosenCardFromAmong's identical pair says which one
  -- may be picked.
  Spec.it s "MkEachCardFromAmong, both keys" $
    Common.assertCodec
      s
      EachCardFromAmong.codec
      ( EachCardFromAmong.MkEachCardFromAmong
          { EachCardFromAmong.slot = SlotName.MkSlotName (Text.pack "revealed"),
            EachCardFromAmong.filter = Filter.HasCardType CardType.Land
          }
      )
      " {\"slot\":\"revealed\",\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s EachCardFromAmong.codec
