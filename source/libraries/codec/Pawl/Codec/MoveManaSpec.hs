module Pawl.Codec.MoveManaSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.MoveMana as MoveMana
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MoveMana as MoveMana
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MoveMana" $ do
  -- CR 106.13: the loser and the gainer, spelled the same way -- Drain Power's
  -- "that player loses all unspent mana and you add the mana lost this way".
  Spec.it s "MkMoveMana, both keys" $
    Common.assertCodec
      s
      MoveMana.codec
      ( MoveMana.MkMoveMana
          { MoveMana.from = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            MoveMana.to = PlayerRef.Relative PlayerRelation.You
          }
      )
      " {\"from\":{\"type\":\"InSlot\",\"value\":\"target\"},\"to\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s MoveMana.codec
