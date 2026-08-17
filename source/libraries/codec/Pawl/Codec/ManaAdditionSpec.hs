module Pawl.Codec.ManaAdditionSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ManaAddition as ManaAddition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaAddition" $ do
  -- CR 109.5's unwritten "you": Llanowar Elves' "Add {G}" names nobody, so the
  -- key is absent from the wire in both directions.
  Spec.it s "MkManaAddition, the unwritten recipient" $
    Common.assertCodec
      s
      ManaAddition.codec
      ( ManaAddition.MkManaAddition
          { ManaAddition.player = PlayerRef.Relative PlayerRelation.You,
            ManaAddition.production = ManaProduction.OfType (ManaType.Colored Color.Green)
          }
      )
      " {\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}} "
  -- Shizuko, Caller of Autumn's "that player adds": the one printing that writes
  -- the key, over the slot CR 603.2b's step event bound.
  Spec.it s "MkManaAddition, a named recipient" $
    Common.assertCodec
      s
      ManaAddition.codec
      ( ManaAddition.MkManaAddition
          { ManaAddition.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer")),
            ManaAddition.production = ManaProduction.OfType (ManaType.Colored Color.Green)
          }
      )
      " {\"player\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaAddition.codec
