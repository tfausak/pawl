module Pawl.Codec.OptionalitySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Optionality as Optionality
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Optionality" $ do
  Spec.it s "Mandatory" $
    Common.assertCodec
      s
      Optionality.codec
      Optionality.Mandatory
      " {\"type\":\"Mandatory\"} "
  -- CR 603.5's unmarked "you may": the asker is elided both ways, so every card
  -- written before the asker existed keeps its spelling.
  Spec.it s "Optional, asker elided" $
    Common.assertCodec
      s
      Optionality.codec
      (Optionality.Optional (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"Optional\"} "
  -- Jungle Wayfinder's "each player may", which is what the value is for.
  Spec.it s "Optional, asker written" $
    Common.assertCodec
      s
      Optionality.codec
      (Optionality.Optional PlayerRef.EachPlayer)
      " {\"type\":\"Optional\",\"value\":{\"type\":\"EachPlayer\"}} "
  -- Any other reference round trips too -- the payload is a whole PlayerRef and
  -- not an enum of the two producers.
  Spec.it s "Optional, asker named by a slot" $
    Common.assertCodec
      s
      Optionality.codec
      (Optionality.Optional (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"Optional\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Optionality.codec
