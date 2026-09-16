module Pawl.Codec.MakeForetoldSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.MakeForetold as MakeForetold
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MakeForetold as MakeForetold
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MakeForetold" $ do
  -- CR 702.143d's first sentence alone (The Foretold Soldier), which is what a
  -- card file that omits the cost gets.
  Spec.it s "the cards, with CR 702.143d's cost absent" $
    Common.assertCodec
      s
      MakeForetold.codec
      MakeForetold.MkMakeForetold
        { MakeForetold.cards = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "foretold")),
          MakeForetold.manaCostReducedBy = Nothing
        }
      " {\"cards\":{\"type\":\"InSlot\",\"value\":\"foretold\"}} "
  -- Ethereal Valkyrie's "its foretell cost is its mana cost reduced by {2}".
  Spec.it s "the granted cost round-trips when it is stated" $
    Common.assertCodec
      s
      MakeForetold.codec
      MakeForetold.MkMakeForetold
        { MakeForetold.cards = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "foretold")),
          MakeForetold.manaCostReducedBy = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])
        }
      " {\"cards\":{\"type\":\"InSlot\",\"value\":\"foretold\"},\"manaCostReducedBy\":[{\"type\":\"Generic\",\"value\":2}]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s MakeForetold.codec
