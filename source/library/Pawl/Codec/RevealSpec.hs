module Pawl.Codec.RevealSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Reveal as Reveal
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Reveal" $ do
  -- CR 701.20a on its own, which is every printing but Wild Evocation: the slot
  -- key is absent rather than null.
  Spec.it s "MkReveal, slot elided" $
    Common.assertCodec
      s
      Reveal.codec
      ( Reveal.MkReveal
          { Reveal.ref = ObjectRef.RandomCardInHand (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))),
            Reveal.slot = Nothing
          }
      )
      " {\"ref\":{\"type\":\"RandomCardInHand\",\"value\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"}}} "
  -- Wild Evocation's reveal, whose later clauses name what it showed.
  Spec.it s "MkReveal, slot written" $
    Common.assertCodec
      s
      Reveal.codec
      ( Reveal.MkReveal
          { Reveal.ref = ObjectRef.RandomCardInHand (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))),
            Reveal.slot = Just (SlotName.MkSlotName (Text.pack "revealed"))
          }
      )
      " {\"ref\":{\"type\":\"RandomCardInHand\",\"value\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"}},\"slot\":\"revealed\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Reveal.codec
