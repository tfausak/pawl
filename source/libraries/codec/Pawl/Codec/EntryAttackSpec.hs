module Pawl.Codec.EntryAttackSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.EntryAttack as EntryAttack
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EntryAttack as EntryAttack
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryAttack" $ do
  Spec.it s "Chosen" $
    Common.assertCodec s EntryAttack.codec EntryAttack.Chosen " {\"type\":\"Chosen\"} "
  Spec.it s "SameAs" $
    Common.assertCodec
      s
      EntryAttack.codec
      (EntryAttack.SameAs (SlotName.MkSlotName (Text.pack "thatReturnedPermanent")))
      " {\"type\":\"SameAs\",\"value\":\"thatReturnedPermanent\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s EntryAttack.codec
