module Pawl.Codec.EntryBlockSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.EntryBlock as EntryBlock
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EntryBlock as EntryBlock
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryBlock" $ do
  Spec.it s "Chosen" $
    Common.assertCodec s EntryBlock.codec EntryBlock.Chosen " {\"type\":\"Chosen\"} "
  Spec.it s "Specified" $
    Common.assertCodec
      s
      EntryBlock.codec
      (EntryBlock.Specified (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"Specified\",\"value\":\"target\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s EntryBlock.codec
