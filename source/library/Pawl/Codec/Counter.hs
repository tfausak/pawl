{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Counter where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Counter as Counter

-- | Destroy's shape, for Destroy's reason: the bound slot is ELIDED when
-- absent, so a card that says nothing about counting its sweep writes only the
-- @ref@ key. What was a bare slot string is now that key's @InSlot@ (#1507).
codec :: Codec.Codec Counter.Counter
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec Counter.ref
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Counter.slot
  pure
    Counter.MkCounter
      { Counter.ref = ref,
        Counter.slot = slot
      }
