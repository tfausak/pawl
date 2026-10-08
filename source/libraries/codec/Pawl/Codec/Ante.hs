{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Ante where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Ante as Ante

-- | A bare object keyed by the record's field names, the slot elided when
-- absent.
codec :: Codec.Codec Ante.Ante
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec Ante.player
  ref <- Fields.required "ref" ObjectRef.codec Ante.ref
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Ante.slot
  pure
    Ante.MkAnte
      { Ante.player = player,
        Ante.ref = ref,
        Ante.slot = slot
      }
