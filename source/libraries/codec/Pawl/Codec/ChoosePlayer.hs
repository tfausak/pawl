{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChoosePlayer where

import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChoosePlayer as ChoosePlayer

-- | A bare object keyed by the record's field names. The scope is REQUIRED
-- rather than defaulted: "choose an opponent" and "choose a player" are
-- different printed sentences, and picking one as the default would let the
-- other be written by omission.
codec :: Codec.Codec ChoosePlayer.ChoosePlayer
codec = Fields.object $ do
  scope <- Fields.required "scope" PlayerScope.codec ChoosePlayer.scope
  slot <- Fields.required "slot" SlotName.codec ChoosePlayer.slot
  pure
    ChoosePlayer.MkChoosePlayer
      { ChoosePlayer.scope = scope,
        ChoosePlayer.slot = slot
      }
