{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChoosePlayerAtRandom where

import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChoosePlayerAtRandom as ChoosePlayerAtRandom

-- | A bare object keyed by the record's field names. The scope is REQUIRED
-- rather than defaulted, for Pawl.Codec.ChoosePlayer's reason: "choose an
-- opponent at random" and "choose a player at random" are different printed
-- sentences, and picking one as the default would let the other be written by
-- omission.
codec :: Codec.Codec ChoosePlayerAtRandom.ChoosePlayerAtRandom
codec = Fields.object $ do
  scope <- Fields.required "scope" PlayerScope.codec ChoosePlayerAtRandom.scope
  slot <- Fields.required "slot" SlotName.codec ChoosePlayerAtRandom.slot
  pure
    ChoosePlayerAtRandom.MkChoosePlayerAtRandom
      { ChoosePlayerAtRandom.scope = scope,
        ChoosePlayerAtRandom.slot = slot
      }
