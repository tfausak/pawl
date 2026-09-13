{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerDesignationTally where

import qualified Pawl.Codec.PlayerDesignation as PlayerDesignation
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerDesignationTally as PlayerDesignationTally

-- | A bare object keyed by the record's field names, Pawl.Codec.PlayerCounterTally's
-- shape. The tag that picks it is Pawl.Codec.Quantity's "HasPlayerDesignation".
codec :: Codec.Codec PlayerDesignationTally.PlayerDesignationTally
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec PlayerDesignationTally.player
  designation <- Fields.required "designation" PlayerDesignation.codec PlayerDesignationTally.designation
  pure PlayerDesignationTally.MkPlayerDesignationTally {PlayerDesignationTally.player = player, PlayerDesignationTally.designation = designation}
