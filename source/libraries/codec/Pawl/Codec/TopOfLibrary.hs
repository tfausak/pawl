{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TopOfLibrary where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec TopOfLibrary.TopOfLibrary
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec TopOfLibrary.player
  count <- Fields.required "count" Common.natural TopOfLibrary.count
  pure TopOfLibrary.MkTopOfLibrary {TopOfLibrary.player = player, TopOfLibrary.count = count}
