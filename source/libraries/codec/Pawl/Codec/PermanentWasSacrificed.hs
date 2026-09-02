{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentWasSacrificed where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentWasSacrificed as PermanentWasSacrificed

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec PermanentWasSacrificed.PermanentWasSacrificed
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec PermanentWasSacrificed.player
  permanent <- Fields.required "permanent" ObjectId.codec PermanentWasSacrificed.permanent
  pure
    PermanentWasSacrificed.MkPermanentWasSacrificed
      { PermanentWasSacrificed.player = player,
        PermanentWasSacrificed.permanent = permanent
      }
