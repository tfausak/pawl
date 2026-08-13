{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentSacrificed where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec PermanentSacrificed.PermanentSacrificed
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec PermanentSacrificed.player
  permanent <- Fields.required "permanent" ObjectId.codec PermanentSacrificed.permanent
  pure
    PermanentSacrificed.MkPermanentSacrificed
      { PermanentSacrificed.player = player,
        PermanentSacrificed.permanent = permanent
      }
