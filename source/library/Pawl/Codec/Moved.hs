{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Moved where

import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.ZoneChange as ZoneChange
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Moved as Moved

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec Moved.Moved
codec = Fields.object $ do
  change <- Fields.required "change" ZoneChange.codec Moved.change
  characteristics <- Fields.required "characteristics" ProjectedCharacteristics.codec Moved.characteristics
  pure
    Moved.MkMoved
      { Moved.change = change,
        Moved.characteristics = characteristics
      }
