{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TopOfLibraryUntil where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes. All three keys are required: a
-- walk with no filter never stops, one with no library has nothing to walk, and
-- one with no count does not say how many matches end it -- @count@ being
-- 'Pawl.Codec.TopOfLibrary''s key, spelled the same way and holding the same
-- 'Pawl.Codec.Quantity', so Treasure Hunt's single match writes
-- @{"type":"Literal","value":1}@ where Open the Way's writes an @InSlot@ naming
-- X.
codec :: Codec.Codec TopOfLibraryUntil.TopOfLibraryUntil
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec TopOfLibraryUntil.player
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) TopOfLibraryUntil.filter
  count <- Fields.required "count" Quantity.codec TopOfLibraryUntil.count
  pure
    TopOfLibraryUntil.MkTopOfLibraryUntil
      { TopOfLibraryUntil.player = player,
        TopOfLibraryUntil.filter = filter_,
        TopOfLibraryUntil.count = count
      }
