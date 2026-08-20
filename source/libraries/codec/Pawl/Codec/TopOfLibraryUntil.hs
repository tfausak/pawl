{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TopOfLibraryUntil where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes. Both keys are required: a walk
-- with no filter never stops, and one with no library has nothing to walk.
codec :: Codec.Codec TopOfLibraryUntil.TopOfLibraryUntil
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec TopOfLibraryUntil.player
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) TopOfLibraryUntil.filter
  pure
    TopOfLibraryUntil.MkTopOfLibraryUntil
      { TopOfLibraryUntil.player = player,
        TopOfLibraryUntil.filter = filter_
      }
