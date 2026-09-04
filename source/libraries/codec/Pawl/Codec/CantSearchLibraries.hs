{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CantSearchLibraries where

import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CantSearchLibraries as CantSearchLibraries

-- | A bare object keyed by the record's field names. Neither field defaults: an
-- unqualified prohibition writes EachPlayer twice, so the card states which
-- libraries and which causes it reaches rather than inheriting them.
codec :: Codec.Codec CantSearchLibraries.CantSearchLibraries
codec = Fields.object $ do
  library <- Fields.required "library" PlayerScope.codec CantSearchLibraries.library
  cause <- Fields.required "cause" PlayerScope.codec CantSearchLibraries.cause
  pure
    CantSearchLibraries.MkCantSearchLibraries
      { CantSearchLibraries.library = library,
        CantSearchLibraries.cause = cause
      }
