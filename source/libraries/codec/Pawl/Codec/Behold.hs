{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Behold where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Behold as Behold

-- | A bare object keyed by the record's field names, Pawl.Codec.ExileMaterials'
-- shape. The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Behold.Behold keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" Common.natural Behold.count
  whichObjects <- Fields.required "whichObjects" (Filter.codec keywordCodec) Behold.whichObjects
  pure Behold.MkBehold {Behold.count = count, Behold.whichObjects = whichObjects}
