{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Protection where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Protection as Protection

-- | A bare object keyed by the record's field names, Pawl.Codec.Cycling's shape.
-- The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Protection.Protection keyword)
codec keywordCodec = Fields.object $ do
  quality <- Fields.required "quality" (Filter.codec keywordCodec) Protection.quality
  spares <- Fields.required "spares" (Common.maybe (Filter.codec keywordCodec)) Protection.spares
  pure Protection.MkProtection {Protection.quality = quality, Protection.spares = spares}
