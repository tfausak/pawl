{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExileMaterials where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExileMaterials as ExileMaterials

-- | A bare object keyed by the record's field names,
-- Pawl.Codec.ExileCardsFromGraveyard's shape. The keyword codec is a PARAMETER;
-- see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (ExileMaterials.ExileMaterials keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" Common.natural ExileMaterials.count
  whichObjects <- Fields.required "whichObjects" (Filter.codec keywordCodec) ExileMaterials.whichObjects
  pure ExileMaterials.MkExileMaterials {ExileMaterials.count = count, ExileMaterials.whichObjects = whichObjects}
