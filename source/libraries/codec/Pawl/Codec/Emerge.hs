{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Emerge where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Emerge as Emerge

-- | A bare object keyed by the record's field names, Pawl.Codec.Equip's shape.
-- The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Emerge.Emerge keyword)
codec keywordCodec = Fields.object $ do
  cost <- Fields.required "cost" (Cost.codec keywordCodec) Emerge.cost
  quality <- Fields.required "quality" (Common.maybe (Filter.codec keywordCodec)) Emerge.quality
  pure Emerge.MkEmerge {Emerge.cost = cost, Emerge.quality = quality}
