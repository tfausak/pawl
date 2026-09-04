{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Equip where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Equip as Equip

-- | A bare object keyed by the record's field names, Pawl.Codec.Cycling's shape.
-- The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Equip.Equip keyword)
codec keywordCodec = Fields.object $ do
  cost <- Fields.required "cost" (Cost.codec keywordCodec) Equip.cost
  quality <- Fields.required "quality" (Common.maybe (Filter.codec keywordCodec)) Equip.quality
  pure Equip.MkEquip {Equip.cost = cost, Equip.quality = quality}
