{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Craft where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.ExileMaterials as ExileMaterials
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Craft as Craft

-- | A bare object keyed by the record's field names, Pawl.Codec.Equip's shape.
-- The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Craft.Craft keyword)
codec keywordCodec = Fields.object $ do
  cost <- Fields.required "cost" (Cost.codec keywordCodec) Craft.cost
  materials <- Fields.required "materials" (ExileMaterials.codec keywordCodec) Craft.materials
  pure Craft.MkCraft {Craft.cost = cost, Craft.materials = materials}
