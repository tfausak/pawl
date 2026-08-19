{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Hybrid where

import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Hybrid as Hybrid

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec Hybrid.Hybrid
codec = Fields.object $ do
  left <- Fields.required "left" ManaType.codec Hybrid.left
  right <- Fields.required "right" ManaType.codec Hybrid.right
  pure Hybrid.MkHybrid {Hybrid.left = left, Hybrid.right = right}
