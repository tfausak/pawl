{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DamageR where

import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Codec.DamageRewrite as DamageRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamageR as DamageR

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec DamageR.DamageR
codec = Fields.object $ do
  matching <- Fields.required "matching" DamagePattern.codec DamageR.matching
  rewrite <- Fields.required "rewrite" DamageRewrite.codec DamageR.rewrite
  pure
    DamageR.MkDamageR
      { DamageR.matching = matching,
        DamageR.rewrite = rewrite
      }
