{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AffectedUnless where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AffectedUnless as AffectedUnless

-- | The bare object the enclosing tag already carried, now with a record behind
-- it to name. The wire format is unchanged.
codec :: Codec.Codec AffectedUnless.AffectedUnless
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec AffectedUnless.affected
  unless <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) AffectedUnless.unless
  pure
    AffectedUnless.MkAffectedUnless
      { AffectedUnless.affected = affected,
        AffectedUnless.unless = unless
      }
