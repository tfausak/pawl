{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AffectedUnless where

import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AffectedUnless as AffectedUnless

-- | The bare object the enclosing tag already carried, now with a record behind
-- it to name.
--
-- "name" DEFAULTS to Nothing, which is what all but one printing writes: CR
-- 116.2d's permission is rare, so a restriction that grants none names no
-- ability for one to refer to.
codec :: Codec.Codec AffectedUnless.AffectedUnless
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec AffectedUnless.affected
  unless <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) AffectedUnless.unless
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) AffectedUnless.name
  pure
    AffectedUnless.MkAffectedUnless
      { AffectedUnless.affected = affected,
        AffectedUnless.unless = unless,
        AffectedUnless.name = name
      }
