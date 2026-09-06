{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActivationProhibition where

import qualified Pawl.Codec.AbilityKind as AbilityKind
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActivationProhibition as ActivationProhibition

-- | An object with two named keys, Pawl.Codec.CounterRestriction's shape with a
-- CR 605.1a ability kind where that one has a counter kind. Untagged for that
-- codec's reason: Pawl.Types.ActivationProhibition is a product with no sum for
-- a tag to discriminate.
--
-- "affected" is REQUIRED and both "kind" and "name" DEFAULT to Nothing, the
-- asymmetry that codec argues: a missing affected set has no defensible default -- the empty
-- one disarms the prohibition and the full one widens it to the whole board --
-- while a missing kind is exactly what Arrest prints, a prohibition naming no
-- kind at all.
codec :: Codec.Codec ActivationProhibition.ActivationProhibition
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec ActivationProhibition.affected
  kind <- Fields.defaulted "kind" Nothing (Common.maybe AbilityKind.codec) ActivationProhibition.kind
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) ActivationProhibition.name
  pure
    ActivationProhibition.MkActivationProhibition
      { ActivationProhibition.affected = affected,
        ActivationProhibition.kind = kind,
        ActivationProhibition.name = name
      }
