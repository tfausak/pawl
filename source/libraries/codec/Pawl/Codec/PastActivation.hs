{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PastActivation where

import qualified Pawl.Codec.AbilityKind as AbilityKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectSnapshot as ObjectSnapshot
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PastActivation as PastActivation

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec PastActivation.PastActivation
codec = Fields.object $ do
  activator <- Fields.required "activator" PlayerId.codec PastActivation.activator
  source <- Fields.required "source" ObjectSnapshot.codec PastActivation.source
  keyword <- Fields.required "keyword" (Common.maybe Keyword.codec) PastActivation.keyword
  kind <- Fields.required "kind" AbilityKind.codec PastActivation.kind
  targets <- Fields.required "targets" (Common.list ObjectSnapshot.codec) PastActivation.targets
  pure
    PastActivation.MkPastActivation
      { PastActivation.activator = activator,
        PastActivation.source = source,
        PastActivation.keyword = keyword,
        PastActivation.kind = kind,
        PastActivation.targets = targets
      }
