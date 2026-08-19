{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SacrificeAnyNumber where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec SacrificeAnyNumber.SacrificeAnyNumber
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) SacrificeAnyNumber.filter
  kind <- Fields.required "kind" (Common.maybe (CounterKind.codec Keyword.codec)) SacrificeAnyNumber.kind
  pure
    SacrificeAnyNumber.MkSacrificeAnyNumber
      { SacrificeAnyNumber.filter = filter_,
        SacrificeAnyNumber.kind = kind
      }
