{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentsBecomeTargeted where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.StackObjectKind as StackObjectKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentsBecomeTargeted as PermanentsBecomeTargeted

-- | A bare object keyed by the record's field names. The filter is required,
-- while the kind is elided when absent for Pawl.Codec.ControllerBecomesTarget's
-- reason: "a spell or ability" is the wider and unnarrowed reading of CR 601.2c.
codec :: Codec.Codec PermanentsBecomeTargeted.PermanentsBecomeTargeted
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PermanentsBecomeTargeted.filter
  kind <- Fields.defaulted "kind" Nothing (Common.maybe StackObjectKind.codec) PermanentsBecomeTargeted.kind
  pure
    PermanentsBecomeTargeted.MkPermanentsBecomeTargeted
      { PermanentsBecomeTargeted.filter = filter_,
        PermanentsBecomeTargeted.kind = kind
      }
