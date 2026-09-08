{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CardArrivedIn where

import qualified Data.Set as Set
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CardArrivedIn as CardArrivedIn

-- | A bare object keyed by the record's field names, as Pawl.Codec.MovedBetween
-- is. `excluding` is defaulted rather than required because the empty set is CR
-- 712.21e's own reading -- "from anywhere" -- rather than a convenience.
codec :: Codec.Codec CardArrivedIn.CardArrivedIn
codec = Fields.object $ do
  to <- Fields.required "to" Zone.codec CardArrivedIn.to
  excluding <- Fields.defaulted "excluding" Set.empty (Common.set Zone.codec) CardArrivedIn.excluding
  pure CardArrivedIn.MkCardArrivedIn {CardArrivedIn.to = to, CardArrivedIn.excluding = excluding}
