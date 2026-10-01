{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SelfCountersRemoved where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SelfCountersRemoved as SelfCountersRemoved
import qualified Pawl.Types.Zone as Zone

-- | The zone defaults to the battlefield, CR 113.6's default, so only an
-- ability that states another zone spells it.
codec :: Codec.Codec SelfCountersRemoved.SelfCountersRemoved
codec = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) SelfCountersRemoved.kind
  zone <- Fields.defaulted "zone" Zone.Battlefield Zone.codec SelfCountersRemoved.zone
  pure
    SelfCountersRemoved.MkSelfCountersRemoved
      { SelfCountersRemoved.kind = kind,
        SelfCountersRemoved.zone = zone
      }
