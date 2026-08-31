{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PutCountersFrom where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's PutCountersFrom arm.
--
-- `kind` defaults to the whole tally -- what CR 122.8's first sentence means --
-- so data/cards/iron-apprentice.json writes no key at all and only a card
-- naming a kind (data/cards/selfless-police-captain.json) writes one.
codec :: Codec.Codec PutCountersFrom.PutCountersFrom
codec = Fields.object $ do
  from <- Fields.required "from" SlotName.codec PutCountersFrom.from
  kind <- Fields.defaulted "kind" Nothing (Common.maybe (CounterKind.codec Keyword.codec)) PutCountersFrom.kind
  ref <- Fields.required "ref" ObjectRef.codec PutCountersFrom.ref
  pure
    PutCountersFrom.MkPutCountersFrom
      { PutCountersFrom.from = from,
        PutCountersFrom.kind = kind,
        PutCountersFrom.ref = ref
      }
