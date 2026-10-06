{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DamagePrevented where

import qualified Data.Map as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Codec.CandidateId as CandidateId
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.ObjectId as ObjectId.Type
import qualified Pawl.Types.Recipient as Recipient.Type

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec DamagePrevented.DamagePrevented
codec = Fields.object $ do
  by <- Fields.required "by" CandidateId.codec DamagePrevented.by
  amounts <- Fields.required "amounts" amountsCodec DamagePrevented.amounts
  pure
    DamagePrevented.MkDamagePrevented
      { DamagePrevented.by = by,
        DamagePrevented.amounts = amounts
      }

-- | The prevented amount per CR 120.1 source and, within it, per recipient: an
-- array of source/count-map entries ascending by source, each count map a
-- 'Common.multiset' so a CR 615.12 count of 0 stays sayable. Shared with
-- Pawl.Codec.Prevention, whose `amounts` is the same map.
amountsCodec :: Codec.Codec (Map.Map ObjectId.Type.ObjectId (Map.Map Recipient.Type.Recipient Natural.Natural))
amountsCodec = Common.keyedList (Common.keyValue ObjectId.codec (Common.multiset Recipient.codec))
