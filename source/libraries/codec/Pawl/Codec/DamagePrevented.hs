{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DamagePrevented where

import qualified Pawl.Codec.CandidateId as CandidateId
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamagePrevented as DamagePrevented

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec DamagePrevented.DamagePrevented
codec = Fields.object $ do
  by <- Fields.required "by" CandidateId.codec DamagePrevented.by
  source <- Fields.required "source" ObjectId.codec DamagePrevented.source
  amounts <- Fields.required "amounts" (Common.multiset Recipient.codec) DamagePrevented.amounts
  pure
    DamagePrevented.MkDamagePrevented
      { DamagePrevented.by = by,
        DamagePrevented.source = source,
        DamagePrevented.amounts = amounts
      }
