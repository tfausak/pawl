{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Prevention where

import qualified Pawl.Codec.CandidateId as CandidateId
import qualified Pawl.Codec.PreventionRider as PreventionRider
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Prevention as Prevention

-- | `by` is CR 615.13's GROUPING key, so an encoder dropping it would let two
-- instances' preventions decode as one -- and it is the same identity the
-- GameEvent this becomes carries, which is what "prevented this way" compares
-- against (Pawl.Types.DamagePrevented).
--
-- `rider` is 'Fields.required' over 'Common.maybe' rather than defaulted, the
-- posture Pawl.Codec.Combat takes: the absent case is an explicit null.
codec :: Codec.Codec Prevention.Prevention
codec = Fields.object $ do
  by <- Fields.required "by" CandidateId.codec Prevention.by
  recipient <- Fields.required "recipient" Recipient.codec Prevention.recipient
  amount <- Fields.required "amount" Common.natural Prevention.amount
  rider <- Fields.required "rider" (Common.maybe PreventionRider.codec) Prevention.rider
  pure
    Prevention.MkPrevention
      { Prevention.by = by,
        Prevention.recipient = recipient,
        Prevention.amount = amount,
        Prevention.rider = rider
      }
