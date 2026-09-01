{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PayGate where

import qualified Pawl.Codec.ClauseIndex as ClauseIndex
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PayBranch as PayBranch
import qualified Pawl.Codec.PayObligation as PayObligation
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PayObligation as PayObligation

-- | The first three keys are required: a gate with no payer names nobody, one
-- with no cost offers nothing, and one with no branch does not say which half of
-- CR 118.12 it is. None has a default an absent key could mean -- least of all
-- the branch, where a default would let a card written a word short play as the
-- opposite card.
--
-- The other three are elided when unmarked, which is what every card but
-- Standstill and Don't Make a Sound writes: CR 118.12a's rewriting makes an
-- "unless" cost optional, a clause that names no other clause makes its own
-- offer, and a cost is offered once rather than once per counter. Nothing in
-- `data/cards/` writes `perCounter` yet -- CR 702.24a's mint is its only
-- producer so far, and a minted ability never goes on the wire -- but the key is
-- a card's to write: Cyclone prints rule 702.24a's shape as ordinary text over a
-- wind counter.
codec :: Codec.Codec PayGate.PayGate
codec = Fields.object $ do
  payer <- Fields.required "payer" PlayerRef.codec PayGate.payer
  cost <- Fields.required "cost" (Cost.codec Keyword.codec) PayGate.cost
  branch <- Fields.required "branch" PayBranch.codec PayGate.branch
  obligation <- Fields.defaulted "obligation" PayObligation.Optional PayObligation.codec PayGate.obligation
  perCounter <- Fields.defaulted "perCounter" Nothing (Common.maybe (CounterKind.codec Keyword.codec)) PayGate.perCounter
  offeredAt <- Fields.defaulted "offeredAt" Nothing (Common.maybe ClauseIndex.codec) PayGate.offeredAt
  pure
    PayGate.MkPayGate
      { PayGate.payer = payer,
        PayGate.cost = cost,
        PayGate.branch = branch,
        PayGate.obligation = obligation,
        PayGate.perCounter = perCounter,
        PayGate.offeredAt = offeredAt
      }
