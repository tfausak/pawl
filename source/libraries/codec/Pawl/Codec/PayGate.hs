{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PayGate where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PayBranch as PayBranch
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PayGate as PayGate

-- | All three keys are required: a gate with no payer names nobody, one with no
-- cost offers nothing, and one with no branch does not say which half of CR
-- 118.12 it is. None has a default an absent key could mean -- least of all the
-- branch, where a default would let a card written a word short play as the
-- opposite card.
codec :: Codec.Codec PayGate.PayGate
codec = Fields.object $ do
  payer <- Fields.required "payer" SlotName.codec PayGate.payer
  cost <- Fields.required "cost" (Cost.codec Keyword.codec) PayGate.cost
  branch <- Fields.required "branch" PayBranch.codec PayGate.branch
  pure PayGate.MkPayGate {PayGate.payer = payer, PayGate.cost = cost, PayGate.branch = branch}
