{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Reinforce where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Reinforce as Reinforce

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464). The keyword codec is a PARAMETER; see
-- Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Reinforce.Reinforce keyword)
codec keywordCodec = Fields.object $ do
  amount <- Fields.required "amount" Common.natural Reinforce.amount
  cost <- Fields.required "cost" (Cost.codec keywordCodec) Reinforce.cost
  pure Reinforce.MkReinforce {Reinforce.amount = amount, Reinforce.cost = cost}
