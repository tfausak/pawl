{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PaidExpiry where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PaidExpiry as PaidExpiry

-- | A bare object keyed by the record's field names, Pawl.Codec.While's shape.
codec :: Codec.Codec PaidExpiry.PaidExpiry
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec PaidExpiry.player
  cost <- Fields.required "cost" (Cost.codec Keyword.codec) PaidExpiry.cost
  pure PaidExpiry.MkPaidExpiry {PaidExpiry.player = player, PaidExpiry.cost = cost}
