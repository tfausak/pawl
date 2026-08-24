module Pawl.Types.PaidExpiry where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 116.2c's pay-to-end offer, as the game remembers it: the price, and the
-- player who may pay it.

-- The PlayerId is CR 109.5's "you", BAKED at arming time (Pawl.Engine.Expiry.arm)
-- for Pawl.Types.While's reason and one of its own: the clause belongs to an
-- ACTIVATED ability, so rule 109.5 makes its "you" the player who activated it,
-- and a Licid whose control changes afterwards must not carry the offer over to
-- its new controller.
data PaidExpiry = MkPaidExpiry
  { player :: PlayerId.PlayerId,
    cost :: Cost.Cost Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
