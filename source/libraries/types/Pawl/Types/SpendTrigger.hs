module Pawl.Types.SpendTrigger where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 603.7a: the delayed triggered ability a Pawl.Types.WhenSpent created as its
-- mana was produced, carried on each unit (Pawl.Types.ManaUnit) until the unit
-- is spent. CR 106.6a: one per mana produced.
--
-- The ability's text, source and controller are fixed at creation (CR 603.7c,
-- 603.7e), so they are stamped here rather than looked up at the spend, when the
-- source may be gone.
data SpendTrigger = MkSpendTrigger
  { casts :: Filter.Filter Keyword.Keyword,
    ability :: TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card),
    source :: ObjectId.ObjectId,
    controller :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
