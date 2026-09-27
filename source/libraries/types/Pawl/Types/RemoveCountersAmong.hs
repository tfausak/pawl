module Pawl.Types.RemoveCountersAmong where

import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.RemovalCount as RemovalCount
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.WhichCounters as WhichCounters

-- | The payload of Pawl.Types.Effect's RemoveCountersAmong arm: counters of one
-- kind, or of any kind (Eventide's Shadow), off the permanents an ObjectRef names, the resolving controller dividing
-- them (CR 608.2d). An ObjectRef for Pawl.Types.MoveCounters' `from`'s reason. `tally` binds how many came off, for "twice that many"
-- (Galloping Lizrog); Pawl.Types.RemoveCounters' `tally`.
data RemoveCountersAmong = MkRemoveCountersAmong
  { count :: RemovalCount.RemovalCount,
    from :: ObjectRef.ObjectRef,
    kind :: WhichCounters.WhichCounters Keyword.Keyword,
    tally :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
