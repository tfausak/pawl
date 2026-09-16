module Pawl.Types.Ward where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally

-- | The payload of Pawl.Types.Keyword's Ward arm: CR 702.21a's cost, plus CR
-- 702.21b's X as a second field on it.
--
-- PARAMETRIC in the keyword, Pawl.Types.Cycling's reason: the cost can name a
-- Keyword and Keyword names THIS. Only @Ward Keyword.Keyword@ is ever written.
data Ward keyword = MkWard
  { -- | CR 702.21a's [cost]. Raffine, Scheming Seer's is {1}, Auntie Ool,
    -- Cursewretch's is a blight. With @perEach@ set this is the cost of ONE of
    -- the things counted, so ward {X} writes {1} here.
    cost :: Cost.Cost keyword,
    -- | CR 702.21b's "some ward abilities include an X in their cost and state
    -- what X is equal to", as a multiplier on the cost above rather than as an
    -- {X} symbol inside it: ward {X} is X copies of {1}, and one copy of a cost
    -- per thing counted is exactly Pawl.Types.PayGate's perEach, which
    -- Pawl.Engine.Resolve.payGatePaidBy evaluates against the RESOLUTION. CR
    -- 107.4b is why the two spellings are one charge -- every generic symbol is
    -- payable with any type of mana, so X copies of {1} and a {X} standing for
    -- the same number take the same mana. That
    -- is what rule 702.21b asks for -- "this value is determined at the time the
    -- ability resolves, not locked in as the ability triggers" -- and it is why
    -- the value cannot ride Cost's own {X}, whose value CR 107.3a fixes at what
    -- CR 601.2b announced (Pawl.Engine.Resolve.announcedXOn).
    --
    -- A Pawl.Types.PlayerCounterTally and NOT the Pawl.Types.Quantity that
    -- Pawl.Engine.Keyword.ward widens it into: Quantity names Keyword in its
    -- ObjectCounters and TimesPaid arms, and Keyword names this, so a Quantity
    -- here would close a module cycle that only parameterizing Quantity over the
    -- keyword could open -- and that parameter would have to cascade through the
    -- fifty-odd types embedding a Quantity. This tally is a documented LEAF, so
    -- it cannot. Scryfall o:"ward {X}", 2026-09-15, returns Minthara, Merciless
    -- Soul alone, and it counts player counters; a printing that stated X as
    -- anything else would be the one to widen this field.
    --
    -- Nothing is every other printing: a ward whose cost is a printed number.
    --
    -- Pawl.LeavesTriggerSpec's "CR 702.21b ward {X}" group proves the
    -- resolution-time read.
    perEach :: Maybe PlayerCounterTally.PlayerCounterTally
  }
  deriving (Eq, Ord, Show)
