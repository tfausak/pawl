module Pawl.Types.GrantPlayFromExile where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's GrantPlayFromExile arm: which objects the
-- permission covers, how long it lasts, and how its holder may pay for what they
-- cast under it.
--
-- SPUN OUT of Pawl.Types.DurationRef, which that type's own haddock asks for as
-- soon as one sharer needs a field the others do not (#1305): PreventAllDamage
-- and GainControl have nothing to say about mana, and a Maybe bolted onto the
-- shared record would have made the field's absence into the tag telling the
-- three arms apart.
--
-- `spending` is CR 118.14's "and mana of any type can be spent to cast that
-- spell", printed on Dire Fleet Daredevil beside the permission itself. It rides
-- the grant rather than the card being exiled, which is rule 118.14's own
-- scoping: the permission is the granting effect's, so the same card cast under
-- some other permission pays its printed colours.
data GrantPlayFromExile = MkGrantPlayFromExile
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef,
    spending :: ManaSpending.ManaSpending
  }
  deriving (Eq, Ord, Show)
