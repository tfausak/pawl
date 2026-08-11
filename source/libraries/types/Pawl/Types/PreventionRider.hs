module Pawl.Types.PreventionRider where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName

-- | CR 615.5: a prevention effect's ADDITIONAL EFFECT, waiting on the shield that
-- carries it -- Test of Faith's "for each 1 damage prevented this way, put a
-- +1/+1 counter on that creature".
--
-- A program plus the environment it must resolve in, kept together because the
-- environment cannot be re-derived when the rider finally runs: CR 400.7 has put
-- the installing spell in its owner's graveyard as a new object, so neither its
-- chosen targets nor CR 109.5's "you" survive on the board. The same posture
-- Pawl.Types.ActiveReplacement.controller takes, and for the same reason.
--
-- `targets` is the installing resolution's chosen map, snapshotted -- what makes
-- "that creature" nameable turns later. `controller` is who performs the rider,
-- which is the shield's controller and not the damage's source.
--
-- What is NOT here is the prevented amount: that is a property of the
-- APPLICATION rather than of the shield, so it rides Pawl.Types.Prevention and
-- is stamped on the shielded recipient by
-- Pawl.Engine.Resolve.runPreventionRiders for Quantity.InSlot to read.
data PreventionRider = MkPreventionRider
  { effects :: Seq.Seq (Effect.Effect Card.Card),
    targets :: Map.Map SlotName.SlotName (Set.Set Recipient.Recipient),
    controller :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
