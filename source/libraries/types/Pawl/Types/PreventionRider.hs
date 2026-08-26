module Pawl.Types.PreventionRider where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ObjectId as ObjectId
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
-- "that creature" nameable turns later. A PRINTED ability chose no targets (CR
-- 115.10a) and carries CR 113.7's reserved self slot there instead, which is what
-- lets its rider say "it" (Protean Hydra); see Pawl.Engine.Replacement
-- .printedRider. `controller` is who performs the rider, which is the shield's
-- controller and not the damage's source.
--
-- `source` is CR 113.7's source of the effect that created the prevention: the
-- resolving spell or ability for a floating shield, and the permanent itself for
-- a static prevention ability. Carried because
-- Pawl.Engine.Resolve.runPreventionRider has to hand the executor an ObjectId
-- and the shielded recipient may be a PLAYER, which has none. For a floating row
-- it names a dead object, the same posture and for the same reason as
-- Pawl.Types.ActiveReplacement.source: CR 400.7 replaced the installing spell
-- long ago.
--
-- What is NOT here is the prevented amount: that is a property of the
-- APPLICATION rather than of the shield, so it rides Pawl.Types.Prevention, and
-- Pawl.Engine.Resolve.runPreventionRider publishes it through
-- Pawl.Types.GameState.ambientAmounts for Quantity.InSlot to read -- an ambient
-- channel rather than a binding on an object, because a shielded player has no
-- object to bind it to.
data PreventionRider = MkPreventionRider
  { effects :: Seq.Seq (Effect.Effect Card.Card),
    targets :: Map.Map SlotName.SlotName (Set.Set Recipient.Recipient),
    controller :: PlayerId.PlayerId,
    source :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
