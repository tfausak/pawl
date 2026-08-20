module Pawl.Types.DelayedTrigger where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnWindow as TurnWindow

-- | CR 603.7: a delayed triggered ability that has been created and is waiting for
-- its trigger event. A concrete `TriggeredAbility Card`, exactly as
-- Source.OfTrigger already carries one.
--
-- `controller` is the player who controlled the SPELL OR ABILITY that created it,
-- as that spell or ability RESOLVED (CR 603.7d-f) -- baked in at arming, never
-- re-derived. A CR 103.6 opening-hand action creates one without a resolution at
-- all (Chancellor of the Forge); there the player who took the action is the
-- controller, which is the same rule read through CR 103.6's actor. `bindings` is the environment captured at that moment, which is how
-- "it" and "that card" (CR 603.7c) survive the resolution that armed the ability.
--
-- `expiry` is CR 603.7b's stated duration, as the game remembers it. Nothing is
-- that rule's default -- fire once, the next time the trigger event occurs -- and
-- an entry carrying it is removed as it fires. Just an expiry keeps the entry
-- armed through firing, and one of Pawl.Engine.Expiry's sweeps is what eventually
-- ends it (CR 514.2's cleanup, for Full Throttle's "this turn"). Firing and the
-- clock are not the only two ways an entry ends: an entry whose `window` names a
-- turn now behind us can no longer fire at all, and settleOnsets retires it
-- whatever its duration says.
--
-- Nothing rather than an Expiry arm meaning "once", because once-ness is not a
-- duration: the rule words it as the ABSENCE of one, and an entry with no
-- duration must survive every time-based sweep -- its one shot is spent by
-- firing, never by the clock.
--
-- An Expiry and not a Duration, for the reason that type's own haddock gives:
-- card data says "this turn", and the game remembers whose turn and when. It is
-- armed by Pawl.Engine.Expiry.arm exactly as a continuous effect's is.
data DelayedTrigger = MkDelayedTrigger
  { ability :: TriggeredAbility.TriggeredAbility Card.Card,
    source :: ObjectId.ObjectId,
    controller :: PlayerId.PlayerId,
    bindings :: Map.Map SlotName.SlotName Binding.Binding,
    -- | Pawl.Types.Onset as the game remembers it: which turns this entry may
    -- fire on. TurnWindow.AnyTurn is the ordinary case, CR 603.7a's floor. The
    -- other two arms are Onset.FromYourNextTurn before and after the turn it
    -- names has begun; see Pawl.Types.TurnWindow.
    --
    -- Not a Maybe: "no restriction" is one of the windows rather than the absence
    -- of one, unlike `expiry` below, where CR 603.7b really does word the default
    -- as having no stated duration.
    --
    -- The window says WHICH TURNS and the ability's own condition says WHICH
    -- EVENT; nothing here can say the second, so an entry settled on the right
    -- turn still fires only on the step its TriggerCondition names.
    window :: TurnWindow.TurnWindow,
    expiry :: Maybe Expiry.Expiry
  }
  deriving (Eq, Ord, Show)
