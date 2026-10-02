module Pawl.Types.Move where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Types.Activation as Activation
import qualified Pawl.Types.Answer as Answer
import qualified Pawl.Types.Casting as Casting
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Paying as Paying
import qualified Pawl.Types.Reference as Reference
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Taking as Taking
import qualified Pawl.Types.TypeSwap as TypeSwap

-- | One decision a scenario makes for a player.
data Move
  = -- | CR 601.2.
    Cast Casting.Casting
  | -- | CR 305.1.
    PlayLand Reference.Reference
  | -- | CR 602.2.
    Activate Activation.Activation
  | -- | CR 508.1a.
    Attack (Seq.Seq Reference.Reference)
  | -- | CR 509.1a, each blocker to the attackers it blocks.
    Block (Map.Map Reference.Reference (Set.Set Reference.Reference))
  | -- | CR 510.1c-d, passed to the engine unvalidated so CR 510.1's checks stay
    -- the engine's.
    AssignDamage (Map.Map Reference.Reference Natural)
  | -- | CR 506.2.
    ChooseDefender Label.Label
  | -- | CR 508.1b.
    ChooseAttackTarget Reference.Reference
  | -- | CR 613.7m: one player's objects stamped at one moment, earliest first.
    OrderTimestamps (Seq.Seq Reference.Reference)
  | -- | CR 603.5 / 608.2d: whether the deciding player takes a printed "may".
    ChooseOptional OptionalDecision.OptionalDecision
  | -- | CR 118.12a: whether the deciding player pays a cost a resolving object offers.
    ChooseToPay Paying.Paying
  | -- | CR 612.1: the land or creature type a text-changing effect swaps.
    ChooseTypeSwap TypeSwap.TypeSwap
  | -- | CR 707.5 / 614.12a: what an object entering as a copy copies, Nothing
    -- declining the card's "may".
    ChooseCopyTarget (Maybe Reference.Reference)
  | -- | CR 601.2c / 603.3d: the targets of each slot a prompt outside a cast
    -- or activation offers, which also answers its announcement of how many.
    ChooseTargets (Map.Map SlotName.SlotName (Seq.Seq Reference.Reference))
  | -- | CR 603.3b: one player's triggers by source, Nothing the sourceless,
    -- in the order put on the stack.
    OrderTriggers (Seq.Seq (Maybe Reference.Reference))
  | -- | Any prompt no dedicated move covers, answered by its name.
    Answer Answer.Answer
  | -- | CR 116.2 / 605.3a: any other action a player takes at priority, named
    -- as the Offered view renders it.
    Take Taking.Taking
  | -- | CR 104.3a.
    Concede
  | -- | CR 117.3d.
    Pass
  deriving (Eq, Ord, Show)
