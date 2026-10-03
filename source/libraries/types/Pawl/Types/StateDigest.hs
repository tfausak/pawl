module Pawl.Types.StateDigest where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.PastActivation as PastActivation

-- | CR 732.3: a game state as Pawl.Engine.Fragmented.digest leaves it for
-- comparison. Ordered by a few cheap fields before the whole record, so two
-- states that differ in who holds priority, what is on the stack, what is on
-- the battlefield or in a mana pool -- most pairs a loop compares -- never
-- walk every object.
data StateDigest = MkStateDigest
  { -- | The state, less what the digest drops. The only field compared.
    unwrap :: GameState.GameState,
    -- | The dropped GameState.events, carried uncompared so a recurrence can
    -- ask which of them the loop wrote.
    events :: Seq.Seq LoggedEvent.LoggedEvent,
    -- | The dropped GameState.activationsThisTurn, carried the same way.
    activations :: Seq.Seq PastActivation.PastActivation
  }
  deriving (Show)

instance Eq StateDigest where
  a == b = compare a b == EQ

instance Ord StateDigest where
  compare x y =
    compare (cheap a) (cheap b) <> compare a b
    where
      a = unwrap x
      b = unwrap y
      cheap gs =
        ( ( GameState.priority gs,
            GameState.passed gs,
            GameState.stack gs,
            GameState.phase gs
          ),
          ( Map.size (GameState.objects gs),
            GameState.battlefield gs,
            GameState.manaPool gs,
            fmap length (GameState.hand gs)
          )
        )
