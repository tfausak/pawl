-- CR 106.4: the one place a Pawl.Types.ManaCount is interpreted. A pure fold --
-- resolve whose pool, keep by the ManaFilter, count the survivors -- that never
-- learns which effect or card produced it, exactly as Pawl.Engine.Count's fold
-- over objects does not.
--
-- Unparameterized, where Pawl.Engine.Count takes a ViewOf and a QuantityOf: a
-- mana unit has no characteristics for a projection to supply and a ManaCount
-- holds no inner Quantity, so there is nothing for a caller to inject and no
-- module cycle to break. Pawl.Engine.Quantity calls this directly.
module Pawl.Engine.ManaCount where

import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaFilter as ManaFilter
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaCount as ManaCount

-- Nothing when the count cannot be determined -- an unresolvable PlayerRef,
-- which is the only way this fold can fail. It propagates, the posture
-- Pawl.Engine.Count.evaluate takes for the same reason.
--
-- Read STRAIGHT OFF GameState.manaPool at the moment of the call, never off a
-- stored or sampled copy: CR 605.3a lets a player activate a mana ability
-- whenever they have priority and CR 605.3b has it resolve immediately without
-- using the stack, so the pool moves with no state-based action (CR 704.3) and
-- no priority pass in between. Pinned by Pawl.PowerToughnessSpec's "CR 106.4 the
-- count is live".
evaluate :: Filter.Context -> GameState -> ManaCount.ManaCount -> Maybe Integer
evaluate context gs count = do
  -- CR 106.4 attaches a pool to a player, so the same PlayerRef reading a
  -- Scope.InZone uses answers WHOSE pool -- one resolution of the reference for
  -- both, which is what keeps the two from disagreeing.
  pids <- Count.playersFor context gs (ManaCount.player count)
  let units = concatMap (\pid -> Mana.unwrap (Game.poolOf pid gs)) pids
  -- CR 106.4's pool is a multiset of UNITS (Pawl.Types.Mana), so counting the
  -- survivors is counting mana: one unit is one mana, whatever its type.
  pure (toInteger (length (filter (ManaFilter.matches (ManaCount.filter count)) units)))
