module Pawl.Engine.Decide where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Pawl.Types.Decider (Decider)
import qualified Pawl.Types.Decider as Decider
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.PlayerControl as PlayerControl
import Pawl.Types.PlayerId (PlayerId)

-- Who actually decides for a player (CR 723.5). A controlled player has their
-- decisions made by the controller; everyone else decides for themselves.
--
-- A LOOKUP and nothing else. There is no active-player guard, because CR 723.3's
-- "a player who's being controlled during their turn is still the active player"
-- says only that control leaves the active player alone -- not that the
-- controlled player is the active one. CR 723.2's control is held on someone
-- else's turn, see #881, so the row's own lifetime ends it: rule 723.1's at
-- Pawl.Engine.Engine's turn handoff, rule 723.2's at Pawl.Engine.Stack's end of
-- resolution.
deciderFor :: PlayerId -> GameState -> Decider
deciderFor pid gs = case Map.lookup pid (GameState.control gs) of
  Just rows -> PlayerControl.decider (effective rows)
  Nothing -> Decider.MkDecider pid

-- CR 723.1a: "the last one to be created is the one that works", and the stack
-- is kept in creation order, so that is its last element. The row that WORKS,
-- which is also the row whose CR 723.7 restriction applies
-- (Pawl.Engine.Mana.manaSourcesGiven).
effective :: NonEmpty.NonEmpty PlayerControl.PlayerControl -> PlayerControl.PlayerControl
effective = NonEmpty.last
