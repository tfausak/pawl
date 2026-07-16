module Pawl.Damage where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Combat as Combat
import qualified Pawl.Decide as Decide
import qualified Pawl.Game as Game
import Pawl.Type.AttackTarget (AttackTarget)
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.Combat as Combat.Type
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt

-- CR 514.2: during the cleanup step, all damage marked on permanents is removed.
--
-- Every object, not just battlefield ones: the field exists on all of them, and
-- CR 514.2 says "all damage marked on permanents (including phased-out
-- permanents)" -- there is no reason to be selective, and being selective is how
-- a stale mark survives.
removeAllDamage :: GameState -> GameState
removeAllDamage gs =
  let clear obj = obj {Object.damage = 0}
   in gs {GameState.objects = Map.map clear (GameState.objects gs)}

-- What one attacking creature assigns, as (damage to creatures, damage to
-- players). CR 510.1a: a creature assigns damage equal to its power, and a
-- creature that would assign 0 or less assigns none at all.
attackerAssignment :: GameState -> (ObjectId, AttackTarget) -> Game ([(ObjectId, Natural)], [(PlayerId, Natural)])
attackerAssignment gs (attacker, target) = case Game.powerOf attacker gs of
  Nothing -> pure ([], [])
  Just p ->
    if p <= 0
      then pure ([], [])
      else do
        let power :: Natural
            power = fromInteger p
        case Set.toList (Combat.blockersOf attacker gs) of
          -- CR 510.1b: unblocked, so it hits what it is attacking.
          [] -> case target of
            AttackTarget.OfPlayer pid -> pure ([], [(pid, power)])
          -- CR 510.1c: exactly one blocker takes ALL of it. Forced, so not asked.
          [blocker] -> pure ([(blocker, power)], [])
          blockers -> case Game.controllerOf attacker gs of
            Nothing -> pure ([], [])
            Just pid -> do
              let decider = Decide.deciderFor pid gs
              chosen <-
                Trans.lift
                  (Program.prompt (Prompt.AssignCombatDamage decider pid attacker (Set.fromList blockers) power))
              -- CR 510.1e checks the assignment AS A WHOLE, so this cannot be
              -- repaired by filtering the way a discard can. An illegal answer is
              -- rejected and the attacker assigns nothing -- the rules' own
              -- degenerate case (CR 510.1b/c), not an invented punishment.
              --
              -- This is NOT the CR 733 rewind. That rule is about a human taking
              -- an illegal action at a table; an enforcing engine never offers
              -- one. Only a broken interpreter reaches here, and re-prompting a
              -- pure `Prompt r -> r` returns the identical wrong answer. See the
              -- spec, section 3.
              let isBlocker o = List.elem o blockers
                  onlyBlockers = all isBlocker (Map.keys chosen)
                  totalsPower = sum (Map.elems chosen) == power
              pure (if onlyBlockers && totalsPower then (Map.toList chosen, []) else ([], []))

-- CR 510.1d: a blocking creature assigns its damage to the creature it blocks.
-- Nothing in M1b can block more than one attacker, so this is always forced.
blockerAssignment :: GameState -> (ObjectId, Set.Set ObjectId) -> [(ObjectId, Natural)]
blockerAssignment gs (attacker, blockers) =
  let assign blocker = case Game.powerOf blocker gs of
        Just p -> if p <= 0 then [] else [(attacker, fromInteger p)]
        Nothing -> []
   in concatMap assign (Set.toList blockers)

-- CR 510.2: all combat damage is dealt SIMULTANEOUSLY.
--
-- Everything is gathered before anything is applied. Applying attacker-by-
-- attacker with state-based actions in between would let a creature die before
-- it deals its damage, and a 2/1 trading with a 2/1 would kill only one of them.
gatherCombatDamage :: Game ([(ObjectId, Natural)], [(PlayerId, Natural)])
gatherCombatDamage = do
  gs <- State.get
  let combat = GameState.combat gs
  parts <- Monad.mapM (attackerAssignment gs) (Map.toList (Combat.Type.attackers combat))
  let fromBlockers = concatMap (blockerAssignment gs) (Map.toList (Combat.Type.blockers combat))
  pure (concatMap fst parts ++ fromBlockers, concatMap snd parts)

applyCombatDamage :: ([(ObjectId, Natural)], [(PlayerId, Natural)]) -> GameState -> GameState
applyCombatDamage (toCreatures, toPlayers) gs =
  let -- CR 120.3e: marked on the creature.
      mark n obj = obj {Object.damage = Object.damage obj + n}
      hurt g (oid, n) = g {GameState.objects = Map.adjust (mark n) oid (GameState.objects g)}
      -- CR 120.3a: damage to a player without infect causes that much life loss.
      drain n player = player {Player.life = Player.life player - toInteger n}
      bleed g (pid, n) = g {GameState.players = Map.adjust (drain n) pid (GameState.players g)}
   in List.foldl' bleed (List.foldl' hurt gs toCreatures) toPlayers

dealCombatDamage :: Game ()
dealCombatDamage = do
  assignment <- gatherCombatDamage
  State.modify' (applyCombatDamage assignment)
