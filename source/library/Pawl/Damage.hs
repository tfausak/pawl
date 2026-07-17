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
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Zone as Zone

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

-- CR 510.2: gather all combat damage from the participants that `assigns` admits,
-- before applying any of it (simultaneity). The filter is how a wave restricts
-- who deals: attackers by their id, blockers by pruning each attacker's set.
--
-- Everything is gathered before anything is applied. Applying attacker-by-
-- attacker with state-based actions in between would let a creature die before
-- it deals its damage, and a 2/1 trading with a 2/1 would kill only one of them.
gatherCombatDamage :: (ObjectId -> Bool) -> Game ([(ObjectId, Natural)], [(PlayerId, Natural)])
gatherCombatDamage assigns = do
  gs <- State.get
  let combat = GameState.combat gs
      attackers = filter (assigns . fst) (Map.toList (Combat.Type.attackers combat))
      blockers = Map.toList (Map.map (Set.filter assigns) (Combat.Type.blockers combat))
  parts <- Monad.mapM (attackerAssignment gs) attackers
  let fromBlockers = concatMap (blockerAssignment gs) blockers
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

-- Deal one combat damage step, returning True iff this was the FIRST of two --
-- i.e. a second combat damage step must be spliced (CR 510.4).
--
-- Which creatures assign is read LIVE off the projection at this boundary (spec
-- §3), never precomputed. `struckFirst` both routes the wave and records CR
-- 510.4's "had first strike or double strike as the first step began" snapshot.
-- Only creatures still on the battlefield assign ("the REMAINING attackers and
-- blockers") -- a striker killed in the first step is gone for the second.
dealCombatDamage :: Game Bool
dealCombatDamage = do
  gs <- State.get
  let combat = GameState.combat gs
      participants =
        Set.union
          (Map.keysSet (Combat.Type.attackers combat))
          (Set.unions (Map.elems (Combat.Type.blockers combat)))
      striking oid = Game.hasKeyword Keyword.FirstStrike oid gs || Game.hasKeyword Keyword.DoubleStrike oid gs
      strikers = Set.filter striking participants
      onBattlefield oid = case Game.lookupObject oid gs of
        Just obj -> Object.zone obj == Zone.Battlefield
        Nothing -> False
  case Combat.Type.struckFirst combat of
    Nothing
      -- CR 510.4 does not apply: no striker, so one step and everyone deals.
      | Set.null strikers -> do
          dealWave onBattlefield
          pure False
      -- CR 510.4: a striker is present. This is the first of two steps; only
      -- first strikers and double strikers deal, and a second step follows.
      | otherwise -> do
          State.modify' (\g -> g {GameState.combat = (GameState.combat g) {Combat.Type.struckFirst = Just strikers}})
          dealWave (\oid -> onBattlefield oid && Set.member oid strikers)
          pure True
    -- CR 510.4 second step: those that had neither first strike nor double strike
    -- as the first step began (not in the snapshot), plus those that currently
    -- have double strike -- and are still on the battlefield.
    Just snapshot -> do
      dealWave (\oid -> onBattlefield oid && (Set.notMember oid snapshot || Game.hasKeyword Keyword.DoubleStrike oid gs))
      pure False

-- Gather this wave's damage under `assigns` and apply it.
dealWave :: (ObjectId -> Bool) -> Game ()
dealWave assigns = do
  assignment <- gatherCombatDamage assigns
  State.modify' (applyCombatDamage assignment)
