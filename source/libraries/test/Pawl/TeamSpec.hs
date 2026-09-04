{-# LANGUAGE GADTs #-}

-- Covers: CR 102.3's teammates and CR 808's Team vs. Team variant --
-- Pawl.Types.Teams, the Pawl.Types.GameSettings field that carries them, and the
-- readers that answer "who are my opponents", one case each: Pawl.Engine.Combat's
-- attackableOpponents (CR 506.2a), Pawl.Engine.Resolve's playerRefPlayers
-- (PlayerRef.Relative Opponent), Pawl.Engine.Target's slot filter (the
-- Filter.IsPlayer atom, which reads Pawl.Engine.Filter's Context), Pawl.Engine's
-- PlayerEffect.inScope (PlayerScope.Opponents), Pawl.Engine.Replacement's
-- matchesZoneOwner (CR 400.3's owner, for a zone-change redirect) and
-- Pawl.Engine.Count's playersFor (the same PlayerRef under a Count).
-- Pawl.BattleSpec holds the seventh, CR 310.12a's protector candidates, beside
-- its siblings.
--
-- FOUR SEATS IN TWO TEAMS throughout, which is the smallest board on which CR
-- 102.3 and CR 806.1 disagree: at three seats in two teams a player has one
-- teammate and one opponent, so "every other player" and "every player not on my
-- team" differ by one seat and a reading that answered the WRONG one seat looks
-- like a reading that answered none. Four seats leave alice two opponents and one
-- teammate, so the count, the poison counters and the attack all separate three
-- readings: CR 102.3's, CR 806.1's free-for-all, and one that names nobody.
--
-- The teams are CR 808.2's -- each team sits together, so turn order
-- [alice, bob, carol, dave] puts alice's teammate bob in the seat a free-for-all
-- reading reaches FIRST. That is deliberate: a defective reading takes bob before
-- it takes anybody else, so no case here can pass by stopping early.
--
-- CR 808.3a needs nothing from these boards: the attack multiple players option
-- is already the one in use by default (Pawl.Types.GameSettings.attackOption),
-- which is why the combat case can read the whole defending group.
module Pawl.TeamSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone

-- CR 808.1 / CR 808.2: alice and bob against carol and dave, each team in
-- adjacent seats of the turn order [alice, bob, carol, dave].
twoTeams :: GameState.GameState -> GameState.GameState
twoTeams = S.inTeams [[S.alice, S.bob], [S.carol, S.dave]]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Teams" $ do
  -- CR 506.2a with CR 102.3: the attacking player's opponents are the defending
  -- players, and a teammate is not one of them.
  --
  -- TWO DECLARATIONS on ONE board, differing only in whom the Piker is announced
  -- as attacking, so the negative cannot pass for want of a legal attacker: the
  -- positive proves the same creature, on the same board, may attack an opponent.
  Spec.it s "CR 102.3 a creature cannot attack its controller's teammate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (mine, staged) = S.addCreature piker S.alice (twoTeams S.fourPlayerGame)
        board =
          staged
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
              GameState.remaining =
                Seq.fromList
                  [ Phase.Combat CombatStep.DeclareAttackers,
                    Phase.Combat CombatStep.DeclareBlockers,
                    Phase.Combat CombatStep.CombatDamage,
                    Phase.Combat CombatStep.EndOfCombat,
                    Phase.PostcombatMain,
                    Phase.Ending EndingStep.EndStep,
                    Phase.Ending EndingStep.Cleanup
                  ]
            }
        settled = S.runPure S.identityAnswer board (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
    Spec.assertBool
      s
      (not (Combat.legalAttackDeclarationAs S.alice [(mine, AttackTarget.OfPlayer S.bob)] settled))
      "CR 102.3 alice's creature may not attack her teammate bob"
    Spec.assertBool
      s
      (Combat.legalAttackDeclarationAs S.alice [(mine, AttackTarget.OfPlayer S.carol)] settled)
      "CR 102.3 the same creature may attack her opponent carol"
    -- CR 802.2 as the proxy behind both: the defending players are exactly the
    -- other team, in APNAP order.
    Spec.assertEqWith
      s
      "CR 802.2 the defending players are the other team"
      (Combat.Type.defenders (GameState.combat settled))
      [S.carol, S.dave]
  -- CR 102.3 through the effect DSL's PlayerRef.Relative Opponent, which
  -- Pawl.Engine.Resolve.Slots.playerRefPlayers resolves.
  --
  -- Prologue to Phyresis, {1}{U} Instant: "Each opponent gets a poison counter.
  -- Draw a card." The whole card is that one instruction plus a draw, so nothing
  -- else can move a counter, and the four totals are read as ONE list so that no
  -- seat's answer can be checked without the others.
  Spec.it s "CR 102.3 each opponent skips the teammate" $ do
    prologue <- S.printingOf s registry "Prologue to Phyresis"
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    let lands = S.landsFor island S.alice 2 (twoTeams S.fourPlayerGame)
        stocked = snd (S.addLibraryCard plains S.alice lands)
        (held, staged) = S.addHandCard prologue S.alice stocked
        board =
          staged
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        cast = S.runPure S.identityAnswer board (S.cast S.alice held)
        after = S.runPure S.identityAnswer cast Engine.priorityLoop
    Spec.assertEqWith
      s
      "CR 102.3 only carol and dave are poisoned"
      (fmap (\pid -> S.playerCounterOf PlayerCounterKind.Poison pid after) [S.alice, S.bob, S.carol, S.dave])
      [0, 0, 1, 1]
    Spec.assertEqWith s "the spell resolved" (GameState.stack after) []
  -- CR 102.3 through a TARGET SLOT, which neither case above reaches: the offer
  -- comes from Pawl.Engine.Target's Filter.IsPlayer atom rather than from a
  -- PlayerRef, and it is filtered against a Context built by Target.slotContext.
  --
  -- Ravenous Rats, {1}{B} Rat: "When this creature enters, target opponent
  -- discards a card." The OFFER is what is asserted, and it is the engine's own
  -- output: an answer naming bob would be filtered out rather than obeyed, so
  -- reading the offer is what distinguishes a slot that never admitted him.
  Spec.it s "CR 102.3 a target opponent slot does not offer the teammate" $ do
    rats <- S.printingOf s registry "Ravenous Rats"
    swamp <- S.printingOf s registry "Swamp"
    let lands = S.landsFor swamp S.alice 2 (twoTeams S.fourPlayerGame)
        (held, staged) = S.addHandCard rats S.alice lands
        board =
          staged
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        recording :: Prompt.Prompt r -> State.State [[Recipient.Recipient]] r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.modify' (<> fmap (Set.toAscList . snd) (Map.elems sets))
            pure (fmap snd sets)
          _ -> pure (S.identityAnswer p)
        cast = S.runPure S.identityAnswer board (S.cast S.alice held)
        offered = State.execState (Engine.runGame recording cast Engine.priorityLoop) []
    Spec.assertEqWith
      s
      "CR 102.3 only carol and dave are offered"
      offered
      [[Recipient.ToPlayer S.carol, Recipient.ToPlayer S.dave]]
  -- CR 702.11c through Pawl.Engine.PlayerEffect.inScope's PlayerScope.Opponents,
  -- which is the reader neither the PlayerRef nor the target-slot case above
  -- touches: "'Hexproof' on a player means 'You can't be the target of spells or
  -- abilities your opponents control.'"
  --
  -- Leyline of Sanctity on BOB, and the three readings come apart on one board:
  -- his teammate alice may still target him, his opponent carol may not, and he
  -- may target himself (rule 702.11c names only opponents). Pawl.TargetSpec's
  -- Leyline case is the same card with no teams, where alice is the opponent.
  Spec.it s "CR 702.11c hexproof from opponents does not stop a teammate" $ do
    leyline <- S.printingOf s registry "Leyline of Sanctity"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, warded) = S.addCreature leyline S.bob (twoTeams S.fourPlayerGame)
    case S.spellTargetSlot bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSlot -> do
        let legalFor who = Target.legalRecipients (Just who) S.noSource theSlot warded
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) (legalFor S.alice)) "CR 102.3 alice, bob's teammate, may still bolt him"
        Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.bob) (legalFor S.carol))) "CR 702.11c carol, his opponent, may not"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) (legalFor S.bob)) "and bob may bolt himself"
  -- CR 102.3 through the zone-owner reader of ControllerRelation.Opponents,
  -- Pawl.Engine.Replacement's matchesZoneOwner (judged, like its siblings, by
  -- relationHolds), which the three cases above do not reach: a zone change
  -- asks who OWNS the moving card (CR 400.3), not who controls anything.
  --
  -- Leyline of the Void, {2}{B}{B} Enchantment: "If a card would be put into an
  -- opponent's graveyard from anywhere, exile it instead." ONE board and one
  -- funnel, with two cards differing in nothing but their owner -- bob's, alice's
  -- teammate, and carol's, her opponent -- so the negative cannot pass for want
  -- of a working redirect: carol's card is exiled on the same board.
  Spec.it s "CR 102.3 a teammate's card is not put into an opponent's graveyard" $ do
    leyline <- S.printingOf s registry "Leyline of the Void"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature leyline S.alice (twoTeams S.fourPlayerGame)
        (teammates, g1) = S.addLibraryCard piker S.bob g0
        (opponents, g2) = S.addLibraryCard piker S.carol g1
        after = S.runPure S.identityAnswer g2 (Event.changeZone teammates Zone.Graveyard)
        alsoAfter = S.runPure S.identityAnswer after (Event.changeZone opponents Zone.Graveyard)
    Spec.assertEqWith s "CR 102.3 bob's card reached bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob alsoAfter)) 1
    Spec.assertEqWith s "and nothing of bob's was exiled" (length (Game.zoneMembers Zone.Exile S.bob alsoAfter)) 0
    Spec.assertEqWith s "CR 614.1a carol's was exiled instead" (length (Game.zoneMembers Zone.Exile S.carol alsoAfter)) 1
    Spec.assertEqWith s "and never reached carol's graveyard" (length (Game.zoneMembers Zone.Graveyard S.carol alsoAfter)) 0
  -- CR 102.3 through a Count over the same reference, which
  -- Pawl.Engine.Count.playersFor resolves and which no effect above reaches.
  --
  -- Tyranid Invasion, {3}{G} Sorcery: "Create a number of 3/3 green Tyranid
  -- Warrior creature tokens with trample equal to the number of opponents you
  -- have." Pawl.CountSpec's group is the same card at three seats with no teams,
  -- where the answer is 2 for a different reason; here three other seats make 2
  -- the team answer and 3 the free-for-all one.
  Spec.it s "CR 102.3 a count of opponents answers two rather than three" $ do
    invasion <- S.printingOf s registry "Tyranid Invasion"
    forest <- S.printingOf s registry "Forest"
    let lands = S.landsFor forest S.alice 4 (twoTeams S.fourPlayerGame)
        (held, staged) = S.addHandCard invasion S.alice lands
        board =
          staged
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        cast = S.runPure S.identityAnswer board (S.cast S.alice held)
        after = S.runPure S.identityAnswer cast Engine.priorityLoop
    Spec.assertEqWith s "one token per opponent, and bob is not one" (length (S.tokensOf after)) 2
    Spec.assertEqWith s "the spell resolved" (GameState.stack after) []
