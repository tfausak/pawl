{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Combat: attack/block legality, combat damage, and the combat
-- keywords (flying, reach, defender, vigilance, haste, first/double strike).
-- Also Pawl.BlockRequirement, whose only consumer is Pawl.Combat's CR 509.1c
-- check.
module Pawl.CombatSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Combat as Combat
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Registry as Registry.Type
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

combatDamageTests :: Registry.Type.Registry -> Tasty.TestTree
combatDamageTests registry =
  Tasty.testGroup
    "CombatDamage"
    [ HU.testCase "CR 510.1b an unblocked attacker damages the defending player" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoard piker 1 0
            after = S.fightWith S.aggressiveAnswer gs
        -- A Piker is a 2/1, and bob starts at 20.
        HU.assertEqual "bob took 2" (Just 18) (S.lifeOf S.bob after),
      HU.testCase "CR 509 a blocked attacker does not damage the player" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoard piker 1 1
            after = S.fightWith S.aggressiveAnswer gs
        HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 510.1c a single blocker takes all the damage, unprompted" $ do
        -- If the engine wrongly prompts here, this interpreter answers with an
        -- empty division, which is illegal (it does not total the attacker's
        -- power), so it is rejected and the blocker takes 0 -- and the assertion
        -- below fails. That is why this proves "unprompted" without an `error`,
        -- which the no-partial-functions rule forbids anyway.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = S.combatBoard piker 1 1
            noAssign :: Prompt.Prompt r -> r
            noAssign p = case p of
              Prompt.AssignCombatDamage {} -> Map.empty
              _ -> S.aggressiveAnswer p
            after = S.fightWith noAssign gs
        case theirs of
          [] -> HU.assertFailure "fixture should have a blocker"
          b : _ -> HU.assertEqual "took 2" (Just 2) (S.damageOf b after),
      HU.testCase "CR 510.2 a 2/1 trade kills BOTH creatures" $ do
        -- The simultaneity test. Sequential damage kills only one, because the
        -- blocker would be in the graveyard before it dealt its damage.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoard piker 1 1
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
        HU.assertEqual "alice's is dead" 0 (S.creaturesInPlay S.alice after)
        HU.assertEqual "bob's is dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 510.1c a free division of 2 across two blockers kills both" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = S.combatBoard piker 1 2
            split :: Prompt.Prompt r -> r
            split p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (fmap (\r -> (r, 1)) (filter S.isCreatureRecipient (Map.keys thresholds)))
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith split gs)
        HU.assertEqual "both blockers dead" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "expected two blockers" 2 (length theirs),
      HU.testCase "CR 510.1c the same 2 damage on one blocker kills only it" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoard piker 1 2
            dump :: Prompt.Prompt r -> r
            dump p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter S.isCreatureRecipient (Map.keys thresholds) of
                  r : _ -> Map.singleton r n
                  [] -> Map.empty
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith dump gs)
        HU.assertEqual "one blocker survives" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 510.1e an illegal division is rejected and deals nothing" $ do
        -- Not a reachable game state: this is the engine's defense against a
        -- broken interpreter. See the spec, section 3.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoard piker 1 2
            cheat :: Prompt.Prompt r -> r
            cheat p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (fmap (\r -> (r, 99)) (filter S.isCreatureRecipient (Map.keys thresholds)))
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith cheat gs)
        HU.assertEqual "both blockers survive" 2 (S.creaturesInPlay S.bob after),
      -- The deterministic successor to the retired "combat happens" property: an
      -- unblocked 2/1 attacker reduces the defender's life by its power.
      HU.testCase "combat deals damage to the defending player" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [piker] []
            after = S.runCombat S.aggressiveAnswer gs
        HU.assertEqual "defender took two" (Just 18) (S.lifeOf S.bob after)
    ]

declaredAttackers :: GameState.GameState -> [ObjectId.ObjectId]
declaredAttackers gs = Map.keys (Combat.Type.attackers (GameState.combat gs))

declareTests :: Registry.Type.Registry -> Tasty.TestTree
declareTests registry =
  Tasty.testGroup
    "Declare"
    [ HU.testCase "CR 508.1f declaring an attacker taps it" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoard piker 1 1
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
        HU.assertEqual "one attacker" mine (declaredAttackers after)
        HU.assertEqual "tapped" 1 (S.tappedCount S.alice after),
      HU.testCase "CR 508.1 attackers attack the defending player" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoard piker 1 1
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
        case mine of
          [] -> HU.assertFailure "fixture should have an attacker"
          oid : _ ->
            HU.assertEqual
              "attacking bob"
              (Just (AttackTarget.OfPlayer S.bob))
              (Map.lookup oid (Combat.Type.attackers (GameState.combat after))),
      HU.testCase "an illegal attacker in the answer is dropped" $ do
        -- The interpreter names bob's creature. It is not alice's to attack with.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = S.combatBoard piker 1 1
            liar :: Prompt.Prompt r -> r
            liar p = case p of
              Prompt.DeclareAttackers {} -> theirs
              _ -> S.aggressiveAnswer p
            after = snd (Engine.runGamePure liar gs (Combat.declareAttackers S.alice))
        HU.assertEqual "nothing attacks" [] (declaredAttackers after),
      HU.testCase "CR 509.1 a blocker is recorded against the attacker it blocks" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, theirs) = S.combatBoard piker 1 1
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
        case mine of
          [] -> HU.assertFailure "fixture should have an attacker"
          attacker : _ ->
            HU.assertEqual "blocked by bob's creature" (Set.fromList theirs) (Combat.blockersOf attacker after),
      HU.testCase "an unblocked attacker has no blockers" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoard piker 1 0
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
        case mine of
          [] -> HU.assertFailure "fixture should have an attacker"
          attacker : _ -> HU.assertBool "unblocked" (not (Combat.isBlocked attacker after)),
      HU.testCase "no legal attackers means no prompt and no attacks" $ do
        -- combatBoard 0 1 gives alice nothing. A prompt here would be the engine
        -- asking a question with exactly one answer.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoard piker 0 1
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
        HU.assertEqual "nothing attacks" [] (declaredAttackers after),
      -- The end-to-end summoning sickness scenario the spec names: a creature
      -- that just arrived cannot attack, and the SAME creature can once its
      -- controller's untap step has settled it. The halves are tested in Tasks 1
      -- and 4; this proves they compose.
      HU.testCase "CR 302.6 a creature cannot attack the turn it arrives, and can after untapping" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoard piker 1 1
            arrived = justArrived gs
            sameTurn = snd (Engine.runGamePure S.aggressiveAnswer arrived (Combat.declareAttackers S.alice))
            nextTurn =
              snd
                . Engine.runGamePure S.aggressiveAnswer arrived
                $ do
                  Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap)
                  Combat.declareAttackers S.alice
        HU.assertEqual "cannot attack the turn it arrives" [] (declaredAttackers sameTurn)
        HU.assertEqual "can attack after untapping" 1 (length (declaredAttackers nextTurn))
    ]

defenderTests :: Registry.Type.Registry -> Tasty.TestTree
defenderTests registry =
  Tasty.testGroup
    "Defender"
    [ HU.testCase "CR 702.3b a creature with defender can't attack" $ do
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [ogreSentry] [piker]
        case mine of
          [] -> HU.assertFailure "fixture should have one creature"
          oid : _ -> HU.assertBool "can't attack" (not (Combat.canAttack S.alice oid gs)),
      HU.testCase "CR 702.3b a creature with defender is not offered as a legal attacker" $ do
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [ogreSentry] [piker]
        HU.assertEqual "none" [] (Combat.legalAttackers S.alice gs),
      HU.testCase "CR 702.3b defender does not stop it blocking" $ do
        -- 702.3b says "can't attack" and nothing else. A defender that could not
        -- block would be a Wall in the pre-2004 sense, and that is not the rule.
        piker <- Registry.printing registry "Goblin Piker"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs, _, theirs) = S.combatBoardOf [piker] [ogreSentry]
        case theirs of
          [] -> HU.assertFailure "fixture should have one blocker"
          oid : _ -> HU.assertBool "may block" (Combat.canBlock S.bob oid gs),
      HU.testCase "a creature without defender is still offered" $ do
        -- The control. If defender were implemented as "nothing may attack", the
        -- test above would pass and this one would fail.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        HU.assertEqual "one" mine (Combat.legalAttackers S.alice gs),
      HU.testCase "CR 702.3b a defender is skipped but its neighbor still attacks" $ do
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [ogreSentry, piker] [piker]
        case mine of
          [_, p] -> HU.assertEqual "only the piker" [p] (Combat.legalAttackers S.alice gs)
          _ -> HU.assertFailure "fixture should have two creatures"
    ]

-- Answers Prompt.ChooseDefender with a named player and records that it was
-- asked; everything else delegates, so the wildcard keeps this out of the
-- -Werror exhaustiveness net.
choosesDefender :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
choosesDefender who p = case p of
  Prompt.ChooseDefender _ asker _ -> do
    State.modify' (\seen -> seen <> [asker])
    pure who
  _ -> pure (S.identityAnswer p)

-- Sibling of choosesDefender that records the prompt's Decider instead of its
-- PlayerId subject. CR 723.1/723.5: while a player is controlled, their
-- controller makes their choices, and Combat.chooseDefender's
-- `Decide.deciderFor pid gs` is what routes ChooseDefender there. choosesDefender
-- above discards the Decider entirely, so it cannot tell that routing apart from
-- a regression to the raw active-player id; this helper is what makes the
-- Decider observable without touching the six existing cases.
choosesDefenderRecordingDecider :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State [Decider.Decider] r
choosesDefenderRecordingDecider who p = case p of
  Prompt.ChooseDefender decider _ _ -> do
    State.modify' (\seen -> seen <> [decider])
    pure who
  _ -> pure (S.identityAnswer p)

-- Records the PlayerId of every Prompt.DeclareBlockers ask AND the blocker
-- candidates that ask was offered, then blocks the first attacker with
-- everything. Two accumulators threaded as one State pair: the ASK is what
-- CR 509.1 is about (who declares), and the OFFER is what CR 509.1a is about
-- (whose creatures are even eligible). Everything else delegates, so the
-- wildcard keeps this out of the -Werror exhaustiveness net.
recordingBlockers :: Prompt.Prompt r -> State.State ([PlayerId.PlayerId], [ObjectId.ObjectId]) r
recordingBlockers p = case p of
  Prompt.DeclareBlockers _ pid candidates attackers -> do
    State.modify' (\(asks, offers) -> (asks <> [pid], offers <> candidates))
    pure $ case attackers of
      [] -> Map.empty
      a : _ -> Map.fromList (fmap (\b -> (b, a)) candidates)
  _ -> pure (S.identityAnswer p)

-- Run Combat.declareBlockers under recordingBlockers. State.runState (State s a)
-- s0 :: (a, s), so the tuple comes back (final state, accumulators) and this
-- flips it to put the accumulators first.
runRecordingBlockers :: GameState.GameState -> (([PlayerId.PlayerId], [ObjectId.ObjectId]), GameState.GameState)
runRecordingBlockers gs =
  let (after, seen) = State.runState (Program.foldProgramM recordingBlockers (State.execStateT Combat.declareBlockers gs)) ([], [])
   in (seen, after)

-- CR 506.2/506.2a/507.1/703.4h: WHO is being attacked. Distinct from
-- defenderTests, which is the Defender KEYWORD (CR 702.3b).
defendingPlayerTests :: Registry.Type.Registry -> Tasty.TestTree
defendingPlayerTests registry =
  Tasty.testGroup
    "DefendingPlayer"
    [ HU.testCase "CR 703.4h/507.1 the active player chooses which opponent is the defending player" $
        -- THREE seats: the whole point. Discriminating against the behaviour this
        -- phase replaces -- taking the head of the candidate list -- because carol
        -- is not the head. Under head-of-list the answer is ignored and bob
        -- defends, so this exact assertion cannot pass.
        --
        -- State.runState (State s a) s0 :: (a, s): here `a` is the GameState
        -- returned by execStateT/foldProgramM and `s` is choosesDefender's own
        -- accumulator, so the tuple comes back (after, asked).
        let (after, asked) =
              State.runState
                (Program.foldProgramM (choosesDefender S.carol) (State.execStateT (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat)) S.threePlayerGame))
                []
         in do
              HU.assertEqual "carol is the defending player" (Just S.carol) (Combat.Type.defender (GameState.combat after))
              HU.assertEqual "and alice, the active player, is who was asked" [S.alice] asked,
      HU.testCase "CR 723.1 a controlled active player's choice of defender routes to their controller" $
        -- THREE seats (alice active, bob, carol) with carol controlling alice
        -- (Mindslaver-style: GameState.activeControl = Just (MkDecider carol)),
        -- the established fixture idiom for a controlled player -- the same shape
        -- GameSpec's CR 723.6 concede test (GameSpec.hs:811-858) and DecideSpec's
        -- CR 723.3 case (DecideSpec.hs:19-23) both set up. Three seats, not two, so
        -- attackableOpponents is [bob, carol] -- a REAL choice -- rather than
        -- CR 506.2's one-candidate elision, which would never build a prompt at all
        -- and so could never discriminate the Decider on one.
        --
        -- CR 723.1: "The affected player is controlled during the entire turn"
        -- (the rest of the rule scopes this to the player's own next turn, which
        -- does not change the point here). Combined with CR 723.5 (the controller
        -- makes the controlled player's choices) and CR 723.3 (the controlled
        -- player is still the active player), alice remains the active player
        -- named in the prompt but carol is who must be asked. Combat.chooseDefender
        -- gets this right by computing `Decide.deciderFor pid gs` rather than
        -- defaulting to `Decider.MkDecider pid`.
        --
        -- Discriminates exactly that regression: a `chooseDefender` that used
        -- `Decider.MkDecider pid` (the raw active player, alice) instead of
        -- `Decide.deciderFor pid gs` would record `[Decider.MkDecider S.alice]`
        -- below -- handing alice's own choice back to her, which is the CR 723.1
        -- violation this test exists to catch -- and none of the other six cases in
        -- this group sets activeControl, so none of them would notice.
        let controlled = S.threePlayerGame {GameState.activeControl = Just (Decider.MkDecider S.carol)}
            (_, deciders) =
              State.runState
                (Program.foldProgramM (choosesDefenderRecordingDecider S.bob) (State.execStateT (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat)) controlled))
                []
         in HU.assertEqual "carol, alice's controller, is who was asked" [Decider.MkDecider S.carol] deciders,
      HU.testCase "CR 506.2 two players: the nonactive player defends and nobody is asked" $
        -- The elision, asserted explicitly rather than inferred from the suite
        -- staying green. CR 507.1's condition is a MULTIPLAYER game; CR 506.2's
        -- second sentence settles a two-player game with nothing to ask.
        -- Discriminating twice over: an implementation that prompted anyway would
        -- put alice in `asked`, and one that skipped the prompt AND the write
        -- would leave Nothing, which Task 4 turns into "no attack is possible".
        let (after, asked) =
              State.runState
                (Program.foldProgramM (choosesDefender S.alice) (State.execStateT (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat)) (Setup.emptyGame S.bothPlayers)))
                []
         in do
              HU.assertEqual "bob defends" (Just S.bob) (Combat.Type.defender (GameState.combat after))
              HU.assertEqual "nobody was asked" [] asked,
      HU.testCase "CR 507.1 a multiplayer game down to one opponent is not asked either" $
        -- The case #169 is actually about: CR 703.4h still applies (the game BEGAN
        -- with three players, CR 800.1), and the choice has one candidate.
        -- Discriminating against an elision keyed on the SEAT COUNT rather than on
        -- the candidate count -- that version would prompt here.
        let gone = Departure.depart Departure.Type.Conceded S.carol S.threePlayerGame
            (after, asked) =
              State.runState
                (Program.foldProgramM (choosesDefender S.carol) (State.execStateT (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat)) gone))
                []
         in do
              HU.assertEqual "bob, the only one left" (Just S.bob) (Combat.Type.defender (GameState.combat after))
              HU.assertEqual "nobody was asked" [] asked,
      HU.testCase "CR 507.1 with no opponents left the action does not happen at all" $
        -- Not reachable in a running game (CR 104.2a ends it), but the branch has
        -- to be total and NonEmpty is why. Discriminating against an
        -- implementation that built the prompt from an empty list.
        let alone = Departure.depart Departure.Type.Conceded S.carol (Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame)
            (after, asked) =
              State.runState
                (Program.foldProgramM (choosesDefender S.bob) (State.execStateT (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat)) alone))
                []
         in do
              HU.assertEqual "nobody defends" Nothing (Combat.Type.defender (GameState.combat after))
              HU.assertEqual "nobody was asked" [] asked,
      HU.testCase "CR 800.4h #181 a turn whose active player has left chooses no defending player, diverging from the next-seat reassignment" $
        -- CR 800.4j: the turn continues without an active player, so the action
        -- the rules assign to the active player has no subject. CR 800.4j is a
        -- priority rule and licenses no more than that; CR 800.4h would hand the
        -- choice to the next player in turn order rather than drop it, so what is
        -- asserted here is pawl's unobservable divergence from CR 800.4h (#181),
        -- not a rules-required outcome. THREE seats, so that two opponents survive and the choice would
        -- otherwise be a real prompt -- at two seats the elision would hide the
        -- guard entirely.
        --
        -- This drives Engine.runTurnBasedActions, and the guard exists at BOTH
        -- ends of that call (Engine.hs's hasActive and chooseDefender's own test),
        -- so this case passes with either one alone and isolates neither. The
        -- sibling case below is the one that isolates chooseDefender's; the
        -- engine-side copy is redundant on this path and has nothing to isolate.
        let gone = Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame
            (after, asked) =
              State.runState
                (Program.foldProgramM (choosesDefender S.carol) (State.execStateT (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat)) gone))
                []
         in do
              HU.assertEqual "no defending player" Nothing (Combat.Type.defender (GameState.combat after))
              HU.assertEqual "and nobody was asked" [] asked,
      HU.testCase "CR 800.4j chooseDefender called directly still chooses nobody" $
        -- The same rule at the other end of the call, reached WITHOUT
        -- Engine.runTurnBasedActions so that only chooseDefender's own membership
        -- test can be responsible. Discriminating exactly that line: with it gone,
        -- alice -- who has left the game -- is asked, and carol becomes the
        -- defending player on a turn CR 800.4j says has no active player to choose
        -- one. Three seats again, so two candidates survive alice's departure and
        -- the single-candidate elision (#169) cannot be what suppresses the ask.
        let gone = Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame
            (after, asked) =
              State.runState
                (Program.foldProgramM (choosesDefender S.carol) (State.execStateT Combat.chooseDefender gone))
                []
         in do
              HU.assertEqual "no defending player" Nothing (Combat.Type.defender (GameState.combat after))
              HU.assertEqual "and nobody was asked" [] asked,
      HU.testCase "CR 507.1 an answer that is not one of the candidates falls back to the first" $
        -- A broken interpreter, not a game state: it names the ACTIVE player.
        -- Discriminating against `defender = Just answer` unchecked, which would
        -- let alice attack herself and, once Task 4 lands, deal combat damage to
        -- the attacking player.
        let (after, _) =
              State.runState
                (Program.foldProgramM (choosesDefender S.alice) (State.execStateT (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat)) S.threePlayerGame))
                []
         in HU.assertEqual "the first candidate, never the active player" (Just S.bob) (Combat.Type.defender (GameState.combat after)),
      HU.testCase "CR 506.2a the candidates are every other player still in the game" $
        -- Three seats, because two cannot tell "the chosen opponent" from "the
        -- only opponent". Discriminating: an implementation that forgot to drop
        -- the active player would answer [alice, bob, carol].
        HU.assertEqual "bob and carol" [S.bob, S.carol] (Combat.attackableOpponents S.threePlayerGame),
      HU.testCase "CR 102.1 a player who has left the game is not a candidate" $
        -- CR 102.1: "A player is one of the people in the game." Four seats so
        -- that TWO candidates survive one departure -- with three seats the
        -- surviving list is a singleton and cannot distinguish "filtered" from
        -- "truncated to one".
        let gone = Departure.depart Departure.Type.Conceded S.bob S.fourPlayerGame
         in do
              HU.assertEqual "bob is dropped, carol and dave remain" [S.carol, S.dave] (Combat.attackableOpponents gone)
              HU.assertEqual "and before he left there were three" [S.bob, S.carol, S.dave] (Combat.attackableOpponents S.fourPlayerGame),
      HU.testCase "CR 506.2a the candidates come back in SEATING order, not player-id order" $
        -- The discriminator between Game.stillPlayingInOrder and
        -- Game.stillPlaying: seated carol-alice-bob with alice attacking,
        -- seating order gives [carol, bob] and the players map gives [bob, carol].
        -- Every other fixture in the suite is seated ascending, so this is the
        -- only place the two readings disagree.
        let rotated = (Setup.emptyGame (S.carol NonEmpty.:| [S.alice, S.bob])) {GameState.activePlayer = S.alice}
         in HU.assertEqual "carol's seat comes first" [S.carol, S.bob] (Combat.attackableOpponents rotated),
      HU.testCase "CR 703.4h no defending player has been chosen before the beginning of combat step" $
        -- Discriminating: a field defaulted to Just <somebody> would let a board
        -- that has never run the turn-based action declare attackers.
        HU.assertEqual "empty combat names nobody" Nothing (Combat.Type.defender (GameState.combat S.threePlayerGame)),
      HU.testCase "CR 506.2 the designation does not outlive the combat phase" $
        -- CR 506.2's sentences are all scoped "During the combat phase", and
        -- CR 703.4h makes the choice per beginning-of-combat step, so a second
        -- combat phase in one turn chooses again. Discriminating: a clearCombat
        -- that reset only attackers and blockers would leave Just carol here, and
        -- the next combat phase would inherit a stale defender.
        let busy = S.threePlayerGame {GameState.combat = (GameState.combat S.threePlayerGame) {Combat.Type.defender = Just S.carol}}
         in HU.assertEqual "cleared at end of combat" Nothing (Combat.Type.defender (GameState.combat (Combat.clearCombat busy))),
      HU.testCase "CR 508.1 every attacker attacks the CHOSEN defending player" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (board, mine, _, _) = S.threePlayerCombat [piker, piker] [piker] [piker]
            -- carol, deliberately not the first candidate.
            ready =
              board
                { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
                  GameState.combat = (GameState.combat board) {Combat.Type.defender = Just S.carol}
                }
            after = S.runPure S.aggressiveAnswer ready (Combat.declareAttackers S.alice)
        HU.assertEqual "both of alice's creatures attack" 2 (length mine)
        -- Discriminating: under the head-of-list behaviour this phase replaces,
        -- every value here is OfPlayer bob, because bob is the first candidate.
        HU.assertEqual
          "and both attack carol"
          (Map.fromList (fmap (\oid -> (oid, AttackTarget.OfPlayer S.carol)) mine))
          (Combat.Type.attackers (GameState.combat after)),
      HU.testCase "CR 508.1 with no defending player chosen, nothing attacks" $ do
        -- Discriminating against a declareAttackers that fell back to computing a
        -- defender when the field is Nothing -- which is the head-of-list
        -- behaviour wearing a different hat. The answerer is maximal (it attacks
        -- with everything offered), so an empty attacker map can only come from
        -- the prompt never being issued.
        piker <- Registry.printing registry "Goblin Piker"
        let (board, mine, _, _) = S.threePlayerCombat [piker] [piker] [piker]
            ready = board {GameState.phase = Phase.Combat CombatStep.DeclareAttackers}
            after = S.runPure S.aggressiveAnswer ready (Combat.declareAttackers S.alice)
        HU.assertEqual "alice really had a legal attacker" [True] (fmap (\oid -> Combat.canAttack S.alice oid ready) mine)
        HU.assertEqual "nobody attacked" Map.empty (Combat.Type.attackers (GameState.combat after))
        HU.assertEqual "and nothing was tapped" 0 (S.tappedCount S.alice after),
      HU.testCase "CR 509.1 only the defending player is asked to declare blockers" $ do
        -- CR 509.1's first sentence names THE defending player, singular. CR 802.4
        -- is the rule that has several of them declare in APNAP order, and it needs
        -- an option pawl cannot express (#175).
        piker <- Registry.printing registry "Goblin Piker"
        let (board, mine, bobs, carols) = S.threePlayerCombat [piker] [piker] [piker]
            attackMap = case mine of
              oid : _ -> Map.singleton oid (AttackTarget.OfPlayer S.carol)
              [] -> Map.empty
            ready =
              board
                { GameState.phase = Phase.Combat CombatStep.DeclareBlockers,
                  GameState.combat =
                    (GameState.combat board)
                      { Combat.Type.defender = Just S.carol,
                        Combat.Type.attackers = attackMap
                      }
                }
            ((asked, offeredBlockers), after) = runRecordingBlockers ready
        HU.assertEqual "the fixture gave bob and carol a blocker each" (1, 1) (length bobs, length carols)
        -- Discriminating: the behaviour this phase replaces loops over every
        -- opponent, so `asked` would be [bob, carol].
        HU.assertEqual "only carol was asked" [S.carol] asked
        -- And bob's untapped creature is never offered, per CR 509.1a.
        HU.assertEqual "bob's creature is in no candidate list" [] (filter (\oid -> elem oid bobs) offeredBlockers)
        HU.assertEqual "carol's block was recorded" 1 (Map.size (Combat.Type.blockers (GameState.combat after))),
      HU.testCase "CR 725.2/507.1 the crown follows whichever opponent was chosen as the defending player" $ do
        -- CR 725.2's second inherent ability: "Whenever a creature deals combat
        -- damage to the monarch, its controller becomes the monarch." bob is the
        -- monarch. alice attacks with an unblocked 2/1; the two runs differ ONLY
        -- in the answer to Prompt.ChooseDefender.
        --
        -- Discriminating: run A is what the deleted head-of-list behaviour did
        -- whatever the answer, so run A alone proves nothing. Run B is
        -- unreachable under it, and the pair is the proof.
        piker <- Registry.printing registry "Goblin Piker"
        let (board, _, _, _) = S.threePlayerCombat [piker] [] []
            crowned = S.withMonarch S.bob board
            hitBob = S.runCombat (S.attackTo S.bob) crowned
            hitCarol = S.runCombat (S.attackTo S.carol) crowned
        -- Run A: attacking the monarch takes the crown.
        HU.assertEqual "bob took 2" (Just 18) (S.lifeOf S.bob hitBob)
        HU.assertEqual "carol was untouched" (Just 20) (S.lifeOf S.carol hitBob)
        HU.assertEqual "alice is the monarch" (Just S.alice) (GameState.monarch hitBob)
        -- Run B: attacking the other opponent does not.
        HU.assertEqual "carol took 2" (Just 18) (S.lifeOf S.carol hitCarol)
        HU.assertEqual "bob was untouched" (Just 20) (S.lifeOf S.bob hitCarol)
        HU.assertEqual "bob keeps the crown" (Just S.bob) (GameState.monarch hitCarol)
        -- And neither run ended the game, so both really played a whole combat.
        HU.assertEqual "no result in run A" Nothing (GameState.result hitBob)
        HU.assertEqual "no result in run B" Nothing (GameState.result hitCarol)
    ]

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- Re-sicken alice's creatures, as though they had just resolved this turn.
justArrived :: GameState.GameState -> GameState.GameState
justArrived gs =
  let sicken o = if Object.owner o == S.alice then o {Object.sickness = Sickness.Sick} else o
   in gs {GameState.objects = fmap sicken (GameState.objects gs)}

hasteTests :: Registry.Type.Registry -> Tasty.TestTree
hasteTests registry =
  Tasty.testGroup
    "Haste"
    [ HU.testCase "CR 702.10b a creature with haste attacks the turn it arrives" $ do
        goblinChariot <- Registry.printing registry "Goblin Chariot"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [goblinChariot] [piker]
            after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.alice))
        HU.assertEqual "attacks" 1 (length (declaredAttackers after)),
      HU.testCase "CR 302.6 the same creature without haste cannot" $ do
        -- The control. Goblin Chariot and Goblin Piker are both 2/2-ish Goblin
        -- Warriors; the ONLY difference the engine can see is the keyword.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [piker] [piker]
            after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.alice))
        HU.assertEqual "cannot attack" [] (declaredAttackers after),
      HU.testCase "CR 702.10b haste is not needed once the creature has settled" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [piker] [piker]
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
        HU.assertEqual "attacks" mine (declaredAttackers after),
      -- The same contrast one layer up: haste GRANTED by a static ability rather
      -- than printed. Concordant Crossroads says "All creatures have haste", so
      -- the very Piker that could not attack in the control case above now can,
      -- and nothing about the Piker itself changed.
      HU.testCase "CR 702.10b Concordant Crossroads grants haste, so a summoning-sick Piker attacks" $ do
        crossroads <- Registry.printing registry "Concordant Crossroads"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [piker] [piker]
            (_, enchanted) = S.addCreature crossroads S.alice (justArrived gs)
            after = snd (Engine.runGamePure S.aggressiveAnswer enchanted (Combat.declareAttackers S.alice))
        HU.assertEqual "attacks anyway" mine (declaredAttackers after),
      HU.testCase "CR 702.10b a hasty creature and a sick one, in the same declaration" $ do
        -- Both sick; only the Chariot may attack. A blanket "sickness ignored"
        -- bug would let both through.
        goblinChariot <- Registry.printing registry "Goblin Chariot"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [goblinChariot, piker] [piker]
            after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.alice))
        case mine of
          [chariot, _] -> HU.assertEqual "only the chariot" [chariot] (declaredAttackers after)
          _ -> HU.assertFailure "fixture should have two creatures"
    ]

controlChangeSicknessTests :: Registry.Type.Registry -> Tasty.TestTree
controlChangeSicknessTests registry =
  Tasty.testGroup
    "ControlChangeSickness"
    [ -- A live steal, with nothing forced: bob's Piker settles under bob at his
      -- untap step, then alice's Control Magic takes it. CR 302.6 asks whether
      -- ALICE has controlled it continuously since HER most recent turn began,
      -- and she has not -- the settle it carries is bob's, not hers.
      HU.testCase "CR 302.6 a creature that just changed control is summoning sick (no haste)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.bob base
            settled = S.runPure S.identityAnswer withCreature (Engine.settleAll S.bob)
            (aura, withAura) = S.addCreature controlMagic S.alice settled
            attached = S.attach aura creature withAura
        HU.assertEqual "alice controls it" (Just S.alice) (Projection.controllerOf creature attached)
        HU.assertBool "but it is summoning sick, so it cannot attack this turn" (not (Combat.canAttack S.alice creature attached)),
      -- CR 302.6 asks for control held CONTINUOUSLY. bob's Control Magic takes
      -- alice's settled Piker; alice later removes the Aura and gets the Piker
      -- back (CR 604.2). Control is hers again and was hers when her turn began,
      -- but not for the whole span between, so she still may not attack with it.
      --
      -- Reachable with the pool as it stands: Control Magic is a sorcery-speed
      -- Aura, so bob can only cast it on his own turn, and alice can only answer
      -- it on hers -- after her untap step has already passed.
      HU.testCase "CR 302.6 control that leaves and returns is not continuous" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.alice base
            settled = S.runPure S.identityAnswer withCreature (Engine.settleAll S.alice)
            (aura, withAura) = S.addCreature controlMagic S.bob settled
            -- The steal is observed the next time the board settles -- the CR
            -- 117.5 sweep, which runs wherever the board can change.
            stolen = S.runPure S.identityAnswer (S.attach aura creature withAura) Engine.settleForPriority
            returned = S.runPure S.identityAnswer stolen (Event.changeZone aura Zone.Graveyard)
        HU.assertEqual "bob held it" (Just S.bob) (Projection.controllerOf creature stolen)
        HU.assertEqual "alice has it back" (Just S.alice) (Projection.controllerOf creature returned)
        HU.assertBool "but not continuously, so it cannot attack" (not (Combat.canAttack S.alice creature returned))
    ]

-- Declare attackers with everything, then hand back the state and the ids.
attacking :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
attacking mine theirs =
  let (gs, ours, yours) = S.combatBoardOf mine theirs
      after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
   in (after, ours, yours)

-- CR 702.36: grant fear to `oid` with a stored continuous effect. No card in the
-- pool has PRINTED fear (Aphotic Wisps grants it at instant speed, which combat
-- fixtures cannot reach mid-step), so this is the M2c granted-keyword posture.
withFear :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withFear oid gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = oid,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Fear,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

evasionTests :: Registry.Type.Registry -> Tasty.TestTree
evasionTests registry =
  Tasty.testGroup
    "Evasion"
    [ HU.testCase "CR 702.9b a declaration in which a ground creature blocks a flier is illegal" $ do
        birdMaiden <- Registry.printing registry "Bird Maiden"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, theirs) = attacking [birdMaiden] [piker]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "illegal" (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs))
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.17b a reach creature may block a flier" $ do
        -- THE FALSIFIER. Fails against any implementation that asks "does the
        -- blocker have flying?"
        birdMaiden <- Registry.printing registry "Bird Maiden"
        nimbleBirdsticker <- Registry.printing registry "Nimble Birdsticker"
        let (gs, mine, theirs) = attacking [birdMaiden] [nimbleBirdsticker]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.9b a flier may block a ground creature" $ do
        -- The asymmetry: 702.9b's second sentence. Fails if flying is implemented
        -- as a symmetric predicate.
        piker <- Registry.printing registry "Goblin Piker"
        birdMaiden <- Registry.printing registry "Bird Maiden"
        let (gs, mine, theirs) = attacking [piker] [birdMaiden]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.9b a flier may block a flier" $ do
        birdMaiden <- Registry.printing registry "Bird Maiden"
        let (gs, mine, theirs) = attacking [birdMaiden] [birdMaiden]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 509.1a a ground creature is still a legal blocker while a flier attacks" $ do
        -- 509.1a is about the blocker ALONE: it can block SOMETHING. This test
        -- fails if evasion is wrongly implemented as a filter on the candidates.
        birdMaiden <- Registry.printing registry "Bird Maiden"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = attacking [birdMaiden] [piker]
        HU.assertEqual "still offered" theirs (Combat.legalBlockers S.bob gs),
      HU.testCase "CR 509.1b an illegal declaration is rejected WHOLE, not repaired" $ do
        -- aggressiveAnswer blocks the first attacker with EVERYTHING, so bob
        -- declares the reach creature (legal) AND the Piker (illegal) on the
        -- flier. Neither may block. A per-pair filter would drop the Piker and
        -- let the Birdsticker's block stand -- which is what M1b does today, and
        -- is unsound: under menace, dropping one blocker from a pair manufactures
        -- an illegal single block.
        birdMaiden <- Registry.printing registry "Bird Maiden"
        nimbleBirdsticker <- Registry.printing registry "Nimble Birdsticker"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [birdMaiden] [nimbleBirdsticker, piker]
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
        case Map.keys (Combat.Type.attackers (GameState.combat after)) of
          [] -> HU.assertFailure "fixture should have an attacker"
          a : _ -> HU.assertEqual "nobody blocks" Set.empty (Combat.blockersOf a after),
      HU.testCase "CR 509.1b a wholly legal declaration is accepted" $ do
        -- The control for the test above: with only the reach creature, the same
        -- interpreter produces a legal declaration and the block stands.
        birdMaiden <- Registry.printing registry "Bird Maiden"
        nimbleBirdsticker <- Registry.printing registry "Nimble Birdsticker"
        let (gs, _, theirs) = S.combatBoardOf [birdMaiden] [nimbleBirdsticker]
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
        case Map.keys (Combat.Type.attackers (GameState.combat after)) of
          [] -> HU.assertFailure "fixture should have an attacker"
          a : _ -> HU.assertEqual "the reach creature blocks" (Set.fromList theirs) (Combat.blockersOf a after),
      HU.testCase "CR 509.1a a Mountain is not a legal blocker, flier or no flier" $ do
        -- The classification, from the other side: `canBlock` asks
        -- is-it-a-creature, never which card it is. M1b (tests cards) "a land may not
        -- attack" but never that a land may not BLOCK, so this closes a real gap
        -- rather than restating one.
        birdMaiden <- Registry.printing registry "Bird Maiden"
        mountain <- Registry.printing registry "Mountain"
        let (gs, mine, _) = attacking [birdMaiden] []
            withLand = snd (S.addCreature mountain S.bob gs)
        case mine of
          [] -> HU.assertFailure "fixture should have an attacker"
          _ : _ -> HU.assertEqual "no legal blockers" [] (Combat.legalBlockers S.bob withLand),
      HU.testCase "CR 702.9b a flier connects past an untapped ground creature, in a real combat" $ do
        -- The integration case, and it is precise rather than vacuous. WITH
        -- flying: nothing may block, bob takes 1, and both creatures live.
        -- WITHOUT flying: the Piker blocks, bob takes 0, and the two TRADE (Bird
        -- Maiden is 1/2, Piker is 2/1). All three assertions distinguish them.
        birdMaiden <- Registry.printing registry "Bird Maiden"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [birdMaiden] [piker]
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
        HU.assertEqual "bob took 1" (Just 19) (S.lifeOf S.bob after)
        HU.assertEqual "the flier lives" 1 (S.creaturesInPlay S.alice after)
        HU.assertEqual "the would-be blocker lives" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 702.36b a red creature may not block a creature with fear" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs0, mine, theirs) = attacking [piker] [piker]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "illegal" (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0)))
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.36b a black creature may block a creature with fear" $ do
        piker <- Registry.printing registry "Goblin Piker"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let (gs0, mine, theirs) = attacking [piker] [typhoidRats]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0))
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.36b an ARTIFACT creature may block a creature with fear" $ do
        -- THE FALSIFIER for reading 702.36b as a colour test alone: Darksteel Myr
        -- is a colourless artifact creature and blocks legally.
        piker <- Registry.printing registry "Goblin Piker"
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let (gs0, mine, theirs) = attacking [piker] [darksteelMyr]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0))
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.36b a devoid creature with a black mana cost may not block a creature with fear" $ do
        -- THE FALSIFIER for reading the blocker's PRINTED colour: the Devoid
        -- Drone's mana cost is {1}{B}, but CR 702.114a makes it colourless (not
        -- black), so it is not a legal blocker of a fear attacker. Fails against
        -- any implementation that reads the blocker's printed colour rather than
        -- its projected colour.
        piker <- Registry.printing registry "Goblin Piker"
        devoidDrone <- Registry.printing registry "Synthetic Devoid Drone"
        let (gs0, mine, theirs) = attacking [piker] [devoidDrone]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "illegal" (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0)))
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.36b fear restricts being blocked, never blocking" $ do
        -- The 702.9b asymmetry, restated for fear: a fear creature blocking a
        -- plain attacker is legal.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs0, mine, theirs) = attacking [piker] [piker]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear b gs0))
          _ -> HU.assertFailure "fixture should have an attacker and a blocker"
    ]

-- Declare attackers with everything, then put a Lure on the first attacker.
-- Attaching directly is S.attach's state-fixture posture -- Pawl.Cast can cast
-- the Aura, but a combat fixture cannot reach a sorcery-speed cast mid-step --
-- and the printing is the real Lure, never a synthetic.
luring :: Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
luring lure mine theirs =
  let (gs, ours, yours) = attacking mine theirs
   in case ours of
        -- Unreachable: every caller passes at least one attacking printing.
        [] -> (gs, ours, yours)
        attacker : _ ->
          let (aura, withAura) = S.addCreature lure S.alice gs
           in (S.attach aura attacker withAura, ours, yours)

-- CR 509.1c, proved by Lure ("All creatures able to block enchanted creature do
-- so") -- the pool's first blocking REQUIREMENT, and the first board on which
-- declining to block is not a legal answer.
--
-- Prized Unicorn ("All creatures able to block this creature do so") is the second
-- carrier, and the one CR 604.2's layer-6 strip needs: it is a CREATURE, so
-- Humility's "each creature loses all abilities" reaches its requirement with no
-- animator in between.
blockRequirementTests :: Registry.Type.Registry -> Tasty.TestTree
blockRequirementTests registry =
  Tasty.testGroup
    "BlockRequirements"
    [ HU.testCase "CR 509.1c declining to block a Lured attacker is illegal" $ do
        -- THE FALSIFIER for a restrictions-only reading of CR 509.1: the empty
        -- declaration disobeys no restriction, which is exactly why 509.1c is a
        -- maximization and not a per-creature check.
        lure <- Registry.printing registry "Lure"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = luring lure [piker] [piker]
        HU.assertBool "no blocks is illegal" (not (Combat.legalBlockDeclaration S.bob Map.empty gs)),
      HU.testCase "CR 509.1 the same board WITHOUT the Lure lets the defender decline" $ do
        -- The control for the test above, and the reason it is not vacuous: the
        -- empty declaration is legal here, so the Lure is what changed the answer.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = attacking [piker] [piker]
        HU.assertBool "no blocks is legal" (Combat.legalBlockDeclaration S.bob Map.empty gs),
      HU.testCase "CR 509.1c blocking the Lured attacker is legal" $ do
        lure <- Registry.printing registry "Lure"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, theirs) = luring lure [piker] [piker]
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 509.1c a creature that CANNOT block the Lured attacker is not required to" $ do
        -- Lure's "able to block" doing its work: the Bird Maiden has flying (CR
        -- 702.9b), so the ground Piker could not block it under any declaration.
        -- No requirement instance exists, the maximum is zero, and declining stays
        -- legal. Fails against an implementation that requires every creature to
        -- block regardless of the restrictions.
        lure <- Registry.printing registry "Lure"
        birdMaiden <- Registry.printing registry "Bird Maiden"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = luring lure [birdMaiden] [piker]
        HU.assertBool "no blocks is legal" (Combat.legalBlockDeclaration S.bob Map.empty gs),
      HU.testCase "CR 509.1a a TAPPED creature is not able to block, so a Lure does not require it" $ do
        -- The other half of "able": CR 509.1a's chosen creatures "must be
        -- untapped", so a tapped creature is never a candidate and carries no
        -- requirement.
        lure <- Registry.printing registry "Lure"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = luring lure [piker] [piker]
        case theirs of
          b : _ -> HU.assertBool "no blocks is legal" (Combat.legalBlockDeclaration S.bob Map.empty (S.tapObject b gs))
          _ -> HU.assertFailure "fixture should have a blocker",
      HU.testCase "CR 509.1c the maximum is over the creatures that CAN block, not all of them" $ do
        -- The maximization biting. A Lured Bird Maiden (flying) is attacking; bob
        -- has a ground Piker, which may not block it, and a Nimble Birdsticker,
        -- which has reach and may. The maximum obtainable without disobeying a
        -- restriction is ONE, and only the Birdsticker's block attains it: the
        -- empty declaration obeys zero and is illegal, and the Piker's block is
        -- illegal under CR 702.9b whatever it would obey.
        lure <- Registry.printing registry "Lure"
        birdMaiden <- Registry.printing registry "Bird Maiden"
        piker <- Registry.printing registry "Goblin Piker"
        nimbleBirdsticker <- Registry.printing registry "Nimble Birdsticker"
        let (gs, mine, theirs) = luring lure [birdMaiden] [piker, nimbleBirdsticker]
        case (mine, theirs) of
          (a : _, [ground, reacher]) -> do
            HU.assertBool "no blocks is illegal" (not (Combat.legalBlockDeclaration S.bob Map.empty gs))
            HU.assertBool "the reach creature blocking is legal" (Combat.legalBlockDeclaration S.bob (Map.singleton reacher a) gs)
            HU.assertBool "the ground creature blocking is illegal" (not (Combat.legalBlockDeclaration S.bob (Map.singleton ground a) gs))
          _ -> HU.assertFailure "fixture should have an attacker and two blockers",
      HU.testCase "CR 509.1c with two able creatures BOTH are required to block" $ do
        -- One Lure over two creatures is TWO requirements, not one -- CR 509.1c
        -- checks "each creature they control". A single block obeys one of two
        -- and is illegal; blocking with both attains the maximum.
        lure <- Registry.printing registry "Lure"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, theirs) = luring lure [piker] [piker, piker]
        case (mine, theirs) of
          (a : _, [first, second]) -> do
            HU.assertBool "one blocker is not enough" (not (Combat.legalBlockDeclaration S.bob (Map.singleton first a) gs))
            HU.assertBool "both blockers is legal" (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, a), (second, a)]) gs)
          _ -> HU.assertFailure "fixture should have an attacker and two blockers",
      HU.testCase "CR 509.1c whole cards: a Lure forces a block through a real declare blockers step" $ do
        -- The gameplay-level case, run through Combat.declareBlockers with an
        -- interpreter that declines to block. Declining is now an illegal answer,
        -- and the maximum leaves exactly one legal declaration -- the rules
        -- forcing it, not the engine choosing.
        --
        -- Precise rather than vacuous, and all three assertions distinguish the
        -- two worlds. WITHOUT the requirement: nobody blocks, bob takes 2 and both
        -- Pikers live. WITH it: the Piker blocks, bob takes nothing, and the two
        -- 2/1s trade.
        lure <- Registry.printing registry "Lure"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [piker] [piker]
            declining :: Prompt.Prompt r -> r
            declining p = case p of
              Prompt.DeclareBlockers {} -> Map.empty
              _ -> S.aggressiveAnswer p
            withLure = case mine of
              -- Unreachable: the fixture has one attacking printing.
              [] -> gs
              a : _ -> let (aura, withAura) = S.addCreature lure S.alice gs in S.attach aura a withAura
            after = S.settleSba (S.fightWith declining withLure)
        HU.assertEqual "bob took nothing" (Just 20) (S.lifeOf S.bob after)
        HU.assertEqual "alice's attacker is dead" 0 (S.creaturesInPlay S.alice after)
        HU.assertEqual "bob's blocker is dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 509.1c declining to block a Prized Unicorn is illegal" $ do
        -- The pool's second blocking requirement, and the first that names its OWN
        -- SOURCE rather than an attachment: "all creatures able to block THIS
        -- CREATURE do so" is Affected.Matching Filter.IsSource, matched against the
        -- attacker's identity. No Aura and no animator anywhere -- the requirement
        -- rides on a creature card. Fails against an implementation that only ever
        -- resolves Affected.Attached.
        prizedUnicorn <- Registry.printing registry "Prized Unicorn"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, theirs) = attacking [prizedUnicorn] [piker]
        HU.assertBool "no blocks is illegal" (not (Combat.legalBlockDeclaration S.bob Map.empty gs))
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "blocking the Unicorn is legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 509.1c a Prized Unicorn does not lure the OTHER attacker alongside it" $ do
        -- IsSource is an identity test, not "every attacker this permanent
        -- controls": with a Piker attacking beside the Unicorn, blocking the Piker
        -- obeys nothing and the maximum is still attained only by blocking the
        -- Unicorn. Fails against an implementation that mints a requirement per
        -- attacker rather than per matching attacker.
        prizedUnicorn <- Registry.printing registry "Prized Unicorn"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, theirs) = attacking [prizedUnicorn, piker] [piker]
        case (mine, theirs) of
          ([unicorn, other], b : _) -> do
            HU.assertBool "blocking the Unicorn is legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b unicorn) gs)
            HU.assertBool "blocking the other attacker instead is illegal" (not (Combat.legalBlockDeclaration S.bob (Map.singleton b other) gs))
          _ -> HU.assertFailure "fixture should have two attackers and a blocker",
      HU.testCase "CR 604.2 Humility strips a Prized Unicorn's block requirement, so declining becomes legal" $ do
        -- CR 604.2: a static ability's continuous effect is active only while the
        -- permanent "remains on the battlefield AND HAS THE ABILITY", so Humility's
        -- CR 613.1f layer-6 LoseAllAbilities takes the requirement with it. Both
        -- worlds are asserted on ONE board so the pair cannot drift: without
        -- Humility declining is illegal, with it the empty declaration becomes a
        -- legal answer. Fails against an implementation that reads
        -- Card.blockRequirements off the printed card.
        --
        -- The third assertion is what keeps the second from passing vacuously: the
        -- combat is still live under Humility -- the Unicorn is still attacking and
        -- the (now 1/1) Piker is still able to block it -- so declining became legal
        -- because the requirement went away, not because there was nothing to block.
        prizedUnicorn <- Registry.printing registry "Prized Unicorn"
        piker <- Registry.printing registry "Goblin Piker"
        humility <- Registry.printing registry "Humility"
        let (gs, mine, theirs) = attacking [prizedUnicorn] [piker]
            underHumility = S.withHumility humility gs
        HU.assertBool "without Humility, no blocks is illegal" (not (Combat.legalBlockDeclaration S.bob Map.empty gs))
        HU.assertBool "under Humility, no blocks is legal" (Combat.legalBlockDeclaration S.bob Map.empty underHumility)
        case (mine, theirs) of
          (a : _, b : _) ->
            HU.assertBool "and blocking is still legal, so the combat is still live" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) underHumility)
          _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 303.4m a Lure that is not attached to anything requires nothing" $ do
        -- CR 303.4m reads the SOURCE's attachment, so an unattached Lure names no
        -- attacker and mints no requirement. The Aura stays ON the battlefield
        -- throughout, so this is not a test that removing it works -- CR 704.5m
        -- would bury it, and no state-based-action pass is run here.
        lure <- Registry.printing registry "Lure"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = attacking [piker] [piker]
            withAura = snd (S.addCreature lure S.alice gs)
        HU.assertBool "no blocks is legal" (Combat.legalBlockDeclaration S.bob Map.empty withAura)
    ]

-- A combat board that has NOT yet declared attackers, with Curse of the Nightly
-- Hunt on the battlefield attached to `who`. The attacking twin of `luring`, and
-- the two differ exactly where the rules do: a blocking requirement is checked
-- after attackers exist, an attacking one before.
--
-- The Curse goes on the ACTIVE player, which is where it bites: "creatures
-- enchanted player controls attack each combat if able" says nothing until the
-- enchanted player has a declare attackers step of their own. Its controller is
-- alice in every case below and never matters -- CR 508.1d asks the active player
-- about their own creatures, not about whose ability is talking.
cursing :: Printing.Printing -> PlayerId.PlayerId -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
cursing curse who mine theirs =
  let (gs, ours, yours) = S.combatBoardOf mine theirs
      (aura, withAura) = S.addCreature curse S.alice gs
   in (S.attachTo aura (Recipient.ToPlayer who) withAura, ours, yours)

-- CR 508.1d, proved by Curse of the Nightly Hunt ("Creatures enchanted player
-- controls attack each combat if able") -- the pool's first attacking REQUIREMENT,
-- and the first board on which declining to attack is not a legal answer.
--
-- The requirement sits ON TOP of CR 508.1a rather than beside it: "if able" is
-- Pawl.Combat.legalAttackers, so a creature that could not have attacked anyway
-- carries no requirement and cannot make declining illegal. Half the group is
-- that half.
attackRequirementTests :: Registry.Type.Registry -> Tasty.TestTree
attackRequirementTests registry =
  Tasty.testGroup
    "AttackRequirements"
    [ HU.testCase "CR 508.1d declining to attack under a Curse of the Nightly Hunt is illegal" $ do
        -- THE FALSIFIER for a restrictions-only reading of CR 508.1: the empty
        -- declaration disobeys no restriction, which is exactly why 508.1d is a
        -- maximization and not a per-creature check.
        curse <- Registry.printing registry "Curse of the Nightly Hunt"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = cursing curse S.alice [piker] []
        HU.assertBool "no attack is illegal" (not (Combat.legalAttackDeclaration S.alice [] gs)),
      HU.testCase "CR 508.1 the same board WITHOUT the Curse lets the active player decline" $ do
        -- The control for the test above, and the reason it is not vacuous:
        -- attacking is optional by default (CR 508.1a chooses "which creatures,
        -- IF ANY"), so the Curse is what changed the answer.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [piker] []
        HU.assertBool "no attack is legal" (Combat.legalAttackDeclaration S.alice [] gs),
      HU.testCase "CR 508.1d attacking with the required creature is legal" $ do
        curse <- Registry.printing registry "Curse of the Nightly Hunt"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = cursing curse S.alice [piker] []
        case mine of
          a : _ -> HU.assertBool "attacking is legal" (Combat.legalAttackDeclaration S.alice [a] gs)
          _ -> HU.assertFailure "fixture should have a creature",
      HU.testCase "CR 303.4m the Curse requires the ENCHANTED player's creatures, not the active player's" $ do
        -- Affected.AttachedPlayerControls read for the wrong player is the bug
        -- this catches: with the Curse on bob, alice's creatures are outside its
        -- set entirely and she may still decline. bob's own creatures are not a
        -- second requirement either -- CR 508.1a's candidates are the ACTIVE
        -- player's, so a nonactive player's creature is never "able" to attack.
        curse <- Registry.printing registry "Curse of the Nightly Hunt"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = cursing curse S.bob [piker] [piker]
        HU.assertBool "no attack is legal" (Combat.legalAttackDeclaration S.alice [] gs),
      HU.testCase "CR 508.1a a TAPPED creature is not able to attack, so the Curse does not require it" $ do
        -- "If able" doing its work, on the clause CR 508.1a states first: the
        -- chosen creatures "must be untapped", so a tapped one is never a
        -- candidate and carries no requirement.
        curse <- Registry.printing registry "Curse of the Nightly Hunt"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = cursing curse S.alice [piker] []
        case mine of
          a : _ -> HU.assertBool "no attack is legal" (Combat.legalAttackDeclaration S.alice [] (S.tapObject a gs))
          _ -> HU.assertFailure "fixture should have a creature",
      HU.testCase "CR 302.6 a summoning sick creature is not able to attack, so the Curse does not require it" $ do
        curse <- Registry.printing registry "Curse of the Nightly Hunt"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = cursing curse S.alice [piker] []
        case mine of
          a : _ ->
            let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) a (GameState.objects gs)}
             in HU.assertBool "no attack is legal" (Combat.legalAttackDeclaration S.alice [] sick)
          _ -> HU.assertFailure "fixture should have a creature",
      HU.testCase "CR 702.3b a Wall of Stone is not required to attack, but the Piker beside it is" $ do
        -- Defender is the one printed CR 508.1c restriction in the pool, and it
        -- reaches the requirement through the same candidate list. Both creatures
        -- on ONE board, so a blanket "nothing is required" bug cannot pass: the
        -- Piker alone attains the maximum, and the Wall neither adds to it nor is
        -- allowed to attack.
        curse <- Registry.printing registry "Curse of the Nightly Hunt"
        wallOfStone <- Registry.printing registry "Wall of Stone"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = cursing curse S.alice [wallOfStone, piker] []
        case mine of
          [wall, p] -> do
            HU.assertBool "no attack is illegal" (not (Combat.legalAttackDeclaration S.alice [] gs))
            HU.assertBool "the Piker alone is legal" (Combat.legalAttackDeclaration S.alice [p] gs)
            HU.assertBool "the Wall may not attack at all" (not (Combat.legalAttackDeclaration S.alice [wall, p] gs))
          _ -> HU.assertFailure "fixture should have a Wall and a Piker",
      HU.testCase "CR 508.1d with two able creatures BOTH are required to attack" $ do
        -- One Curse over two creatures is TWO requirements, not one -- CR 508.1d
        -- checks "each creature they control". Attacking with one obeys one of
        -- two and is illegal; attacking with both attains the maximum.
        curse <- Registry.printing registry "Curse of the Nightly Hunt"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = cursing curse S.alice [piker, piker] []
        case mine of
          [first, second] -> do
            HU.assertBool "one attacker is not enough" (not (Combat.legalAttackDeclaration S.alice [first] gs))
            HU.assertBool "both attackers is legal" (Combat.legalAttackDeclaration S.alice [first, second] gs)
          _ -> HU.assertFailure "fixture should have two creatures",
      HU.testCase "CR 303.4m a Curse that is not attached to anything requires nothing" $ do
        -- CR 303.4m reads the SOURCE's attachment, so an unattached Curse names no
        -- player and mints no requirement. The Aura stays ON the battlefield
        -- throughout, so this is not a test that removing it works -- CR 704.5m
        -- would bury it, and no state-based-action pass is run here.
        curse <- Registry.printing registry "Curse of the Nightly Hunt"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [piker] []
            withAura = snd (S.addCreature curse S.alice gs)
        HU.assertBool "no attack is legal" (Combat.legalAttackDeclaration S.alice [] withAura),
      HU.testCase "CR 508.1d whole cards: a Curse forces an attack through a real declare attackers step" $ do
        -- The gameplay-level case, run through Engine.runStep -- the priority loop
        -- and the CR 703.4i turn-based action, not a direct call -- with an
        -- interpreter that declines to attack. Declining is now an illegal answer,
        -- and the maximum leaves the rules forcing the attack rather than the
        -- engine choosing it.
        --
        -- Precise rather than vacuous, and both worlds are asserted. WITHOUT the
        -- Curse the declining interpreter attacks with nothing and bob stays at
        -- 20; WITH it the Piker attacks, taps, and bob takes 2.
        curse <- Registry.printing registry "Curse of the Nightly Hunt"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = cursing curse S.alice [piker] []
            (plain, _, _) = S.combatBoardOf [piker] []
            declining :: Prompt.Prompt r -> r
            declining p = case p of
              Prompt.DeclareAttackers {} -> []
              _ -> S.aggressiveAnswer p
            after = S.runCombat declining gs
            control = S.runCombat declining plain
        HU.assertEqual "without the Curse, bob takes nothing" (Just 20) (S.lifeOf S.bob control)
        HU.assertEqual "with it, bob takes two" (Just 18) (S.lifeOf S.bob after)
        case mine of
          a : _ -> do
            HU.assertEqual "and the creature really was declared" [a] (S.attackerDeclarationsOf after)
            -- CR 508.1f: declaring taps it. The forced declaration is a real one,
            -- not a bookkeeping entry.
            HU.assertEqual "and tapped" (Just TapState.Tapped) (tapStateOf a after)
          _ -> HU.assertFailure "fixture should have a creature"
    ]

vigilanceTests :: Registry.Type.Registry -> Tasty.TestTree
vigilanceTests registry =
  Tasty.testGroup
    "Vigilance"
    [ HU.testCase "CR 702.20b attacking doesn't tap a creature with vigilance, but does tap its neighbor" $ do
        -- Both creatures in ONE declaration, so a blanket "nothing taps" bug
        -- cannot pass: the Piker must still tap.
        windseekerCentaur <- Registry.printing registry "Windseeker Centaur"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [windseekerCentaur, piker] [piker]
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
        case mine of
          [centaur, p] -> do
            HU.assertEqual "both attacking" 2 (length (declaredAttackers after))
            HU.assertEqual "the centaur is untapped" (Just TapState.Untapped) (tapStateOf centaur after)
            HU.assertEqual "the piker is tapped" (Just TapState.Tapped) (tapStateOf p after)
          _ -> HU.assertFailure "fixture should have two attackers",
      HU.testCase "CR 702.20b vigilance still attacks" $ do
        -- Vigilance is not a legality question: the creature is declared as an
        -- attacker exactly as normal. It simply skips CR 508.1f's tap.
        windseekerCentaur <- Registry.printing registry "Windseeker Centaur"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [windseekerCentaur] [piker]
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
        HU.assertEqual "attacking" mine (declaredAttackers after),
      HU.testCase "CR 702.20b an untapped vigilant attacker can still be blocked" $ do
        -- It is attacking, so it is in the Combat record, tapped or not.
        windseekerCentaur <- Registry.printing registry "Windseeker Centaur"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, theirs) = S.combatBoardOf [windseekerCentaur] [piker]
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
        case mine of
          [] -> HU.assertFailure "fixture should have an attacker"
          attacker : _ -> HU.assertEqual "blocked" (Set.fromList theirs) (Combat.blockersOf attacker after)
    ]

combatLegalityTests :: Registry.Type.Registry -> Tasty.TestTree
combatLegalityTests registry =
  Tasty.testGroup
    "CombatLegality"
    [ HU.testCase "a Settled untapped creature may attack" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoard piker 1 0
        case mine of
          [] -> HU.assertFailure "fixture should have an attacker"
          oid : _ -> HU.assertBool "may attack" (Combat.canAttack S.alice oid gs),
      HU.testCase "CR 302.6 a summoning sick creature may not attack" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoard piker 1 0
        case mine of
          [] -> HU.assertFailure "fixture should have an attacker"
          oid : _ ->
            let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
             in HU.assertBool "may not attack" (not (Combat.canAttack S.alice oid sick)),
      HU.testCase "CR 508.1a a tapped creature may not attack" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoard piker 1 0
        case mine of
          [] -> HU.assertFailure "fixture should have an attacker"
          oid : _ ->
            let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
             in HU.assertBool "may not attack" (not (Combat.canAttack S.alice oid tapped)),
      HU.testCase "a land may not attack" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = (S.landsInPlay mountain 1) {GameState.activePlayer = S.alice}
        case Game.zoneMembers Zone.Battlefield S.alice gs of
          [] -> HU.assertFailure "fixture should have one Mountain"
          oid : _ -> HU.assertBool "may not attack" (not (Combat.canAttack S.alice oid gs)),
      HU.testCase "you may not attack with a creature you do not control" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = S.combatBoard piker 1 1
        case theirs of
          [] -> HU.assertFailure "fixture should have a blocker"
          oid : _ -> HU.assertBool "not alice's" (not (Combat.canAttack S.alice oid gs)),
      -- CR 302.6 restricts attacking and tap abilities. It says NOTHING about
      -- blocking, and getting this wrong is the classic beginner bug.
      HU.testCase "CR 302.6 a summoning sick creature MAY block" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = S.combatBoard piker 1 1
        case theirs of
          [] -> HU.assertFailure "fixture should have a blocker"
          oid : _ ->
            let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
             in HU.assertBool "may block" (Combat.canBlock S.bob oid sick),
      HU.testCase "CR 509.1a a tapped creature may not block" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = S.combatBoard piker 1 1
        case theirs of
          [] -> HU.assertFailure "fixture should have a blocker"
          oid : _ ->
            let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
             in HU.assertBool "may not block" (not (Combat.canBlock S.bob oid tapped)),
      HU.testCase "legalAttackers lists exactly the active player's creatures" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoard piker 2 3
        HU.assertEqual "two" mine (Combat.legalAttackers S.alice gs),
      HU.testCase "CR 508.1a a player can attack with a creature they control but do not own" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            gs0 = S.giveControl oid S.alice base
        HU.assertBool "alice may attack with it" (elem oid (Combat.legalAttackers S.alice gs0))
        HU.assertBool "bob may not (not the controller, not active)" (notElem oid (Combat.legalAttackers S.bob gs0)),
      HU.testCase "combat starts empty and clears" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoard piker 1 0
            busy = case mine of
              [] -> gs
              oid : _ ->
                gs
                  { GameState.combat =
                      Combat.Type.MkCombat
                        { Combat.Type.attackers = Map.singleton oid (AttackTarget.OfPlayer S.bob),
                          Combat.Type.blockers = Map.empty,
                          Combat.Type.struckFirst = Nothing,
                          Combat.Type.joinedUnder = Map.singleton oid S.alice,
                          Combat.Type.attackersJoined = True,
                          Combat.Type.defender = Just S.bob
                        }
                  }
        HU.assertEqual "starts empty" Map.empty (Combat.Type.attackers (GameState.combat gs))
        HU.assertEqual "clears" Map.empty (Combat.Type.attackers (GameState.combat (Combat.clearCombat busy)))
    ]

keywordTests :: Registry.Type.Registry -> Tasty.TestTree
keywordTests registry =
  let gs0 = Setup.emptyGame S.bothPlayers
      -- Each M2a printing carries exactly its one keyword and no other.
      carriesOnly (name, keyword) =
        HU.testCase (name <> " carries exactly " <> show keyword) $ do
          printing <- Registry.printing registry name
          let (oid, gs) = S.addCreature printing S.alice gs0
          HU.assertEqual "keywords" (Map.singleton keyword 1) (Projection.keywordsOf oid gs)
          HU.assertBool "hasKeyword" (Projection.hasKeyword keyword oid gs)
   in Tasty.testGroup
        "Keyword"
        ( fmap carriesOnly S.m2aKeywords
            <> [ HU.testCase "a Piker has no keywords" $ do
                   piker <- Registry.printing registry "Goblin Piker"
                   let (oid, gs) = S.addCreature piker S.alice gs0
                   HU.assertEqual "none" Map.empty (Projection.keywordsOf oid gs)
                   HU.assertBool "no flying" (not (Projection.hasKeyword Keyword.Flying oid gs)),
                 HU.testCase "a Mountain has no keywords" $ do
                   mountain <- Registry.printing registry "Mountain"
                   let gs = S.landsInPlay mountain 1
                   case Game.zoneMembers Zone.Battlefield S.alice gs of
                     [] -> HU.assertFailure "fixture should have one Mountain"
                     oid : _ -> HU.assertEqual "none" Map.empty (Projection.keywordsOf oid gs),
                 HU.testCase "an unknown id has no keywords" $
                   HU.assertEqual "none" Map.empty (Projection.keywordsOf (ObjectId.MkObjectId 999) gs0),
                 -- Flying is on Bird Maiden and NOT on Nimble Birdsticker. If this
                 -- passes while the reach case above also passes, the two keywords
                 -- are genuinely distinct rather than one flag.
                 HU.testCase "reach is not flying" $ do
                   nimbleBirdsticker <- Registry.printing registry "Nimble Birdsticker"
                   let (oid, gs) = S.addCreature nimbleBirdsticker S.alice gs0
                   HU.assertBool "no flying" (not (Projection.hasKeyword Keyword.Flying oid gs))
               ]
        )

-- Run whole steps until the first-strike combat damage step has been dealt
-- (struckFirst is set) or combat ends, so a test can observe the board BETWEEN
-- the two combat damage steps.
runToFirstStrikeDone :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToFirstStrikeDone answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (Combat.Type.struckFirst (GameState.combat g))
          || not (S.inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 24 gs0

firstStrikeTests :: Registry.Type.Registry -> Tasty.TestTree
firstStrikeTests registry =
  Tasty.testGroup
    "FirstStrike"
    [ HU.testCase "CR 702.7b a first striker kills a vanilla blocker and lives" $ do
        -- The tiger (2/1 first strike) kills the Piker (2/1) in the first-strike
        -- step; the SBA between steps buries it before it can deal, so the tiger
        -- survives at zero damage.
        sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [sabretoothTiger] [piker]
            after = S.runCombat S.aggressiveAnswer gs
        HU.assertEqual "the blocker is dead" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "the first striker lives" 1 (S.creaturesInPlay S.alice after),
      HU.testCase "CR 510.2 the control: two vanilla 2/1s trade" $ do
        -- With a Piker in the tiger's place there is one combat damage step and
        -- both die. So first strike is the sole cause above.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [piker] [piker]
            after = S.runCombat S.aggressiveAnswer gs
        HU.assertEqual "alice's is dead" 0 (S.creaturesInPlay S.alice after)
        HU.assertEqual "bob's is dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 702.4b a double striker deals twice to an unblocked player" $ do
        -- The raptor (2/1 double strike) deals 2 in each step: bob loses 4.
        ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
        let (gs, _, _) = S.combatBoardOf [ridgetopRaptor] []
            after = S.runCombat S.aggressiveAnswer gs
        HU.assertEqual "bob took 4" (Just 16) (S.lifeOf S.bob after),
      HU.testCase "CR 702.7b the control: a first striker deals once to a player" $ do
        sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
        let (gs, _, _) = S.combatBoardOf [sabretoothTiger] []
            after = S.runCombat S.aggressiveAnswer gs
        HU.assertEqual "bob took 2" (Just 18) (S.lifeOf S.bob after),
      HU.testCase "CR 510.1b the control: a vanilla creature deals once to a player" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [piker] []
            after = S.runCombat S.aggressiveAnswer gs
        HU.assertEqual "bob took 2" (Just 18) (S.lifeOf S.bob after),
      HU.testCase "CR 510.4 double strike kills a 3/3 across two steps; first strike does not" $ do
        -- The raptor deals 2 + 2 = 4 to the Ogre (3/3), killing it. A first
        -- striker deals 2 once, and the Ogre lives.
        ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
        sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let raptorVs = S.combatBoardOf [ridgetopRaptor] [ogreSentry]
            tigerVs = S.combatBoardOf [sabretoothTiger] [ogreSentry]
            afterRaptor = S.runCombat S.aggressiveAnswer (frst raptorVs)
            afterTiger = S.runCombat S.aggressiveAnswer (frst tigerVs)
        HU.assertEqual "double strike kills the Ogre" 0 (S.creaturesInPlay S.bob afterRaptor)
        HU.assertEqual "first strike leaves the Ogre" 1 (S.creaturesInPlay S.bob afterTiger),
      HU.testCase "CR 510.4 a striker killed in the first step does not deal in the second" $ do
        -- Raptor (double strike) and tiger (first strike) each block-kill the
        -- other in the first step. Neither is "remaining" for the second step, so
        -- no second-wave damage; both are simply dead.
        ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
        sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
        let (gs, _, _) = S.combatBoardOf [ridgetopRaptor] [sabretoothTiger]
            after = S.runCombat S.aggressiveAnswer gs
        HU.assertEqual "attacker dead" 0 (S.creaturesInPlay S.alice after)
        HU.assertEqual "blocker dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 510.4 the mixed board: first strike once, vanilla once, double strike twice" $ do
        -- Tiger (first strike), raptor (double strike) and Piker (vanilla) all
        -- attack unblocked. First-strike step: tiger 2 + raptor 2 = 4. Second
        -- step: raptor 2 + Piker 2 = 4. bob: 20 - 8 = 12. The naive "strikers in
        -- step one, everyone else in step two" drops the raptor's second hit and
        -- lands bob at 14.
        sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
        ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [sabretoothTiger, ridgetopRaptor, piker] []
            mid = runToFirstStrikeDone S.aggressiveAnswer gs
            after = S.runCombat S.aggressiveAnswer gs
        HU.assertEqual "after the first-strike step, bob took 4" (Just 16) (S.lifeOf S.bob mid)
        HU.assertEqual "after both steps, bob took 8" (Just 12) (S.lifeOf S.bob after)
    ]

-- Attacks and blocks with everything, and casts whenever a cast is legal --
-- aggressiveAnswer's combat decisions with castAnswer's priority decision. The
-- end-of-combat group needs both: the attack has to happen for there to be an
-- attacking creature, and the spell has to be cast for the attacking-ness to be
-- observable.
attackAndCast :: Prompt.Prompt r -> r
attackAndCast p = case p of
  Prompt.ChooseAction {} -> S.castAnswer p
  _ -> S.aggressiveAnswer p

-- alice attacks with one Piker while holding a Kill Shot and exactly the three
-- Plains that pay for it; bob has nothing, so the attack is unblocked. Sits at
-- the declare attackers step like every combatBoardOf board, so the ENGINE
-- declares the attack and carries it forward -- the combat record this group
-- observes is never hand-written. S.addCreature is what puts the Plains out: it
-- is the "any printing, on the battlefield, untapped and Settled" helper its
-- haddock says it is, and lands need exactly that.
killShotBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
killShotBoard plains piker killShot =
  let (gs0, _, _) = S.combatBoardOf [piker] []
      addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature plains S.alice g))
      (withCard, _) = S.handOne killShot (addLands 3 gs0)
   in -- handOne parks its state in a precombat main phase; this board is mid-combat.
      withCard {GameState.phase = GameState.phase gs0, GameState.priority = GameState.priority gs0}

-- Run whole steps until `step` is the current phase, WITHOUT running it, so a
-- test can play that one step itself under a different answerer. Bounded so a
-- bug cannot loop forever. Stops early if combat is left, so a caller that names
-- a step this combat never reaches gets the state at the exit rather than a
-- hang.
runToStep :: Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToStep step answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || GameState.phase g == step
          || not (S.inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 8 gs0

-- Run whole steps until the end of combat step is the current phase, WITHOUT
-- running it, so a test can play that one step itself under a different
-- answerer.
runToEndOfCombat :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToEndOfCombat = runToStep (Phase.Combat CombatStep.EndOfCombat)

-- CR 511.3: creatures are removed from combat as the end of combat step ENDS, so
-- they are still attacking for the whole of that step -- including its priority
-- round (CR 511.1), where the active player may cast an instant. Kill Shot
-- ("Destroy target attacking creature") is what makes the window observable: it
-- has a legal target during the end of combat step and none after it.
endOfCombatTests :: Registry.Type.Registry -> Tasty.TestTree
endOfCombatTests registry =
  Tasty.testGroup
    "EndOfCombat"
    [ HU.testCase "CR 511.3 whole card: Kill Shot destroys an attacker during the end of combat step" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        killShot <- Registry.printing registry "Kill Shot"
        let atEnd = runToEndOfCombat S.aggressiveAnswer (killShotBoard plains piker killShot)
            after = snd (Engine.runGamePure attackAndCast atEnd Engine.runStep)
        HU.assertEqual "the step under test is the end of combat step" (Phase.Combat CombatStep.EndOfCombat) (GameState.phase atEnd)
        HU.assertBool "the Piker is still attacking as the step begins" (not (Map.null (Combat.Type.attackers (GameState.combat atEnd))))
        HU.assertEqual "the attacker was destroyed" 0 (S.creaturesInPlay S.alice after),
      HU.testCase "CR 511.3 the removal still happens, one step later: combat is empty once the step ends" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        killShot <- Registry.printing registry "Kill Shot"
        let atEnd = runToEndOfCombat S.aggressiveAnswer (killShotBoard plains piker killShot)
            after = snd (Engine.runGamePure S.aggressiveAnswer atEnd Engine.runStep)
        HU.assertEqual "the combat phase is over" Phase.PostcombatMain (GameState.phase after)
        HU.assertEqual "no attackers" Map.empty (Combat.Type.attackers (GameState.combat after))
        -- CR 506.2's designation is scoped to the combat phase, and clearCombat
        -- resets it alongside the attackers.
        HU.assertEqual "no defending player" Nothing (Combat.Type.defender (GameState.combat after)),
      HU.testCase "CR 511.3 the twin: the same Kill Shot has no target in the postcombat main phase" $ do
        -- The discriminator for the case above. If IsAttacking simply read True
        -- for every creature, or if combat were never cleared at all, this would
        -- kill the Piker too.
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        killShot <- Registry.printing registry "Kill Shot"
        let atEnd = runToEndOfCombat S.aggressiveAnswer (killShotBoard plains piker killShot)
            postcombat = snd (Engine.runGamePure S.aggressiveAnswer atEnd Engine.runStep)
            after = snd (Engine.runGamePure attackAndCast postcombat Engine.runStep)
        HU.assertEqual "the step under test is the postcombat main phase" Phase.PostcombatMain (GameState.phase postcombat)
        HU.assertEqual "the Piker survives" 1 (S.creaturesInPlay S.alice after)
    ]

-- The state out of a combatBoardOf triple.
frst :: (a, b, c) -> a
frst (a, _, _) = a

m2bExitTests :: Registry.Type.Registry -> Tasty.TestTree
m2bExitTests registry =
  Tasty.testGroup
    "M2bExit"
    [ HU.testCase "the milestone: first strike breaks the trade, double strike doubles the hit, no attacker no damage" $ do
        sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
        piker <- Registry.printing registry "Goblin Piker"
        ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
        let trade = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [sabretoothTiger] [piker]))
            doubled = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [ridgetopRaptor] []))
            quiet = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [] []))
        HU.assertEqual "first striker lives" 1 (S.creaturesInPlay S.alice trade)
        HU.assertEqual "its would-be killer is dead" 0 (S.creaturesInPlay S.bob trade)
        HU.assertEqual "double striker deals 4" (Just 16) (S.lifeOf S.bob doubled)
        HU.assertEqual "an attacker-less turn deals nothing" (Just 20) (S.lifeOf S.bob quiet)
    ]

-- alice is mid-combat with three Pikers; bob holds a Ray of Command and exactly
-- the four Islands that pay for it, and controls nothing else. The board sits at
-- the declare attackers step like every combatBoardOf board, so the ENGINE
-- declares the attack and carries it forward: no test here writes the combat
-- record. S.addCreature is what puts the Islands out -- the "any printing, on the
-- battlefield, untapped and Settled" helper its haddock says it is.
rayBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, [ObjectId.ObjectId])
rayBoard island piker ray =
  let (gs0, mine, _) = S.combatBoardOf [piker, piker, piker] []
      addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature island S.bob g))
   in (snd (S.addHandCard ray S.bob (addLands 4 gs0)), mine)

-- Attack with everything except `homebody`, never block, cast whenever a cast is
-- offered, and aim every target at `victim`.
--
-- Blocks are DECLINED rather than routed, and that is what keeps the two legs
-- comparable: a stolen attacker arrives untapped (Ray of Command untaps it) and
-- hasty under its new controller, so an aggressive blocker answer would have bob
-- block with the very creature the case is about and hide the damage question
-- behind CR 509.1's routing.
steal :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
steal homebody victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareAttackers _ _ ids -> filter (/= homebody) ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- Block with everything, cast whenever a cast is offered, aim every target at
-- `victim`. The blocker-side twin of `steal`.
snatch :: ObjectId.ObjectId -> Prompt.Prompt r -> r
snatch victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  _ -> S.aggressiveAnswer p

-- CR 506.4: "A permanent is removed from combat if it leaves the battlefield, if
-- its controller changes, ..." -- and a creature so removed "stops being an
-- attacking, blocking, blocked, and/or unblocked creature".
--
-- Ray of Command is the pool's producer for the control-change clause: {3}{U},
-- INSTANT, "Untap target creature an opponent controls and gain control of it
-- until end of turn. That creature gains haste until end of turn." Act of Treason
-- has the same three effects and cannot reach this window at all, because it is a
-- sorcery -- which is why this clause was worked card-driven rather than built
-- speculatively. Ray of Command's remaining sentence, the delayed trigger that
-- taps the creature when its controller loses it, is not implemented (#287).
--
-- Every leg runs whole steps through Engine.runStep, so the combat record under
-- test is the engine's own and the removal is observed where a player would see
-- it: at the CR 117.5 settle that follows the spell resolving. Each leg stops at
-- the end of combat step, where CR 511.3 says the record still reads live.
controlChangeRemovalTests :: Registry.Type.Registry -> Tasty.TestTree
controlChangeRemovalTests registry =
  Tasty.testGroup
    "ControlChangeRemoval"
    [ HU.testCase "CR 506.4 whole card: Ray of Command on an attacker removes THAT attacker from combat, and it deals no combat damage" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        rayOfCommand <- Registry.printing registry "Ray of Command"
        killShot <- Registry.printing registry "Kill Shot"
        case (rayBoard island piker rayOfCommand, S.spellTargetSpec killShot) of
          ((gs, [stolen, other, homebody]), Just attackingSpec) -> do
            let atEnd = runToEndOfCombat (steal homebody stolen) gs
                attackers = Combat.Type.attackers (GameState.combat atEnd)
                legal = Target.legalRecipients Nothing S.noSource attackingSpec atEnd
            HU.assertEqual "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (Phase.Combat CombatStep.EndOfCombat) (GameState.phase atEnd)
            HU.assertEqual "bob really did gain control of it" (Just S.bob) (Projection.controllerOf stolen atEnd)
            HU.assertBool "CR 506.4: so it is no longer an attacking creature" (Map.notMember stolen attackers)
            HU.assertBool "the attacker bob left alone is untouched" (Map.member other attackers)
            -- The discriminating assertion: the unfixed engine keeps the stolen
            -- Piker in the record and deals its 2 alongside the other's.
            HU.assertEqual "CR 510.1: bob takes only the surviving attacker's 2" (Just 18) (S.lifeOf S.bob atEnd)
            -- CR 508.1k through the door a card actually uses: Kill Shot's own
            -- committed target spec is Pool.Creatures narrowed by IsAttacking.
            HU.assertBool "Filter.IsAttacking no longer finds the stolen creature" (not (Set.member (Recipient.ToCreature stolen) legal))
            HU.assertBool "and still finds the one that is attacking" (Set.member (Recipient.ToCreature other) legal)
          _ -> HU.assertFailure "fixture should have three Pikers and Kill Shot a 'target' slot",
      HU.testCase "CR 506.4 the twin: the same Ray of Command on a creature that is not in combat leaves combat intact" $ do
        -- The control leg, and the reason the case above is not passing for a
        -- trivial reason. The SAME card resolves, the SAME settle runs, and
        -- control really does change -- just not for a combatant. A sampler that
        -- cleared combat whenever it saw a control change, or whenever anything
        -- resolved, would take the attackers out here too.
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        rayOfCommand <- Registry.printing registry "Ray of Command"
        case rayBoard island piker rayOfCommand of
          (gs, [one, two, homebody]) -> do
            let atEnd = runToEndOfCombat (steal homebody homebody) gs
                attackers = Combat.Type.attackers (GameState.combat atEnd)
            HU.assertEqual "bob gained control of the creature that stayed home" (Just S.bob) (Projection.controllerOf homebody atEnd)
            HU.assertBool "both attackers are still attacking" (Map.member one attackers && Map.member two attackers)
            HU.assertEqual "so bob takes both hits" (Just 16) (S.lifeOf S.bob atEnd)
          _ -> HU.assertFailure "fixture should have three Pikers",
      HU.testCase "CR 506.4 a stolen BLOCKER is removed from combat, and CR 509.1h leaves the attacker blocked" $ do
        -- The blocker side of the same clause, and the interaction the
        -- Combat.blockers shape exists for: Game.removeFromCombat drops the
        -- blocker from the SET while the attacker's KEY survives, so the attacker
        -- stays blocked and (CR 510.1c) assigns no combat damage at all.
        --
        -- The theft has to land after blocks are declared, so the declare
        -- attackers step is played under an answerer that does not cast and only
        -- the declare blockers step onwards sees `snatch`.
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        rayOfCommand <- Registry.printing registry "Ray of Command"
        let (gs0, mine, theirs) = S.combatBoardOf [piker] [piker]
            addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature island S.alice g))
            gs = snd (S.addHandCard rayOfCommand S.alice (addLands 4 gs0))
        case (mine, theirs) of
          (attacker : _, blocker : _) -> do
            let atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
                atEnd = runToEndOfCombat (snatch blocker) atBlockers
                -- The control leg: the same board and the same blocks, with alice
                -- never casting. Two 2/1 Pikers then trade and both die.
                traded = runToEndOfCombat S.aggressiveAnswer atBlockers
            HU.assertEqual "the leg hands over at the declare blockers step, so `snatch` is what declares the blocks and then casts" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase atBlockers)
            -- The discriminating assertion, and first because it is the one the
            -- unfixed engine fails: with the blocker still in the record the two
            -- Pikers trade, and the ids below stop resolving at all.
            HU.assertBool "CR 510.1c: neither creature was dealt combat damage" (S.onBattlefield attacker atEnd && S.onBattlefield blocker atEnd)
            HU.assertEqual "alice really did gain control of the blocker" (Just S.alice) (Projection.controllerOf blocker atEnd)
            HU.assertEqual "CR 506.4: it is blocking nothing" Set.empty (Combat.blockersOf attacker atEnd)
            HU.assertBool "CR 509.1h: but the attacker remains blocked" (Combat.isBlocked attacker atEnd)
            HU.assertEqual "and bob takes nothing" (Just 20) (S.lifeOf S.bob atEnd)
            HU.assertBool "control leg: with no theft the two Pikers trade and both die" (not (S.onBattlefield attacker traded) && not (S.onBattlefield blocker traded))
          _ -> HU.assertFailure "fixture did not build an attacker and a blocker"
    ]

-- Labyrinth of Skophos' SECOND activated ability -- "{4}, {T}: Remove target
-- attacking or blocking creature from combat" -- read off the JSON-loaded
-- printing rather than hand-built, so every leg below exercises the codec's
-- parse of the committed card data (S.spellTargetSpec's posture, for an
-- activated ability rather than a spell). The first is the land's "{T}: Add
-- {C}".
removalAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
removalAbility printing = case Card.Type.activatedAbilities (Printing.card printing) of
  [_, ability] -> Just ability
  _ -> Nothing

-- That ability's "target" slot spec: CR 601.2c's narrowing, reached for an
-- activated ability through CR 602.2b, which for this card is Pool.Creatures
-- under `Or [IsAttacking, IsBlocking]`.
removalSpec :: ActivatedAbility.ActivatedAbility Card.Type.Card -> Maybe TargetSpec.TargetSpec
removalSpec ability =
  Map.lookup
    (SlotName.MkSlotName (Text.pack "target"))
    (Modal.allTargetSpecs (ActivatedAbility.modal ability))

-- alice is mid-combat with one creature per printing in `mine`; bob defends with
-- one per printing in `theirs`. `who` also controls a Labyrinth of Skophos and
-- the four lands that pay its {4}. S.addCreature is what puts all five out --
-- the "any printing, on the battlefield, untapped and Settled" helper its
-- haddock says it is, which is what a land needs.
skophosBoard ::
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  [Printing.Printing] ->
  [Printing.Printing] ->
  (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId)
skophosBoard labyrinth land who mine theirs =
  let (gs0, ours, yours) = S.combatBoardOf mine theirs
      addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature land who g))
      (mazeId, gs1) = S.addCreature labyrinth who (addLands 4 gs0)
   in (gs1, ours, yours, mazeId)

-- Fire the Labyrinth's removal ability once, aim it at `victim`, and pay the {4}
-- with anything BUT the Labyrinth itself: CR 601.2g pays an activation's mana
-- before its components (Pawl.Activate), so tapping the land for its own {C}
-- would leave the {T} unpayable and revert the whole activation. Choosing around
-- that is the player's job, not the engine's.
--
-- Every other prompt falls through to S.aggressiveAnswer, so attacks and blocks
-- still happen.
--
-- STATEFUL, and it has to be, for GameSpec's illegalActivationAnswer reason: an
-- answerer that names the same Activate at every ask never lets the priority
-- loop terminate once the activation stops SUCCEEDING. A rejected one is a
-- no-op (CR 601.2c: an answer outside the legal target set reverts the whole
-- activation), so the cost goes unpaid, the land stays untapped, and the same
-- action is offered again forever. That is unreachable while the engine is
-- right, and this was written pure first: breaking Filter.IsBlocking on purpose
-- hung the suite instead of failing it, which is not a test.
mazeAnswer ::
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card ->
  ObjectId.ObjectId ->
  Prompt.Prompt r ->
  State.State Bool r
mazeAnswer mazeId ability victim p = case p of
  Prompt.ChooseAction _ _ actions -> do
    tried <- State.get
    if tried || notElem (A.Activate mazeId ability) actions
      then pure A.Pass
      else do
        State.put True
        pure (A.Activate mazeId ability)
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Recipient.ToCreature victim)) sets)
  Prompt.ChooseManaSource _ _ candidates ->
    pure (Maybe.fromMaybe (NonEmpty.head candidates) (List.find (/= mazeId) (NonEmpty.toList candidates)))
  _ -> pure (S.aggressiveAnswer p)

-- runToEndOfCombat's stateful twin, for the answerer above: the same bounded
-- walk of whole steps, threading the "have I activated yet" flag across them.
runToEndOfCombatWith ::
  (forall r. Prompt.Prompt r -> State.State Bool r) ->
  GameState.GameState ->
  GameState.GameState
runToEndOfCombatWith answer gs0 =
  let go n g s =
        if n <= (0 :: Int)
          || GameState.phase g == Phase.Combat CombatStep.EndOfCombat
          || not (S.inCombatPhase (GameState.phase g))
          then g
          else
            let ((_, g1), s1) = State.runState (Engine.runGame answer g Engine.runStep) s
             in go (n - 1) g1 s1
   in go 8 gs0 False

-- Attack with everything except `homebody`, and otherwise behave aggressively --
-- so the board carries an attacking creature, a blocking creature and a creature
-- that is neither, which is what the target filter has to tell apart.
stayHomeAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
stayHomeAnswer homebody p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (/= homebody) ids
  _ -> S.aggressiveAnswer p

-- CR 506.4: "A permanent is removed from combat if ... an effect specifically
-- removes it from combat." The rule's one clause a card ASKS for, rather than a
-- condition the engine has to notice -- and Labyrinth of Skophos is the pool's
-- producer: "{T}: Add {C}. / {4}, {T}: Remove target attacking or blocking
-- creature from combat." (Land, Murders at Karlov Manor Commander; oracle text
-- checked against Scryfall.)
--
-- Every leg runs whole steps through Engine.runStep, so the combat record under
-- test is the engine's own: the fixture declares nothing by hand. The two damage
-- legs stop at the end of combat step, where CR 511.3 says the record still
-- reads live; the filter leg stops one step earlier, before anything dies.
--
-- Removal is removal only. Nothing here puts a creature back into combat, which
-- is what the rules say too -- the glossary's "removed from combat" entry has
-- the permanent take "no further involvement in that combat phase".
effectRemovalTests :: Registry.Type.Registry -> Tasty.TestTree
effectRemovalTests registry =
  Tasty.testGroup
    "EffectRemoval"
    [ HU.testCase "CR 506.4 whole card: Labyrinth of Skophos removes target ATTACKING creature, and it deals no combat damage" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        labyrinth <- Registry.printing registry "Labyrinth of Skophos"
        case (removalAbility labyrinth, skophosBoard labyrinth island S.bob [piker] []) of
          (Just ability, (gs, [attacker], _, mazeId)) -> do
            let atEnd = runToEndOfCombatWith (mazeAnswer mazeId ability attacker) gs
                quiet = runToEndOfCombat S.aggressiveAnswer gs
                legal = fmap (\spec -> Target.legalRecipients Nothing S.noSource spec atEnd) (removalSpec ability)
            HU.assertEqual "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (Phase.Combat CombatStep.EndOfCombat) (GameState.phase atEnd)
            HU.assertEqual "the ability really was activated: its {T} component was paid" (Just TapState.Tapped) (tapStateOf mazeId atEnd)
            HU.assertBool "CR 506.4: the Piker stopped being an attacking creature" (Map.notMember attacker (Combat.Type.attackers (GameState.combat atEnd)))
            -- The discriminating assertion: with the removal missing, the Piker
            -- stays in the record and deals its 2.
            HU.assertEqual "CR 510.1: so bob takes nothing" (Just 20) (S.lifeOf S.bob atEnd)
            HU.assertEqual "and the card's own target filter no longer finds it" (Just False) (fmap (Set.member (Recipient.ToCreature attacker)) legal)
            HU.assertBool "control leg: unactivated, the Piker is still attacking" (Map.member attacker (Combat.Type.attackers (GameState.combat quiet)))
            HU.assertEqual "and bob takes its 2" (Just 18) (S.lifeOf S.bob quiet)
          _ -> HU.assertFailure "fixture should give bob a Labyrinth with two abilities and alice one Piker",
      HU.testCase "CR 509.1h a removed BLOCKER leaves the attacker blocked, so nothing is dealt combat damage" $ do
        -- The blocker side of the same clause, and the interaction
        -- Game.removeFromCombat's two-way edit of Combat.blockers exists for: the
        -- blocker leaves the SET while the attacker's KEY survives, so the
        -- attacker stays blocked and (CR 510.1c) assigns no combat damage at all.
        --
        -- alice holds the Labyrinth and aims it at her opponent's blocker, so the
        -- removal has to land after blocks are declared: the declare attackers
        -- step is played under an answerer that never activates, and only the
        -- declare blockers step onwards sees `mazeAnswer`.
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        labyrinth <- Registry.printing registry "Labyrinth of Skophos"
        case (removalAbility labyrinth, skophosBoard labyrinth island S.alice [piker] [piker]) of
          (Just ability, (gs, [attacker], [blocker], mazeId)) -> do
            let atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
                atEnd = runToEndOfCombatWith (mazeAnswer mazeId ability blocker) atBlockers
                -- The control leg: the same board and the same blocks, with the
                -- ability never activated. Two 2/1 Pikers then trade.
                traded = runToEndOfCombat S.aggressiveAnswer atBlockers
            HU.assertEqual "the leg hands over at the declare blockers step, so the blocks are declared before the activation" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase atBlockers)
            HU.assertEqual "the ability really was activated" (Just TapState.Tapped) (tapStateOf mazeId atEnd)
            HU.assertBool "CR 510.1c: neither creature was dealt combat damage" (S.onBattlefield attacker atEnd && S.onBattlefield blocker atEnd)
            HU.assertEqual "CR 506.4: the removed creature is blocking nothing" Set.empty (Combat.blockersOf attacker atEnd)
            HU.assertBool "CR 509.1h: but the attacker remains blocked" (Combat.isBlocked attacker atEnd)
            HU.assertEqual "so bob takes nothing either" (Just 20) (S.lifeOf S.bob atEnd)
            HU.assertBool "control leg: unactivated, the two Pikers trade and both die" (not (S.onBattlefield attacker traded) && not (S.onBattlefield blocker traded))
          _ -> HU.assertFailure "fixture should give alice a Labyrinth and an attacker, and bob a blocker",
      HU.testCase "CR 601.2c the card's filter admits the attacker and the blocker and rejects the creature that stayed home" $ do
        -- Or [IsAttacking, IsBlocking], and both halves are load-bearing: with
        -- IsAttacking alone the blocker would be rejected, and with no filter at
        -- all the homebody would be admitted.
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        labyrinth <- Registry.printing registry "Labyrinth of Skophos"
        case (removalAbility labyrinth, skophosBoard labyrinth island S.alice [piker, piker] [piker]) of
          (Just ability, (gs, [attacker, homebody], [blocker], _)) -> do
            -- The combat damage step is the vantage point: blockers have been
            -- declared and nothing has died yet.
            let atDamage = runToStep (Phase.Combat CombatStep.CombatDamage) (stayHomeAnswer homebody) gs
                legal = fmap (\spec -> Target.legalRecipients Nothing S.noSource spec atDamage) (removalSpec ability)
                admits oid = fmap (Set.member (Recipient.ToCreature oid)) legal
            HU.assertEqual "the fixture reached the combat damage step with blocks declared" (Phase.Combat CombatStep.CombatDamage) (GameState.phase atDamage)
            HU.assertBool "the blocker really is blocking the attacker" (Set.member blocker (Combat.blockersOf attacker atDamage))
            HU.assertEqual "IsAttacking admits the attacker" (Just True) (admits attacker)
            HU.assertEqual "IsBlocking admits the blocker" (Just True) (admits blocker)
            HU.assertEqual "and the creature in neither role is rejected" (Just False) (admits homebody)
          _ -> HU.assertFailure "fixture should give alice two Pikers and a Labyrinth, and bob a blocker"
    ]

-- alice is mid-combat with Opalescence, Living Plane and a Goblin Piker, plus one
-- Forest that Living Plane has made a 1/1 creature; bob defends with nothing but
-- the two Swamps that pay for the Doom Blade in his hand. The board sits at the
-- declare attackers step like every combatBoardOf board, so the ENGINE declares
-- the attack: no test here writes the combat record.
--
-- Returns alice's three combatBoardOf permanents in printing order alongside the
-- Forest, which is added separately because it is not one of them.
unmakeBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, [ObjectId.ObjectId], ObjectId.ObjectId)
unmakeBoard opalescence livingPlane piker forest swamp doomBlade =
  let (gs0, mine, _) = S.combatBoardOf [opalescence, livingPlane, piker] []
      (land, gs1) = S.addCreature forest S.alice gs0
      addSwamps n g = if n <= (0 :: Int) then g else addSwamps (n - 1) (snd (S.addCreature swamp S.bob g))
   in (snd (S.addHandCard doomBlade S.bob (addSwamps 2 gs1)), mine, land)

-- alice attacks with `land` alone, nobody blocks, and whoever is offered a cast
-- takes it and aims every target at `victim`. The shape of `steal` above, with a
-- reason of its own for declining blocks: bob's own lands are 1/1 creatures while
-- Living Plane lives, so an aggressive blocker answer would put them in front of
-- the attacker and hide the question this asks behind CR 509.1's routing.
unmake :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
unmake land victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareAttackers _ _ ids -> filter (== land) ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- alice attacks with one Goblin Piker and holds the Doom Blade and the two
-- Swamps that pay for it; bob defends with Opalescence, Living Plane and the
-- Forest that Living Plane has made a 1/1 creature. The mirror of unmakeBoard,
-- with the animator on the DEFENDING side so the creature that stops being one is
-- a blocker.
unblockBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId)
unblockBoard opalescence livingPlane piker forest swamp doomBlade =
  let (gs0, mine, theirs) = S.combatBoardOf [piker] [opalescence, livingPlane]
      (land, gs1) = S.addCreature forest S.bob gs0
      addSwamps n g = if n <= (0 :: Int) then g else addSwamps (n - 1) (snd (S.addCreature swamp S.alice g))
   in (snd (S.addHandCard doomBlade S.alice (addSwamps 2 gs1)), mine, theirs, land)

-- Attack with `attacker` alone and cast nothing. The declare attackers step of
-- the blocker leg is played under this, so alice's Swamps -- 1/1 creatures while
-- Living Plane lives, and therefore legal attackers -- stay untapped to pay for
-- the Doom Blade she casts a step later.
attackOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
attackOnly attacker p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
  _ -> S.aggressiveAnswer p

-- Block the first attacker with `blocker` alone and cast nothing: the control leg
-- of the blocker case, where Living Plane is left alone and the block resolves
-- into an ordinary trade.
blockOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
blockOnly blocker p = case p of
  Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
    a : _ -> Map.singleton blocker a
    [] -> Map.empty
  _ -> S.aggressiveAnswer p

-- Block the first attacker with `blocker` alone, cast whenever a cast is offered,
-- and aim every target at `victim`. Blocking with everything instead would put
-- Living Plane -- a 4/4 creature while Opalescence is out -- in front of the
-- attacker too, and killing it would then be a blocker LEAVING THE BATTLEFIELD,
-- which is a different clause of CR 506.4.
unblock :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
unblock blocker victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
    a : _ -> Map.singleton blocker a
    [] -> Map.empty
  _ -> S.aggressiveAnswer p

-- CR 506.4: "A permanent is removed from combat if ... it's an attacking or
-- blocking creature that ... stops being a creature."
--
-- The clause has no one-card producer, and does not need one: the rule asks about
-- the permanent's creature-ness, not about how it was lost. Three pool cards make
-- it happen through the layer system alone, and every oracle text below was
-- checked against Scryfall.
--
--   * Living Plane ({2}{G}{G} World Enchantment, "All lands are 1/1 creatures
--     that are still lands") is what makes a Forest able to attack or block.
--   * Opalescence ({2}{W}{W} Enchantment, "Each other non-Aura enchantment is a
--     creature in addition to its other types and has base power and base
--     toughness each equal to its mana value") is what puts Living Plane itself
--     within reach of a creature-removal spell. Without it nothing in the pool
--     can touch an enchantment at instant speed, which is why this clause waited
--     on a producer rather than being built speculatively.
--   * Doom Blade ({1}{B} Instant, "Destroy target nonblack creature") kills the
--     green Living Plane in the priority round after the declaration.
--
-- CR 611.3b is what makes that enough: a static ability's continuous effect
-- "applies at all times that the permanent generating it is on the battlefield",
-- so Living Plane leaving takes the animation with it and the Forest stops being
-- a creature WITHOUT leaving the battlefield. That is what makes this the types
-- clause rather than CR 506.4's leaves-the-battlefield one, and each leg asserts
-- the Forest is still on the battlefield to pin it.
--
-- Every leg runs whole steps through Engine.runStep and stops at the end of
-- combat step, where CR 511.3 says the record still reads live.
typeChangeRemovalTests :: Registry.Type.Registry -> Tasty.TestTree
typeChangeRemovalTests registry =
  Tasty.testGroup
    "TypeChangeRemoval"
    [ HU.testCase "CR 506.4 whole cards: an attacking Forest that stops being a creature is removed from combat" $ do
        forest <- Registry.printing registry "Forest"
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        opalescence <- Registry.printing registry "Opalescence"
        livingPlane <- Registry.printing registry "Living Plane"
        doomBlade <- Registry.printing registry "Doom Blade"
        case unmakeBoard opalescence livingPlane piker forest swamp doomBlade of
          (gs, [_, plane, _], land) -> do
            let atEnd = runToEndOfCombat (unmake land plane) gs
                attackers = Combat.Type.attackers (GameState.combat atEnd)
            HU.assertEqual "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (Phase.Combat CombatStep.EndOfCombat) (GameState.phase atEnd)
            HU.assertBool "the Forest really was attacking: the declaration happened while it was a creature" (elem land (S.attackerDeclarationsOf atEnd))
            HU.assertBool "the Doom Blade really did kill Living Plane" (not (S.onBattlefield plane atEnd))
            HU.assertBool "CR 611.3b: so the Forest stopped being a creature" (not (Projection.isCreatureOf land atEnd))
            HU.assertBool "and is still on the battlefield, so this is the types clause and not the leaves-the-battlefield one" (S.onBattlefield land atEnd)
            -- The discriminating assertion: the unfixed engine leaves the Forest
            -- in the record as an attacking creature, which CR 506.3 says a
            -- noncreature permanent cannot be.
            HU.assertBool "CR 506.4: it is no longer an attacking creature" (Map.notMember land attackers)
            HU.assertEqual "CR 510.1: and bob takes nothing" (Just 20) (S.lifeOf S.bob atEnd)
          _ -> HU.assertFailure "fixture should give alice Opalescence, Living Plane and a Piker",
      HU.testCase "CR 506.4 the twin: the same Doom Blade on a creature that is not the animator leaves combat intact" $ do
        -- The control leg, and the reason the case above is not passing for a
        -- trivial reason. The SAME card resolves, the SAME settle runs, and a
        -- creature really does die -- just not the one the Forest's creature-ness
        -- hangs on. A sampler that cleared combat whenever anything died, or
        -- whenever anything resolved, would take the Forest out here too.
        forest <- Registry.printing registry "Forest"
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        opalescence <- Registry.printing registry "Opalescence"
        livingPlane <- Registry.printing registry "Living Plane"
        doomBlade <- Registry.printing registry "Doom Blade"
        case unmakeBoard opalescence livingPlane piker forest swamp doomBlade of
          (gs, [_, plane, homebody], land) -> do
            let atEnd = runToEndOfCombat (unmake land homebody) gs
                attackers = Combat.Type.attackers (GameState.combat atEnd)
            HU.assertBool "the Piker that stayed home died instead" (not (S.onBattlefield homebody atEnd))
            HU.assertBool "Living Plane survives" (S.onBattlefield plane atEnd)
            HU.assertBool "so the Forest is still a creature" (Projection.isCreatureOf land atEnd)
            HU.assertBool "and still attacking" (Map.member land attackers)
            HU.assertEqual "so bob takes its 1" (Just 19) (S.lifeOf S.bob atEnd)
          _ -> HU.assertFailure "fixture should give alice Opalescence, Living Plane and a Piker",
      HU.testCase "CR 509.1h a BLOCKER that stops being a creature leaves the attacker blocked, so nothing is dealt combat damage" $ do
        -- The blocker side of the same clause, through the same performer: the
        -- Forest leaves the SET while the attacker's KEY survives, so the Piker
        -- stays blocked and CR 510.1c gives it nobody to assign damage to.
        --
        -- This is the leg where the removal is observable as DAMAGE. An attacker
        -- that stops being a creature loses its power along with its card type, so
        -- Damage.attackerAssignment's Projection.powerOf already declines to
        -- assign anything for it; a stale BLOCKER is screened only for liveness
        -- (Damage's onBattlefield filter), so the unfixed engine marks the
        -- attacker's 2 on a land that is no longer a creature at all.
        --
        -- The kill has to land after blocks are declared, so the declare attackers
        -- step is played under an answerer that never casts and only the declare
        -- blockers step onwards sees `unblock`.
        forest <- Registry.printing registry "Forest"
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        opalescence <- Registry.printing registry "Opalescence"
        livingPlane <- Registry.printing registry "Living Plane"
        doomBlade <- Registry.printing registry "Doom Blade"
        case unblockBoard opalescence livingPlane piker forest swamp doomBlade of
          (gs, [attacker], [_, plane], land) -> do
            let atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackOnly attacker) gs
                atEnd = runToEndOfCombat (unblock land plane) atBlockers
                -- The control leg: the same board and the same block, with alice
                -- never casting. The 2/1 Piker and the 1/1 Forest then trade.
                traded = runToEndOfCombat (blockOnly land) atBlockers
            HU.assertEqual "the leg hands over at the declare blockers step, so the block is declared before the kill" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase atBlockers)
            HU.assertBool "the Doom Blade really did kill Living Plane" (not (S.onBattlefield plane atEnd))
            HU.assertBool "CR 611.3b: so the Forest stopped being a creature" (not (Projection.isCreatureOf land atEnd))
            HU.assertBool "and is still on the battlefield" (S.onBattlefield land atEnd)
            -- The discriminating assertion: the unfixed engine leaves the Forest
            -- in the blocker set and marks the Piker's 2 on it.
            HU.assertEqual "CR 510.1c: nothing was dealt combat damage" (Just 0) (S.damageOf land atEnd)
            HU.assertEqual "CR 506.4: the Forest is blocking nothing" Set.empty (Combat.blockersOf attacker atEnd)
            HU.assertBool "CR 509.1h: but the attacker remains blocked" (Combat.isBlocked attacker atEnd)
            HU.assertEqual "so bob takes nothing either" (Just 20) (S.lifeOf S.bob atEnd)
            HU.assertBool "control leg: with Living Plane left alone the Piker and the Forest trade" (not (S.onBattlefield attacker traded) && not (S.onBattlefield land traded))
          _ -> HU.assertFailure "fixture should give alice a Piker and bob Opalescence, Living Plane and a Forest"
    ]

-- CR 508.4: "If a creature is put onto the battlefield attacking, its controller
-- chooses which defending player ... it's attacking ... Such creatures are
-- 'attacking' but, for the purposes of trigger events and effects, they never
-- 'attacked'."
--
-- Hanweir Garrison is the pool's only source of one: "Whenever this creature
-- attacks, create two 1/1 red Human creature tokens that are tapped and
-- attacking."
putOntoBattlefieldAttackingTests :: Registry.Type.Registry -> Tasty.TestTree
putOntoBattlefieldAttackingTests registry =
  Tasty.testGroup
    "PutOntoBattlefieldAttacking"
    [ HU.testCase "CR 508.4 whole card: Hanweir Garrison's two Humans enter tapped and attacking" $ do
        garrison <- Registry.printing registry "Hanweir Garrison"
        let (gs, mine, _) = S.combatBoardOf [garrison] []
            -- The vantage point is the declare blockers step: the trigger fired
            -- at the declaration (CR 508.2b) and resolved in the declare
            -- attackers step's priority round, and CR 511.3 has not yet cleared
            -- the record.
            atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            tokens = S.tokensOf atBlockers
            attackers = Combat.Type.attackers (GameState.combat atBlockers)
            sicknessOf oid = fmap Object.sickness (Game.lookupObject oid atBlockers)
        HU.assertEqual "the fixture reached the declare blockers step" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase atBlockers)
        HU.assertEqual "the trigger fired once: two tokens" 2 (length tokens)
        mapM_ (\oid -> HU.assertEqual "tapped" (Just TapState.Tapped) (tapStateOf oid atBlockers)) tokens
        mapM_ (\oid -> HU.assertEqual "attacking bob" (Just (AttackTarget.OfPlayer S.bob)) (Map.lookup oid attackers)) tokens
        -- CR 302.6 restricts a creature from ATTACKING, and CR 508.4c exempts a
        -- creature put onto the battlefield attacking from the restrictions that
        -- apply to the declaration of attackers -- so a token that has been
        -- controlled for no time at all is attacking anyway.
        mapM_ (\oid -> HU.assertEqual "still summoning sick" (Just Sickness.Sick) (sicknessOf oid)) tokens
        case mine of
          [garrisonId] -> HU.assertEqual "and the Garrison itself is attacking" (Just (AttackTarget.OfPlayer S.bob)) (Map.lookup garrisonId attackers)
          _ -> HU.assertFailure "fixture should have one Hanweir Garrison",
      HU.testCase "CR 508.3a the tokens are attacking, and the attack trigger fired only for the Garrison" $ do
        -- THE discriminating case, and the one a naive implementation gets
        -- wrong: CR 508.3a's "such abilities won't trigger if a creature is put
        -- onto the battlefield attacking", and CR 508.4's "such creatures are
        -- 'attacking' but ... they never 'attacked'". An engine that put the
        -- tokens into combat by routing them through the declaration would
        -- record them here, and every "whenever a creature attacks" ability
        -- would then fire for the tokens as well.
        --
        -- Two Garrisons, so the assertion is a LIST and not a singleton: a
        -- declaration really does record one entry per creature, which is what
        -- makes the tokens' absence a fact about the tokens rather than about
        -- the shape of the log.
        garrison <- Registry.printing registry "Hanweir Garrison"
        let (gs, mine, _) = S.combatBoardOf [garrison, garrison] []
            atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            tokens = S.tokensOf atBlockers
            attackers = Combat.Type.attackers (GameState.combat atBlockers)
        HU.assertEqual "each Garrison's trigger fired once: four tokens" 4 (length tokens)
        HU.assertEqual "all six creatures are attacking" 6 (Map.size attackers)
        HU.assertEqual "but only the two Garrisons were DECLARED" mine (S.attackerDeclarationsOf atBlockers)
        mapM_ (\oid -> HU.assertBool "no token was declared" (notElem oid (S.attackerDeclarationsOf atBlockers))) tokens,
      HU.testCase "CR 510.1b the tokens deal combat damage like any attacker" $ do
        garrison <- Registry.printing registry "Hanweir Garrison"
        let (gs, _, _) = S.combatBoardOf [garrison] []
            after = S.runCombat S.aggressiveAnswer gs
        -- The 2/3 Garrison plus two 1/1 tokens, all unblocked, against bob's 20.
        HU.assertEqual "bob takes 2 + 1 + 1" (Just 16) (S.lifeOf S.bob after)
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Combat"
    [ combatLegalityTests registry,
      declareTests registry,
      combatDamageTests registry,
      keywordTests registry,
      firstStrikeTests registry,
      endOfCombatTests registry,
      m2bExitTests registry,
      defenderTests registry,
      defendingPlayerTests registry,
      vigilanceTests registry,
      hasteTests registry,
      evasionTests registry,
      blockRequirementTests registry,
      attackRequirementTests registry,
      controlChangeSicknessTests registry,
      controlChangeRemovalTests registry,
      typeChangeRemovalTests registry,
      effectRemovalTests registry,
      putOntoBattlefieldAttackingTests registry
    ]
