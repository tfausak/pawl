{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Combat: attack/block legality, combat damage, and the combat
-- keywords (flying, reach, defender, vigilance, haste, first/double strike).
module Pawl.CombatSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Combat as Combat
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Combat as Combat.Type
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Departure as Departure.Type
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.Expiry as Expiry
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone
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

-- CR 506.2/506.2a/507.1/703.4h: WHO is being attacked. Distinct from
-- defenderTests, which is the Defender KEYWORD (CR 702.3b).
defendingPlayerTests :: Tasty.TestTree
defendingPlayerTests =
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
      HU.testCase "CR 800.4j a turn whose active player has left chooses no defending player" $
        -- CR 800.4j: the turn continues without an active player, so the actions
        -- the rules assign to the active player have nobody to perform them.
        -- THREE seats, so that two opponents survive and the choice would
        -- otherwise be a real prompt -- at two seats the elision would hide the
        -- guard entirely.
        let gone = Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame
            (after, asked) =
              State.runState
                (Program.foldProgramM (choosesDefender S.carol) (State.execStateT (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat)) gone))
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
        -- The discriminator between Departure.stillPlayingInOrder and
        -- Departure.stillPlaying: seated carol-alice-bob with alice attacking,
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
         in do
              HU.assertEqual "set" (Just S.carol) (Combat.Type.defender (GameState.combat busy))
              HU.assertEqual "and cleared at end of combat" Nothing (Combat.Type.defender (GameState.combat (Combat.clearCombat busy)))
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
    [ -- SYNTHETIC (labeled crutch, spec §4): a "steal until end of turn, no haste"
      -- effect. A real card would grant haste (masking CR 302.6) or be an Aura
      -- (Attach, out of M4.5 scope). Retired by the Auras / Control Magic phase (#33).
      HU.testCase "CR 302.6 a creature that just changed control is summoning sick (no haste)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            slot = SlotName.MkSlotName (Text.pack "target")
            steal =
              Resolve.applyEffect
                oid
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature oid))
                (Effect.GainControl Duration.UntilEndOfTurn slot)
            after = snd (Engine.runGamePure S.identityAnswer base steal)
        HU.assertEqual "alice controls it" (Just S.alice) (Projection.controllerOf oid after)
        HU.assertBool "but it is summoning sick, so it cannot attack this turn" (not (Combat.canAttack S.alice oid after))
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
      HU.testCase "the defending player is the non-active player" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoard piker 1 1
        HU.assertEqual "bob defends" [S.bob] (Combat.defendingPlayers gs),
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
          HU.assertEqual "keywords" (Set.singleton keyword) (Projection.keywordsOf oid gs)
          HU.assertBool "hasKeyword" (Projection.hasKeyword keyword oid gs)
   in Tasty.testGroup
        "Keyword"
        ( fmap carriesOnly S.m2aKeywords
            <> [ HU.testCase "a Piker has no keywords" $ do
                   piker <- Registry.printing registry "Goblin Piker"
                   let (oid, gs) = S.addCreature piker S.alice gs0
                   HU.assertEqual "none" Set.empty (Projection.keywordsOf oid gs)
                   HU.assertBool "no flying" (not (Projection.hasKeyword Keyword.Flying oid gs)),
                 HU.testCase "a Mountain has no keywords" $ do
                   mountain <- Registry.printing registry "Mountain"
                   let gs = S.landsInPlay mountain 1
                   case Game.zoneMembers Zone.Battlefield S.alice gs of
                     [] -> HU.assertFailure "fixture should have one Mountain"
                     oid : _ -> HU.assertEqual "none" Set.empty (Projection.keywordsOf oid gs),
                 HU.testCase "an unknown id has no keywords" $
                   HU.assertEqual "none" Set.empty (Projection.keywordsOf (ObjectId.MkObjectId 999) gs0),
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

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Combat"
    [ combatLegalityTests registry,
      declareTests registry,
      combatDamageTests registry,
      keywordTests registry,
      firstStrikeTests registry,
      m2bExitTests registry,
      defenderTests registry,
      defendingPlayerTests,
      vigilanceTests registry,
      hasteTests registry,
      evasionTests registry,
      controlChangeSicknessTests registry
    ]
