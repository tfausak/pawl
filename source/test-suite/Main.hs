{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Action as Action
import qualified Pawl.Card as Card
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Replay as Replay
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Combat as Combat.Type
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.Departure as Departure
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Mana as Mana.Type
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.ManaUnit as ManaUnit
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified System.Random as Random
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree =
  Tasty.testGroup
    "pawl"
    [ programTests,
      cardTests,
      turnTests,
      turnDataTests,
      skipTests,
      gameTests,
      actionTests,
      setupTests,
      sbaTests,
      engineTests,
      replayTests,
      propertyTests,
      ruleTests,
      quantityTests,
      manaTests,
      deckTests,
      discardTests,
      castTests,
      stackTests,
      castEngineTests,
      sicknessTests,
      damageTests,
      objectFactTests,
      creatureSbaTests,
      combatLegalityTests,
      combatReplayTests,
      declareTests,
      combatDamageTests,
      keywordTests,
      m2aCardTests,
      m2bCardTests,
      firstStrikeTests,
      defenderTests,
      vigilanceTests,
      hasteTests,
      evasionTests
    ]

lifeOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe Integer
lifeOf pid gs = fmap Player.life (Map.lookup pid (GameState.players gs))

-- Attack with everything, block per the given plan, then deal damage.
fightWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
fightWith answer gs =
  snd $ Engine.runGamePure answer gs $ do
    Combat.declareAttackers alice
    Combat.declareBlockers
    Damage.dealCombatDamage

combatDamageTests :: Tasty.TestTree
combatDamageTests =
  Tasty.testGroup
    "CombatDamage"
    [ HU.testCase "CR 510.1b an unblocked attacker damages the defending player" $
        let (gs, _, _) = combatBoard 1 0
            after = fightWith aggressiveAnswer gs
         in -- A Piker is a 2/1, and bob starts at 20.
            HU.assertEqual "bob took 2" (Just 18) (lifeOf bob after),
      HU.testCase "CR 509 a blocked attacker does not damage the player" $
        let (gs, _, _) = combatBoard 1 1
            after = fightWith aggressiveAnswer gs
         in HU.assertEqual "bob untouched" (Just 20) (lifeOf bob after),
      HU.testCase "CR 510.1c a single blocker takes all the damage, unprompted" $
        -- If the engine wrongly prompts here, this interpreter answers with an
        -- empty division, which is illegal (it does not total the attacker's
        -- power), so it is rejected and the blocker takes 0 -- and the assertion
        -- below fails. That is why this proves "unprompted" without an `error`,
        -- which the no-partial-functions rule forbids anyway.
        let (gs, _, theirs) = combatBoard 1 1
            noAssign :: Prompt.Prompt r -> r
            noAssign p = case p of
              Prompt.AssignCombatDamage {} -> Map.empty
              _ -> aggressiveAnswer p
            after = fightWith noAssign gs
         in case theirs of
              [] -> HU.assertFailure "fixture should have a blocker"
              b : _ -> HU.assertEqual "took 2" (Just 2) (damageOf b after),
      HU.testCase "CR 510.2 a 2/1 trade kills BOTH creatures" $
        -- The simultaneity test. Sequential damage kills only one, because the
        -- blocker would be in the graveyard before it dealt its damage.
        let (gs, _, _) = combatBoard 1 1
            after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
         in do
              HU.assertEqual "alice's is dead" 0 (creaturesInPlay alice after)
              HU.assertEqual "bob's is dead" 0 (creaturesInPlay bob after),
      HU.testCase "CR 510.1c a free division of 2 across two blockers kills both" $
        let (gs, _, theirs) = combatBoard 1 2
            split :: Prompt.Prompt r -> r
            split p = case p of
              Prompt.AssignCombatDamage _ _ _ ids _ -> Map.fromList (map (\b -> (b, 1)) (Set.toList ids))
              _ -> aggressiveAnswer p
            after = Sba.checkStateBasedActions (fightWith split gs)
         in do
              HU.assertEqual "both blockers dead" 0 (creaturesInPlay bob after)
              HU.assertEqual "expected two blockers" 2 (length theirs),
      HU.testCase "CR 510.1c the same 2 damage on one blocker kills only it" $
        let (gs, _, _) = combatBoard 1 2
            dump :: Prompt.Prompt r -> r
            dump p = case p of
              Prompt.AssignCombatDamage _ _ _ ids n -> case Set.toList ids of
                b : _ -> Map.singleton b n
                [] -> Map.empty
              _ -> aggressiveAnswer p
            after = Sba.checkStateBasedActions (fightWith dump gs)
         in HU.assertEqual "one blocker survives" 1 (creaturesInPlay bob after),
      HU.testCase "CR 510.1e an illegal division is rejected and deals nothing" $
        -- Not a reachable game state: this is the engine's defense against a
        -- broken interpreter. See the spec, section 3.
        let (gs, _, _) = combatBoard 1 2
            cheat :: Prompt.Prompt r -> r
            cheat p = case p of
              Prompt.AssignCombatDamage _ _ _ ids _ -> Map.fromList (map (\b -> (b, 99)) (Set.toList ids))
              _ -> aggressiveAnswer p
            after = Sba.checkStateBasedActions (fightWith cheat gs)
         in HU.assertEqual "both blockers survive" 2 (creaturesInPlay bob after)
    ]

-- Attacks with everything and blocks the first attacker with everything.
-- Deliberately maximal: it makes combat happen without the test having to
-- hand-build a Combat record.
aggressiveAnswer :: Prompt.Prompt r -> r
aggressiveAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction {} -> A.Pass
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> Map.empty
    a : _ -> Map.fromList (map (\b -> (b, a)) mine)
  Prompt.AssignCombatDamage _ _ _ ids n -> case Set.toList ids of
    b : _ -> Map.singleton b n
    [] -> Map.empty

declaredAttackers :: GameState.GameState -> [ObjectId.ObjectId]
declaredAttackers gs = Map.keys (Combat.Type.attackers (GameState.combat gs))

declareTests :: Tasty.TestTree
declareTests =
  Tasty.testGroup
    "Declare"
    [ HU.testCase "CR 508.1f declaring an attacker taps it" $
        let (gs, mine, _) = combatBoard 1 1
            after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
         in do
              HU.assertEqual "one attacker" mine (declaredAttackers after)
              HU.assertEqual "tapped" 1 (tappedCount alice after),
      HU.testCase "CR 508.1 attackers attack the defending player" $
        let (gs, mine, _) = combatBoard 1 1
            after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              oid : _ ->
                HU.assertEqual
                  "attacking bob"
                  (Just (AttackTarget.OfPlayer bob))
                  (Map.lookup oid (Combat.Type.attackers (GameState.combat after))),
      HU.testCase "an illegal attacker in the answer is dropped" $
        -- The interpreter names bob's creature. It is not alice's to attack with.
        let (gs, _, theirs) = combatBoard 1 1
            liar :: Prompt.Prompt r -> r
            liar p = case p of
              Prompt.DeclareAttackers {} -> theirs
              _ -> aggressiveAnswer p
            after = snd (Engine.runGamePure liar gs (Combat.declareAttackers alice))
         in HU.assertEqual "nothing attacks" [] (declaredAttackers after),
      HU.testCase "CR 509.1 a blocker is recorded against the attacker it blocks" $
        let (gs, mine, theirs) = combatBoard 1 1
            steps = do
              Combat.declareAttackers alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure aggressiveAnswer gs steps)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              attacker : _ ->
                HU.assertEqual "blocked by bob's creature" (Set.fromList theirs) (Combat.blockersOf attacker after),
      HU.testCase "an unblocked attacker has no blockers" $
        let (gs, mine, _) = combatBoard 1 0
            steps = do
              Combat.declareAttackers alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure aggressiveAnswer gs steps)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              attacker : _ -> HU.assertBool "unblocked" (not (Combat.isBlocked attacker after)),
      HU.testCase "no legal attackers means no prompt and no attacks" $
        -- combatBoard 0 1 gives alice nothing. A prompt here would be the engine
        -- asking a question with exactly one answer.
        let (gs, _, _) = combatBoard 0 1
            after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
         in HU.assertEqual "nothing attacks" [] (declaredAttackers after),
      -- The end-to-end summoning sickness scenario the spec names: a creature
      -- that just arrived cannot attack, and the SAME creature can once its
      -- controller's untap step has settled it. The halves are tested in Tasks 1
      -- and 4; this proves they compose.
      HU.testCase "CR 302.6 a creature cannot attack the turn it arrives, and can after untapping" $
        let (gs, _, _) = combatBoard 1 1
            arrived = justArrived gs
            sameTurn = snd (Engine.runGamePure aggressiveAnswer arrived (Combat.declareAttackers alice))
            nextTurn =
              snd $
                Engine.runGamePure aggressiveAnswer arrived $ do
                  Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap)
                  Combat.declareAttackers alice
         in do
              HU.assertEqual "cannot attack the turn it arrives" [] (declaredAttackers sameTurn)
              HU.assertEqual "can attack after untapping" 1 (length (declaredAttackers nextTurn))
    ]

combatReplayTests :: Tasty.TestTree
combatReplayTests =
  let decider = Decide.deciderFor alice (Setup.emptyGame bothPlayers)
      oid = ObjectId.MkObjectId 7
      attackPrompt = Prompt.DeclareAttackers decider alice [oid]
      blockPrompt = Prompt.DeclareBlockers decider bob [oid] [oid]
      damagePrompt = Prompt.AssignCombatDamage decider alice oid (Set.singleton oid) 2
   in Tasty.testGroup
        "CombatReplay"
        [ HU.testCase "attackers round-trip through the transcript" $
            HU.assertEqual "round trip" (Just [oid]) (Replay.decode attackPrompt (Replay.encode attackPrompt [oid])),
          HU.testCase "blockers round-trip through the transcript" $
            let answer = Map.singleton oid oid
             in HU.assertEqual "round trip" (Just answer) (Replay.decode blockPrompt (Replay.encode blockPrompt answer)),
          HU.testCase "a damage assignment round-trips through the transcript" $
            let answer :: Map.Map ObjectId.ObjectId Natural.Natural
                answer = Map.singleton oid 2
             in HU.assertEqual "round trip" (Just answer) (Replay.decode damagePrompt (Replay.encode damagePrompt answer)),
          HU.testCase "a mismatched response decodes to Nothing" $
            HU.assertEqual "mismatch" Nothing (Replay.decode attackPrompt (Response.Shuffled [oid])),
          HU.testCase "defaultAnswer attacks with nothing" $
            HU.assertEqual "no attacks" [] (Replay.defaultAnswer attackPrompt),
          HU.testCase "defaultAnswer blocks with nothing" $
            HU.assertEqual "no blocks" Map.empty (Replay.defaultAnswer blockPrompt),
          HU.testCase "defaultAnswer assigns a LEGAL division" $
            -- Total must equal the attacker's power, or the fallback would be
            -- rejected by Task 7's validation and deal no damage at all.
            HU.assertEqual "all to one blocker" (Map.singleton oid 2) (Replay.defaultAnswer damagePrompt)
        ]

-- alice is active with one Settled creature per printing in `mine`; bob defends
-- with one per printing in `theirs`. Returns their ids alongside the state, in
-- the order the printings were given.
combatBoardOf :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoardOf mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = addCreature p pid g in (ids ++ [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll alice mine (Setup.emptyGame bothPlayers)
      (yours, gs2) = addAll bob theirs gs1
   in ( gs2
          { GameState.activePlayer = alice,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            -- The steps after declare attackers, so a runStep-driven test (Tasks
            -- 2 and 4) can advance through combat. Direct-call tests ignore it.
            GameState.remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain,
                  Phase.Ending EndingStep.EndStep,
                  Phase.Ending EndingStep.Cleanup
                ]
          },
        ours,
        yours
      )

-- alice is active with `a` Settled Pikers; bob defends with `b` Settled Pikers.
-- Returns the attackers' ids and the blockers' ids alongside the state.
combatBoard :: Int -> Int -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoard a b = combatBoardOf (replicate a Card.pikerPrinting) (replicate b Card.pikerPrinting)

defenderTests :: Tasty.TestTree
defenderTests =
  Tasty.testGroup
    "Defender"
    [ HU.testCase "CR 702.3b a creature with defender can't attack" $
        let (gs, mine, _) = combatBoardOf [Card.ogreSentryPrinting] [Card.pikerPrinting]
         in case mine of
              [] -> HU.assertFailure "fixture should have one creature"
              oid : _ -> HU.assertBool "can't attack" (not (Combat.canAttack alice oid gs)),
      HU.testCase "CR 702.3b a creature with defender is not offered as a legal attacker" $
        let (gs, _, _) = combatBoardOf [Card.ogreSentryPrinting] [Card.pikerPrinting]
         in HU.assertEqual "none" [] (Combat.legalAttackers alice gs),
      HU.testCase "CR 702.3b defender does not stop it blocking" $
        -- 702.3b says "can't attack" and nothing else. A defender that could not
        -- block would be a Wall in the pre-2004 sense, and that is not the rule.
        let (gs, _, theirs) = combatBoardOf [Card.pikerPrinting] [Card.ogreSentryPrinting]
         in case theirs of
              [] -> HU.assertFailure "fixture should have one blocker"
              oid : _ -> HU.assertBool "may block" (Combat.canBlock bob oid gs),
      HU.testCase "a creature without defender is still offered" $
        -- The control. If defender were implemented as "nothing may attack", the
        -- test above would pass and this one would fail.
        let (gs, mine, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
         in HU.assertEqual "one" mine (Combat.legalAttackers alice gs),
      HU.testCase "CR 702.3b a defender is skipped but its neighbor still attacks" $
        let (gs, mine, _) = combatBoardOf [Card.ogreSentryPrinting, Card.pikerPrinting] [Card.pikerPrinting]
         in case mine of
              [_, piker] -> HU.assertEqual "only the piker" [piker] (Combat.legalAttackers alice gs)
              _ -> HU.assertFailure "fixture should have two creatures"
    ]

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- Re-sicken alice's creatures, as though they had just resolved this turn.
justArrived :: GameState.GameState -> GameState.GameState
justArrived gs =
  let sicken o = if Object.owner o == alice then o {Object.sickness = Sickness.Sick} else o
   in gs {GameState.objects = Map.map sicken (GameState.objects gs)}

hasteTests :: Tasty.TestTree
hasteTests =
  Tasty.testGroup
    "Haste"
    [ HU.testCase "CR 702.10b a creature with haste attacks the turn it arrives" $
        let (gs, _, _) = combatBoardOf [Card.goblinChariotPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer (justArrived gs) (Combat.declareAttackers alice))
         in HU.assertEqual "attacks" 1 (length (declaredAttackers after)),
      HU.testCase "CR 302.6 the same creature without haste cannot" $
        -- The control. Goblin Chariot and Goblin Piker are both 2/2-ish Goblin
        -- Warriors; the ONLY difference the engine can see is the keyword.
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer (justArrived gs) (Combat.declareAttackers alice))
         in HU.assertEqual "cannot attack" [] (declaredAttackers after),
      HU.testCase "CR 702.10b haste is not needed once the creature has settled" $
        let (gs, mine, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
         in HU.assertEqual "attacks" mine (declaredAttackers after),
      HU.testCase "CR 702.10b a hasty creature and a sick one, in the same declaration" $
        -- Both sick; only the Chariot may attack. A blanket "sickness ignored"
        -- bug would let both through.
        let (gs, mine, _) = combatBoardOf [Card.goblinChariotPrinting, Card.pikerPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer (justArrived gs) (Combat.declareAttackers alice))
         in case mine of
              [chariot, _] -> HU.assertEqual "only the chariot" [chariot] (declaredAttackers after)
              _ -> HU.assertFailure "fixture should have two creatures"
    ]

-- Declare attackers with everything, then hand back the state and the ids.
attacking :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
attacking mine theirs =
  let (gs, ours, yours) = combatBoardOf mine theirs
      after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
   in (after, ours, yours)

evasionTests :: Tasty.TestTree
evasionTests =
  Tasty.testGroup
    "Evasion"
    [ HU.testCase "CR 702.9b a declaration in which a ground creature blocks a flier is illegal" $
        let (gs, mine, theirs) = attacking [Card.birdMaidenPrinting] [Card.pikerPrinting]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "illegal" (not (Combat.legalBlockDeclaration bob (Map.singleton b a) gs))
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.17b a reach creature may block a flier" $
        -- THE FALSIFIER. Fails against any implementation that asks "does the
        -- blocker have flying?"
        let (gs, mine, theirs) = attacking [Card.birdMaidenPrinting] [Card.nimbleBirdstickerPrinting]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration bob (Map.singleton b a) gs)
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.9b a flier may block a ground creature" $
        -- The asymmetry: 702.9b's second sentence. Fails if flying is implemented
        -- as a symmetric predicate.
        let (gs, mine, theirs) = attacking [Card.pikerPrinting] [Card.birdMaidenPrinting]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration bob (Map.singleton b a) gs)
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.9b a flier may block a flier" $
        let (gs, mine, theirs) = attacking [Card.birdMaidenPrinting] [Card.birdMaidenPrinting]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration bob (Map.singleton b a) gs)
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 509.1a a ground creature is still a legal blocker while a flier attacks" $
        -- 509.1a is about the blocker ALONE: it can block SOMETHING. This test
        -- fails if evasion is wrongly implemented as a filter on the candidates.
        let (gs, _, theirs) = attacking [Card.birdMaidenPrinting] [Card.pikerPrinting]
         in HU.assertEqual "still offered" theirs (Combat.legalBlockers bob gs),
      HU.testCase "CR 509.1b an illegal declaration is rejected WHOLE, not repaired" $
        -- aggressiveAnswer blocks the first attacker with EVERYTHING, so bob
        -- declares the reach creature (legal) AND the Piker (illegal) on the
        -- flier. Neither may block. A per-pair filter would drop the Piker and
        -- let the Birdsticker's block stand -- which is what M1b does today, and
        -- is unsound: under menace, dropping one blocker from a pair manufactures
        -- an illegal single block.
        let (gs, _, _) = combatBoardOf [Card.birdMaidenPrinting] [Card.nimbleBirdstickerPrinting, Card.pikerPrinting]
            steps = do
              Combat.declareAttackers alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure aggressiveAnswer gs steps)
         in case Map.keys (Combat.Type.attackers (GameState.combat after)) of
              [] -> HU.assertFailure "fixture should have an attacker"
              a : _ -> HU.assertEqual "nobody blocks" Set.empty (Combat.blockersOf a after),
      HU.testCase "CR 509.1b a wholly legal declaration is accepted" $
        -- The control for the test above: with only the reach creature, the same
        -- interpreter produces a legal declaration and the block stands.
        let (gs, _, theirs) = combatBoardOf [Card.birdMaidenPrinting] [Card.nimbleBirdstickerPrinting]
            steps = do
              Combat.declareAttackers alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure aggressiveAnswer gs steps)
         in case Map.keys (Combat.Type.attackers (GameState.combat after)) of
              [] -> HU.assertFailure "fixture should have an attacker"
              a : _ -> HU.assertEqual "the reach creature blocks" (Set.fromList theirs) (Combat.blockersOf a after),
      HU.testCase "CR 509.1a a Mountain is not a legal blocker, flier or no flier" $
        -- The classification, from the other side: `canBlock` asks
        -- is-it-a-creature, never which card it is. M1b tests "a land may not
        -- attack" but never that a land may not BLOCK, so this closes a real gap
        -- rather than restating one.
        let (gs, mine, _) = attacking [Card.birdMaidenPrinting] []
            withLand = snd (addCreature Card.mountainPrinting bob gs)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              _ : _ -> HU.assertEqual "no legal blockers" [] (Combat.legalBlockers bob withLand),
      HU.testCase "CR 702.9b a flier connects past an untapped ground creature, in a real combat" $
        -- The integration case, and it is precise rather than vacuous. WITH
        -- flying: nothing may block, bob takes 1, and both creatures live.
        -- WITHOUT flying: the Piker blocks, bob takes 0, and the two TRADE (Bird
        -- Maiden is 1/2, Piker is 2/1). All three assertions distinguish them.
        let (gs, _, _) = combatBoardOf [Card.birdMaidenPrinting] [Card.pikerPrinting]
            after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
         in do
              HU.assertEqual "bob took 1" (Just 19) (lifeOf bob after)
              HU.assertEqual "the flier lives" 1 (creaturesInPlay alice after)
              HU.assertEqual "the would-be blocker lives" 1 (creaturesInPlay bob after)
    ]

vigilanceTests :: Tasty.TestTree
vigilanceTests =
  Tasty.testGroup
    "Vigilance"
    [ HU.testCase "CR 702.20b attacking doesn't tap a creature with vigilance, but does tap its neighbor" $
        -- Both creatures in ONE declaration, so a blanket "nothing taps" bug
        -- cannot pass: the Piker must still tap.
        let (gs, mine, _) = combatBoardOf [Card.windseekerCentaurPrinting, Card.pikerPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
         in case mine of
              [centaur, piker] -> do
                HU.assertEqual "both attacking" 2 (length (declaredAttackers after))
                HU.assertEqual "the centaur is untapped" (Just TapState.Untapped) (tapStateOf centaur after)
                HU.assertEqual "the piker is tapped" (Just TapState.Tapped) (tapStateOf piker after)
              _ -> HU.assertFailure "fixture should have two attackers",
      HU.testCase "CR 702.20b vigilance still attacks" $
        -- Vigilance is not a legality question: the creature is declared as an
        -- attacker exactly as normal. It simply skips CR 508.1f's tap.
        let (gs, mine, _) = combatBoardOf [Card.windseekerCentaurPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
         in HU.assertEqual "attacking" mine (declaredAttackers after),
      HU.testCase "CR 702.20b an untapped vigilant attacker can still be blocked" $
        -- It is attacking, so it is in the Combat record, tapped or not.
        let (gs, mine, theirs) = combatBoardOf [Card.windseekerCentaurPrinting] [Card.pikerPrinting]
            steps = do
              Combat.declareAttackers alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure aggressiveAnswer gs steps)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              attacker : _ -> HU.assertEqual "blocked" (Set.fromList theirs) (Combat.blockersOf attacker after)
    ]

combatLegalityTests :: Tasty.TestTree
combatLegalityTests =
  Tasty.testGroup
    "CombatLegality"
    [ HU.testCase "a Settled untapped creature may attack" $
        let (gs, mine, _) = combatBoard 1 0
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              oid : _ -> HU.assertBool "may attack" (Combat.canAttack alice oid gs),
      HU.testCase "CR 302.6 a summoning sick creature may not attack" $
        let (gs, mine, _) = combatBoard 1 0
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              oid : _ ->
                let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
                 in HU.assertBool "may not attack" (not (Combat.canAttack alice oid sick)),
      HU.testCase "CR 508.1a a tapped creature may not attack" $
        let (gs, mine, _) = combatBoard 1 0
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              oid : _ ->
                let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
                 in HU.assertBool "may not attack" (not (Combat.canAttack alice oid tapped)),
      HU.testCase "a land may not attack" $
        let gs = (mountainsInPlay 1) {GameState.activePlayer = alice}
         in case Game.zoneMembers Zone.Battlefield alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ -> HU.assertBool "may not attack" (not (Combat.canAttack alice oid gs)),
      HU.testCase "you may not attack with a creature you do not control" $
        let (gs, _, theirs) = combatBoard 1 1
         in case theirs of
              [] -> HU.assertFailure "fixture should have a blocker"
              oid : _ -> HU.assertBool "not alice's" (not (Combat.canAttack alice oid gs)),
      -- CR 302.6 restricts attacking and tap abilities. It says NOTHING about
      -- blocking, and getting this wrong is the classic beginner bug.
      HU.testCase "CR 302.6 a summoning sick creature MAY block" $
        let (gs, _, theirs) = combatBoard 1 1
         in case theirs of
              [] -> HU.assertFailure "fixture should have a blocker"
              oid : _ ->
                let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
                 in HU.assertBool "may block" (Combat.canBlock bob oid sick),
      HU.testCase "CR 509.1a a tapped creature may not block" $
        let (gs, _, theirs) = combatBoard 1 1
         in case theirs of
              [] -> HU.assertFailure "fixture should have a blocker"
              oid : _ ->
                let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
                 in HU.assertBool "may not block" (not (Combat.canBlock bob oid tapped)),
      HU.testCase "legalAttackers lists exactly the active player's creatures" $
        let (gs, mine, _) = combatBoard 2 3
         in HU.assertEqual "two" mine (Combat.legalAttackers alice gs),
      HU.testCase "the defending player is the non-active player" $
        let (gs, _, _) = combatBoard 1 1
         in HU.assertEqual "bob defends" [bob] (Combat.defendingPlayers gs),
      HU.testCase "combat starts empty and clears" $
        let (gs, mine, _) = combatBoard 1 0
            busy = case mine of
              [] -> gs
              oid : _ ->
                gs
                  { GameState.combat =
                      Combat.Type.MkCombat
                        { Combat.Type.attackers = Map.singleton oid (AttackTarget.OfPlayer bob),
                          Combat.Type.blockers = Map.empty,
                          Combat.Type.struckFirst = Nothing
                        }
                  }
         in do
              HU.assertEqual "starts empty" Map.empty (Combat.Type.attackers (GameState.combat gs))
              HU.assertEqual "clears" Map.empty (Combat.Type.attackers (GameState.combat (Combat.clearCombat busy)))
    ]

objectFactTests :: Tasty.TestTree
objectFactTests =
  Tasty.testGroup
    "ObjectFacts"
    [ HU.testCase "a Piker's power and toughness are 2 and 1" $
        let (oid, gs) = addPiker alice (Setup.emptyGame bothPlayers)
         in do
              HU.assertEqual "power" (Just 2) (Game.powerOf oid gs)
              HU.assertEqual "toughness" (Just 1) (Game.toughnessOf oid gs),
      HU.testCase "a Mountain has no power or toughness" $
        let gs = mountainsInPlay 1
         in case Game.zoneMembers Zone.Battlefield alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ -> do
                HU.assertEqual "power" Nothing (Game.powerOf oid gs)
                HU.assertEqual "toughness" Nothing (Game.toughnessOf oid gs),
      HU.testCase "controllerOf is the owner while nothing can change control" $
        let (oid, gs) = addPiker bob (Setup.emptyGame bothPlayers)
         in HU.assertEqual "controller" (Just bob) (Game.controllerOf oid gs),
      HU.testCase "an unknown id has no facts" $
        let gs = Setup.emptyGame bothPlayers
            missing = ObjectId.MkObjectId 999
         in do
              HU.assertEqual "power" Nothing (Game.powerOf missing gs)
              HU.assertEqual "controller" Nothing (Game.controllerOf missing gs)
    ]

creatureSbaTests :: Tasty.TestTree
creatureSbaTests =
  Tasty.testGroup
    "CreatureSba"
    [ HU.testCase "CR 704.5g a creature with lethal damage is destroyed" $
        let (oid, gs) = addPiker alice (Setup.emptyGame bothPlayers)
            after = Sba.checkStateBasedActions (markDamage oid 1 gs)
         in do
              HU.assertEqual "off the battlefield" [] (Game.zoneMembers Zone.Battlefield alice after)
              HU.assertEqual "in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard alice after)),
      HU.testCase "CR 704.5g damage below toughness is not lethal" $
        let (oid, gs) = addPiker alice (Setup.emptyGame bothPlayers)
            -- A Piker is 2/1, so 0 marked damage is survivable and 1 is not.
            after = Sba.checkStateBasedActions (markDamage oid 0 gs)
         in HU.assertEqual "still there" 1 (length (Game.zoneMembers Zone.Battlefield alice after)),
      HU.testCase "CR 704.5g a Mountain with damage marked is not destroyed" $
        -- Not a creature: 704.5f/g do not apply. This is the classification
        -- doing its job -- the check never asks WHICH card it is.
        let gs = mountainsInPlay 1
         in case Game.zoneMembers Zone.Battlefield alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ ->
                HU.assertEqual
                  "survives"
                  1
                  (length (Game.zoneMembers Zone.Battlefield alice (Sba.checkStateBasedActions (markDamage oid 5 gs)))),
      HU.testCase "a destroyed creature conserves objects" $
        let (oid, gs) = addPiker alice (Setup.emptyGame bothPlayers)
            marked = markDamage oid 1 gs
         in HU.assertEqual
              "conserved"
              (Game.objectCount marked)
              (Game.objectCount (Sba.checkStateBasedActions marked))
    ]

damageOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural.Natural
damageOf oid gs = fmap Object.damage (Game.lookupObject oid gs)

markDamage :: ObjectId.ObjectId -> Natural.Natural -> GameState.GameState -> GameState.GameState
markDamage oid n gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.damage = n}) oid (GameState.objects gs)}

damageTests :: Tasty.TestTree
damageTests =
  Tasty.testGroup
    "Damage"
    [ HU.testCase "a permanent starts with no damage marked" $
        let (oid, gs) = addPiker alice (Setup.emptyGame bothPlayers)
         in HU.assertEqual "none" (Just 0) (damageOf oid gs),
      HU.testCase "CR 514.2 marked damage is removed" $
        let (oid, gs) = addPiker alice (Setup.emptyGame bothPlayers)
         in HU.assertEqual "removed" (Just 0) (damageOf oid (Damage.removeAllDamage (markDamage oid 1 gs))),
      HU.testCase "CR 514.2 damage wears off at the cleanup step" $
        let (oid, gs) = addPiker alice (Setup.emptyGame bothPlayers)
            marked = markDamage oid 1 gs
            after = snd (Engine.runGamePure identityAnswer marked (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
         in HU.assertEqual "worn off" (Just 0) (damageOf oid after),
      HU.testCase "CR 400.7 a new object carries no damage forward" $
        let (oid, gs) = addPiker alice (Setup.emptyGame bothPlayers)
            marked = markDamage oid 1 gs
            after = Game.changeZone oid Zone.Graveyard marked
         in case Game.zoneMembers Zone.Graveyard alice after of
              [] -> HU.assertFailure "expected a card in the graveyard"
              new : _ -> HU.assertEqual "fresh object, no damage" (Just 0) (damageOf new after)
    ]

-- A Piker put onto the battlefield under pid's control, untapped and Settled.
--
-- Any printing, on the battlefield under pid's control, untapped and Settled.
addCreature :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addCreature printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled
          }
   in ( oid,
        gs1
          { GameState.objects = Map.insert oid obj (GameState.objects gs1),
            GameState.battlefield = Set.insert oid (GameState.battlefield gs1)
          }
      )

addPiker :: PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addPiker = addCreature Card.pikerPrinting

-- The printings M2a adds, paired with the single keyword each must carry.
m2aPrintings :: [(Printing.Printing, Keyword.Keyword)]
m2aPrintings =
  [ (Card.birdMaidenPrinting, Keyword.Flying),
    (Card.nimbleBirdstickerPrinting, Keyword.Reach),
    (Card.ogreSentryPrinting, Keyword.Defender),
    (Card.windseekerCentaurPrinting, Keyword.Vigilance),
    (Card.goblinChariotPrinting, Keyword.Haste)
  ]

keywordTests :: Tasty.TestTree
keywordTests =
  let gs0 = Setup.emptyGame bothPlayers
      -- Each M2a printing carries exactly its one keyword and no other.
      carriesOnly (printing, keyword) =
        let (oid, gs) = addCreature printing alice gs0
            name = Text.unpack (Card.Type.name (Printing.card printing))
         in HU.testCase (name ++ " carries exactly " ++ show keyword) $ do
              HU.assertEqual "keywords" (Set.singleton keyword) (Game.keywordsOf oid gs)
              HU.assertBool "hasKeyword" (Game.hasKeyword keyword oid gs)
   in Tasty.testGroup
        "Keyword"
        ( map carriesOnly m2aPrintings
            ++ [ HU.testCase "a Piker has no keywords" $
                   let (oid, gs) = addPiker alice gs0
                    in do
                         HU.assertEqual "none" Set.empty (Game.keywordsOf oid gs)
                         HU.assertBool "no flying" (not (Game.hasKeyword Keyword.Flying oid gs)),
                 HU.testCase "a Mountain has no keywords" $
                   let gs = mountainsInPlay 1
                    in case Game.zoneMembers Zone.Battlefield alice gs of
                         [] -> HU.assertFailure "fixture should have one Mountain"
                         oid : _ -> HU.assertEqual "none" Set.empty (Game.keywordsOf oid gs),
                 HU.testCase "an unknown id has no keywords" $
                   HU.assertEqual "none" Set.empty (Game.keywordsOf (ObjectId.MkObjectId 999) gs0),
                 -- Flying is on Bird Maiden and NOT on Nimble Birdsticker. If this
                 -- passes while the reach case above also passes, the two keywords
                 -- are genuinely distinct rather than one flag.
                 HU.testCase "reach is not flying" $
                   let (oid, gs) = addCreature Card.nimbleBirdstickerPrinting alice gs0
                    in HU.assertBool "no flying" (not (Game.hasKeyword Keyword.Flying oid gs))
               ]
        )

redCost :: [ManaSymbol.ManaSymbol] -> Maybe ManaCost.ManaCost
redCost symbols = Just (ManaCost.MkManaCost symbols)

m2aCardTests :: Tasty.TestTree
m2aCardTests =
  let card = Printing.card
      red = ManaSymbol.OfType (ManaType.Colored Color.Red)
   in Tasty.testGroup
        "M2aCards"
        [ HU.testCase "Bird Maiden is a {2}{R} 1/2 Human Bird with flying" $ do
            HU.assertEqual "name" (Text.pack "Bird Maiden") (Card.Type.name (card Card.birdMaidenPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card Card.birdMaidenPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power (card Card.birdMaidenPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card Card.birdMaidenPrinting))
            HU.assertEqual
              "subtypes"
              (Set.fromList [Subtype.Human, Subtype.Bird])
              (TypeLine.subtypes (Card.Type.typeLine (card Card.birdMaidenPrinting))),
          HU.testCase "Nimble Birdsticker is a {2}{R} 2/3 Goblin with reach" $ do
            HU.assertEqual "name" (Text.pack "Nimble Birdsticker") (Card.Type.name (card Card.nimbleBirdstickerPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card Card.nimbleBirdstickerPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.nimbleBirdstickerPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness (card Card.nimbleBirdstickerPrinting)),
          HU.testCase "Ogre Sentry is a {1}{R} 3/3 Ogre Warrior with defender" $ do
            HU.assertEqual "name" (Text.pack "Ogre Sentry") (Card.Type.name (card Card.ogreSentryPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red]) (Card.Type.manaCost (card Card.ogreSentryPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power (card Card.ogreSentryPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness (card Card.ogreSentryPrinting)),
          HU.testCase "Windseeker Centaur is a {1}{R}{R} 2/2 Centaur with vigilance" $ do
            HU.assertEqual "name" (Text.pack "Windseeker Centaur") (Card.Type.name (card Card.windseekerCentaurPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red, red]) (Card.Type.manaCost (card Card.windseekerCentaurPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.windseekerCentaurPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card Card.windseekerCentaurPrinting)),
          HU.testCase "Goblin Chariot is a {2}{R} 2/2 Goblin Warrior with haste" $ do
            HU.assertEqual "name" (Text.pack "Goblin Chariot") (Card.Type.name (card Card.goblinChariotPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card Card.goblinChariotPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.goblinChariotPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card Card.goblinChariotPrinting)),
          HU.testCase "all five are creatures and none is a land" $
            HU.assertBool "creatures" $
              all
                (\(p, _) -> Card.isCreature (card p) && not (Card.isLand (card p)))
                m2aPrintings
        ]

sicknessOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Sickness.Sickness
sicknessOf oid gs = fmap Object.sickness (Game.lookupObject oid gs)

sicknessTests :: Tasty.TestTree
sicknessTests =
  Tasty.testGroup
    "Sickness"
    [ HU.testCase "CR 302.6 a permanent entering the battlefield is summoning sick" $
        -- changeZone mints a new object, so the id to inspect is the new one.
        let (gs, oid) = pikerInHand 3 Phase.PrecombatMain
            after = Game.changeZone oid Zone.Battlefield gs
         in case Game.zoneMembers Zone.Battlefield alice after of
              [] -> HU.assertFailure "expected a permanent"
              ids -> case filter (\o -> sicknessOf o after == Just Sickness.Sick) ids of
                [] -> HU.assertFailure "the new permanent should be Sick"
                _ -> pure (),
      HU.testCase "CR 302.6 the untap step settles the active player's permanents" $
        let (oid, gs) = addPiker alice (Setup.emptyGame bothPlayers)
            sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
            after = snd (Engine.runGamePure identityAnswer sick (Engine.settleAll alice))
         in HU.assertEqual "settled" (Just Sickness.Settled) (sicknessOf oid after),
      HU.testCase "CR 302.6 settling does not touch the other player's permanents" $
        let (oid, gs) = addPiker bob (Setup.emptyGame bothPlayers)
            sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
            after = snd (Engine.runGamePure identityAnswer sick (Engine.settleAll alice))
         in HU.assertEqual "still sick" (Just Sickness.Sick) (sicknessOf oid after)
    ]

-- Casts when legal, otherwise plays a land, otherwise passes.
castAnswer :: Prompt.Prompt r -> r
castAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ ids n -> case Set.toList ids of
    b : _ -> Map.singleton b n
    [] -> Map.empty
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          A.Cast _ -> True
          _ -> False
        isPlay a = case a of
          A.Play _ -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> case filter isPlay actions of
            h : _ -> h
            [] -> A.Pass

castGameState :: GameState.GameState
castGameState =
  snd (Engine.runGamePure castAnswer (Setup.emptyGame bothPlayers) (Engine.playFrom bothPlayers))

castEngineTests :: Tasty.TestTree
castEngineTests =
  Tasty.testGroup
    "CastEngine"
    [ HU.testCase "a castable Piker is offered as a legal action" $
        let (gs, oid) = pikerInHand 2 Phase.PrecombatMain
         in HU.assertBool "offered" (elem (A.Cast oid) (Action.legalActions alice gs)),
      HU.testCase "an unaffordable Piker is not offered" $
        let (gs, oid) = pikerInHand 1 Phase.PrecombatMain
         in HU.assertBool "not offered" (notElem (A.Cast oid) (Action.legalActions alice gs)),
      HU.testCase "casting actually happens in a full game" $
        HU.assertBool "creatures resolved" (creaturesInPlay alice castGameState > 0),
      HU.testCase "a casting game still terminates" $
        HU.assertBool "has result" (Maybe.isJust (GameState.result castGameState)),
      HU.testCase "a casting game conserves objects" $
        HU.assertEqual "objects" 120 (Game.objectCount castGameState),
      HU.testCase "CR 500.4 no mana floats at the end of a game" $
        HU.assertEqual "pools empty" Map.empty (GameState.manaPool castGameState)
    ]

creaturesInPlay :: PlayerId.PlayerId -> GameState.GameState -> Int
creaturesInPlay pid gs =
  let isCreatureObject oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.isCreature (Printing.card printing)
   in length (filter isCreatureObject (Game.zoneMembers Zone.Battlefield pid gs))

-- A Piker cast and left on the stack, ready to resolve.
pikerOnStack :: GameState.GameState
pikerOnStack =
  let (gs, oid) = pikerInHand 3 Phase.PrecombatMain
   in snd (Engine.runGamePure identityAnswer gs (Cast.castSpell alice oid))

stackTests :: Tasty.TestTree
stackTests =
  Tasty.testGroup
    "Stack"
    [ HU.testCase "CR 608.3 a resolving creature spell becomes a permanent" $
        let after = Stack.resolveTop pikerOnStack
         in do
              HU.assertEqual "stack empty" 0 (length (GameState.stack after))
              -- Four, not one: pikerInHand 3 leaves three Mountains in play.
              HU.assertEqual "four permanents" 4 (length (Game.zoneMembers Zone.Battlefield alice after))
              HU.assertEqual "one of them a creature" 1 (creaturesInPlay alice after),
      HU.testCase "CR 400.7 the permanent is a new object" $
        let after = Stack.resolveTop pikerOnStack
         in case GameState.stack pikerOnStack of
              [] -> HU.assertFailure "fixture should have a spell on the stack"
              top : _ -> HU.assertEqual "old id gone" Nothing (Game.lookupObject top after),
      HU.testCase "the permanent is a Piker on the battlefield" $
        -- The object the spell resolved INTO, not just any permanent: the
        -- fixture already has three Mountains in play, and zoneMembers is
        -- ordered by id, so the front of that list is Mountain id 0.
        let before = Game.zoneMembers Zone.Battlefield alice pikerOnStack
            after = Stack.resolveTop pikerOnStack
            isNew o = notElem o before
            fresh = filter isNew (Game.zoneMembers Zone.Battlefield alice after)
         in case fresh of
              [] -> HU.assertFailure "expected a new permanent"
              oid : _ -> case Game.lookupObject oid after of
                Nothing -> HU.assertFailure "battlefield id should resolve"
                Just obj -> do
                  HU.assertEqual "zone" Zone.Battlefield (Object.zone obj)
                  case Object.source obj of
                    Source.OfCard printing ->
                      HU.assertBool "creature" (Card.isCreature (Printing.card printing)),
      HU.testCase "resolving conserves objects" $
        HU.assertEqual
          "conserved"
          (Game.objectCount pikerOnStack)
          (Game.objectCount (Stack.resolveTop pikerOnStack)),
      HU.testCase "resolving an empty stack is a no-op" $
        let gs = Setup.emptyGame bothPlayers
         in HU.assertEqual "unchanged" gs (Stack.resolveTop gs)
    ]

-- alice has n untapped Mountains in play and one Piker in hand, in a chosen phase.
pikerInHand :: Int -> Phase.Phase -> (GameState.GameState, ObjectId.ObjectId)
pikerInHand n ph =
  let base = mountainsInPlay n
      (oid, gs1) = Game.freshObjectId base
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard Card.pikerPrinting,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled
          }
      gs2 =
        gs1
          { GameState.objects = Map.insert oid obj (GameState.objects gs1),
            GameState.hand = Map.insert alice (Seq.singleton oid) (GameState.hand gs1),
            GameState.phase = ph,
            GameState.activePlayer = alice,
            GameState.priority = Just alice
          }
   in (gs2, oid)

castTests :: Tasty.TestTree
castTests =
  Tasty.testGroup
    "Cast"
    [ HU.testCase "a Piker is castable with two Mountains in a main phase" $
        let (gs, oid) = pikerInHand 2 Phase.PrecombatMain
         in HU.assertBool "castable" (Cast.castable alice oid gs),
      HU.testCase "a Piker is not castable with one Mountain" $
        let (gs, oid) = pikerInHand 1 Phase.PrecombatMain
         in HU.assertBool "unaffordable" (not (Cast.castable alice oid gs)),
      HU.testCase "CR 601.3a no creature spell in the upkeep" $
        let (gs, oid) = pikerInHand 2 (Phase.Beginning BeginningStep.Upkeep)
         in HU.assertBool "wrong timing" (not (Cast.castable alice oid gs)),
      HU.testCase "CR 601.3a no creature spell with a non-empty stack" $
        let (gs, oid) = pikerInHand 2 Phase.PrecombatMain
            busy = gs {GameState.stack = [ObjectId.MkObjectId 999]}
         in HU.assertBool "stack not empty" (not (Cast.castable alice oid busy)),
      HU.testCase "CR 601.3a a non-active player cannot cast at sorcery speed" $
        let (gs, oid) = pikerInHand 2 Phase.PrecombatMain
            bobsTurn = gs {GameState.activePlayer = bob}
         in HU.assertBool "not active" (not (Cast.castable alice oid bobsTurn)),
      HU.testCase "a Mountain in hand is not castable: lands have no mana cost" $
        HU.assertBool "no cost" $
          not (Cast.castable alice (ObjectId.MkObjectId 0) (oneMountainState Phase.PrecombatMain)),
      HU.testCase "CR 601 casting puts a NEW object on the stack and taps two lands" $
        let (gs, oid) = pikerInHand 3 Phase.PrecombatMain
            after = snd (Engine.runGamePure identityAnswer gs (Cast.castSpell alice oid))
         in do
              HU.assertEqual "stack depth" 1 (length (GameState.stack after))
              HU.assertEqual "hand empty" 0 (handSize alice after)
              HU.assertEqual "lands tapped" 2 (tappedCount alice after)
              HU.assertEqual "conserved" (Game.objectCount gs) (Game.objectCount after)
              -- CR 400.7: the card on the stack is a new object, not the old id.
              HU.assertEqual "old id gone" Nothing (Game.lookupObject oid after),
      HU.testCase "the stack object is still a Piker on the stack" $
        let (gs, oid) = pikerInHand 3 Phase.PrecombatMain
            after = snd (Engine.runGamePure identityAnswer gs (Cast.castSpell alice oid))
         in case GameState.stack after of
              [] -> HU.assertFailure "expected one object on the stack"
              top : _ -> case Game.lookupObject top after of
                Nothing -> HU.assertFailure "stack id should resolve"
                Just obj -> do
                  HU.assertEqual "zone" Zone.Stack (Object.zone obj)
                  case Object.source obj of
                    Source.OfCard printing ->
                      HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.Type.name (Printing.card printing))
    ]

-- Discards from the BACK of hand. Deliberately unlike every fallback, so the
-- CR 514.2 test proves the prompted choice is actually honored.
discardLastAnswer :: Prompt.Prompt r -> r
discardLastAnswer p = case p of
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ ids n -> case Set.toList ids of
    b : _ -> Map.singleton b n
    [] -> Map.empty
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction {} -> A.Pass
  Prompt.ChooseDiscard _ _ ids n -> lastN (fromIntegral n) ids

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- Bob draws to eight, then discards at cleanup under discardLastAnswer.
bobDiscardChoice :: (GameState.GameState, [ObjectId.ObjectId])
bobDiscardChoice =
  let start = Setup.emptyGame bothPlayers
      steps = do
        Setup.newGame bothPlayers
        State.modify' $ \gs -> gs {GameState.activePlayer = bob, GameState.turnNumber = 2}
        drawStep
        beforeCleanup <- State.gets (Game.zoneMembers Zone.Hand bob)
        Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)
        pure beforeCleanup
      (held, final) = Engine.runGamePure discardLastAnswer start steps
   in (final, held)

discardTests :: Tasty.TestTree
discardTests =
  Tasty.testGroup
    "Discard"
    [ HU.testCase "CR 514.2 discard trims to hand size" $
        HU.assertEqual "hand" 7 (handSize bob (fst bobDiscardChoice)),
      HU.testCase "CR 514.2 the prompted choice is honored" $
        let (final, held) = bobDiscardChoice
            kept = Game.zoneMembers Zone.Hand bob final
            -- discardLastAnswer pitched the last card, so the first seven of the
            -- pre-cleanup hand are exactly what survives. Ids are stable here:
            -- the kept cards never changed zones.
            expected = take 7 held
         in HU.assertEqual "kept the front seven" expected kept
    ]

countByName :: Text.Text -> PlayerId.PlayerId -> GameState.GameState -> Int
countByName wanted pid gs =
  let named oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
      inLibrary = filter named (Game.zoneMembers Zone.Library pid gs)
      inHand = filter named (Game.zoneMembers Zone.Hand pid gs)
   in length inLibrary + length inHand

deckTests :: Tasty.TestTree
deckTests =
  Tasty.testGroup
    "Deck"
    [ HU.testCase "the deck is 60 cards" $
        HU.assertEqual "size" 60 (length Setup.deckList),
      HU.testCase "deckSize agrees with deckList" $
        HU.assertEqual "agrees" (length Setup.deckList) Setup.deckSize,
      HU.testCase "36 Mountains per player" $
        HU.assertEqual "mountains" 36 (countByName (Text.pack "Mountain") alice setupState),
      HU.testCase "8 Bird Maidens per player" $
        HU.assertEqual "maidens" 8 (countByName (Text.pack "Bird Maiden") alice setupState),
      HU.testCase "16 Pikers per player" $
        -- Bird Maiden REPLACES Pikers rather than joining them: the list stays at
        -- 60, so conservation stays at 120 and M1b's property is untouched.
        HU.assertEqual "pikers" 16 (countByName (Text.pack "Goblin Piker") bob setupState)
    ]

-- alice controls n untapped Mountains on the battlefield, nothing else.
mountainsInPlay :: Int -> GameState.GameState
mountainsInPlay n =
  let add gs _ =
        let (oid, gs1) = Game.freshObjectId gs
            obj =
              Object.MkObject
                { Object.owner = alice,
                  Object.source = Source.OfCard Card.mountainPrinting,
                  Object.zone = Zone.Battlefield,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled
                }
         in gs1
              { GameState.objects = Map.insert oid obj (GameState.objects gs1),
                GameState.battlefield = Set.insert oid (GameState.battlefield gs1)
              }
   in List.foldl' add (Setup.emptyGame bothPlayers) [1 .. n]

pikerCost :: ManaCost.ManaCost
pikerCost = ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]

poolSize :: PlayerId.PlayerId -> GameState.GameState -> Int
poolSize pid gs = case Mana.poolOf pid gs of
  Mana.Type.MkMana units -> length units

tappedCount :: PlayerId.PlayerId -> GameState.GameState -> Int
tappedCount pid gs =
  let isTapped oid = case Game.lookupObject oid gs of
        Just obj -> Object.tapped obj == TapState.Tapped
        Nothing -> False
   in length (filter isTapped (Game.zoneMembers Zone.Battlefield pid gs))

manaTests :: Tasty.TestTree
manaTests =
  Tasty.testGroup
    "Mana"
    [ HU.testCase "CR 305.6 a Mountain's red mana ability comes from its subtype" $
        HU.assertEqual
          "red"
          (Just (ManaType.Colored Color.Red))
          (Mana.subtypeMana Subtype.Mountain),
      HU.testCase "a Goblin grants no mana ability" $
        HU.assertEqual "none" Nothing (Mana.subtypeMana Subtype.Goblin),
      HU.testCase "an empty pool starts empty" $
        HU.assertEqual "empty" 0 (poolSize alice (mountainsInPlay 2)),
      HU.testCase "tapping a Mountain taps it and adds one red unit" $
        let gs = mountainsInPlay 1
         in case Game.zoneMembers Zone.Battlefield alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ -> do
                let after = Mana.tapForMana oid gs
                HU.assertEqual "tapped" 1 (tappedCount alice after)
                HU.assertEqual
                  "pool"
                  (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red}])
                  (Mana.poolOf alice after),
      HU.testCase "two Mountains can pay {1}{R}" $
        HU.assertBool "affordable" (Mana.canPay alice pikerCost (mountainsInPlay 2)),
      HU.testCase "one Mountain cannot pay {1}{R}" $
        HU.assertBool "unaffordable" (not (Mana.canPay alice pikerCost (mountainsInPlay 1))),
      HU.testCase "no Mountains cannot pay {1}{R}" $
        HU.assertBool "unaffordable" (not (Mana.canPay alice pikerCost (mountainsInPlay 0))),
      HU.testCase "paying {1}{R} taps exactly two of three Mountains and leaves no float" $
        case Mana.payCost alice pikerCost (mountainsInPlay 3) of
          Nothing -> HU.assertFailure "three Mountains should pay {1}{R}"
          Just after -> do
            HU.assertEqual "tapped" 2 (tappedCount alice after)
            HU.assertEqual "no float" 0 (poolSize alice after),
      HU.testCase "CR 500.4 mana pools empty" $
        let gs = mountainsInPlay 1
         in case Game.zoneMembers Zone.Battlefield alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ ->
                HU.assertEqual "emptied" 0 (poolSize alice (Mana.emptyManaPools (Mana.tapForMana oid gs)))
    ]

quantityTests :: Tasty.TestTree
quantityTests =
  Tasty.testGroup
    "Quantity"
    [ HU.testCase "a literal evaluates to itself" $
        HU.assertEqual
          "literal"
          (Just 2)
          (Quantity.evaluate (Setup.emptyGame bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal 2)),
      HU.testCase "a literal may be negative" $
        HU.assertEqual
          "negative"
          (Just (-1))
          (Quantity.evaluate (Setup.emptyGame bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal (-1)))
    ]

alice, bob :: PlayerId.PlayerId
alice = PlayerId.MkPlayerId 0
bob = PlayerId.MkPlayerId 1

bothPlayers :: NonEmpty.NonEmpty PlayerId.PlayerId
bothPlayers = alice NonEmpty.:| [bob]

-- A GameState with a single Mountain in alice's hand, in a chosen phase.
oneMountainState :: Phase.Phase -> GameState.GameState
oneMountainState ph =
  let oid = ObjectId.MkObjectId 0
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard Card.mountainPrinting,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick
          }
   in GameState.MkGameState
        { GameState.objects = Map.singleton oid obj,
          GameState.library = Map.empty,
          GameState.hand = Map.singleton alice (Seq.singleton oid),
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.exile = mempty,
          GameState.stack = [],
          GameState.players = Map.empty,
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.turnOrder = [alice],
          GameState.activePlayer = alice,
          GameState.phase = ph,
          GameState.remaining = Turn.laterPhases,
          GameState.priority = Just alice,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.nextObjectId = ObjectId.MkObjectId 1,
          GameState.drewFromEmpty = mempty,
          GameState.landPlayed = mempty
        }

gameTests :: Tasty.TestTree
gameTests =
  let after = Game.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield (oneMountainState Phase.PrecombatMain)
   in Tasty.testGroup
        "Game"
        [ HU.testCase "changeZone preserves object count" $
            HU.assertEqual "count" 1 (Game.objectCount after),
          HU.testCase "changeZone drops the old id" $
            HU.assertEqual "old gone" Nothing (Game.lookupObject (ObjectId.MkObjectId 0) after),
          HU.testCase "the moved object is on the battlefield, owner preserved" $
            HU.assertEqual
              "moved"
              ( Just
                  Object.MkObject
                    { Object.owner = alice,
                      Object.source = Source.OfCard Card.mountainPrinting,
                      Object.zone = Zone.Battlefield,
                      Object.tapped = TapState.Untapped,
                      Object.damage = 0,
                      Object.sickness = Sickness.Sick
                    }
              )
              (Game.lookupObject (ObjectId.MkObjectId 1) after)
        ]

actionTests :: Tasty.TestTree
actionTests =
  Tasty.testGroup
    "Action"
    [ HU.testCase "a land in hand is playable in a main phase" $
        HU.assertBool "play" (A.Play (ObjectId.MkObjectId 0) `elem` Action.legalActions alice (oneMountainState Phase.PrecombatMain)),
      HU.testCase "passing is always legal" $
        HU.assertBool "pass" (A.Pass `elem` Action.legalActions alice (oneMountainState Phase.PrecombatMain)),
      HU.testCase "no land play outside a main phase" $
        HU.assertEqual "only pass" [A.Pass] (Action.legalActions alice (oneMountainState (Phase.Beginning BeginningStep.Upkeep))),
      HU.testCase "no second land after one is played" $
        let gs = (oneMountainState Phase.PrecombatMain) {GameState.landPlayed = Set.singleton alice}
         in HU.assertEqual "only pass" [A.Pass] (Action.legalActions alice gs)
    ]

-- Identity interpreter: shuffle returns ids unchanged; actions never occur here.
identityAnswer :: Prompt.Prompt r -> r
identityAnswer p = case p of
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ ids n -> case Set.toList ids of
    b : _ -> Map.singleton b n
    [] -> Map.empty
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction {} -> A.Pass
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids

setupState :: GameState.GameState
setupState =
  Program.foldProgram
    identityAnswer
    (State.execStateT (Setup.newGame bothPlayers) (Setup.emptyGame bothPlayers))

setupTests :: Tasty.TestTree
setupTests =
  Tasty.testGroup
    "Setup"
    [ HU.testCase "120 objects after setup" $
        HU.assertEqual "count" 120 (Game.objectCount setupState),
      HU.testCase "each library has 53 after opening draws" $
        HU.assertEqual "library" 53 (length (Game.zoneMembers Zone.Library alice setupState)),
      HU.testCase "each hand has 7" $
        HU.assertEqual "hand" 7 (length (Game.zoneMembers Zone.Hand bob setupState)),
      HU.testCase "active player is first in turn order" $
        HU.assertEqual "active" alice (GameState.activePlayer setupState)
    ]

sbaBase :: GameState.GameState
sbaBase = Setup.emptyGame bothPlayers

sbaTests :: Tasty.TestTree
sbaTests =
  Tasty.testGroup
    "Sba"
    [ HU.testCase "drew-from-empty loses" $
        let after = Sba.checkStateBasedActions sbaBase {GameState.drewFromEmpty = Set.singleton alice}
         in HU.assertEqual "alice lost" (Just (Status.Departed Departure.Lost)) (fmap Player.status (Map.lookup alice (GameState.players after))),
      HU.testCase "one remaining player wins" $
        let after = Sba.checkStateBasedActions sbaBase {GameState.drewFromEmpty = Set.singleton alice}
         in HU.assertEqual "bob won" (Just (Result.Won bob)) (GameState.result after),
      HU.testCase "life <= 0 loses" $
        let gs = sbaBase {GameState.players = Map.insert alice (Player.MkPlayer {Player.life = 0, Player.status = Status.Playing}) (GameState.players sbaBase)}
         in HU.assertEqual "bob won" (Just (Result.Won bob)) (GameState.result (Sba.checkStateBasedActions gs)),
      HU.testCase "simultaneous last departures draw" $
        let after = Sba.checkStateBasedActions sbaBase {GameState.drewFromEmpty = Set.fromList [alice, bob]}
         in HU.assertEqual "draw" (Just Result.Drawn) (GameState.result after)
    ]

goldfishResult :: (Result.Result, GameState.GameState)
goldfishResult =
  Engine.runGamePure identityAnswer (Setup.emptyGame bothPlayers) (Engine.playFrom bothPlayers)

-- Always plays a land when one is legal, otherwise passes.
playLandAnswer :: Prompt.Prompt r -> r
playLandAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ ids n -> case Set.toList ids of
    b : _ -> Map.singleton b n
    [] -> Map.empty
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.ChooseAction _ _ actions ->
    let isPlay a = case a of
          A.Play _ -> True
          A.Pass -> False
          A.Cast _ -> False
     in case filter isPlay actions of
          h : _ -> h
          [] -> A.Pass

landState :: GameState.GameState
landState =
  snd (Engine.runGamePure playLandAnswer (Setup.emptyGame bothPlayers) (Engine.playFrom bothPlayers))

-- Alice is active on turns 1, 3, 5, …; bob on 2, 4, 6, …. With one land play per
-- turn (CR 305.2) a player can never have more lands out than turns taken.
turnsTaken :: PlayerId.PlayerId -> GameState.GameState -> Int
turnsTaken pid gs =
  let total = fromIntegral (GameState.turnNumber gs)
   in if pid == alice then (total + 1) `div` 2 else total `div` 2

engineTests :: Tasty.TestTree
engineTests =
  Tasty.testGroup
    "Engine"
    [ HU.testCase "goldfish game ends with the starting player winning" $
        HU.assertEqual "winner" (Result.Won alice) (fst goldfishResult),
      HU.testCase "card conservation holds at end" $
        HU.assertEqual "objects" 120 (Game.objectCount (snd goldfishResult)),
      HU.testCase "playing lands fills the battlefield" $
        HU.assertBool "non-empty" $
          not (null (Game.zoneMembers Zone.Battlefield alice landState)),
      HU.testCase "land play conserves cards" $
        HU.assertEqual "objects" 120 (Game.objectCount landState),
      HU.testCase "CR 305.2 at most one land per turn" $
        HU.assertBool "no double land plays" $
          length (Game.zoneMembers Zone.Battlefield alice landState) <= turnsTaken alice landState
            && length (Game.zoneMembers Zone.Battlefield bob landState) <= turnsTaken bob landState
    ]

replayTests :: Tasty.TestTree
replayTests =
  let start = Setup.emptyGame bothPlayers
      game = Engine.playFrom bothPlayers
      -- Recorded with playLandAnswer, whose choices differ from Replay's
      -- exhausted-transcript fallback. That keeps these assertions honest: the
      -- transcript has to actually carry the decisions.
      ((_, recorded), transcript) = Replay.record playLandAnswer start game
   in Tasty.testGroup
        "Replay"
        [ HU.testCase "replaying a recorded game reproduces the final state" $
            HU.assertEqual "final states equal" recorded (snd (Replay.replay transcript start game)),
          HU.testCase "the transcript is what carries the decisions" $
            HU.assertBool "empty log diverges" $
              recorded /= snd (Replay.replay [] start game),
          HU.testCase "a recorded goldfish also replays" $
            let ((_, gf), gfLog) = Replay.record identityAnswer start game
             in HU.assertEqual "goldfish" gf (snd (Replay.replay gfLog start game))
        ]

-- A StdGen-driven interpreter: random shuffle and random legal action.
randomAnswer :: Prompt.Prompt r -> State.State Random.StdGen r
randomAnswer p = case p of
  Prompt.DeclareAttackers _ _ ids -> do
    g <- State.get
    let (keep, g') = Random.uniformR (0, length ids) g
    State.put g'
    pure (take keep ids)
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> pure Map.empty
    a : _ -> do
      g <- State.get
      let (keep, g') = Random.uniformR (0, length mine) g
      State.put g'
      pure (Map.fromList (map (\b -> (b, a)) (take keep mine)))
  -- The damage division stays canonical rather than random: a random division
  -- would usually be illegal (it must total the attacker's power), and this
  -- property suite is not the place to test the rejection path.
  Prompt.AssignCombatDamage _ _ _ ids n -> pure $ case Set.toList ids of
    b : _ -> Map.singleton b n
    [] -> Map.empty
  Prompt.Shuffle ids -> do
    g <- State.get
    let (g1, g2) = Random.splitGen g
    State.put g2
    pure (shuffleWith g1 ids)
  Prompt.ChooseDiscard _ _ ids n -> pure (take (fromIntegral n) ids)
  Prompt.ChooseAction _ _ actions -> do
    g <- State.get
    let n = length actions
        (i, g') = Random.uniformR (0, max 0 (n - 1)) g
    State.put g'
    pure (pick actions (min (n - 1) (max 0 i)))

-- Total index into a list; the engine always offers at least Pass, so the
-- fallback is unreachable in practice but keeps this free of partial functions.
pick :: [A.Action] -> Int -> A.Action
pick actions i = case drop i actions of
  h : _ -> h
  [] -> A.Pass

shuffleWith :: Random.StdGen -> [a] -> [a]
shuffleWith g xs =
  let unfoldInts :: Random.StdGen -> [Int]
      unfoldInts gen = let (v, gen') = Random.uniform gen in v : unfoldInts gen'
      insertByKey y ys = case ys of
        [] -> [y]
        z : zs -> if fst y <= fst z then y : z : zs else z : insertByKey y zs
      keys = take (length xs) (unfoldInts g)
   in map snd (foldr insertByKey [] (zip keys xs))

runRandomGame :: Int -> GameState.GameState
runRandomGame s =
  let start = Setup.emptyGame bothPlayers
      game = Engine.playFrom bothPlayers
      (_, final) = State.evalState (Program.foldProgramM randomAnswer (State.runStateT game start)) (Random.mkStdGen s)
   in final

nextIdOf :: GameState.GameState -> Integer
nextIdOf gs = case GameState.nextObjectId gs of
  ObjectId.MkObjectId n -> toInteger n

propertyTests :: Tasty.TestTree
propertyTests =
  Tasty.testGroup
    "Properties"
    [ QC.testProperty "conservation: 120 objects at end" $ \s ->
        Game.objectCount (runRandomGame s) QC.=== 120,
      -- The property that matters most now. Combat is the first thing that can
      -- end a game before the library runs out.
      QC.testProperty "every game terminates with a result" $ \s ->
        QC.property (Maybe.isJust (GameState.result (runRandomGame s))),
      QC.testProperty "at least 120 ids were minted" $ \s ->
        QC.property (nextIdOf (runRandomGame s) >= 120),
      QC.testProperty "no mana floats at the end" $ \s ->
        GameState.manaPool (runRandomGame s) QC.=== Map.empty,
      -- Replaces M0's "no life changes". Nothing in M1b GAINS life, so any
      -- increase is a bug. Dies at lifelink (M2), which is how M2 announces
      -- itself -- exactly as this property's ancestor announced M1b.
      QC.testProperty "life never increases" $ \s ->
        QC.property (all (\pl -> Player.life pl <= Setup.startingLife) (Map.elems (GameState.players (runRandomGame s)))),
      -- The M1b exit criterion, asserted rather than assumed: across 100 seeds,
      -- at least one game must see damage actually change someone's life total.
      -- Without this, every combat path could silently no-op and the suite would
      -- still be green.
      QC.testProperty "combat happens: some seed changes a life total" $
        QC.once $
          QC.property $
            any someLifeChanged [1 .. 100 :: Int]
    ]

-- Did anyone's life total move over the course of the game this seed produces?
someLifeChanged :: Int -> Bool
someLifeChanged s =
  let moved pl = Player.life pl /= Setup.startingLife
   in any moved (Map.elems (GameState.players (runRandomGame s)))

-- Run setup, then a scripted tweak, then whatever steps the scenario needs.
scenario :: Game.Type.Game () -> GameState.GameState
scenario steps =
  snd $ Engine.runGamePure identityAnswer (Setup.emptyGame bothPlayers) $ do
    Setup.newGame bothPlayers
    steps

drawStep :: Game.Type.Game ()
drawStep = Engine.runTurnBasedActions (Phase.Beginning BeginningStep.DrawStep)

-- Alice starts, so her turn-1 draw is skipped.
aliceFirstDraw :: GameState.GameState
aliceFirstDraw = scenario drawStep

-- Bob is not the starting player, so his draw happens normally.
bobFirstDraw :: GameState.GameState
bobFirstDraw = scenario $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = bob, GameState.turnNumber = 2}
  drawStep

bobAfterCleanup :: GameState.GameState
bobAfterCleanup = scenario $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = bob, GameState.turnNumber = 2}
  drawStep
  Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)

deckedOut :: GameState.GameState
deckedOut = scenario $ do
  State.modify' $ \gs ->
    gs
      { GameState.library = Map.insert alice Seq.empty (GameState.library gs),
        GameState.turnNumber = 3
      }
  drawStep
  Engine.checkSba

handSize :: PlayerId.PlayerId -> GameState.GameState -> Int
handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)

librarySize :: PlayerId.PlayerId -> GameState.GameState -> Int
librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)

-- Records every player asked for an action, in order, and casts when it can.
-- Recording is the point: whether the caster RETAINS priority is only visible in
-- who gets asked next.
recordingAnswer :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
recordingAnswer p = case p of
  Prompt.DeclareAttackers {} -> pure []
  Prompt.DeclareBlockers {} -> pure Map.empty
  Prompt.AssignCombatDamage _ _ _ ids n -> pure $ case Set.toList ids of
    b : _ -> Map.singleton b n
    [] -> Map.empty
  Prompt.Shuffle ids -> pure ids
  Prompt.ChooseDiscard _ _ ids n -> pure (take (fromIntegral n) ids)
  Prompt.ChooseAction _ pid actions -> do
    State.modify' (\asked -> asked ++ [pid])
    let isCast a = case a of
          A.Cast _ -> True
          _ -> False
    pure $ case filter isCast actions of
      h : _ -> h
      [] -> A.Pass

-- pikerInHand already builds on Setup.emptyGame bothPlayers, so turnOrder is
-- [alice, bob] and both players are in the players map.
askedPlayers :: [PlayerId.PlayerId]
askedPlayers =
  let (gs, _) = pikerInHand 3 Phase.PrecombatMain
   in State.execState
        (Program.foldProgramM recordingAnswer (State.runStateT Engine.priorityLoop gs))
        []

ruleTests :: Tasty.TestTree
ruleTests =
  Tasty.testGroup
    "Rules"
    [ HU.testCase "CR 117.4 a full round of passes resolves the stack, not the step" $
        -- With a spell on the stack, everyone passing must RESOLVE it and keep
        -- the step alive. Under M0's rule the step would simply end with the
        -- spell still sitting on the stack.
        let (gs, oid) = pikerInHand 3 Phase.PrecombatMain
            steps = do
              Cast.castSpell alice oid
              Engine.priorityLoop
            after = snd (Engine.runGamePure identityAnswer gs steps)
         in do
              HU.assertEqual "stack emptied" 0 (length (GameState.stack after))
              HU.assertEqual "piker resolved onto the battlefield" 1 (creaturesInPlay alice after),
      HU.testCase "CR 117.3c the caster is asked again, rather than passing priority on" $
        -- alice is asked, casts, and must be asked AGAIN before bob gets a turn.
        -- If priority wrongly advanced to the next player, this would be
        -- [alice, bob, ...] instead.
        HU.assertEqual "alice twice, then bob" [alice, alice, bob] (take 3 askedPlayers),
      HU.testCase "CR 103.7a starting player skips first draw" $ do
        HU.assertEqual "hand" 7 (handSize alice aliceFirstDraw)
        HU.assertEqual "library" 53 (librarySize alice aliceFirstDraw),
      HU.testCase "CR 103.7a only the starting player skips" $ do
        HU.assertEqual "hand" 8 (handSize bob bobFirstDraw)
        HU.assertEqual "library" 52 (librarySize bob bobFirstDraw),
      HU.testCase "CR 514.2 discard to hand size" $
        HU.assertEqual "hand" 7 (handSize bob bobAfterCleanup),
      HU.testCase "CR 704.5b deck-out loses" $
        HU.assertEqual
          "alice departed"
          (Just (Status.Departed Departure.Lost))
          (fmap Player.status (Map.lookup alice (GameState.players deckedOut))),
      HU.testCase "CR 704.5b the survivor wins" $
        HU.assertEqual "bob won" (Just (Result.Won bob)) (GameState.result deckedOut)
    ]

-- A toy instruction set for exercising Program.
data Toy r where
  Ask :: Toy Int

toyProgram :: Program.Program Toy Int
toyProgram = do
  x <- Program.prompt Ask
  y <- Program.prompt Ask
  pure (x + y)

programTests :: Tasty.TestTree
programTests =
  Tasty.testGroup
    "Program"
    [ HU.testCase "pure interpreter threads answers" $
        let answer :: Toy b -> b
            answer i = case i of Ask -> 21
         in HU.assertEqual "21 + 21" 42 (Program.foldProgram answer toyProgram),
      HU.testCase "effectful interpreter runs in order" $
        let answer :: Toy b -> State.State [Int] b
            answer i = case i of
              Ask -> do
                xs <- State.get
                case xs of
                  h : t -> do State.put t; pure h
                  [] -> pure 0
         in HU.assertEqual "1 + 2" 3 (State.evalState (Program.foldProgramM answer toyProgram) [1, 2])
    ]

pikerCard :: Card.Type.Card
pikerCard = Printing.card Card.pikerPrinting

cardTests :: Tasty.TestTree
cardTests =
  Tasty.testGroup
    "Card"
    [ HU.testCase "Mountain printing is named Mountain" $
        HU.assertEqual "name" (Text.pack "Mountain") (Card.Type.name (Printing.card Card.mountainPrinting)),
      HU.testCase "Mountain is a Land" $
        HU.assertBool "isLand" (Card.isLand (Printing.card Card.mountainPrinting)),
      HU.testCase "Mountain has the Mountain subtype" $
        HU.assertBool "subtype" $
          Set.member Subtype.Mountain (TypeLine.subtypes (Card.Type.typeLine (Printing.card Card.mountainPrinting))),
      HU.testCase "Mountain type line contains Land" $
        HU.assertBool "cardtype" $
          Set.member CardType.Land (TypeLine.types (Card.Type.typeLine (Printing.card Card.mountainPrinting))),
      -- CR 202.1: a land has no mana cost. Not a zero cost -- no cost at all.
      HU.testCase "Mountain has no mana cost" $
        HU.assertEqual "no cost" Nothing (Card.Type.manaCost (Printing.card Card.mountainPrinting)),
      HU.testCase "Mountain has no power or toughness" $ do
        HU.assertEqual "power" Nothing (Card.Type.power (Printing.card Card.mountainPrinting))
        HU.assertEqual "toughness" Nothing (Card.Type.toughness (Printing.card Card.mountainPrinting)),
      HU.testCase "Piker printing is named Goblin Piker" $
        HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.Type.name pikerCard),
      HU.testCase "Piker costs {1}{R}" $
        HU.assertEqual
          "cost"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]))
          (Card.Type.manaCost pikerCard),
      HU.testCase "Piker is a 2/1" $ do
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power pikerCard)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness pikerCard),
      HU.testCase "Piker is a Goblin Warrior" $
        HU.assertEqual
          "subtypes"
          (Set.fromList [Subtype.Goblin, Subtype.Warrior])
          (TypeLine.subtypes (Card.Type.typeLine pikerCard)),
      HU.testCase "Piker is a creature and not a land" $ do
        HU.assertBool "creature" (Card.isCreature pikerCard)
        HU.assertBool "not land" (not (Card.isLand pikerCard)),
      -- CR 110.1: the classification resolution turns on. Never card identity.
      HU.testCase "CR 110.1 both a Piker and a Mountain are permanents" $ do
        HU.assertBool "piker" (Card.isPermanent pikerCard)
        HU.assertBool "mountain" (Card.isPermanent (Printing.card Card.mountainPrinting))
    ]

turnTests :: Tasty.TestTree
turnTests =
  Tasty.testGroup
    "Turn"
    [ HU.testCase "firstPhase is the untap step" $
        HU.assertEqual "firstPhase" (Phase.Beginning BeginningStep.Untap) Turn.firstPhase,
      HU.testCase "a turn has twelve steps" $
        HU.assertEqual "twelve" 12 (length Turn.allPhases),
      HU.testCase "firstPhase and laterPhases reconstruct the turn template" $
        HU.assertEqual "reconstruct" (Seq.fromList (drop 1 Turn.allPhases)) Turn.laterPhases,
      HU.testCase "untap and cleanup grant no priority" $
        HU.assertBool "no priority" $
          not (Turn.grantsPriority (Phase.Beginning BeginningStep.Untap))
            && not (Turn.grantsPriority (Phase.Ending EndingStep.Cleanup)),
      QC.testProperty "a turn never revisits a phase" $
        QC.property (length Turn.allPhases == length (dedupe Turn.allPhases))
    ]

turnDataTests :: Tasty.TestTree
turnDataTests =
  Tasty.testGroup
    "TurnData"
    [ HU.testCase "advance pops the schedule head into the current phase" $
        let gs0 = Setup.emptyGame bothPlayers
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
                }
            after = snd (Engine.runGamePure aggressiveAnswer gs Engine.advance)
         in do
              HU.assertEqual "phase" (Phase.Combat CombatStep.BeginningOfCombat) (GameState.phase after)
              HU.assertEqual "remaining" (Seq.fromList [Phase.PostcombatMain]) (GameState.remaining after),
      HU.testCase "advance on an empty schedule hands off the turn" $
        let gs0 = Setup.emptyGame bothPlayers
            gs =
              gs0
                { GameState.phase = Phase.Ending EndingStep.Cleanup,
                  GameState.remaining = Seq.empty,
                  GameState.activePlayer = alice,
                  GameState.turnNumber = 1
                }
            after = snd (Engine.runGamePure aggressiveAnswer gs Engine.advance)
         in do
              HU.assertEqual "new active player" bob (GameState.activePlayer after)
              HU.assertEqual "phase reset" Turn.firstPhase (GameState.phase after)
              HU.assertEqual "schedule refilled" Turn.laterPhases (GameState.remaining after)
              HU.assertEqual "turn incremented" 2 (GameState.turnNumber after),
      HU.testCase "a fresh game starts at untap with the rest of the turn scheduled" $
        let gs = Setup.emptyGame bothPlayers
         in do
              HU.assertEqual "phase" Turn.firstPhase (GameState.phase gs)
              HU.assertEqual "remaining" Turn.laterPhases (GameState.remaining gs)
    ]

skipTests :: Tasty.TestTree
skipTests =
  Tasty.testGroup
    "Skip"
    [ HU.testCase "CR 508.8 dropSkippedCombatSteps removes declare blockers and combat damage" $
        let full =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
            expected = Seq.fromList [Phase.Combat CombatStep.EndOfCombat, Phase.PostcombatMain]
         in HU.assertEqual "dropped" expected (Turn.dropSkippedCombatSteps full),
      HU.testCase "CR 508.8 no attacker declared skips to end of combat" $
        -- Nobody has a creature, so no attackers are declared: the declare
        -- blockers and combat damage steps must not run at all.
        let (gs, _, _) = combatBoardOf [] []
            after = snd (Engine.runGamePure aggressiveAnswer gs Engine.runStep)
         in HU.assertEqual "jumped past the two dead steps" (Phase.Combat CombatStep.EndOfCombat) (GameState.phase after),
      HU.testCase "CR 508.8 an attacker keeps the declare blockers step" $
        -- The control: with an attacker, the step after declare attackers is
        -- declare blockers, exactly as before. So the skip is not "always skip".
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] []
            after = snd (Engine.runGamePure aggressiveAnswer gs Engine.runStep)
         in HU.assertEqual "declare blockers still next" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase after),
      HU.testCase "CR 508.8 an attacker-less combat changes no life total" $
        -- End to end: run the whole combat region. No attackers means no damage,
        -- and the turn still leaves combat cleanly.
        let (gs, _, _) = combatBoardOf [] []
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "bob untouched" (Just 20) (lifeOf bob after)
              HU.assertEqual "alice untouched" (Just 20) (lifeOf alice after)
              HU.assertBool "left combat" (not (inCombatPhase (GameState.phase after)))
    ]

-- Run whole steps through the engine while the current phase is in the combat
-- phase, stopping once combat is left or the game ends. Bounded so a bug cannot
-- loop forever.
runCombat :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runCombat answer gs0 =
  let go n g =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || not (inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 24 gs0

inCombatPhase :: Phase.Phase -> Bool
inCombatPhase p = case p of
  Phase.Combat _ -> True
  _ -> False

m2bCardTests :: Tasty.TestTree
m2bCardTests =
  let card = Printing.card
      red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      gs0 = Setup.emptyGame bothPlayers
   in Tasty.testGroup
        "M2bCards"
        [ HU.testCase "Sabretooth Tiger is a {2}{R} 2/1 Cat with first strike" $ do
            HU.assertEqual "name" (Text.pack "Sabretooth Tiger") (Card.Type.name (card Card.sabretoothTigerPrinting))
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, red])) (Card.Type.manaCost (card Card.sabretoothTigerPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.sabretoothTigerPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (card Card.sabretoothTigerPrinting))
            HU.assertEqual "subtypes" (Set.singleton Subtype.Cat) (TypeLine.subtypes (Card.Type.typeLine (card Card.sabretoothTigerPrinting)))
            HU.assertEqual "keyword" (Set.singleton Keyword.FirstStrike) (Card.Type.keywords (card Card.sabretoothTigerPrinting)),
          HU.testCase "Ridgetop Raptor is a {3}{R} 2/1 Dinosaur Beast with double strike" $ do
            HU.assertEqual "name" (Text.pack "Ridgetop Raptor") (Card.Type.name (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, red])) (Card.Type.manaCost (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "subtypes" (Set.fromList [Subtype.Dinosaur, Subtype.Beast]) (TypeLine.subtypes (Card.Type.typeLine (card Card.ridgetopRaptorPrinting)))
            HU.assertEqual "keyword" (Set.singleton Keyword.DoubleStrike) (Card.Type.keywords (card Card.ridgetopRaptorPrinting)),
          HU.testCase "the tiger has first strike through the projection" $
            let (oid, gs) = addCreature Card.sabretoothTigerPrinting alice gs0
             in do
                  HU.assertBool "first strike" (Game.hasKeyword Keyword.FirstStrike oid gs)
                  HU.assertBool "not double strike" (not (Game.hasKeyword Keyword.DoubleStrike oid gs)),
          HU.testCase "the raptor has double strike through the projection" $
            let (oid, gs) = addCreature Card.ridgetopRaptorPrinting alice gs0
             in do
                  HU.assertBool "double strike" (Game.hasKeyword Keyword.DoubleStrike oid gs)
                  HU.assertBool "not first strike" (not (Game.hasKeyword Keyword.FirstStrike oid gs)),
          HU.testCase "both are 2/1s, the same body as a Piker" $
            let bodyOf p = (Card.Type.power (card p), Card.Type.toughness (card p))
             in do
                  HU.assertEqual "tiger body" (bodyOf Card.pikerPrinting) (bodyOf Card.sabretoothTigerPrinting)
                  HU.assertEqual "raptor body" (bodyOf Card.pikerPrinting) (bodyOf Card.ridgetopRaptorPrinting)
        ]

-- Run whole steps until the first-strike combat damage step has been dealt
-- (struckFirst is set) or combat ends, so a test can observe the board BETWEEN
-- the two combat damage steps.
runToFirstStrikeDone :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToFirstStrikeDone answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (Combat.Type.struckFirst (GameState.combat g))
          || not (inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 24 gs0

firstStrikeTests :: Tasty.TestTree
firstStrikeTests =
  Tasty.testGroup
    "FirstStrike"
    [ HU.testCase "CR 702.7b a first striker kills a vanilla blocker and lives" $
        -- The tiger (2/1 first strike) kills the Piker (2/1) in the first-strike
        -- step; the SBA between steps buries it before it can deal, so the tiger
        -- survives at zero damage.
        let (gs, _, _) = combatBoardOf [Card.sabretoothTigerPrinting] [Card.pikerPrinting]
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "the blocker is dead" 0 (creaturesInPlay bob after)
              HU.assertEqual "the first striker lives" 1 (creaturesInPlay alice after),
      HU.testCase "CR 510.2 the control: two vanilla 2/1s trade" $
        -- With a Piker in the tiger's place there is one combat damage step and
        -- both die. So first strike is the sole cause above.
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "alice's is dead" 0 (creaturesInPlay alice after)
              HU.assertEqual "bob's is dead" 0 (creaturesInPlay bob after),
      HU.testCase "CR 702.4b a double striker deals twice to an unblocked player" $
        -- The raptor (2/1 double strike) deals 2 in each step: bob loses 4.
        let (gs, _, _) = combatBoardOf [Card.ridgetopRaptorPrinting] []
            after = runCombat aggressiveAnswer gs
         in HU.assertEqual "bob took 4" (Just 16) (lifeOf bob after),
      HU.testCase "CR 702.7b the control: a first striker deals once to a player" $
        let (gs, _, _) = combatBoardOf [Card.sabretoothTigerPrinting] []
            after = runCombat aggressiveAnswer gs
         in HU.assertEqual "bob took 2" (Just 18) (lifeOf bob after),
      HU.testCase "CR 510.1b the control: a vanilla creature deals once to a player" $
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] []
            after = runCombat aggressiveAnswer gs
         in HU.assertEqual "bob took 2" (Just 18) (lifeOf bob after),
      HU.testCase "CR 510.4 double strike kills a 3/3 across two steps; first strike does not" $
        -- The raptor deals 2 + 2 = 4 to the Ogre (3/3), killing it. A first
        -- striker deals 2 once, and the Ogre lives.
        let raptorVs = combatBoardOf [Card.ridgetopRaptorPrinting] [Card.ogreSentryPrinting]
            tigerVs = combatBoardOf [Card.sabretoothTigerPrinting] [Card.ogreSentryPrinting]
            afterRaptor = runCombat aggressiveAnswer (frst raptorVs)
            afterTiger = runCombat aggressiveAnswer (frst tigerVs)
         in do
              HU.assertEqual "double strike kills the Ogre" 0 (creaturesInPlay bob afterRaptor)
              HU.assertEqual "first strike leaves the Ogre" 1 (creaturesInPlay bob afterTiger),
      HU.testCase "CR 510.4 a striker killed in the first step does not deal in the second" $
        -- Raptor (double strike) and tiger (first strike) each block-kill the
        -- other in the first step. Neither is "remaining" for the second step, so
        -- no second-wave damage; both are simply dead.
        let (gs, _, _) = combatBoardOf [Card.ridgetopRaptorPrinting] [Card.sabretoothTigerPrinting]
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "attacker dead" 0 (creaturesInPlay alice after)
              HU.assertEqual "blocker dead" 0 (creaturesInPlay bob after),
      HU.testCase "CR 510.4 the mixed board: first strike once, vanilla once, double strike twice" $
        -- Tiger (first strike), raptor (double strike) and Piker (vanilla) all
        -- attack unblocked. First-strike step: tiger 2 + raptor 2 = 4. Second
        -- step: raptor 2 + Piker 2 = 4. bob: 20 - 8 = 12. The naive "strikers in
        -- step one, everyone else in step two" drops the raptor's second hit and
        -- lands bob at 14.
        let (gs, _, _) = combatBoardOf [Card.sabretoothTigerPrinting, Card.ridgetopRaptorPrinting, Card.pikerPrinting] []
            mid = runToFirstStrikeDone aggressiveAnswer gs
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "after the first-strike step, bob took 4" (Just 16) (lifeOf bob mid)
              HU.assertEqual "after both steps, bob took 8" (Just 12) (lifeOf bob after)
    ]

-- The state out of a combatBoardOf triple.
frst :: (a, b, c) -> a
frst (a, _, _) = a

dedupe :: (Eq a) => [a] -> [a]
dedupe xs = case xs of
  [] -> []
  h : t -> h : dedupe (filter (/= h) t)
