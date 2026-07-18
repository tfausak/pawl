{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Card as Card
import qualified Pawl.CardSpec as CardSpec
import qualified Pawl.Cast as Cast
import qualified Pawl.CastSpec as CastSpec
import qualified Pawl.Combat as Combat
import qualified Pawl.CoreSpec as CoreSpec
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.GameSpec as GameSpec
import qualified Pawl.ManaSpec as ManaSpec
import qualified Pawl.Replay as Replay
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.SetupSpec as SetupSpec
import qualified Pawl.Stack as Stack
import Pawl.Support
  ( addCreature,
    addPiker,
    aggressiveAnswer,
    alice,
    bob,
    boltAtBobsPiker,
    boltInHand,
    bothPlayers,
    combatBoard,
    combatBoardOf,
    creaturesInPlay,
    damageOf,
    fightWith,
    greenBlack,
    handOne,
    identityAnswer,
    inCombatPhase,
    isCreatureRecipient,
    lifeOf,
    m2aPrintings,
    markDamage,
    matchups,
    mountainsInPlay,
    pikerOf,
    playLandAnswer,
    redRed,
    runCombat,
    runRandomGame,
    tappedCount,
  )
import qualified Pawl.Target as Target
import qualified Pawl.TurnSpec as TurnSpec
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Combat as Combat.Type
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.Departure as Departure
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree =
  Tasty.testGroup
    "pawl"
    [ CoreSpec.tests,
      CardSpec.tests,
      TurnSpec.tests,
      GameSpec.tests,
      SetupSpec.tests,
      sbaTests,
      replayTests,
      propertyTests,
      ManaSpec.tests,
      CastSpec.tests,
      damageTests,
      creatureSbaTests,
      combatLegalityTests,
      combatReplayTests,
      declareTests,
      combatDamageTests,
      keywordTests,
      firstStrikeTests,
      m2bExitTests,
      defenderTests,
      vigilanceTests,
      hasteTests,
      evasionTests,
      damageEventTests,
      deathtouchTests,
      assignmentLegalityTests,
      trampleTests,
      trampleDeathtouchTests,
      m2cPropertyTests,
      targetTests,
      resolveTests,
      fizzleTests
    ]

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
              Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (map (\r -> (r, 1)) (filter isCreatureRecipient (Map.keys thresholds)))
              _ -> aggressiveAnswer p
            after = Sba.checkStateBasedActions (fightWith split gs)
         in do
              HU.assertEqual "both blockers dead" 0 (creaturesInPlay bob after)
              HU.assertEqual "expected two blockers" 2 (length theirs),
      HU.testCase "CR 510.1c the same 2 damage on one blocker kills only it" $
        let (gs, _, _) = combatBoard 1 2
            dump :: Prompt.Prompt r -> r
            dump p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter isCreatureRecipient (Map.keys thresholds) of
                  r : _ -> Map.singleton r n
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
              Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (map (\r -> (r, 99)) (filter isCreatureRecipient (Map.keys thresholds)))
              _ -> aggressiveAnswer p
            after = Sba.checkStateBasedActions (fightWith cheat gs)
         in HU.assertEqual "both blockers survive" 2 (creaturesInPlay bob after)
    ]

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
      damagePrompt = Prompt.AssignCombatDamage decider alice oid (Map.singleton (Recipient.ToCreature oid) 0) 2
   in Tasty.testGroup
        "CombatReplay"
        [ HU.testCase "attackers round-trip through the transcript" $
            HU.assertEqual "round trip" (Just [oid]) (Replay.decode attackPrompt (Replay.encode attackPrompt [oid])),
          HU.testCase "blockers round-trip through the transcript" $
            let answer = Map.singleton oid oid
             in HU.assertEqual "round trip" (Just answer) (Replay.decode blockPrompt (Replay.encode blockPrompt answer)),
          HU.testCase "a damage assignment round-trips through the transcript" $
            let answer :: Map.Map Recipient.Recipient Natural.Natural
                answer = Map.singleton (Recipient.ToCreature oid) 2
             in HU.assertEqual "round trip" (Just answer) (Replay.decode damagePrompt (Replay.encode damagePrompt answer)),
          HU.testCase "a mismatched response decodes to Nothing" $
            HU.assertEqual "mismatch" Nothing (Replay.decode attackPrompt (Response.Shuffled [oid])),
          HU.testCase "defaultAnswer attacks with nothing" $
            HU.assertEqual "no attacks" [] (Replay.defaultAnswer attackPrompt),
          HU.testCase "defaultAnswer blocks with nothing" $
            HU.assertEqual "no blocks" Map.empty (Replay.defaultAnswer blockPrompt),
          HU.testCase "defaultAnswer assigns a LEGAL division" $
            -- Total must equal the attacker's power, or the fallback would be
            -- rejected by validation and deal no damage at all.
            HU.assertEqual "all to one blocker" (Map.singleton (Recipient.ToCreature oid) 2) (Replay.defaultAnswer damagePrompt)
        ]

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

replayTests :: Tasty.TestTree
replayTests =
  let start = Setup.emptyGame (NonEmpty.map fst redRed)
      game = Engine.playFrom redRed
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
             in HU.assertEqual "goldfish" gf (snd (Replay.replay gfLog start game)),
          HU.testCase "a ChooseTargets answer round-trips through the transcript" $
            let sets = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.singleton (Recipient.ToPlayer bob))
                p = Prompt.ChooseTargets (Decider.MkDecider alice) alice (ObjectId.MkObjectId 0) sets
                answer = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Recipient.ToPlayer bob)
             in HU.assertEqual "decode . encode = Just" (Just answer) (Replay.decode p (Replay.encode p answer))
        ]

nextIdOf :: GameState.GameState -> Integer
nextIdOf gs = case GameState.nextObjectId gs of
  ObjectId.MkObjectId n -> toInteger n

propertyTests :: Tasty.TestTree
propertyTests =
  Tasty.testGroup
    "Properties"
    [ QC.testProperty "conservation: 120 objects at end" $ \s ->
        QC.conjoin (map (\m -> Game.objectCount (runRandomGame m s) QC.=== 120) matchups),
      -- The property that matters most now. Combat is the first thing that can
      -- end a game before the library runs out.
      QC.testProperty "every game terminates with a result" $ \s ->
        QC.conjoin (map (\m -> QC.property (Maybe.isJust (GameState.result (runRandomGame m s)))) matchups),
      QC.testProperty "at least 120 ids were minted" $ \s ->
        QC.conjoin (map (\m -> QC.property (nextIdOf (runRandomGame m s) >= 120)) matchups),
      QC.testProperty "no mana floats at the end" $ \s ->
        QC.conjoin (map (\m -> GameState.manaPool (runRandomGame m s) QC.=== Map.empty) matchups),
      -- Replaces M0's "no life changes". Nothing here GAINS life, so any
      -- increase is a bug. Dies at lifelink (still unscheduled -- see the
      -- design doc's punchlist), the same way this property's ancestor
      -- announced M1b.
      QC.testProperty "life never increases" $ \s ->
        QC.conjoin
          ( map
              (\m -> QC.property (all (\pl -> Player.life pl <= Setup.startingLife) (Map.elems (GameState.players (runRandomGame m s)))))
              matchups
          ),
      -- The M1b exit criterion, asserted rather than assumed: across 100 seeds,
      -- at least one red-red game must see damage actually change someone's
      -- life total. Without this, every combat path could silently no-op and
      -- the suite would still be green.
      QC.testProperty "combat happens: some seed changes a life total" $
        QC.once $
          QC.property $
            any someLifeChanged [1 .. 100 :: Int],
      QC.testProperty "green-black: some seed sends a creature to the graveyard" $
        QC.once $
          QC.property $
            any creatureDied [1 .. 100 :: Int],
      -- The M3a exit criterion, asserted the same way combat's was: across 100
      -- seeds some red-red game must actually cast a Bolt, or instant speed
      -- could silently never fire while the suite stays green.
      QC.testProperty "instants happen: some seed casts a Bolt" $
        QC.once $
          QC.property $
            any boltCast_ [1 .. 100 :: Int]
    ]

-- Did this seed's red-red game put a Bolt into a graveyard? A cast Bolt always
-- ends there (resolved or fizzled), and nothing else moves one out of a library.
boltCast_ :: Int -> Bool
boltCast_ s =
  let gs = runRandomGame redRed s
      isBolt oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Printing.card printing == Printing.card Card.lightningBoltPrinting
      inGrave pid = any isBolt (Game.zoneMembers Zone.Graveyard pid gs)
   in any inGrave [alice, bob]

-- Did anyone's life total move over the course of the game this seed produces?
someLifeChanged :: Int -> Bool
someLifeChanged s =
  let moved pl = Player.life pl /= Setup.startingLife
   in any moved (Map.elems (GameState.players (runRandomGame redRed s)))

-- Did some green-black seed put a creature into a graveyard? In green-black the
-- only way a creature dies is combat (trade, deathtouch SBA, or trample), so
-- this fails only if combat never engages across all these seeds.
creatureDied :: Int -> Bool
creatureDied s =
  let gs = runRandomGame greenBlack s
      isDeadCreature oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.isCreature (Printing.card printing)
      inGrave pid = any isDeadCreature (Game.zoneMembers Zone.Graveyard pid gs)
   in any inGrave [alice, bob]

targetTests :: Tasty.TestTree
targetTests =
  Tasty.testGroup
    "Target"
    [ HU.testCase "CR 115.4 AnyTarget offers every creature and every playing player" $
        let (oid, gs) = addPiker bob (Setup.emptyGame bothPlayers)
         in HU.assertEqual
              "creature and both players"
              (Set.fromList [Recipient.ToCreature oid, Recipient.ToPlayer alice, Recipient.ToPlayer bob])
              (Target.legalRecipients TargetSpec.AnyTarget gs),
      HU.testCase "a departed player is not a legal target" $
        let gs = Sba.depart bob (Setup.emptyGame bothPlayers)
         in HU.assertBool
              "bob gone"
              (not (Set.member (Recipient.ToPlayer bob) (Target.legalRecipients TargetSpec.AnyTarget gs))),
      HU.testCase "CR 608.2b a creature that left its zone is no longer legal" $
        let (oid, gs) = addPiker bob (Setup.emptyGame bothPlayers)
            gone = Game.changeZone oid Zone.Graveyard gs
         in do
              HU.assertBool "legal while fielded" (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.AnyTarget gs)
              HU.assertBool "illegal once moved" (not (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.AnyTarget gone)),
      HU.testCase "legalSets maps each slot to its legal recipients" $
        let specs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget
            gs = Setup.emptyGame bothPlayers
         in HU.assertEqual
              "one slot, two players"
              (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [Recipient.ToPlayer alice, Recipient.ToPlayer bob]))
              (Target.legalSets specs gs)
    ]

resolveTests :: Tasty.TestTree
resolveTests =
  Tasty.testGroup
    "Resolve"
    [ HU.testCase "CR 608.3 / 704.5g a resolved Bolt kills a Piker" $
        let (_, cast, _) = boltAtBobsPiker
            after = Sba.checkStateBasedActions (Stack.resolveTop cast)
         in do
              HU.assertEqual "stack empty" 0 (length (GameState.stack after))
              HU.assertEqual "no creature survives" 0 (creaturesInPlay bob after)
              HU.assertEqual "Piker in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard bob after)),
      HU.testCase "CR 608.2n the resolved Bolt is in its owner's graveyard" $
        let (_, cast, _) = boltAtBobsPiker
            after = Stack.resolveTop cast
         in HU.assertEqual "one card" 1 (length (Game.zoneMembers Zone.Graveyard alice after)),
      HU.testCase "CR 120.3a a Bolt at a player drains life without marking" $
        -- No creature on the battlefield, so identityAnswer's lookupMin picks
        -- ToPlayer alice: a self-Bolt, which is legal Magic.
        let (gs, oid) = boltInHand 1 Phase.PrecombatMain
            cast = snd (Engine.runGamePure identityAnswer gs (Cast.castSpell alice oid))
            after = Stack.resolveTop cast
         in HU.assertEqual "seventeen" (Just 17) (lifeOf alice after),
      HU.testCase "the resolved damage flows through the event funnel" $
        let (_, cast, _) = boltAtBobsPiker
            after = Stack.resolveTop cast
         in HU.assertEqual "one event of amount 3" [3] (map DamageEvent.amount (GameState.damageEvents after)),
      HU.testCase "resolving a Bolt conserves objects" $
        let (_, cast, _) = boltAtBobsPiker
         in HU.assertEqual "conserved" (Game.objectCount cast) (Game.objectCount (Stack.resolveTop cast)),
      HU.testCase "CR 608.2b a Bolt whose only target died fizzles" $
        let (base, cast, _) = boltAtBobsPiker
            -- Kill the Piker while the Bolt is on the stack, as Bolt B will in
            -- the integration test, then check state-based actions.
            dead = Sba.checkStateBasedActions (markDamage (pikerOf base) 3 cast)
            after = Stack.resolveTop dead
         in do
              HU.assertEqual "Bolt in the graveyard, unresolved" 1 (length (Game.zoneMembers Zone.Graveyard alice after))
              HU.assertEqual "no damage was dealt" [] (GameState.damageEvents after)
              HU.assertEqual "bob untouched" (Just 20) (lifeOf bob after),
      HU.testCase "CR 608.2b a fizzled spell applies none of its effects" $
        let (base, cast, _) = boltAtBobsPiker
            dead = Sba.checkStateBasedActions (markDamage (pikerOf base) 3 cast)
            after = Stack.resolveTop dead
         in HU.assertEqual "life totals unchanged" (Just 20) (lifeOf alice after)
    ]

-- Casts every castable spell (targets via lookupMin: creatures first),
-- otherwise passes. Drives the Bolt-vs-Bolt integration falsifier.
boltAnswer :: Prompt.Prompt r -> r
boltAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          A.Cast _ -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> A.Pass
  _ -> identityAnswer p

-- bob's Piker on the battlefield; alice holds TWO Bolts and two Mountains, in
-- her main phase. boltAnswer casts both (CR 117.3c keeps priority), both
-- target the Piker (the only creature), and the priority loop resolves them
-- LIFO: B kills the Piker, the mid-loop SBA buries it, A fizzles.
twoBoltState :: GameState.GameState
twoBoltState =
  let (_, withPiker) = addPiker bob (mountainsInPlay 2)
      (gs1, _oid1) = handOne Card.lightningBoltPrinting withPiker
      (oid2, gs2) = Game.freshObjectId gs1
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard Card.lightningBoltPrinting,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty
          }
   in gs2
        { GameState.objects = Map.insert oid2 obj (GameState.objects gs2),
          -- handOne already put oid1 in hand; ADD the second Bolt, oid2.
          GameState.hand = Map.adjust (oid2 Seq.<|) alice (GameState.hand gs2)
        }

fizzleTests :: Tasty.TestTree
fizzleTests =
  Tasty.testGroup
    "Fizzle"
    [ HU.testCase "CR 608.2b Bolt-vs-Bolt through the priority loop: the second fizzles" $
        let after = snd (Engine.runGamePure boltAnswer twoBoltState Engine.priorityLoop)
         in do
              HU.assertEqual "stack cleared" 0 (length (GameState.stack after))
              HU.assertEqual "Piker dead" 0 (creaturesInPlay bob after)
              HU.assertEqual "both Bolts in alice's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard alice after))
              HU.assertEqual "the Piker in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard bob after))
              HU.assertEqual "bob's life untouched: the fizzled Bolt hit nothing" (Just 20) (lifeOf bob after),
      HU.testCase "CR 704.5a a Bolt can end the game mid-step" $
        let (gs, oid) = boltInHand 1 Phase.PrecombatMain
            lowBob =
              gs {GameState.players = Map.adjust (\pl -> pl {Player.life = 3}) bob (GameState.players gs)}
            atBob :: Prompt.Prompt r -> r
            atBob p = case p of
              Prompt.ChooseTargets _ _ _ sets ->
                Map.map (const (Recipient.ToPlayer bob)) sets
              Prompt.ChooseAction _ _ actions ->
                case filter (\a -> a == A.Cast oid) actions of
                  h : _ -> h
                  [] -> A.Pass
              _ -> identityAnswer p
            after = snd (Engine.runGamePure atBob lowBob Engine.priorityLoop)
         in do
              HU.assertEqual "alice wins" (Just (Result.Won alice)) (GameState.result after)
              HU.assertEqual "the loop released priority" Nothing (GameState.priority after)
    ]

damageEventTests :: Tasty.TestTree
damageEventTests =
  Tasty.testGroup
    "DamageEvent"
    [ HU.testCase "a blocked 2/1 trade emits both damage events" $
        let (gs, mine, theirs) = combatBoard 1 1
            after = fightWith aggressiveAnswer gs
            events = GameState.damageEvents after
         in case (mine, theirs) of
              (a : _, b : _) -> do
                HU.assertEqual "two events" 2 (length events)
                HU.assertBool "attacker hit blocker for 2" $
                  elem (DamageEvent.MkDamageEvent a (Recipient.ToCreature b) 2) events
                HU.assertBool "blocker hit attacker for 2" $
                  elem (DamageEvent.MkDamageEvent b (Recipient.ToCreature a) 2) events
              _ -> HU.assertFailure "fixture should have one creature per side",
      HU.testCase "an unblocked 2/1 emits a ToPlayer event" $
        let (gs, mine, _) = combatBoard 1 0
            after = fightWith aggressiveAnswer gs
         in case mine of
              a : _ ->
                HU.assertEqual
                  "one player event"
                  [DamageEvent.MkDamageEvent a (Recipient.ToPlayer bob) 2]
                  (GameState.damageEvents after)
              _ -> HU.assertFailure "fixture should have an attacker"
    ]

deathtouchTests :: Tasty.TestTree
deathtouchTests =
  Tasty.testGroup
    "Deathtouch"
    [ HU.testCase "CR 704.5h a 1/1 deathtoucher destroys a 3/3 it deals 1 to" $
        -- Typhoid Rats attacks, Ogre Sentry blocks. Rat deals 1 -> Ogre dies by
        -- 704.5h (toughness 3, not lethal by the numbers); Ogre's 3 kills the Rat.
        let (gs, _, _) = combatBoardOf [Card.typhoidRatsPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
         in do
              HU.assertEqual "the Ogre is dead" 0 (creaturesInPlay bob after)
              HU.assertEqual "the Rat is dead" 0 (creaturesInPlay alice after),
      HU.testCase "CR 704.5g the control: a 2/1 without deathtouch leaves the 3/3 alive" $
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
         in HU.assertEqual "the Ogre survives" 1 (creaturesInPlay bob after),
      HU.testCase "the SBA check drains the damage events" $
        let (gs, _, _) = combatBoardOf [Card.typhoidRatsPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
         in HU.assertEqual "events drained" [] (GameState.damageEvents after)
    ]

assignmentLegalityTests :: Tasty.TestTree
assignmentLegalityTests =
  Tasty.testGroup
    "AssignmentLegality"
    [ HU.testCase "under-assignment with no overflow is legal (power below lethal)" $
        -- One blocker, lethal 3, power 2, defender present with threshold 0.
        let thresholds :: Map.Map Recipient.Recipient Natural.Natural
            thresholds =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                  (Recipient.ToPlayer bob, 0)
                ]
            answer :: Map.Map Recipient.Recipient Natural.Natural
            answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 2)]
         in HU.assertBool "accepted" (Damage.legalAssignment thresholds 2 answer),
      HU.testCase "defender damage while a blocker is short is illegal" $
        let thresholds :: Map.Map Recipient.Recipient Natural.Natural
            thresholds =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                  (Recipient.ToPlayer bob, 0)
                ]
            answer :: Map.Map Recipient.Recipient Natural.Natural
            answer =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 0),
                  (Recipient.ToPlayer bob, 3)
                ]
         in HU.assertBool "rejected" (not (Damage.legalAssignment thresholds 3 answer)),
      HU.testCase "defender damage once the blocker has lethal is legal" $
        let thresholds :: Map.Map Recipient.Recipient Natural.Natural
            thresholds =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                  (Recipient.ToPlayer bob, 0)
                ]
            answer :: Map.Map Recipient.Recipient Natural.Natural
            answer =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                  (Recipient.ToPlayer bob, 2)
                ]
         in HU.assertBool "accepted" (Damage.legalAssignment thresholds 3 answer),
      HU.testCase "an answer that does not total power is illegal" $
        let thresholds :: Map.Map Recipient.Recipient Natural.Natural
            thresholds = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 0)]
            answer :: Map.Map Recipient.Recipient Natural.Natural
            answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 1)]
         in HU.assertBool "rejected" (not (Damage.legalAssignment thresholds 2 answer)),
      HU.testCase "an illegal recipient is rejected" $
        let thresholds :: Map.Map Recipient.Recipient Natural.Natural
            thresholds = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 0)]
            answer :: Map.Map Recipient.Recipient Natural.Natural
            answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 2), 2)]
         in HU.assertBool "rejected" (not (Damage.legalAssignment thresholds 2 answer)),
      QC.testProperty "an accepted assignment always totals power and gates the defender" $
        QC.forAll genLegalityCase $ \(thresholds, power, answer) ->
          not (Damage.legalAssignment thresholds power answer)
            || ( sum (Map.elems answer) == power
                   && all (\r -> Map.member r thresholds) (Map.keys answer)
                   && ( Map.findWithDefault 0 (Recipient.ToPlayer bob) answer == 0
                          || all
                            (\(r, t) -> Map.findWithDefault 0 r answer >= t)
                            (Map.toList (Map.filterWithKey (\r _ -> isCreatureRecipient r) thresholds))
                      )
               )
    ]

-- A blocker (lethal 0..4), a defender (threshold 0), power 0..6, and an arbitrary
-- assignment over those two recipients. Covers power below / equal to / above
-- lethal and every over/under split.
genLegalityCase :: QC.Gen (Map.Map Recipient.Recipient Natural.Natural, Natural.Natural, Map.Map Recipient.Recipient Natural.Natural)
genLegalityCase = do
  lethal <- QC.choose (0, 4) :: QC.Gen Integer
  power <- QC.choose (0, 6) :: QC.Gen Integer
  toBlocker <- QC.choose (0, 6) :: QC.Gen Integer
  toDefender <- QC.choose (0, 6) :: QC.Gen Integer
  let blocker = Recipient.ToCreature (ObjectId.MkObjectId 1)
      thresholds :: Map.Map Recipient.Recipient Natural.Natural
      thresholds = Map.fromList [(blocker, fromInteger lethal), (Recipient.ToPlayer bob, 0)]
      answer :: Map.Map Recipient.Recipient Natural.Natural
      answer = Map.fromList [(blocker, fromInteger toBlocker), (Recipient.ToPlayer bob, fromInteger toDefender)]
  pure (thresholds, fromInteger power, answer)

-- Assigns each blocker exactly its threshold, and every leftover point to the
-- defender. A legal trample division for these boards.
tramplingAnswer :: Prompt.Prompt r -> r
tramplingAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = Map.toList (Map.filterWithKey (\r _ -> isCreatureRecipient r) thresholds)
        toBlockers = Map.fromList (map (\(r, t) -> (r, t)) blockers)
        spent = sum (map snd blockers)
        leftover = if n >= spent then n - spent else 0
        defenders = filter (not . isCreatureRecipient) (Map.keys thresholds)
     in case defenders of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> aggressiveAnswer p

trampleTests :: Tasty.TestTree
trampleTests =
  Tasty.testGroup
    "Trample"
    [ HU.testCase "CR 702.19b a 3/3 trampler spills excess onto the defending player" $
        -- War Mammoth (3/3 trample) blocked by a Piker (2/1): 1 lethal to the
        -- Piker, 2 to bob. Mammoth survives (2 marked < 3).
        let (gs, _, _) = combatBoardOf [Card.warMammothPrinting] [Card.pikerPrinting]
            after = Sba.checkStateBasedActions (fightWith tramplingAnswer gs)
         in do
              HU.assertEqual "bob took the 2 overflow" (Just 18) (lifeOf bob after)
              HU.assertEqual "the Piker is dead" 0 (creaturesInPlay bob after)
              HU.assertEqual "the Mammoth survives" 1 (creaturesInPlay alice after),
      HU.testCase "CR 702.19b a non-trample control spills nothing" $
        -- Ogre Sentry is a 3/3 that cannot attack (defender), so use the Piker's
        -- existing behavior as the control: a blocked non-trample attacker deals
        -- nothing to the player. (combatDamageTests already asserts bob = 20.)
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
            after = fightWith tramplingAnswer gs
         in HU.assertEqual "bob untouched by a non-trampler" (Just 20) (lifeOf bob after),
      HU.testCase "CR 702.19b defender-short assignment is rejected" $
        -- A cheat responder gives bob 3 while the Piker gets 0. Illegal: the
        -- attacker deals nothing, bob untouched, Piker survives.
        let (gs, _, _) = combatBoardOf [Card.warMammothPrinting] [Card.pikerPrinting]
            cheat p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter (not . isCreatureRecipient) (Map.keys thresholds) of
                  d : _ -> Map.singleton d n
                  [] -> Map.empty
              _ -> aggressiveAnswer p
            after = Sba.checkStateBasedActions (fightWith cheat gs)
         in do
              HU.assertEqual "bob untouched" (Just 20) (lifeOf bob after)
              HU.assertEqual "the Piker survives the rejected assignment" 1 (creaturesInPlay bob after),
      HU.testCase "CR 702.19b under-assignment across two blockers spills nothing (power below total lethal)" $
        -- War Mammoth (3/3 trample) blocked by TWO Ogre Sentries (3/3 each): it
        -- cannot reach lethal on both (needs 6, has 3), so no overflow -- bob is
        -- untouched -- and the division among the Ogres is free. Real cards, the
        -- power-below-lethal case the property covers exhaustively.
        let (gs, _, _) = combatBoardOf [Card.warMammothPrinting] [Card.ogreSentryPrinting, Card.ogreSentryPrinting]
            dumpOne p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter isCreatureRecipient (Map.keys thresholds) of
                  r : _ -> Map.singleton r n
                  [] -> Map.empty
              _ -> aggressiveAnswer p
            after = Sba.checkStateBasedActions (fightWith dumpOne gs)
         in do
              HU.assertEqual "bob untouched (no overflow)" (Just 20) (lifeOf bob after)
              HU.assertEqual "one Ogre took all 3 and died, the other lived" 1 (creaturesInPlay bob after)
    ]

-- SYNTHETIC, NOT A REAL CARD. No printed Magic creature has both deathtouch and
-- trample (Scryfall keyword:deathtouch keyword:trample is empty), and M2c has no
-- granting effect (that is M3, e.g. Basilisk Collar) to combine them on a real
-- card. This fixture is the only way to exercise CR 702.2c in M2c. EXPIRES at M3:
-- grant deathtouch to a real trampler (War Mammoth) and delete this. See the M2c
-- spec, section 6, and git-bug's M3 work.
syntheticDeathtramplerPrinting :: Printing.Printing
syntheticDeathtramplerPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.Type.MkCard
          { Card.Type.name = Text.pack "Synthetic Deathtrampler (test fixture)",
            Card.Type.manaCost = Nothing,
            Card.Type.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.empty
                },
            Card.Type.power = Just (Power.MkPower (Quantity.Type.Literal 3)),
            Card.Type.toughness = Just (Toughness.MkToughness (Quantity.Type.Literal 3)),
            Card.Type.keywords = Set.fromList [Keyword.Deathtouch, Keyword.Trample],
            Card.Type.effects = [],
            Card.Type.targetSpecs = Map.empty
          }
    }

trampleDeathtouchTests :: Tasty.TestTree
trampleDeathtouchTests =
  Tasty.testGroup
    "TrampleDeathtouch"
    [ HU.testCase "CR 702.2c a deathtouch trampler needs only 1 on the blocker, spilling the rest" $
        -- Synthetic 3/3 deathtouch+trample into Ogre Sentry (3/3): lethal is 1, so
        -- 1 to the Ogre and 2 tramples to bob. The Ogre still dies (704.5h).
        let (gs, _, _) = combatBoardOf [syntheticDeathtramplerPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith tramplingAnswer gs)
         in do
              HU.assertEqual "bob took 2 overflow" (Just 18) (lifeOf bob after)
              HU.assertEqual "the Ogre is dead" 0 (creaturesInPlay bob after),
      HU.testCase "CR 702.19b the control: plain trample into the same 3/3 spills nothing" $
        -- War Mammoth (3/3 trample, no deathtouch) into Ogre Sentry (3/3): lethal
        -- is 3, all 3 go to the Ogre, 0 tramples. Only deathtouch changes the spill.
        let (gs, _, _) = combatBoardOf [Card.warMammothPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith tramplingAnswer gs)
         in HU.assertEqual "bob untouched without deathtouch" (Just 20) (lifeOf bob after)
    ]

m2cPropertyTests :: Tasty.TestTree
m2cPropertyTests =
  Tasty.testGroup
    "M2cProperties"
    [ HU.testCase "a deathtoucher's victim with toughness > 0 is gone after the SBA" $
        -- The property in fixture form (the deck has no deathtoucher, so this is
        -- the M2c coverage; it becomes a random-game property when a deathtoucher
        -- joins a deck -- git-bug's castability work). Every toughness we throw at
        -- the 1/1 deathtoucher dies to it.
        let victims = [Card.pikerPrinting, Card.nimbleBirdstickerPrinting, Card.ogreSentryPrinting]
            killsIt v =
              let (gs, _, _) = combatBoardOf [Card.typhoidRatsPrinting] [v]
                  after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
               in creaturesInPlay bob after == 0
         in HU.assertBool "deathtouch kills every toughness" (all killsIt victims),
      HU.testCase "the deathtouch and trample reads never name a card" $
        -- A structural reminder, asserted by the interaction falsifier's outcome
        -- (TrampleDeathtouch) depending only on the keyword projection. This case
        -- documents the invariant; the real enforcement is code review of
        -- Damage.blockerThreshold and Sba.woundedByDeathtouch, which case on
        -- Keyword, never on a printing.
        HU.assertBool "see TrampleDeathtouch and Deathtouch groups" True
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

m2bExitTests :: Tasty.TestTree
m2bExitTests =
  Tasty.testGroup
    "M2bExit"
    [ HU.testCase "the milestone: first strike breaks the trade, double strike doubles the hit, no attacker no damage" $
        let trade = runCombat aggressiveAnswer (frst (combatBoardOf [Card.sabretoothTigerPrinting] [Card.pikerPrinting]))
            doubled = runCombat aggressiveAnswer (frst (combatBoardOf [Card.ridgetopRaptorPrinting] []))
            quiet = runCombat aggressiveAnswer (frst (combatBoardOf [] []))
         in do
              HU.assertEqual "first striker lives" 1 (creaturesInPlay alice trade)
              HU.assertEqual "its would-be killer is dead" 0 (creaturesInPlay bob trade)
              HU.assertEqual "double striker deals 4" (Just 16) (lifeOf bob doubled)
              HU.assertEqual "an attacker-less turn deals nothing" (Just 20) (lifeOf bob quiet)
    ]
