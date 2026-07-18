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
import qualified Pawl.CombatSpec as CombatSpec
import qualified Pawl.CoreSpec as CoreSpec
import qualified Pawl.DamageSpec as DamageSpec
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
  ( addPiker,
    alice,
    bob,
    boltAtBobsPiker,
    boltInHand,
    bothPlayers,
    creaturesInPlay,
    greenBlack,
    handOne,
    identityAnswer,
    lifeOf,
    markDamage,
    matchups,
    mountainsInPlay,
    pikerOf,
    playLandAnswer,
    redRed,
    runRandomGame,
  )
import qualified Pawl.Target as Target
import qualified Pawl.TurnSpec as TurnSpec
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
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
      DamageSpec.tests,
      replayTests,
      propertyTests,
      ManaSpec.tests,
      CastSpec.tests,
      CombatSpec.tests,
      combatReplayTests,
      targetTests,
      resolveTests,
      fizzleTests
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
