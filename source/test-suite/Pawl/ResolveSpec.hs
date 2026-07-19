{-# LANGUAGE GADTs #-}

-- Covers Pawl.Resolve and Pawl.Target: targeting legality, spell resolution, and
-- the CR 608.2b fizzle.
module Pawl.ResolveSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Card as Card
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

targetTests :: Tasty.TestTree
targetTests =
  Tasty.testGroup
    "Target"
    [ HU.testCase "CR 115.4 AnyTarget offers every creature and every playing player" $
        let (oid, gs) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual
              "creature and both players"
              (Set.fromList [Recipient.ToCreature oid, Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob])
              (Target.legalRecipients TargetSpec.AnyTarget gs),
      HU.testCase "a departed player is not a legal target" $
        let gs = Sba.depart S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertBool
              "bob gone"
              (not (Set.member (Recipient.ToPlayer S.bob) (Target.legalRecipients TargetSpec.AnyTarget gs))),
      HU.testCase "CR 608.2b a creature that left its zone is no longer legal" $
        let (oid, gs) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
            gone = Game.changeZone oid Zone.Graveyard gs
         in do
              HU.assertBool "legal while fielded" (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.AnyTarget gs)
              HU.assertBool "illegal once moved" (not (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.AnyTarget gone)),
      HU.testCase "legalSets maps each slot to its legal recipients" $
        let specs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget
            gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual
              "one slot, two players"
              (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]))
              (Target.legalSets specs gs),
      HU.testCase "CR 115.4 CreatureTarget offers creatures but no players" $
        let (oid, gs) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual
              "just the creature"
              (Set.singleton (Recipient.ToCreature oid))
              (Target.legalRecipients TargetSpec.CreatureTarget gs),
      HU.testCase "CR 601.2c CreatureTarget has an empty legal set with no creatures" $
        HU.assertBool
          "nothing to target"
          (Set.null (Target.legalRecipients TargetSpec.CreatureTarget (Setup.emptyGame S.bothPlayers))),
      HU.testCase "CR 608.2b a creature that left is no longer a legal CreatureTarget" $
        let (oid, gs) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
            gone = Game.changeZone oid Zone.Graveyard gs
         in do
              HU.assertBool "legal while fielded" (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.CreatureTarget gs)
              HU.assertBool "illegal once moved" (not (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.CreatureTarget gone)),
      HU.testCase "CR 115 SpellOrPermanentTarget offers battlefield permanents and stack spells" $
        let (permId, gs) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertBool
              "the permanent is a legal object target"
              (Set.member (Recipient.ToObject permId) (Target.legalRecipients TargetSpec.SpellOrPermanentTarget gs)),
      HU.testCase "LandTarget offers a land as an object target, not a creature or player" $
        let gs = S.mountainsInPlay 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
         in do
              HU.assertBool "the land is legal" (Set.member (Recipient.ToObject landId) (Target.legalRecipients TargetSpec.LandTarget gs))
              HU.assertBool "no players" (not (Set.member (Recipient.ToPlayer S.alice) (Target.legalRecipients TargetSpec.LandTarget gs)))
    ]

resolveTests :: Tasty.TestTree
resolveTests =
  Tasty.testGroup
    "Resolve"
    [ HU.testCase "CR 608.3 / 704.5g a resolved Bolt kills a Piker" $
        let (_, cast, _) = S.boltAtBobsPiker
            after = Sba.checkStateBasedActions (Stack.resolveTop cast)
         in do
              HU.assertEqual "stack empty" 0 (length (GameState.stack after))
              HU.assertEqual "no creature survives" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "Piker in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 608.2n the resolved Bolt is in its owner's graveyard" $
        let (_, cast, _) = S.boltAtBobsPiker
            after = Stack.resolveTop cast
         in HU.assertEqual "one card" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 120.3a a Bolt at a player drains life without marking" $
        -- No creature on the battlefield, so identityAnswer's lookupMin picks
        -- ToPlayer alice: a self-Bolt, which is legal Magic.
        let (gs, oid) = S.boltInHand 1 Phase.PrecombatMain
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid))
            after = Stack.resolveTop cast
         in HU.assertEqual "seventeen" (Just 17) (S.lifeOf S.alice after),
      HU.testCase "the resolved damage flows through the event funnel" $
        let (_, cast, _) = S.boltAtBobsPiker
            after = Stack.resolveTop cast
         in HU.assertEqual "one event of amount 3" [3] (map DamageEvent.amount (GameState.damageEvents after)),
      HU.testCase "resolving a Bolt conserves objects" $
        let (_, cast, _) = S.boltAtBobsPiker
         in HU.assertEqual "conserved" (Game.objectCount cast) (Game.objectCount (Stack.resolveTop cast)),
      HU.testCase "CR 608.2b a Bolt whose only target died fizzles" $
        let (base, cast, _) = S.boltAtBobsPiker
            -- Kill the Piker while the Bolt is on the stack, as Bolt B will in
            -- the integration test, then check state-based actions.
            dead = Sba.checkStateBasedActions (S.markDamage (S.pikerOf base) 3 cast)
            after = Stack.resolveTop dead
         in do
              HU.assertEqual "Bolt in the graveyard, unresolved" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
              HU.assertEqual "no damage was dealt" [] (GameState.damageEvents after)
              HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 608.2b a fizzled spell applies none of its effects" $
        let (base, cast, _) = S.boltAtBobsPiker
            dead = Sba.checkStateBasedActions (S.markDamage (S.pikerOf base) 3 cast)
            after = Stack.resolveTop dead
         in HU.assertEqual "life totals unchanged" (Just 20) (S.lifeOf S.alice after),
      -- The deterministic successor to the retired "instants happen" property: a
      -- Bolt cast in a game and resolved ends in its owner's graveyard.
      HU.testCase "a cast Bolt reaches its owner's graveyard" $
        let (_, cast, _) = S.boltAtBobsPiker
            after = Stack.resolveTop cast
         in HU.assertEqual "one card in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
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
  _ -> S.identityAnswer p

-- bob's Piker on the battlefield; alice holds TWO Bolts and two Mountains, in
-- her main phase. boltAnswer casts both (CR 117.3c keeps priority), both
-- target the Piker (the only creature), and the priority loop resolves them
-- LIFO: B kills the Piker, the mid-loop SBA buries it, A fizzles.
twoBoltState :: GameState.GameState
twoBoltState =
  let (_, withPiker) = S.addPiker S.bob (S.mountainsInPlay 2)
      (gs1, _oid1) = S.handOne Card.lightningBoltPrinting withPiker
      (oid2, gs2) = Game.freshObjectId gs1
      obj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.source = Source.OfCard Card.lightningBoltPrinting,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = Timestamp.MkTimestamp 0
          }
   in gs2
        { GameState.objects = Map.insert oid2 obj (GameState.objects gs2),
          -- handOne already put oid1 in hand; ADD the second Bolt, oid2.
          GameState.hand = Map.adjust (oid2 Seq.<|) S.alice (GameState.hand gs2)
        }

fizzleTests :: Tasty.TestTree
fizzleTests =
  Tasty.testGroup
    "Fizzle"
    [ HU.testCase "CR 608.2b Bolt-vs-Bolt through the priority loop: the second fizzles" $
        let after = snd (Engine.runGamePure boltAnswer twoBoltState Engine.priorityLoop)
         in do
              HU.assertEqual "stack cleared" 0 (length (GameState.stack after))
              HU.assertEqual "Piker dead" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "both Bolts in alice's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.alice after))
              HU.assertEqual "the Piker in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "bob's life untouched: the fizzled Bolt hit nothing" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 704.5a a Bolt can end the game mid-step" $
        let (gs, oid) = S.boltInHand 1 Phase.PrecombatMain
            lowBob =
              gs {GameState.players = Map.adjust (\pl -> pl {Player.life = 3}) S.bob (GameState.players gs)}
            atBob :: Prompt.Prompt r -> r
            atBob p = case p of
              Prompt.ChooseTargets _ _ _ sets ->
                Map.map (const (Recipient.ToPlayer S.bob)) sets
              Prompt.ChooseAction _ _ actions ->
                case filter (\a -> a == A.Cast oid) actions of
                  h : _ -> h
                  [] -> A.Pass
              _ -> S.identityAnswer p
            after = snd (Engine.runGamePure atBob lowBob Engine.priorityLoop)
         in do
              HU.assertEqual "alice wins" (Just (Result.Won S.alice)) (GameState.result after)
              HU.assertEqual "the loop released priority" Nothing (GameState.priority after)
    ]

tests :: Tasty.TestTree
tests = Tasty.testGroup "Resolve" [targetTests, resolveTests, fizzleTests]
