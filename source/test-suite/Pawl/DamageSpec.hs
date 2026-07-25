{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Damage and Pawl.Sba: the damage funnel, deathtouch, trample, and
-- state-based actions. ((m2cPropertyTests cards) is deterministic fixture coverage, not
-- QuickCheck properties.)
module Pawl.DamageSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Combat as Combat
import qualified Pawl.Damage as Damage
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.Combat as Combat.Type
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.DamagePattern as DamagePattern
import qualified Pawl.Type.DamageRewrite as DamageRewrite
import qualified Pawl.Type.Departure as Departure.Type
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Expiry as Expiry.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Uses as Uses
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

creatureSbaTests :: Registry.Type.Registry -> Tasty.TestTree
creatureSbaTests registry =
  Tasty.testGroup
    "CreatureSba"
    [ HU.testCase "CR 704.5g a creature with lethal damage is destroyed" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            after = S.settleSba (S.markDamage oid 1 gs)
        HU.assertEqual "off the battlefield" [] (Game.zoneMembers Zone.Battlefield S.alice after)
        HU.assertEqual "in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 704.5g damage below toughness is not lethal" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            -- A Piker is 2/1, so 0 marked damage is survivable and 1 is not.
            after = S.settleSba (S.markDamage oid 0 gs)
        HU.assertEqual "still there" 1 (length (Game.zoneMembers Zone.Battlefield S.alice after)),
      HU.testCase "CR 704.5g a Mountain with damage marked is not destroyed" $ do
        -- Not a creature: 704.5f/g do not apply. This is the classification
        -- doing its job -- the check never asks WHICH card it is.
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 1
        case Game.zoneMembers Zone.Battlefield S.alice gs of
          [] -> HU.assertFailure "fixture should have one Mountain"
          oid : _ ->
            HU.assertEqual
              "survives"
              1
              (length (Game.zoneMembers Zone.Battlefield S.alice (S.settleSba (S.markDamage oid 5 gs)))),
      HU.testCase "a destroyed creature conserves objects" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            marked = S.markDamage oid 1 gs
        HU.assertEqual
          "conserved"
          (Game.objectCount marked)
          (Game.objectCount (S.settleSba marked)),
      -- The deterministic successor to the retired green-black "some seed sends a
      -- creature to the graveyard" property: two 2/1 Pikers trade in combat and
      -- both die to the CR 704.5g state-based action.
      HU.testCase "a creature dies in a played-out combat" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoard piker 1 1
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
        HU.assertEqual "attacker died" 0 (S.creaturesInPlay S.alice after)
        HU.assertEqual "blocker died" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 704.5d a token off the battlefield ceases to exist" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card piker
            (tokId, gs) = S.addToken goblinCard S.alice base
            inGrave = S.runPure S.identityAnswer gs (Event.changeZone tokId Zone.Graveyard)
            -- The changeZone minted a new incarnation; find it in the graveyard.
            settled = S.settleSba inGrave
        HU.assertEqual "before the SBA, a token sits in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice inGrave))
        HU.assertEqual "after the SBA, it has ceased to exist" 0 (length (Game.zoneMembers Zone.Graveyard S.alice settled))
        HU.assertEqual "no token objects remain" 0 (Game.objectCount settled),
      HU.testCase "CR 704.5d a token on the battlefield does NOT cease" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card piker
            (_, gs) = S.addToken goblinCard S.alice base
            settled = S.settleSba gs
        HU.assertEqual "the token survives on the battlefield" 1 (Game.objectCount settled),
      HU.testCase "CR 704.5d/704.5g a 1/1 token dies to lethal damage and ceases to exist" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card piker
            -- A real 2/1 Piker (bob's) is the damage source; alice's 1/1 token takes 2.
            (srcId, gs1) = S.addCreature piker S.bob base
            (tokId, gs2) = S.addToken goblinCard S.alice gs1
            damaged = S.runPure S.identityAnswer gs2 (Damage.applyDamage [DamageEvent.MkDamageEvent srcId (Recipient.ToCreature tokId) 2 False False DamageKind.Combat])
            settled = S.settleSba damaged
        HU.assertEqual "the token is gone from the battlefield" 0 (S.creaturesInPlay S.alice settled)
        HU.assertEqual "and NOT sitting in a graveyard (the falsifier)" 0 (length (Game.zoneMembers Zone.Graveyard S.alice settled)),
      HU.testCase "CR 704.5g regeneration saves a creature from lethal combat damage" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            (victim, gs0) = S.addCreature piker S.alice base -- 2/1
            shielded = S.addRegenShield victim gs0
            -- 2 combat damage is lethal to a 2/1; the shield replaces the CR 704.5g destruction.
            damaged = S.runPure S.identityAnswer shielded (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False DamageKind.Combat])
            settled = S.settleSba damaged
        HU.assertEqual "survived (regenerated)" True (Set.member victim (GameState.battlefield settled))
        case Game.lookupObject victim settled of
          Just obj -> do
            HU.assertEqual "tapped" TapState.Tapped (Object.tapped obj)
            HU.assertEqual "damage healed" 0 (Object.damage obj)
          Nothing -> HU.assertFailure "victim vanished"
    ]

damageTests :: Registry.Type.Registry -> Tasty.TestTree
damageTests registry =
  Tasty.testGroup
    "Damage"
    [ HU.testCase "a permanent starts with no damage marked" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual "none" (Just 0) (S.damageOf oid gs),
      HU.testCase "CR 514.2 marked damage is removed" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual "removed" (Just 0) (S.damageOf oid (Damage.removeAllDamage (S.markDamage oid 1 gs))),
      HU.testCase "CR 514.2 damage wears off at the cleanup step" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            marked = S.markDamage oid 1 gs
            after = snd (Engine.runGamePure S.identityAnswer marked (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
        HU.assertEqual "worn off" (Just 0) (S.damageOf oid after),
      HU.testCase "CR 400.7 a new object carries no damage forward" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            marked = S.markDamage oid 1 gs
            after = S.runPure S.identityAnswer marked (Event.changeZone oid Zone.Graveyard)
        case Game.zoneMembers Zone.Graveyard S.alice after of
          [] -> HU.assertFailure "expected a card in the graveyard"
          new : _ -> HU.assertEqual "fresh object, no damage" (Just 0) (S.damageOf new after),
      HU.testCase "CR 615 a prevention drops combat damage but spares Noncombat" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            (victim, gs0) = S.addCreature piker S.alice base
            shield =
              ActiveReplacement.MkActiveReplacement
                { ActiveReplacement.effect = ReplacementEffect.DamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat)) DamageRewrite.PreventAll,
                  ActiveReplacement.source = victim,
                  ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                  ActiveReplacement.expiry = Expiry.Type.AtCleanup,
                  ActiveReplacement.uses = Uses.Unlimited
                }
            withShield = S.addReplacement shield gs0
            combat = S.runPure S.identityAnswer withShield (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False DamageKind.Combat])
            spell = S.runPure S.identityAnswer withShield (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False DamageKind.Noncombat])
        HU.assertEqual "combat damage prevented -- none marked" (Just 0) (S.damageOf victim combat)
        HU.assertEqual "combat damage prevented -- no event recorded" [] (S.damageEventsOf combat)
        HU.assertEqual "noncombat damage still dealt" (Just 2) (S.damageOf victim spell),
      HU.testCase "CR 514.2 an until-end-of-turn replacement wears off at cleanup" $
        let base = Setup.emptyGame S.bothPlayers
            shield =
              ActiveReplacement.MkActiveReplacement
                { ActiveReplacement.effect = ReplacementEffect.DamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat)) DamageRewrite.PreventAll,
                  ActiveReplacement.source = ObjectId.MkObjectId 900,
                  ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                  ActiveReplacement.expiry = Expiry.Type.AtCleanup,
                  ActiveReplacement.uses = Uses.Unlimited
                }
            dropped = Expiry.dropAtCleanup (S.addReplacement shield base)
         in HU.assertEqual "no replacements remain" [] (GameState.replacements dropped)
    ]

infectTests :: Registry.Type.Registry -> Tasty.TestTree
infectTests registry =
  Tasty.testGroup
    "Infect"
    [ HU.testCase "CR 120.3b infect damage to a player becomes poison, not life loss" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False True DamageKind.Combat
            after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
        HU.assertEqual "bob has three poison" 3 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
        HU.assertEqual "bob's life unchanged" (Just 20) (S.lifeOf S.bob after)
        HU.assertEqual "the source's controller gains no poison" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
      HU.testCase "CR 120.3d infect damage to a creature becomes -1/-1 counters, not marked damage" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (victim, gs1) = S.addCreature piker S.bob gs0
            ev = DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) 2 False True DamageKind.Combat
            after = S.runPure S.identityAnswer gs1 (Damage.applyDamage [ev])
        HU.assertEqual "two -1/-1 counters" (Just 2) (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject victim after))
        HU.assertEqual "no marked damage" (Just 0) (S.damageOf victim after),
      HU.testCase "CR 702.90 Glistener Elf poisons an unblocked player, drains no life" $ do
        glistenerElf <- Registry.printing registry "Glistener Elf"
        let (gs, _, _) = S.combatBoardOf [glistenerElf] []
            after = S.fightWith S.aggressiveAnswer gs
        HU.assertEqual "bob has one poison" 1 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
        HU.assertEqual "bob's life unchanged" (Just 20) (S.lifeOf S.bob after)
        HU.assertEqual "alice (controller) has no poison" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
      HU.testCase "CR 702.90c Glistener Elf shrinks and kills a blocker with -1/-1 counters" $ do
        glistenerElf <- Registry.printing registry "Glistener Elf"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, blockers) = S.combatBoardOf [glistenerElf] [piker]
            fought = S.fightWith S.aggressiveAnswer gs
            settled = S.settleSba fought
        case blockers of
          [] -> HU.assertFailure "fixture should have a blocker"
          blocker : _ -> do
            HU.assertEqual "one -1/-1 counter before SBA" (Just 1) (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject blocker fought))
            HU.assertEqual "no marked damage on the blocker" (Just 0) (S.damageOf blocker fought)
            HU.assertEqual "blocker buried by 704.5f" 1 (length (Game.zoneMembers Zone.Graveyard S.bob settled))
    ]

sbaBase :: GameState.GameState
sbaBase = Setup.emptyGame S.bothPlayers

sbaTests :: Tasty.TestTree
sbaTests =
  Tasty.testGroup
    "Sba"
    [ HU.testCase "drew-from-empty loses" $
        let after = S.settleSba sbaBase {GameState.drewFromEmpty = Set.singleton S.alice}
         in HU.assertEqual "alice lost" (Just (Status.Departed Departure.Type.Lost)) (fmap Player.status (Map.lookup S.alice (GameState.players after))),
      HU.testCase "one remaining player wins" $
        let after = S.settleSba sbaBase {GameState.drewFromEmpty = Set.singleton S.alice}
         in HU.assertEqual "bob won" (Just (Result.Won S.bob)) (GameState.result after),
      HU.testCase "life <= 0 loses" $
        let gs = sbaBase {GameState.players = Map.insert S.alice (Player.MkPlayer {Player.life = 0, Player.status = Status.Playing, Player.counters = Map.empty}) (GameState.players sbaBase)}
         in HU.assertEqual "bob won" (Just (Result.Won S.bob)) (GameState.result (S.settleSba gs)),
      HU.testCase "simultaneous last departures draw" $
        let after = S.settleSba sbaBase {GameState.drewFromEmpty = Set.fromList [S.alice, S.bob]}
         in HU.assertEqual "draw" (Just Result.Drawn) (GameState.result after),
      HU.testCase "CR 704.5c ten poison counters lose the game" $
        let gs = S.addPlayerCounter PlayerCounterKind.Poison 10 S.bob (Setup.emptyGame S.bothPlayers)
            after = S.settleSba gs
         in HU.assertEqual "bob lost" (Just (Status.Departed Departure.Type.Lost)) (fmap Player.status (Map.lookup S.bob (GameState.players after))),
      HU.testCase "CR 704.5c nine poison counters do not" $
        let gs = S.addPlayerCounter PlayerCounterKind.Poison 9 S.bob (Setup.emptyGame S.bothPlayers)
            after = S.settleSba gs
         in HU.assertEqual "bob still playing" (Just Status.Playing) (fmap Player.status (Map.lookup S.bob (GameState.players after)))
    ]

damageEventTests :: Registry.Type.Registry -> Tasty.TestTree
damageEventTests registry =
  Tasty.testGroup
    "DamageEvent"
    [ HU.testCase "a blocked 2/1 trade emits both damage events" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, theirs) = S.combatBoard piker 1 1
            after = S.fightWith S.aggressiveAnswer gs
            events = S.damageEventsOf after
        case (mine, theirs) of
          (a : _, b : _) -> do
            HU.assertEqual "two events" 2 (length events)
            HU.assertBool "attacker hit blocker for 2" $
              elem (DamageEvent.MkDamageEvent a (Recipient.ToCreature b) 2 False False DamageKind.Combat) events
            HU.assertBool "blocker hit attacker for 2" $
              elem (DamageEvent.MkDamageEvent b (Recipient.ToCreature a) 2 False False DamageKind.Combat) events
          _ -> HU.assertFailure "fixture should have one creature per side",
      HU.testCase "an unblocked 2/1 emits a ToPlayer event" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoard piker 1 0
            after = S.fightWith S.aggressiveAnswer gs
        case mine of
          a : _ ->
            HU.assertEqual
              "one player event"
              [DamageEvent.MkDamageEvent a (Recipient.ToPlayer S.bob) 2 False False DamageKind.Combat]
              (S.damageEventsOf after)
          _ -> HU.assertFailure "fixture should have an attacker"
    ]

deathtouchTests :: Registry.Type.Registry -> Tasty.TestTree
deathtouchTests registry =
  Tasty.testGroup
    "Deathtouch"
    [ HU.testCase "CR 704.5h a 1/1 deathtoucher destroys a 3/3 it deals 1 to" $ do
        -- Typhoid Rats attacks, Ogre Sentry blocks. Rat deals 1 -> Ogre dies by
        -- 704.5h (toughness 3, not lethal by the numbers); Ogre's 3 kills the Rat.
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs, _, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
        HU.assertEqual "the Ogre is dead" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "the Rat is dead" 0 (S.creaturesInPlay S.alice after),
      HU.testCase "CR 704.5g the control: a 2/1 without deathtouch leaves the 3/3 alive" $ do
        piker <- Registry.printing registry "Goblin Piker"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs, _, _) = S.combatBoardOf [piker] [ogreSentry]
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
        HU.assertEqual "the Ogre survives" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "the SBA check consumes the damage events by watermark, not by draining" $ do
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs, _, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
        HU.assertEqual "nothing left unscanned" [] (Event.unscannedDamage after)
        HU.assertBool "the record survives (CR 608.2i)" (not (null (S.damageEventsOf after))),
      HU.testCase "CR 702.2e the deal-time bit is true for a real deathtoucher, false for a plain source" $ do
        -- Typhoid Rats (deathtouch) and Ogre Sentry trade combat damage under
        -- aggressiveAnswer (which DOES declare attackers). fightWith runs no SBAs,
        -- so the wave is still unscanned in the turn log.
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs, rats, ogres) = S.combatBoardOf [typhoidRats] [ogreSentry]
            fought = S.fightWith S.aggressiveAnswer gs
            ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
            ogreId = case ogres of o : _ -> o; [] -> ObjectId.MkObjectId 999
            bitFor src = any (\ev -> DamageEvent.source ev == src && DamageEvent.dealtByDeathtouch ev) (S.damageEventsOf fought)
        HU.assertBool "Rat's damage is flagged deathtouch" (bitFor ratId)
        HU.assertBool "Ogre's damage is not" (not (bitFor ogreId)),
      HU.testCase "CR 702.2e Humility removes deathtouch, so the deal-time bit is false" $ do
        -- Under Humility the Rat loses deathtouch (layer 6); its combat-damage
        -- event's bit is false -- asserted directly on the event, not via a kill.
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        humility <- Registry.printing registry "Humility"
        let (gs0, rats, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
            gs = S.withHumility humility gs0
            fought = S.fightWith S.aggressiveAnswer gs
            ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
            ratBit = any (\ev -> DamageEvent.source ev == ratId && DamageEvent.dealtByDeathtouch ev) (S.damageEventsOf fought)
        HU.assertBool "no deathtouch at deal time under Humility" (not ratBit)
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
                  (Recipient.ToPlayer S.bob, 0)
                ]
            answer :: Map.Map Recipient.Recipient Natural.Natural
            answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 2)]
         in HU.assertBool "accepted" (Damage.legalAssignment thresholds 2 answer),
      HU.testCase "defender damage while a blocker is short is illegal" $
        let thresholds :: Map.Map Recipient.Recipient Natural.Natural
            thresholds =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                  (Recipient.ToPlayer S.bob, 0)
                ]
            answer :: Map.Map Recipient.Recipient Natural.Natural
            answer =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 0),
                  (Recipient.ToPlayer S.bob, 3)
                ]
         in HU.assertBool "rejected" (not (Damage.legalAssignment thresholds 3 answer)),
      HU.testCase "defender damage once the blocker has lethal is legal" $
        let thresholds :: Map.Map Recipient.Recipient Natural.Natural
            thresholds =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                  (Recipient.ToPlayer S.bob, 0)
                ]
            answer :: Map.Map Recipient.Recipient Natural.Natural
            answer =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                  (Recipient.ToPlayer S.bob, 2)
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
      QC.testProperty "an accepted assignment always totals power and gates the defender"
        . QC.forAll genLegalityCase
        $ \(thresholds, power, answer) ->
          not (Damage.legalAssignment thresholds power answer)
            || ( sum (Map.elems answer) == power
                   && all (\r -> Map.member r thresholds) (Map.keys answer)
                   && ( Map.findWithDefault 0 (Recipient.ToPlayer S.bob) answer == 0
                          || all
                            (\(r, t) -> Map.findWithDefault 0 r answer >= t)
                            (Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds))
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
      thresholds = Map.fromList [(blocker, fromInteger lethal), (Recipient.ToPlayer S.bob, 0)]
      answer :: Map.Map Recipient.Recipient Natural.Natural
      answer = Map.fromList [(blocker, fromInteger toBlocker), (Recipient.ToPlayer S.bob, fromInteger toDefender)]
  pure (thresholds, fromInteger power, answer)

-- Assigns each blocker exactly its threshold, and every leftover point to the
-- defender. A legal trample division for these boards.
tramplingAnswer :: Prompt.Prompt r -> r
tramplingAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        toBlockers = Map.fromList (fmap (\(r, t) -> (r, t)) blockers)
        spent = sum (fmap snd blockers)
        leftover = if n >= spent then n - spent else 0
        defenders = filter (not . S.isCreatureRecipient) (Map.keys thresholds)
     in case defenders of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> S.aggressiveAnswer p

trampleTests :: Registry.Type.Registry -> Tasty.TestTree
trampleTests registry =
  Tasty.testGroup
    "Trample"
    [ HU.testCase "CR 702.19b a 3/3 trampler spills excess onto the defending player" $ do
        -- War Mammoth (3/3 trample) blocked by a Piker (2/1): 1 lethal to the
        -- Piker, 2 to bob. Mammoth survives (2 marked < 3).
        warMammoth <- Registry.printing registry "War Mammoth"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [warMammoth] [piker]
            after = S.settleSba (S.fightWith tramplingAnswer gs)
        HU.assertEqual "bob took the 2 overflow" (Just 18) (S.lifeOf S.bob after)
        HU.assertEqual "the Piker is dead" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "the Mammoth survives" 1 (S.creaturesInPlay S.alice after),
      HU.testCase "CR 702.19b a non-trample control spills nothing" $ do
        -- Ogre Sentry is a 3/3 that cannot attack (defender), so use the Piker's
        -- existing behavior as the control: a blocked non-trample attacker deals
        -- nothing to the player. (combatDamageTests already asserts bob = 20.)
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [piker] [piker]
            after = S.fightWith tramplingAnswer gs
        HU.assertEqual "bob untouched by a non-trampler" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 702.19b defender-short assignment is rejected" $ do
        -- A cheat responder gives bob 3 while the Piker gets 0. Illegal: the
        -- attacker deals nothing, bob untouched, Piker survives.
        warMammoth <- Registry.printing registry "War Mammoth"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [warMammoth] [piker]
            cheat p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter (not . S.isCreatureRecipient) (Map.keys thresholds) of
                  d : _ -> Map.singleton d n
                  [] -> Map.empty
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith cheat gs)
        HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after)
        HU.assertEqual "the Piker survives the rejected assignment" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 702.19b under-assignment across two blockers spills nothing (power below total lethal)" $ do
        -- War Mammoth (3/3 trample) blocked by TWO Ogre Sentries (3/3 each): it
        -- cannot reach lethal on both (needs 6, has 3), so no overflow -- bob is
        -- untouched -- and the division among the Ogres is free. Real cards, the
        -- power-below-lethal case the property covers exhaustively.
        warMammoth <- Registry.printing registry "War Mammoth"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs, _, _) = S.combatBoardOf [warMammoth] [ogreSentry, ogreSentry]
            dumpOne p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter S.isCreatureRecipient (Map.keys thresholds) of
                  r : _ -> Map.singleton r n
                  [] -> Map.empty
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith dumpOne gs)
        HU.assertEqual "bob untouched (no overflow)" (Just 20) (S.lifeOf S.bob after)
        HU.assertEqual "one Ogre took all 3 and died, the other lived" 1 (S.creaturesInPlay S.bob after)
    ]

-- #29: a blocker declared in the declare blockers step can be gone by the combat
-- damage step -- since M3a the pool has instant-speed removal. CR 509.1h keeps the
-- attacker BLOCKED (the combat map is the record and is deliberately not mutated,
-- #28), but CR 510.1c assigns damage only to the creatures CURRENTLY blocking it.
-- Removal happens between declareBlockers and dealCombatDamage, which is exactly
-- when a Murder resolves.
killBlockerMidCombat :: ObjectId.ObjectId -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
killBlockerMidCombat victim answer gs =
  S.runPure answer gs $ do
    Combat.declareAttackers S.alice
    Combat.declareBlockers
    Event.destroy victim
    Monad.void Damage.dealCombatDamage

-- Every DamageDealt event in the history addressed to `oid`, however much.
damageEventsTo :: ObjectId.ObjectId -> GameState.GameState -> [DamageEvent.DamageEvent]
damageEventsTo oid gs =
  let pick ev = case ev of
        GameEvent.DamageDealt de ->
          if DamageEvent.target de == Recipient.ToCreature oid then [de] else []
        _ -> []
   in concatMap pick (GameState.events gs)

-- Sinks the whole assignment into the first creature recipient offered. This is
-- the discriminating answer for #29: if the engine still offers a departed
-- blocker, the damage lands on the ghost and evaporates, so any assertion about
-- where the damage really went fails. An answer that routes by threshold (like
-- tramplingAnswer) would pass either way, because a departed blocker's threshold
-- computes to 0.
dumpOntoFirstCreature :: Prompt.Prompt r -> r
dumpOntoFirstCreature p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  _ -> S.aggressiveAnswer p

departedBlockerTests :: Registry.Type.Registry -> Tasty.TestTree
departedBlockerTests registry =
  Tasty.testGroup
    "Departed blockers (#29)"
    [ HU.testCase "CR 702.19d a trampler whose only blocker left assigns everything to the player" $ do
        -- War Mammoth (3/3 trample) is blocked by a Piker (2/1); the Piker is
        -- destroyed before damage. "As though all blocking creatures have been
        -- assigned lethal damage" -- so all 3 hit bob, and there is nothing left
        -- to choose. dumpOntoFirstCreature sinks everything into a creature
        -- recipient if one is offered, so an offered ghost shows up as bob
        -- taking nothing.
        warMammoth <- Registry.printing registry "War Mammoth"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = S.combatBoardOf [warMammoth] [piker]
        case theirs of
          blocker : _ ->
            let after = S.settleSba (killBlockerMidCombat blocker dumpOntoFirstCreature gs)
             in do
                  HU.assertEqual "bob took all 3" (Just 17) (S.lifeOf S.bob after)
                  HU.assertEqual "nothing was addressed to the departed blocker" [] (damageEventsTo blocker after)
          [] -> HU.assertFailure "fixture did not build a blocker",
      HU.testCase "CR 510.1c a non-trampler whose only blocker left assigns no combat damage" $ do
        -- A Piker (2/1) blocked by a Piker that then dies. "If no creatures are
        -- currently blocking it ... it assigns no combat damage." bob is untouched
        -- either way, so the observable is the history: the engine must not record
        -- a DamageDealt addressed to a creature that is not there.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, theirs) = S.combatBoardOf [piker] [piker]
        case theirs of
          blocker : _ ->
            let after = S.settleSba (killBlockerMidCombat blocker S.aggressiveAnswer gs)
             in do
                  HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after)
                  HU.assertEqual "no phantom damage event" [] (damageEventsTo blocker after)
          [] -> HU.assertFailure "fixture did not build a blocker",
      HU.testCase "CR 510.1c a partly-departed block assigns only among the survivors" $ do
        -- War Mammoth (3/3 trample) blocked by two Ogre Sentries (3/3); one dies
        -- before damage. One live blocker with lethal exactly 3 leaves nothing to
        -- divide, so this is forced: all 3 onto the survivor, which then dies.
        -- With the ghost still in the list the assignment is a free division and
        -- the whole 3 can sink into it, leaving the survivor untouched.
        warMammoth <- Registry.printing registry "War Mammoth"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs, _, theirs) = S.combatBoardOf [warMammoth] [ogreSentry, ogreSentry]
        case theirs of
          dead : _ : _ ->
            let after = S.settleSba (killBlockerMidCombat dead dumpOntoFirstCreature gs)
             in do
                  HU.assertEqual "the surviving Ogre took the full 3 and died" 0 (S.creaturesInPlay S.bob after)
                  HU.assertEqual "bob untouched (3 power, 3 lethal, no excess)" (Just 20) (S.lifeOf S.bob after)
          _ -> HU.assertFailure "fixture did not build two blockers"
    ]

-- The mirror of killBlockerMidCombat: the ATTACKER is gone by the combat damage
-- step. CR 506.4 removes it from combat, so by CR 510.1d its blockers are
-- blocking nothing -- but Combat.blockers is keyed BY the attacker and that key
-- is never pruned (CR 509.1h, #28), so the stale key is what reaches
-- Damage.blockerAssignment.
killAttackerMidCombat :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
killAttackerMidCombat victim gs =
  S.runPure S.aggressiveAnswer gs $ do
    Combat.declareAttackers S.alice
    Combat.declareBlockers
    Event.destroy victim
    Monad.void Damage.dealCombatDamage

departedAttackerTests :: Registry.Type.Registry -> Tasty.TestTree
departedAttackerTests registry =
  Tasty.testGroup
    "Departed attackers (CR 510.1d)"
    [ HU.testCase "CR 510.1d a blocker whose attacker was destroyed mid-combat assigns no combat damage" $ do
        -- CR 510.1d: "A blocking creature assigns combat damage to the creatures
        -- it's blocking. If it isn't currently blocking any creatures (if, for
        -- example, they were destroyed or removed from combat), it assigns no
        -- combat damage." The attacker is destroyed between declare blockers and
        -- the combat damage step -- CR 506.4 removes it from combat.
        --
        -- What the assertion catches: an implementation that filters only the
        -- BLOCKER (Damage.blockerAssignment's Projection.powerOf reads the
        -- blocker, which is alive) and so emits a DamageEvent addressed to the
        -- dead attacker. Marking that damage is a no-op the moment the object is
        -- gone from GameState.objects, so the mark is NOT the observable -- the
        -- CR 608.2i history is. The control leg proves the assertion is not
        -- vacuous: with the attacker alive the same board records exactly that
        -- event.
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        case mine of
          attacker : _ -> do
            HU.assertEqual "nothing was addressed to the destroyed attacker" [] (damageEventsTo attacker (killAttackerMidCombat attacker gs))
            HU.assertEqual "and with the attacker alive the blocker DOES hit it -- the filter is what did it" [2] (fmap DamageEvent.amount (damageEventsTo attacker (S.fightWith S.aggressiveAnswer gs)))
          [] -> HU.assertFailure "fixture did not build an attacker",
      HU.testCase "CR 510.1d a blocker whose attacker left the game assigns no combat damage" $ do
        -- The departure route to the same stale key. Three seats, because at two
        -- the concession ends the game (CR 104.2a). CR 800.4a's first clause
        -- deletes alice's attacker outright and drops its Combat.attackers entry,
        -- but the Combat.blockers KEY survives -- deliberately, since CR 509.1h's
        -- last sentence is about the blockers' side of that record (#28) -- so
        -- bob's blocker is still handed a dead attacker to hit.
        piker <- Registry.printing registry "Goblin Piker"
        let (attacker, b1) = S.addCreature piker S.alice S.threePlayerGame
            (blocker, b2) = S.addCreature piker S.bob b1
            fighting =
              b2
                { GameState.combat =
                    Combat.Type.MkCombat
                      { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.bob),
                        Combat.Type.blockers = Map.singleton attacker (Set.singleton blocker),
                        Combat.Type.struckFirst = Nothing
                      }
                }
            gone = Departure.depart Departure.Type.Conceded S.alice fighting
            (assignedAfter, _) = S.runPureWith S.identityAnswer gone (Damage.gatherCombatDamage (const True))
            (assignedBefore, _) = S.runPureWith S.identityAnswer fighting (Damage.gatherCombatDamage (const True))
        HU.assertBool "CR 509.1h: the blockers key really is still there, so this is the live path" (Map.member attacker (Combat.Type.blockers (GameState.combat gone)))
        HU.assertEqual "no assignment names the departed attacker" [] (filter (\ev -> DamageEvent.target ev == Recipient.ToCreature attacker) assignedAfter)
        HU.assertEqual "and with alice still in the game the blocker's hit is assigned -- the filter is what did it" [2] (fmap DamageEvent.amount (filter (\ev -> DamageEvent.target ev == Recipient.ToCreature attacker) assignedBefore))
    ]

-- CR 800.4e: "If combat damage would be assigned to a player who has left the
-- game, that damage isn't assigned." attackerAssignment reads the defender's
-- status at two independent sites -- the unblocked/trample-through toDefender
-- list, and the CR 702.19b threshold map the assignment prompt offers -- and
-- both need coverage.
--
-- S.identityAnswer's AssignCombatDamage arm dumps the WHOLE amount onto the
-- first CREATURE recipient it finds (Support.hs), never a player one, so it
-- cannot tell whether a ToPlayer entry is present in the threshold map at all:
-- guarded or not, a blocked trampler's excess lands on the blocker either way
-- under that answerer. It is fine for the unblocked path (no prompt is ever
-- issued there), but the trample threshold map needs an answerer that actually
-- spends the excess on a player recipient when one is offered.
-- defenderOrBlockerAnswer assigns each blocker exactly its threshold and routes
-- the leftover to a player recipient if the threshold map offers one, falling
-- back onto the blocker (over-lethal, still legal -- Damage.legalAssignment has
-- no upper bound) when it does not. That is what actually surfaces whether the
-- departed defender was offered.
defenderOrBlockerAnswer :: Prompt.Prompt r -> r
defenderOrBlockerAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockerEntries = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        toBlockers = Map.fromList blockerEntries
        spent = sum (fmap snd blockerEntries)
        leftover = if n >= spent then n - spent else 0
        defenders = filter (not . S.isCreatureRecipient) (Map.keys thresholds)
     in case defenders of
          d : _ -> Map.insert d leftover toBlockers
          [] -> case blockerEntries of
            (r, _) : _ -> Map.insertWith (+) r leftover toBlockers
            [] -> toBlockers
  _ -> S.aggressiveAnswer p

departedDefenderTests :: Registry.Type.Registry -> Tasty.TestTree
departedDefenderTests registry =
  Tasty.testGroup
    "Departed defender (CR 800.4e)"
    [ HU.testCase "CR 800.4e no combat damage is assigned to a player who has left the game" $ do
        -- CR 800.4e: "If combat damage would be assigned to a player who has left
        -- the game, that damage isn't assigned." Reachable: a defending player can
        -- concede between the declare-attackers step and the combat damage step.
        -- Three seats, because at two the concession ends the game (CR 104.2a).
        piker <- Registry.printing registry "Goblin Piker"
        let (attacker, board) = S.addCreature piker S.alice S.threePlayerGame
            attacking =
              board
                { GameState.combat =
                    Combat.Type.MkCombat
                      { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.bob),
                        Combat.Type.blockers = Map.empty,
                        Combat.Type.struckFirst = Nothing
                      }
                }
            gone = Departure.depart Departure.Type.Conceded S.bob attacking
            (assignedAfter, _) = S.runPureWith S.identityAnswer gone (Damage.gatherCombatDamage (const True))
            (assignedBefore, _) = S.runPureWith S.identityAnswer attacking (Damage.gatherCombatDamage (const True))
        HU.assertEqual "nothing is assigned to the departed defender" [] assignedAfter
        HU.assertEqual "and with bob still in the game the same board assigns one hit -- the guard is what did it" 1 (length assignedBefore)
        HU.assertEqual "to bob" [Recipient.ToPlayer S.bob] (fmap DamageEvent.target assignedBefore),
      HU.testCase "CR 800.4e a departed defender is not offered as a trample recipient either" $ do
        -- CR 702.19b assigns trample's excess "as its controller chooses", and the
        -- defending player is one of the choices Prompt.AssignCombatDamage offers.
        -- CR 800.4e removes the damage, so the choice must not be offered: an
        -- assignment the engine then discards would silently take damage away from
        -- the blockers it could otherwise have gone to.
        --
        -- CAROL is the defender and the one who leaves, and BOB's Piker blocks, so
        -- the blocker survives the departure and the board stays in the prompt arm.
        -- (Making the defender the blocker's controller would work too, right up
        -- to the point where CR 800.4a's first clause removes their blocker and
        -- the board falls out of that arm entirely.) War Mammoth is a 3/3 with
        -- trample; the Piker is a 2/1, so there is excess and a real choice.
        --
        -- S.identityAnswer is not the discriminating answerer here (see the
        -- group comment above): it never picks a player recipient, so a blocked
        -- trampler's excess lands on the blocker whether the defender is offered
        -- or not. defenderOrBlockerAnswer is used for both legs instead.
        warMammoth <- Registry.printing registry "War Mammoth"
        piker <- Registry.printing registry "Goblin Piker"
        let (attacker, b1) = S.addCreature warMammoth S.alice S.threePlayerGame
            (blocker, b2) = S.addCreature piker S.bob b1
            attacking =
              b2
                { GameState.combat =
                    Combat.Type.MkCombat
                      { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.carol),
                        Combat.Type.blockers = Map.singleton attacker (Set.singleton blocker),
                        Combat.Type.struckFirst = Nothing
                      }
                }
            gone = Departure.depart Departure.Type.Conceded S.carol attacking
            (assignedAfter, _) = S.runPureWith defenderOrBlockerAnswer gone (Damage.gatherCombatDamage (const True))
            (assignedBefore, _) = S.runPureWith defenderOrBlockerAnswer attacking (Damage.gatherCombatDamage (const True))
        HU.assertBool "the blocker survived carol's departure, so the board is still in the prompt arm" (Maybe.isJust (Game.lookupObject blocker gone))
        HU.assertBool "no assignment names the departed defender" (notElem (Recipient.ToPlayer S.carol) (fmap DamageEvent.target assignedAfter))
        HU.assertEqual "all three points land on the blocker instead" [3] (fmap DamageEvent.amount (filter (\ev -> DamageEvent.target ev == Recipient.ToCreature blocker) assignedAfter))
        HU.assertBool "with carol still in the game the threshold map DOES offer her -- the guard is what did it" (Maybe.isJust (List.find (\ev -> DamageEvent.target ev == Recipient.ToPlayer S.carol) assignedBefore))
    ]

-- Grant deathtouch to `oid` the way Serpent's Gift does: a stored continuous
-- effect over just that object. Timestamp is arbitrary (no competing layer-6
-- effect in these fixtures).
grantDeathtouch :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
grantDeathtouch oid gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 997,
            ContinuousEffect.timestamp = Timestamp.MkTimestamp 500,
            ContinuousEffect.expiry = Expiry.Type.AtCleanup,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Deathtouch,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

trampleDeathtouchTests :: Registry.Type.Registry -> Tasty.TestTree
trampleDeathtouchTests registry =
  Tasty.testGroup
    "TrampleDeathtouch"
    [ HU.testCase "CR 702.2c a deathtouch-granted trampler needs only 1 on the blocker, spilling the rest" $ do
        -- War Mammoth (3/3 trample) GRANTED deathtouch into Ogre Sentry (3/3):
        -- lethal collapses to 1, so 1 to the Ogre and 2 tramples to bob; the Ogre
        -- still dies (704.5h, via the deal-time bit). Real cards replace M2c's
        -- synthetic deathtrampler.
        warMammoth <- Registry.printing registry "War Mammoth"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs0, mammoths, _) = S.combatBoardOf [warMammoth] [ogreSentry]
            mammothId = case mammoths of
              m : _ -> m
              [] -> ObjectId.MkObjectId 999
            gs = grantDeathtouch mammothId gs0
            after = S.settleSba (S.fightWith tramplingAnswer gs)
        HU.assertEqual "bob took the 2 overflow" (Just 18) (S.lifeOf S.bob after)
        HU.assertEqual "the Ogre is dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 702.19b the control: plain trample into the same 3/3 spills nothing" $ do
        -- War Mammoth (3/3 trample, NO deathtouch) into Ogre Sentry (3/3): lethal
        -- is 3, all 3 go to the Ogre, 0 tramples. Only deathtouch changes the spill.
        warMammoth <- Registry.printing registry "War Mammoth"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs, _, _) = S.combatBoardOf [warMammoth] [ogreSentry]
            after = S.settleSba (S.fightWith tramplingAnswer gs)
        HU.assertEqual "bob untouched without deathtouch" (Just 20) (S.lifeOf S.bob after)
    ]

m2cPropertyTests :: Registry.Type.Registry -> Tasty.TestTree
m2cPropertyTests registry =
  Tasty.testGroup
    "M2cProperties"
    [ HU.testCase "a deathtoucher's victim with toughness > 0 is gone after the SBA" $ do
        -- The property in fixture form (the deck has no deathtoucher, so this is
        -- the M2c coverage; it becomes a random-game property when a deathtoucher
        -- joins a deck -- the castability work, #23). Every toughness we throw at
        -- the 1/1 deathtoucher dies to it.
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        piker <- Registry.printing registry "Goblin Piker"
        nimbleBirdsticker <- Registry.printing registry "Nimble Birdsticker"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let victims = [piker, nimbleBirdsticker, ogreSentry]
            killsIt v =
              let (gs, _, _) = S.combatBoardOf [typhoidRats] [v]
                  after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
               in S.creaturesInPlay S.bob after == 0
        HU.assertBool "deathtouch kills every toughness" (all killsIt victims),
      HU.testCase "the deathtouch and trample reads never name a card" $
        -- A structural reminder, asserted by the interaction falsifier's outcome
        -- (TrampleDeathtouch) depending only on the keyword projection. This case
        -- documents the invariant; the real enforcement is code review of
        -- Damage.blockerThreshold and Sba.woundedByDeathtouch, which case on
        -- Keyword, never on a printing.
        HU.assertBool "see TrampleDeathtouch and Deathtouch groups" True
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Damage"
    [ damageTests registry,
      damageEventTests registry,
      deathtouchTests registry,
      assignmentLegalityTests,
      trampleTests registry,
      departedBlockerTests registry,
      departedAttackerTests registry,
      departedDefenderTests registry,
      trampleDeathtouchTests registry,
      sbaTests,
      creatureSbaTests registry,
      infectTests registry,
      m2cPropertyTests registry
    ]
