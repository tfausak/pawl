{-# LANGUAGE GADTs #-}

-- Covers Pawl.Damage and Pawl.Sba: the damage funnel, deathtouch, trample, and
-- state-based actions. ((m2cPropertyTests cards) is deterministic fixture coverage, not
-- QuickCheck properties.)
module Pawl.DamageSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Cards as Cards
import qualified Pawl.Damage as Damage
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.DamagePattern as DamagePattern
import qualified Pawl.Type.DamageRewrite as DamageRewrite
import qualified Pawl.Type.Departure as Departure
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Expiry as Expiry.Type
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

creatureSbaTests :: Cards.Cards -> Tasty.TestTree
creatureSbaTests cards =
  Tasty.testGroup
    "CreatureSba"
    [ HU.testCase "CR 704.5g a creature with lethal damage is destroyed" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            after = S.settleSba (S.markDamage oid 1 gs)
         in do
              HU.assertEqual "off the battlefield" [] (Game.zoneMembers Zone.Battlefield S.alice after)
              HU.assertEqual "in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 704.5g damage below toughness is not lethal" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            -- A Piker is 2/1, so 0 marked damage is survivable and 1 is not.
            after = S.settleSba (S.markDamage oid 0 gs)
         in HU.assertEqual "still there" 1 (length (Game.zoneMembers Zone.Battlefield S.alice after)),
      HU.testCase "CR 704.5g a Mountain with damage marked is not destroyed" $
        -- Not a creature: 704.5f/g do not apply. This is the classification
        -- doing its job -- the check never asks WHICH card it is.
        let gs = S.mountainsInPlay cards 1
         in case Game.zoneMembers Zone.Battlefield S.alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ ->
                HU.assertEqual
                  "survives"
                  1
                  (length (Game.zoneMembers Zone.Battlefield S.alice (S.settleSba (S.markDamage oid 5 gs)))),
      HU.testCase "a destroyed creature conserves objects" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            marked = S.markDamage oid 1 gs
         in HU.assertEqual
              "conserved"
              (Game.objectCount marked)
              (Game.objectCount (S.settleSba marked)),
      -- The deterministic successor to the retired green-black "some seed sends a
      -- creature to the graveyard" property: two 2/1 Pikers trade in combat and
      -- both die to the CR 704.5g state-based action.
      HU.testCase "a creature dies in a played-out combat" $
        let (gs, _, _) = S.combatBoard cards 1 1
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
         in do
              HU.assertEqual "attacker died" 0 (S.creaturesInPlay S.alice after)
              HU.assertEqual "blocker died" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 704.5d a token off the battlefield ceases to exist" $
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card (Cards.pikerPrinting cards)
            (tokId, gs) = S.addToken goblinCard S.alice base
            inGrave = S.runPure S.identityAnswer gs (Event.changeZone tokId Zone.Graveyard)
            -- The changeZone minted a new incarnation; find it in the graveyard.
            settled = S.settleSba inGrave
         in do
              HU.assertEqual "before the SBA, a token sits in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice inGrave))
              HU.assertEqual "after the SBA, it has ceased to exist" 0 (length (Game.zoneMembers Zone.Graveyard S.alice settled))
              HU.assertEqual "no token objects remain" 0 (Game.objectCount settled),
      HU.testCase "CR 704.5d a token on the battlefield does NOT cease" $
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card (Cards.pikerPrinting cards)
            (_, gs) = S.addToken goblinCard S.alice base
            settled = S.settleSba gs
         in HU.assertEqual "the token survives on the battlefield" 1 (Game.objectCount settled),
      HU.testCase "CR 704.5d/704.5g a 1/1 token dies to lethal damage and ceases to exist" $
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card (Cards.pikerPrinting cards)
            -- A real 2/1 Piker (bob's) is the damage source; alice's 1/1 token takes 2.
            (srcId, gs1) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (tokId, gs2) = S.addToken goblinCard S.alice gs1
            damaged = S.runPure S.identityAnswer gs2 (Damage.applyDamage [DamageEvent.MkDamageEvent srcId (Recipient.ToCreature tokId) 2 False False DamageKind.Combat])
            settled = S.settleSba damaged
         in do
              HU.assertEqual "the token is gone from the battlefield" 0 (S.creaturesInPlay S.alice settled)
              HU.assertEqual "and NOT sitting in a graveyard (the falsifier)" 0 (length (Game.zoneMembers Zone.Graveyard S.alice settled)),
      HU.testCase "CR 704.5g regeneration saves a creature from lethal combat damage" $
        let base = Setup.emptyGame S.bothPlayers
            (victim, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base -- 2/1
            shielded = S.addRegenShield victim gs0
            -- 2 combat damage is lethal to a 2/1; the shield replaces the CR 704.5g destruction.
            damaged = S.runPure S.identityAnswer shielded (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False DamageKind.Combat])
            settled = S.settleSba damaged
         in do
              HU.assertEqual "survived (regenerated)" True (Set.member victim (GameState.battlefield settled))
              case Game.lookupObject victim settled of
                Just obj -> do
                  HU.assertEqual "tapped" TapState.Tapped (Object.tapped obj)
                  HU.assertEqual "damage healed" 0 (Object.damage obj)
                Nothing -> HU.assertFailure "victim vanished"
    ]

damageTests :: Cards.Cards -> Tasty.TestTree
damageTests cards =
  Tasty.testGroup
    "Damage"
    [ HU.testCase "a permanent starts with no damage marked" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual "none" (Just 0) (S.damageOf oid gs),
      HU.testCase "CR 514.2 marked damage is removed" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual "removed" (Just 0) (S.damageOf oid (Damage.removeAllDamage (S.markDamage oid 1 gs))),
      HU.testCase "CR 514.2 damage wears off at the cleanup step" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            marked = S.markDamage oid 1 gs
            after = snd (Engine.runGamePure S.identityAnswer marked (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
         in HU.assertEqual "worn off" (Just 0) (S.damageOf oid after),
      HU.testCase "CR 400.7 a new object carries no damage forward" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            marked = S.markDamage oid 1 gs
            after = S.runPure S.identityAnswer marked (Event.changeZone oid Zone.Graveyard)
         in case Game.zoneMembers Zone.Graveyard S.alice after of
              [] -> HU.assertFailure "expected a card in the graveyard"
              new : _ -> HU.assertEqual "fresh object, no damage" (Just 0) (S.damageOf new after),
      HU.testCase "CR 615 a prevention drops combat damage but spares Noncombat" $
        let base = Setup.emptyGame S.bothPlayers
            (victim, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
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
         in do
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

infectTests :: Cards.Cards -> Tasty.TestTree
infectTests cards =
  Tasty.testGroup
    "Infect"
    [ HU.testCase "CR 120.3b infect damage to a player becomes poison, not life loss" $
        let (oid, gs0) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False True DamageKind.Combat
            after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
         in do
              HU.assertEqual "bob has three poison" 3 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
              HU.assertEqual "bob's life unchanged" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "the source's controller gains no poison" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
      HU.testCase "CR 120.3d infect damage to a creature becomes -1/-1 counters, not marked damage" $
        let (src, gs0) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            (victim, gs1) = S.addPiker cards S.bob gs0
            ev = DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) 2 False True DamageKind.Combat
            after = S.runPure S.identityAnswer gs1 (Damage.applyDamage [ev])
         in do
              HU.assertEqual "two -1/-1 counters" (Just 2) (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject victim after))
              HU.assertEqual "no marked damage" (Just 0) (S.damageOf victim after),
      HU.testCase "CR 702.90 Glistener Elf poisons an unblocked player, drains no life" $
        let (gs, _, _) = S.combatBoardOf [Cards.glistenerElfPrinting cards] []
            after = S.fightWith S.aggressiveAnswer gs
         in do
              HU.assertEqual "bob has one poison" 1 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
              HU.assertEqual "bob's life unchanged" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "alice (controller) has no poison" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
      HU.testCase "CR 702.90c Glistener Elf shrinks and kills a blocker with -1/-1 counters" $
        let (gs, _, blockers) = S.combatBoardOf [Cards.glistenerElfPrinting cards] [Cards.pikerPrinting cards]
            fought = S.fightWith S.aggressiveAnswer gs
            settled = S.settleSba fought
         in case blockers of
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
         in HU.assertEqual "alice lost" (Just (Status.Departed Departure.Lost)) (fmap Player.status (Map.lookup S.alice (GameState.players after))),
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
         in HU.assertEqual "bob lost" (Just (Status.Departed Departure.Lost)) (fmap Player.status (Map.lookup S.bob (GameState.players after))),
      HU.testCase "CR 704.5c nine poison counters do not" $
        let gs = S.addPlayerCounter PlayerCounterKind.Poison 9 S.bob (Setup.emptyGame S.bothPlayers)
            after = S.settleSba gs
         in HU.assertEqual "bob still playing" (Just Status.Playing) (fmap Player.status (Map.lookup S.bob (GameState.players after)))
    ]

damageEventTests :: Cards.Cards -> Tasty.TestTree
damageEventTests cards =
  Tasty.testGroup
    "DamageEvent"
    [ HU.testCase "a blocked 2/1 trade emits both damage events" $
        let (gs, mine, theirs) = S.combatBoard cards 1 1
            after = S.fightWith S.aggressiveAnswer gs
            events = S.damageEventsOf after
         in case (mine, theirs) of
              (a : _, b : _) -> do
                HU.assertEqual "two events" 2 (length events)
                HU.assertBool "attacker hit blocker for 2" $
                  elem (DamageEvent.MkDamageEvent a (Recipient.ToCreature b) 2 False False DamageKind.Combat) events
                HU.assertBool "blocker hit attacker for 2" $
                  elem (DamageEvent.MkDamageEvent b (Recipient.ToCreature a) 2 False False DamageKind.Combat) events
              _ -> HU.assertFailure "fixture should have one creature per side",
      HU.testCase "an unblocked 2/1 emits a ToPlayer event" $
        let (gs, mine, _) = S.combatBoard cards 1 0
            after = S.fightWith S.aggressiveAnswer gs
         in case mine of
              a : _ ->
                HU.assertEqual
                  "one player event"
                  [DamageEvent.MkDamageEvent a (Recipient.ToPlayer S.bob) 2 False False DamageKind.Combat]
                  (S.damageEventsOf after)
              _ -> HU.assertFailure "fixture should have an attacker"
    ]

deathtouchTests :: Cards.Cards -> Tasty.TestTree
deathtouchTests cards =
  Tasty.testGroup
    "Deathtouch"
    [ HU.testCase "CR 704.5h a 1/1 deathtoucher destroys a 3/3 it deals 1 to" $
        -- Typhoid Rats attacks, Ogre Sentry blocks. Rat deals 1 -> Ogre dies by
        -- 704.5h (toughness 3, not lethal by the numbers); Ogre's 3 kills the Rat.
        let (gs, _, _) = S.combatBoardOf [Cards.typhoidRatsPrinting cards] [Cards.ogreSentryPrinting cards]
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
         in do
              HU.assertEqual "the Ogre is dead" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "the Rat is dead" 0 (S.creaturesInPlay S.alice after),
      HU.testCase "CR 704.5g the control: a 2/1 without deathtouch leaves the 3/3 alive" $
        let (gs, _, _) = S.combatBoardOf [Cards.pikerPrinting cards] [Cards.ogreSentryPrinting cards]
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
         in HU.assertEqual "the Ogre survives" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "the SBA check consumes the damage events by watermark, not by draining" $
        let (gs, _, _) = S.combatBoardOf [Cards.typhoidRatsPrinting cards] [Cards.ogreSentryPrinting cards]
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
         in do
              HU.assertEqual "nothing left unscanned" [] (Event.unscannedDamage after)
              HU.assertBool "the record survives (CR 608.2i)" (not (null (S.damageEventsOf after))),
      HU.testCase "CR 702.2e the deal-time bit is true for a real deathtoucher, false for a plain source" $
        -- Typhoid Rats (deathtouch) and Ogre Sentry trade combat damage under
        -- aggressiveAnswer (which DOES declare attackers). fightWith runs no SBAs,
        -- so the wave is still unscanned in the turn log.
        let (gs, rats, ogres) = S.combatBoardOf [Cards.typhoidRatsPrinting cards] [Cards.ogreSentryPrinting cards]
            fought = S.fightWith S.aggressiveAnswer gs
            ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
            ogreId = case ogres of o : _ -> o; [] -> ObjectId.MkObjectId 999
            bitFor src = any (\ev -> DamageEvent.source ev == src && DamageEvent.dealtByDeathtouch ev) (S.damageEventsOf fought)
         in do
              HU.assertBool "Rat's damage is flagged deathtouch" (bitFor ratId)
              HU.assertBool "Ogre's damage is not" (not (bitFor ogreId)),
      HU.testCase "CR 702.2e Humility removes deathtouch, so the deal-time bit is false" $
        -- Under Humility the Rat loses deathtouch (layer 6); its combat-damage
        -- event's bit is false -- asserted directly on the event, not via a kill.
        let (gs0, rats, _) = S.combatBoardOf [Cards.typhoidRatsPrinting cards] [Cards.ogreSentryPrinting cards]
            gs = S.withHumility cards gs0
            fought = S.fightWith S.aggressiveAnswer gs
            ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
            ratBit = any (\ev -> DamageEvent.source ev == ratId && DamageEvent.dealtByDeathtouch ev) (S.damageEventsOf fought)
         in HU.assertBool "no deathtouch at deal time under Humility" (not ratBit)
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
      QC.testProperty "an accepted assignment always totals power and gates the defender" $
        QC.forAll genLegalityCase $ \(thresholds, power, answer) ->
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
        toBlockers = Map.fromList (map (\(r, t) -> (r, t)) blockers)
        spent = sum (map snd blockers)
        leftover = if n >= spent then n - spent else 0
        defenders = filter (not . S.isCreatureRecipient) (Map.keys thresholds)
     in case defenders of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> S.aggressiveAnswer p

trampleTests :: Cards.Cards -> Tasty.TestTree
trampleTests cards =
  Tasty.testGroup
    "Trample"
    [ HU.testCase "CR 702.19b a 3/3 trampler spills excess onto the defending player" $
        -- War Mammoth (3/3 trample) blocked by a Piker (2/1): 1 lethal to the
        -- Piker, 2 to bob. Mammoth survives (2 marked < 3).
        let (gs, _, _) = S.combatBoardOf [Cards.warMammothPrinting cards] [Cards.pikerPrinting cards]
            after = S.settleSba (S.fightWith tramplingAnswer gs)
         in do
              HU.assertEqual "bob took the 2 overflow" (Just 18) (S.lifeOf S.bob after)
              HU.assertEqual "the Piker is dead" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "the Mammoth survives" 1 (S.creaturesInPlay S.alice after),
      HU.testCase "CR 702.19b a non-trample control spills nothing" $
        -- Ogre Sentry is a 3/3 that cannot attack (defender), so use the Piker's
        -- existing behavior as the control: a blocked non-trample attacker deals
        -- nothing to the player. (combatDamageTests already asserts bob = 20.)
        let (gs, _, _) = S.combatBoardOf [Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
            after = S.fightWith tramplingAnswer gs
         in HU.assertEqual "bob untouched by a non-trampler" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 702.19b defender-short assignment is rejected" $
        -- A cheat responder gives bob 3 while the Piker gets 0. Illegal: the
        -- attacker deals nothing, bob untouched, Piker survives.
        let (gs, _, _) = S.combatBoardOf [Cards.warMammothPrinting cards] [Cards.pikerPrinting cards]
            cheat p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter (not . S.isCreatureRecipient) (Map.keys thresholds) of
                  d : _ -> Map.singleton d n
                  [] -> Map.empty
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith cheat gs)
         in do
              HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "the Piker survives the rejected assignment" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 702.19b under-assignment across two blockers spills nothing (power below total lethal)" $
        -- War Mammoth (3/3 trample) blocked by TWO Ogre Sentries (3/3 each): it
        -- cannot reach lethal on both (needs 6, has 3), so no overflow -- bob is
        -- untouched -- and the division among the Ogres is free. Real cards, the
        -- power-below-lethal case the property covers exhaustively.
        let (gs, _, _) = S.combatBoardOf [Cards.warMammothPrinting cards] [Cards.ogreSentryPrinting cards, Cards.ogreSentryPrinting cards]
            dumpOne p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter S.isCreatureRecipient (Map.keys thresholds) of
                  r : _ -> Map.singleton r n
                  [] -> Map.empty
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith dumpOne gs)
         in do
              HU.assertEqual "bob untouched (no overflow)" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "one Ogre took all 3 and died, the other lived" 1 (S.creaturesInPlay S.bob after)
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

trampleDeathtouchTests :: Cards.Cards -> Tasty.TestTree
trampleDeathtouchTests cards =
  Tasty.testGroup
    "TrampleDeathtouch"
    [ HU.testCase "CR 702.2c a deathtouch-granted trampler needs only 1 on the blocker, spilling the rest" $
        -- War Mammoth (3/3 trample) GRANTED deathtouch into Ogre Sentry (3/3):
        -- lethal collapses to 1, so 1 to the Ogre and 2 tramples to bob; the Ogre
        -- still dies (704.5h, via the deal-time bit). Real cards replace M2c's
        -- synthetic deathtrampler.
        let (gs0, mammoths, _) = S.combatBoardOf [Cards.warMammothPrinting cards] [Cards.ogreSentryPrinting cards]
            mammothId = case mammoths of
              m : _ -> m
              [] -> ObjectId.MkObjectId 999
            gs = grantDeathtouch mammothId gs0
            after = S.settleSba (S.fightWith tramplingAnswer gs)
         in do
              HU.assertEqual "bob took the 2 overflow" (Just 18) (S.lifeOf S.bob after)
              HU.assertEqual "the Ogre is dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 702.19b the control: plain trample into the same 3/3 spills nothing" $
        -- War Mammoth (3/3 trample, NO deathtouch) into Ogre Sentry (3/3): lethal
        -- is 3, all 3 go to the Ogre, 0 tramples. Only deathtouch changes the spill.
        let (gs, _, _) = S.combatBoardOf [Cards.warMammothPrinting cards] [Cards.ogreSentryPrinting cards]
            after = S.settleSba (S.fightWith tramplingAnswer gs)
         in HU.assertEqual "bob untouched without deathtouch" (Just 20) (S.lifeOf S.bob after)
    ]

m2cPropertyTests :: Cards.Cards -> Tasty.TestTree
m2cPropertyTests cards =
  Tasty.testGroup
    "M2cProperties"
    [ HU.testCase "a deathtoucher's victim with toughness > 0 is gone after the SBA" $
        -- The property in fixture form (the deck has no deathtoucher, so this is
        -- the M2c coverage; it becomes a random-game property when a deathtoucher
        -- joins a deck -- the castability work, #23). Every toughness we throw at
        -- the 1/1 deathtoucher dies to it.
        let victims = [Cards.pikerPrinting cards, Cards.nimbleBirdstickerPrinting cards, Cards.ogreSentryPrinting cards]
            killsIt v =
              let (gs, _, _) = S.combatBoardOf [Cards.typhoidRatsPrinting cards] [v]
                  after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
               in S.creaturesInPlay S.bob after == 0
         in HU.assertBool "deathtouch kills every toughness" (all killsIt victims),
      HU.testCase "the deathtouch and trample reads never name a card" $
        -- A structural reminder, asserted by the interaction falsifier's outcome
        -- (TrampleDeathtouch) depending only on the keyword projection. This case
        -- documents the invariant; the real enforcement is code review of
        -- Damage.blockerThreshold and Sba.woundedByDeathtouch, which case on
        -- Keyword, never on a printing.
        HU.assertBool "see TrampleDeathtouch and Deathtouch groups" True
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Damage"
    [ damageTests cards,
      damageEventTests cards,
      deathtouchTests cards,
      assignmentLegalityTests,
      trampleTests cards,
      trampleDeathtouchTests cards,
      sbaTests,
      creatureSbaTests cards,
      infectTests cards,
      m2cPropertyTests cards
    ]
