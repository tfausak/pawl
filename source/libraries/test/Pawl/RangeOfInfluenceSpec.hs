{-# LANGUAGE GADTs #-}

-- Covers: CR 801's limited range of influence option -- Pawl.Types.RangeOfInfluence,
-- the Pawl.Types.GameSettings field that carries it, Pawl.Engine.Game.inRangeOf,
-- and its readers: Pawl.Engine.Combat's attackableOpponents (CR 801.3),
-- Pawl.Engine.Target's legalRecipientsGiven (CR 801.4),
-- Pawl.Engine.Activate's activatableGiven (CR 801.6), Pawl.Engine.Sba's
-- fallsOff and becomesUnattached (CR 801.8 / CR 801.9),
-- Pawl.Engine.Projection's affectsWith and Pawl.Engine.PlayerEffect's applies
-- (CR 801.10 for static abilities), Pawl.Engine.Projection.View's
-- controlNames (CR 801.10 for a layer-2 grant), Pawl.Engine.CombatRestriction's
-- attackLimit and blockLimit (CR 801.10 for a declaration's bound) and Sba's
-- worldVictims (CR 801.12), Pawl.Engine.Event.Trigger's eventWithinRange (CR
-- 801.7), Pawl.Engine.Replacement's reaches, preventsInRange and
-- redirectDestination (CR 801.13), Pawl.Engine.Resolve.Slots'
-- playerRefPlayers and battlefieldMatching, Pawl.Engine.Target.zoneScopePlayers,
-- Pawl.Engine.Resolve.Effect's objectRefRecipients, Pawl.Engine.Count's
-- playersFor and the choice offers Pawl.Engine.Players.offer and
-- Game.inRangeOf feed (CR 801.5a, 801.10, 801.11), and
-- Pawl.Engine.Resolve.Effect's WinGame and DrawGame (CR 801.14,
-- 801.15), and Pawl.Engine.Engine's checkMandatoryLoop (CR 801.16); and CR
-- 801.2c's turn-start seating, Pawl.Types.GameState's departedThisTurn.
--
-- FOUR SEATS, at range 1 unless a case says otherwise, turn order [alice, bob,
-- carol, dave]: bob and dave sit next to alice and carol sits two seats away.
-- Three seats would cut nothing, every seat being adjacent there. Each negative
-- is paired with the same board at an unlimited range.
module Pawl.RangeOfInfluenceSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import Pawl.PreventionSpec (theAbility)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Range of influence" $ do
  -- CR 801.3: alice's Goblin Piker may attack only the opponents within one
  -- seat. ONE board declared at two ranges, and at range 1 a declaration against
  -- dave -- the adjacent seat reached round the end of the turn order -- is legal
  -- where the one against carol is not.
  Spec.it s "CR 801.3 a creature cannot attack an opponent outside its controller's range" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (mine, staged) = S.addPermanent piker S.alice S.fourPlayerGame
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
        settle gs = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
        declares at gs = Combat.legalAttackDeclarationAs S.alice [(mine, AttackTarget.OfPlayer at)] (settle gs)
    Spec.assertBool s (not (declares S.carol (S.withRange 1 board))) "CR 801.3 at range 1 alice's creature may not attack carol, two seats away"
    Spec.assertBool s (declares S.carol board) "at an unlimited range it may"
    Spec.assertBool s (declares S.dave (S.withRange 1 board)) "CR 801.2 and at range 1 it may attack dave, one seat away the other way round"

  -- CR 801.2d for objects under CR 801.4: Lightning Bolt's "any target" over a
  -- Goblin Piker and Invasion of Dominaria, both controlled by carol. The Piker is
  -- out of alice's range; the battle is back in it once bob, one seat away,
  -- protects it -- CR 801.2d's second sentence.
  Spec.it s "CR 801.2d an object is in range by its controller, and a battle by its protector" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    piker <- S.printingOf s registry "Goblin Piker"
    invasion <- S.printingOf s registry "Invasion of Dominaria"
    let (theirs, g0) = S.addPermanent piker S.carol S.fourPlayerGame
        (battle, g1) = S.addPermanent invasion S.carol g0
        protectedBy who gs = gs {GameState.objects = Map.adjust (\o -> o {Object.protector = who}) battle (GameState.objects gs)}
    case S.spellTargetSlot bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot
        Spec.assertBool s (not (Set.member (Recipient.ToCreature theirs) (legal (S.withRange 1 g1)))) "CR 801.4 at range 1 alice may not bolt carol's creature"
        Spec.assertBool s (Set.member (Recipient.ToCreature theirs) (legal g1)) "at an unlimited range she may"
        Spec.assertBool s (Set.member (Recipient.ToBattle battle) (legal (S.withRange 1 (protectedBy (Just S.bob) g1)))) "CR 801.2d at range 1 she may bolt carol's battle while bob protects it"
        Spec.assertBool s (not (Set.member (Recipient.ToBattle battle) (legal (S.withRange 1 (protectedBy Nothing g1))))) "and not while nobody in range does"

  -- CR 801.6: Glittering Lion ({2}{W} Creature -- Cat 2/2), "{3}: Until end of
  -- turn, this creature loses 'Prevent all damage that would be dealt to this
  -- creature.' Any player may activate this ability." carol controls it; alice
  -- and bob each have three Plains to pay with.
  Spec.it s "CR 801.6 a player cannot activate an ability of an object outside their range" $ do
    plains <- S.printingOf s registry "Plains"
    lionPrinting <- S.printingOf s registry "Glittering Lion"
    let lands = S.landsFor plains S.bob 3 (S.landsFor plains S.alice 3 S.fourPlayerGame)
        (lion, g0) = S.addPermanent lionPrinting S.carol lands
        board = g0 {GameState.phase = Phase.PrecombatMain}
        offeredTo pid gs = any (\a -> case a of A.Activate o _ -> o == lion; _ -> False) (Action.legalActions pid gs {GameState.priority = Just pid})
    Spec.assertBool s (not (offeredTo S.alice (S.withRange 1 board))) "CR 801.6 at range 1 alice is not offered the Lion carol controls"
    Spec.assertBool s (offeredTo S.alice board) "at an unlimited range she is"
    Spec.assertBool s (offeredTo S.bob (S.withRange 1 board)) "and bob, one seat from carol, is at range 1"

  -- CR 801.8 / CR 801.9: alice's Goblin Piker (2/1) wears her Unholy Strength
  -- (+2/+1) and her Bonesplitter (+2/+0), and carol's Control Magic has taken
  -- it. Carol reaches two seats and alice one, so the Piker now sits outside
  -- alice's range while carol's own Aura stays legal. Paired with the same board
  -- at an unlimited range.
  Spec.it s "CR 801.8 / 801.9 an Aura or Equipment on a host outside its controller's range falls off" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    unholy <- S.printingOf s registry "Unholy Strength"
    splitter <- S.printingOf s registry "Bonesplitter"
    controlMagic <- S.printingOf s registry "Control Magic"
    let (creature, g0) = S.addPermanent piker S.alice S.fourPlayerGame
        (aura, g1) = S.addPermanent unholy S.alice g0
        (equipment, g2) = S.addPermanent splitter S.alice g1
        (steal, g3) = S.addPermanent controlMagic S.carol g2
        board = S.attach steal creature (S.attach equipment creature (S.attach aura creature g3))
        ranged gs =
          let ranges = RangeOfInfluence.MkRangeOfInfluence (Map.fromList [(S.alice, 1), (S.bob, 1), (S.carol, 2), (S.dave, 1)])
           in gs {GameState.settings = (GameState.settings gs) {GameSettings.rangeOfInfluence = ranges}}
        limited = S.settleSba (ranged board)
        unlimited = S.settleSba board
    Spec.assertEqWith s "CR 801.8 / 801.9 at alice's range 1 the Piker loses both bonuses" (S.powerToughnessOf creature limited) (Just (2, 1))
    Spec.assertBool s (not (S.onBattlefield aura limited)) "CR 801.8 alice's Unholy Strength leaves the battlefield"
    Spec.assertEqWith s "CR 801.9 alice's Bonesplitter is unattached" (fmap Object.attachedTo (Game.lookupObject equipment limited)) (Just Nothing)
    Spec.assertBool s (S.onBattlefield equipment limited) "CR 801.9 and stays on the battlefield"
    Spec.assertBool s (S.onBattlefield steal limited) "carol's Control Magic, on a creature she controls, stays"
    Spec.assertEqWith s "at an unlimited range the Piker keeps both" (S.powerToughnessOf creature unlimited) (Just (6, 2))

  -- CR 801.10 for a static ability: alice's Living Plane ("All lands are 1/1
  -- creatures that are still lands.") over a Forest each for bob, one seat
  -- away, and carol, two seats away.
  Spec.it s "CR 801.10 a static ability does not reach a permanent outside its controller's range" $ do
    livingPlane <- S.printingOf s registry "Living Plane"
    forest <- S.printingOf s registry "Forest"
    let (_, g0) = S.addPermanent livingPlane S.alice S.fourPlayerGame
        (bobs, g1) = S.addPermanent forest S.bob g0
        (carols, board) = S.addPermanent forest S.carol g1
    Spec.assertEqWith s "CR 801.10 at range 1 carol's Forest is not a creature" (S.powerToughnessOf carols (S.withRange 1 board)) Nothing
    Spec.assertEqWith s "at an unlimited range it is a 1/1" (S.powerToughnessOf carols board) (Just (1, 1))
    Spec.assertEqWith s "and bob's Forest, in range, is a 1/1 at range 1" (S.powerToughnessOf bobs (S.withRange 1 board)) (Just (1, 1))

  -- CR 801.10 for a player ability: alice's Gnat Miser ("Each opponent's
  -- maximum hand size is reduced by one.") over bob, one seat away, and carol,
  -- two seats away.
  Spec.it s "CR 801.10 a player ability does not reach a player outside its controller's range" $ do
    miser <- S.printingOf s registry "Gnat Miser"
    let (_, board) = S.addPermanent miser S.alice S.fourPlayerGame
    Spec.assertEqWith s "CR 801.10 at range 1 carol keeps a maximum hand size of seven" (PlayerEffect.maximumHandSize S.carol (S.withRange 1 board)) (Just 7)
    Spec.assertEqWith s "at an unlimited range hers is six" (PlayerEffect.maximumHandSize S.carol board) (Just 6)
    Spec.assertEqWith s "and bob's, in range, is six at range 1" (PlayerEffect.maximumHandSize S.bob (S.withRange 1 board)) (Just 6)

  -- CR 801.10 for a layer-2 grant: alice's Synthetic Goblin Dominion ("You
  -- control all Goblins.") over a Goblin Piker each for bob, one seat away, and
  -- carol, two seats away.
  Spec.it s "CR 801.10 a control grant does not take a permanent outside its controller's range" $ do
    dominion <- S.printingOf s registry "Synthetic Goblin Dominion"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent dominion S.alice S.fourPlayerGame
        (bobs, g1) = S.addPermanent piker S.bob g0
        (carols, board) = S.addPermanent piker S.carol g1
    Spec.assertEqWith s "CR 801.10 at range 1 carol keeps her Goblin" (Projection.controllerOf carols (S.withRange 1 board)) (Just S.carol)
    Spec.assertEqWith s "at an unlimited range alice takes it" (Projection.controllerOf carols board) (Just S.alice)
    Spec.assertEqWith s "and at range 1 alice takes bob's, in range" (Projection.controllerOf bobs (S.withRange 1 board)) (Just S.alice)

  -- CR 801.10 for a bound on a declaration: alice's Silent Arbiter ("No more
  -- than one creature can attack each combat. No more than one creature can
  -- block each combat.") over carol, two seats away. Carol attacks dave, beside
  -- her, with two Goblin Pikers; and when bob attacks carol, carol blocks with
  -- two.
  Spec.it s "CR 801.10 a limit on attackers or blockers does not bind a player outside its controller's range" $ do
    arbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent arbiter S.alice S.fourPlayerGame
        (first, g1) = S.addPermanent piker S.carol g0
        (second, g2) = S.addPermanent piker S.carol g1
        (attacker, g3) = S.addPermanent piker S.bob g2
        attackBoard =
          g3
            { GameState.activePlayer = S.carol,
              GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.dave, S.bob]}
            }
        blockBoard =
          g3
            { GameState.activePlayer = S.bob,
              GameState.phase = Phase.Combat CombatStep.DeclareBlockers,
              GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.carol], Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.carol)}
            }
        both = fmap (\oid -> (oid, AttackTarget.OfPlayer S.dave)) [first, second]
        doubleBlock = Map.fromList [(first, Set.singleton attacker), (second, Set.singleton attacker)]
    Spec.assertBool s (Combat.legalAttackDeclarationAs S.carol both (S.withRange 1 attackBoard)) "CR 801.10 at range 1 carol attacks dave with both Pikers"
    Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.carol both attackBoard)) "at an unlimited range the Arbiter holds her to one"
    Spec.assertBool s (Combat.legalBlockDeclaration S.carol doubleBlock (S.withRange 1 blockBoard)) "CR 801.10 at range 1 carol blocks with both"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.carol doubleBlock blockBoard)) "at an unlimited range she may block with only one"

  -- CR 801.12: alice's Living Plane, then a newer Concordant Crossroads, each
  -- stamped by the settle that follows its entry (Engine.sampleWorldSince).
  -- Carol's is two seats from alice; bob's is one.
  Spec.it s "CR 801.12 the world rule compares only world permanents within range" $ do
    livingPlane <- S.printingOf s registry "Living Plane"
    crossroads <- S.printingOf s registry "Concordant Crossroads"
    let addWorld printing pid gs =
          let (oid, placed) = S.addPermanent printing pid gs
           in (oid, S.runPure S.identityAnswer placed Engine.sampleWorldSince)
        (plane, g0) = addWorld livingPlane S.alice S.fourPlayerGame
        (_, carols) = addWorld crossroads S.carol g0
        (_, bobs) = addWorld crossroads S.bob g0
        pass gs = S.runPure S.identityAnswer gs (Engine.sampleWorldSince >> Sba.checkStateBasedActions)
    Spec.assertBool s (S.onBattlefield plane (pass (S.withRange 1 carols))) "CR 801.12 at range 1 alice's Living Plane survives carol's newer Crossroads"
    Spec.assertBool s (not (S.onBattlefield plane (pass carols))) "at an unlimited range it is put into the graveyard"
    Spec.assertBool s (not (S.onBattlefield plane (pass (S.withRange 1 bobs)))) "and at range 1 bob's newer Crossroads, in range, buries it"

  -- CR 801.11 for a SOURCELESS ability: CR 702.179d's speed increase has no
  -- source whose controller CR 801.7 could read, but it "doesn't see ... events
  -- outside its controller's range of influence". Carol, two seats from alice,
  -- loses life on alice's turn. The board differs from its control in the range
  -- alone, and from its positive pair in which opponent lost the life.
  Spec.it s "CR 801.11 an opponent's life loss outside range raises no speed" $ do
    let fast = S.fourPlayerGame {GameState.players = Map.adjust (\p -> p {Player.speed = Just 1}) S.alice (GameState.players S.fourPlayerGame)}
        losing pid = S.withEvents [GameEvent.LifeLost (LifeChange.MkLifeChange pid 2)] fast
        settle gs =
          let placed = S.runPure S.identityAnswer gs Engine.placePendingTriggers
           in if null (GameState.stack placed) then placed else S.runPure S.identityAnswer placed Stack.resolveTop
        speedAfter gs = Player.speed =<< Map.lookup S.alice (GameState.players (settle gs))
    Spec.assertEqWith s "alice is the active player" (GameState.activePlayer fast) S.alice
    Spec.assertEqWith s "CR 801.11 at range 1 carol's loss raises nothing" (speedAfter (S.withRange 1 (losing S.carol))) (Just 1)
    Spec.assertEqWith s "at an unlimited range it raises alice's speed" (speedAfter (losing S.carol)) (Just 2)
    Spec.assertEqWith s "and at range 1 bob's loss, in range, raises it" (speedAfter (S.withRange 1 (losing S.bob))) (Just 2)

  -- CR 805.4 with CR 801.7/801.11: under shared team turns the team's step is
  -- dave's own step, though the event names bob, the active player, two seats
  -- from dave. So dave's printed upkeep trigger (Bitterblossom) and his
  -- inherent end step draw as the monarch (CR 725.2) both fire at range 1.
  -- Paired with the same boards at an unlimited range.
  Spec.it s "CR 805.4/801.7 a teammate two seats from the active player still sees the team's own step begin" $ do
    bitterblossom <- S.printingOf s registry "Bitterblossom"
    island <- S.printingOf s registry "Island"
    let shared =
          (S.inTeams [[S.alice], [S.bob, S.carol, S.dave]] S.fourPlayerGame)
            { GameState.activePlayer = S.bob
            }
        teamed = shared {GameState.settings = (GameState.settings shared) {GameSettings.sharedTeamTurns = True}}
        stocked = snd (S.addLibraryCard island S.dave (snd (S.addLibraryCard island S.dave teamed)))
        (_, withBlossom) = S.addPermanent bitterblossom S.dave stocked
        settle gs =
          let placed = S.runPure S.identityAnswer gs Engine.placePendingTriggers
           in if null (GameState.stack placed) then placed else S.runPure S.identityAnswer placed Stack.resolveTop
        upkeep = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Beginning BeginningStep.Upkeep) S.bob)] withBlossom
        endStep = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.bob)] (S.withMonarch S.dave stocked)
        handOf gs = length (Game.zoneMembers Zone.Hand S.dave gs)
    Spec.assertBool s (not (Game.wasInRangeOf S.dave S.bob (S.withRange 1 teamed))) "at range 1 bob is outside dave's range"
    Spec.assertEqWith s "CR 805.4/801.7 Bitterblossom cost dave a life at the team's upkeep" (S.lifeOf S.dave (settle (S.withRange 1 upkeep))) (fmap (subtract 1) (S.lifeOf S.dave upkeep))
    Spec.assertEqWith s "CR 725.2/801.11 dave, the monarch, drew at the team's end step" (handOf (settle (S.withRange 1 endStep))) (handOf endStep + 1)
    Spec.assertEqWith s "as he does at an unlimited range" (handOf (settle endStep)) (handOf endStep + 1)

  -- CR 801.7 for an object the event involves: alice's Soul Warden ("Whenever
  -- another creature enters, you gain 1 life.") sees a Goblin Piker enter under
  -- carol, two seats away, and one under bob, one seat away.
  Spec.it s "CR 801.7 a triggered ability does not trigger on an object outside its controller's range" $ do
    warden <- S.printingOf s registry "Soul Warden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent warden S.alice S.fourPlayerGame
        gainAfter pid gs =
          let (entrant, placed) = S.addPermanent piker pid gs
           in S.lifeOf S.alice (entering entrant placed)
    Spec.assertEqWith s "CR 801.7 at range 1 carol's Piker gains alice nothing" (gainAfter S.carol (S.withRange 1 g0)) (Just 20)
    Spec.assertEqWith s "at an unlimited range it gains her 1" (gainAfter S.carol g0) (Just 21)
    Spec.assertEqWith s "and at range 1 bob's, in range, gains her 1" (gainAfter S.bob (S.withRange 1 g0)) (Just 21)

  -- CR 801.7a and its example: carol controls a Goblin Piker alice owns, and
  -- each controls a Super Shredder ("Whenever another permanent leaves the
  -- battlefield, put a +1/+1 counter on Super Shredder."). The Piker dies into
  -- alice's graveyard, and the departure is read under carol, who controlled it
  -- as it left.
  Spec.it s "CR 801.7a a permanent leaving the battlefield is read under the controller it left with" $ do
    shredder <- S.printingOf s registry "Super Shredder"
    piker <- S.printingOf s registry "Goblin Piker"
    let (alices, g0) = S.addPermanent shredder S.alice S.fourPlayerGame
        (carols, g1) = S.addPermanent shredder S.carol g0
        (victim, g2) = S.addPermanent piker S.alice g1
        board = S.markDamage victim 1 (S.giveControl victim S.carol g2)
        dies gs = resolveAll (snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority))
        limited = dies (S.withRange 1 board)
    Spec.assertBool s (not (S.onBattlefield victim limited)) "the Piker died"
    Spec.assertEqWith s "CR 801.7a at range 1 carol's Shredder sees it leave" (S.powerToughnessOf carols limited) (Just (2, 2))
    Spec.assertEqWith s "and alice's, two seats from carol, does not" (S.powerToughnessOf alices limited) (Just (1, 1))
    Spec.assertEqWith s "at an unlimited range alice's does" (S.powerToughnessOf alices (dies board)) (Just (2, 2))

  -- CR 801.13b's second case: alice's Selfless Squire ("When this creature
  -- enters, prevent all damage that would be dealt to you this turn.") names
  -- the recipient, so an out-of-range source does not matter. Carol reaches two
  -- seats and everyone else one, so her Goblin Piker attacks alice (CR 801.3)
  -- from outside alice's range.
  Spec.it s "CR 801.13b a prevention naming the recipient needs only the recipient in range" $ do
    squire <- S.printingOf s registry "Selfless Squire"
    piker <- S.printingOf s registry "Goblin Piker"
    let (entrant, g0) = S.addPermanent squire S.alice S.fourPlayerGame
        (carols, g1) = S.addPermanent piker S.carol g0
        ranges = RangeOfInfluence.MkRangeOfInfluence (Map.fromList [(S.alice, 1), (S.bob, 1), (S.carol, 2), (S.dave, 1)])
        ranged = g1 {GameState.settings = (GameState.settings g1) {GameSettings.rangeOfInfluence = ranges}}
        shielded = entering entrant ranged
    Spec.assertBool s (not (Game.inRangeOf S.alice S.carol shielded)) "carol is outside alice's range"
    Spec.assertEqWith s "CR 801.13b carol's Piker deals alice nothing" (S.lifeOf S.alice (strike S.carol [carols] S.alice shielded)) (Just 20)

  -- CR 801.13a for a zone change: alice's Leyline of the Void ("If a card would
  -- be put into an opponent's graveyard from anywhere, exile it instead.") while
  -- a Goblin Piker dies under carol, two seats away, and one under bob, one seat
  -- away.
  Spec.it s "CR 801.13a a zone-change replacement does not reach a card outside its controller's range" $ do
    leyline <- S.printingOf s registry "Leyline of the Void"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent leyline S.alice S.fourPlayerGame
        diesUnder pid gs =
          let (victim, placed) = S.addPermanent piker pid gs
              after = resolveAll (snd (Engine.runGamePure S.identityAnswer (S.markDamage victim 1 placed) Engine.settleForPriority))
           in fmap ZoneChange.to (filter ((== victim) . ZoneChange.departed) (S.zoneChangesOf after))
    Spec.assertEqWith s "CR 801.13a at range 1 carol's Piker goes to her graveyard" (diesUnder S.carol (S.withRange 1 g0)) [Zone.Graveyard]
    Spec.assertEqWith s "at an unlimited range it is exiled" (diesUnder S.carol g0) [Zone.Exile]
    Spec.assertEqWith s "and at range 1 bob's, in range, is exiled" (diesUnder S.bob (S.withRange 1 g0)) [Zone.Exile]

  -- CR 801.13a for a damage amount: alice's Furnace of Rath ("If a source would
  -- deal damage to a permanent or player, it deals double that damage to that
  -- permanent or player instead.") while bob's Goblin Piker attacks carol, two
  -- seats from alice, and then alice.
  Spec.it s "CR 801.13a a damage-doubling replacement does not reach a recipient outside its controller's range" $ do
    furnace <- S.printingOf s registry "Furnace of Rath"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent furnace S.alice S.fourPlayerGame
        (bobs, board) = S.addPermanent piker S.bob g0
    Spec.assertEqWith s "CR 801.13a at range 1 carol takes 2" (S.lifeOf S.carol (strike S.bob [bobs] S.carol (S.withRange 1 board))) (Just 18)
    Spec.assertEqWith s "at an unlimited range she takes 4" (S.lifeOf S.carol (strike S.bob [bobs] S.carol board)) (Just 16)
    Spec.assertEqWith s "and at range 1 alice, in range, takes 4" (S.lifeOf S.alice (strike S.bob [bobs] S.alice (S.withRange 1 board))) (Just 16)

  -- CR 801.10 for every graveyard: alice's Rest in Peace ("When this enchantment
  -- enters, exile all graveyards.") enters while carol, two seats away, and bob,
  -- one seat away, each have a Goblin Piker in their graveyard.
  Spec.it s "CR 801.10 a zone sweep does not reach the zone of a player outside its controller's range" $ do
    rest <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    let (bobs, g0) = S.addObjectIn Zone.Graveyard piker S.bob S.fourPlayerGame
        (carols, g1) = S.addObjectIn Zone.Graveyard piker S.carol g0
        stillThere oid gs = fmap Object.zone (Game.lookupObject oid gs) == Just Zone.Graveyard
        enters ranged =
          let (entrant, placed) = S.addPermanent rest S.alice (ranged g1)
           in entering entrant placed
    Spec.assertBool s (stillThere carols (enters (S.withRange 1))) "CR 801.10 at range 1 carol's graveyard is untouched"
    Spec.assertBool s (not (stillThere bobs (enters (S.withRange 1)))) "and bob's, in range, is exiled"
    Spec.assertBool s (not (stillThere carols (enters id))) "at an unlimited range carol's is exiled too"

  -- CR 801.10 for damage to each opponent: alice's Fanatic of Mogis ("When this
  -- creature enters, it deals damage to each opponent equal to your devotion to
  -- red.") enters at devotion 1.
  Spec.it s "CR 801.10 damage to each opponent does not reach an opponent outside its controller's range" $ do
    fanatic <- S.printingOf s registry "Fanatic of Mogis"
    let enters ranged =
          let (entrant, placed) = S.addPermanent fanatic S.alice (ranged S.fourPlayerGame)
           in entering entrant placed
    Spec.assertEqWith s "CR 801.10 at range 1 carol is untouched" (S.lifeOf S.carol (enters (S.withRange 1))) (Just 20)
    Spec.assertEqWith s "and bob, in range, takes 1" (S.lifeOf S.bob (enters (S.withRange 1))) (Just 19)
    Spec.assertEqWith s "at an unlimited range carol takes 1 too" (S.lifeOf S.carol (enters id)) (Just 19)

  -- CR 801.11: alice's Malignus ("Malignus's power and toughness are each equal
  -- to half the highest life total among your opponents, rounded up.") while
  -- carol, two seats away, is at 40 and bob and dave, one seat away, at 20.
  Spec.it s "CR 801.11 an ability gets no information from outside its controller's range" $ do
    malignus <- S.printingOf s registry "Malignus"
    let (creature, g0) = S.addPermanent malignus S.alice S.fourPlayerGame
        board = g0 {GameState.players = Map.adjust (\p -> p {Player.life = 40}) S.carol (GameState.players g0)}
    Spec.assertEqWith s "CR 801.11 at range 1 Malignus reads only bob's and dave's 20" (S.powerToughnessOf creature (S.withRange 1 board)) (Just (10, 10))
    Spec.assertEqWith s "at an unlimited range it reads carol's 40" (S.powerToughnessOf creature board) (Just (20, 20))

  -- CR 801.5a for a resolving "choose a player": alice's Stadium Vendors ("When
  -- this creature enters, choose a player. That player adds two mana ...").
  Spec.it s "CR 801.5a a resolving choice of player offers only players within the controller's range" $ do
    mountain <- S.printingOf s registry "Mountain"
    vendors <- S.printingOf s registry "Stadium Vendors"
    let (spellId, board) = S.addHandCard vendors S.alice (S.landsFor mountain S.alice 4 S.fourPlayerGame)
        recording :: Prompt.Prompt r -> State.State [[PlayerId.PlayerId]] r
        recording p = case p of
          Prompt.ChoosePlayer _ _ _ offer -> State.modify' (<> [NonEmpty.toList offer]) >> pure (NonEmpty.head offer)
          _ -> pure (S.identityAnswer p)
        offered gs = State.execState (Engine.runGame recording (S.runPure S.identityAnswer (onMain gs) (S.cast S.alice spellId)) Engine.priorityLoop) []
    Spec.assertEqWith s "CR 801.5a at range 1 the trigger does not offer carol" (offered (S.withRange 1 board)) [[S.alice, S.bob, S.dave]]
    Spec.assertEqWith s "at an unlimited range it does" (offered board) [[S.alice, S.bob, S.carol, S.dave]]

  -- CR 801.5a for the as-enters opponent choices: Snake of the Golden Grove's
  -- tribute (CR 702.104a) and Null Chamber's naming opponent.
  Spec.it s "CR 801.5a tribute and a naming opponent offer only opponents within the controller's range" $ do
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    snake <- S.printingOf s registry "Snake of the Golden Grove"
    chamber <- S.printingOf s registry "Null Chamber"
    let recording :: Prompt.Prompt r -> State.State [[PlayerId.PlayerId]] r
        recording p = case p of
          Prompt.ChooseOpponent _ _ _ offer -> State.modify' (<> [NonEmpty.toList offer]) >> pure (NonEmpty.head offer)
          _ -> pure (S.identityAnswer p)
        offeredFor card land lands =
          let (spellId, board) = S.addHandCard card S.alice (S.landsFor land S.alice lands S.fourPlayerGame)
              offered gs = State.execState (Engine.runGame recording (S.runPure S.identityAnswer (onMain gs) (S.cast S.alice spellId)) Engine.priorityLoop) []
           in (offered (S.withRange 1 board), offered board)
    Spec.assertEqWith s "CR 801.5a at range 1 tribute does not offer carol, and does at an unlimited range" (offeredFor snake forest 5) ([[S.bob, S.dave]], [[S.bob, S.carol, S.dave]])
    Spec.assertEqWith s "CR 801.5a at range 1 Null Chamber does not offer carol, and does at an unlimited range" (offeredFor chamber plains 4) ([[S.bob, S.dave]], [[S.bob, S.carol, S.dave]])

  -- CR 801.5a for CR 303.4f's entry choice: alice's Replenish returns
  -- Archnemesis ("Enchant opponent") and Pacifism ("Enchant creature") to the
  -- battlefield, with a Goblin Piker under each of bob, carol and dave.
  Spec.it s "CR 801.5a an Aura put onto the battlefield is offered only hosts within its chooser's range" $ do
    plains <- S.printingOf s registry "Plains"
    replenish <- S.printingOf s registry "Replenish"
    archnemesis <- S.printingOf s registry "Archnemesis"
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (bobs, g0) = S.addPermanent piker S.bob (S.landsFor plains S.alice 4 S.fourPlayerGame)
        (carols, g1) = S.addPermanent piker S.carol g0
        (daves, g2) = S.addPermanent piker S.dave g1
        (_, g3) = S.addGraveyardCard archnemesis S.alice g2
        (_, g4) = S.addGraveyardCard pacifism S.alice g3
        (spellId, board) = S.addHandCard replenish S.alice g4
        recording :: Prompt.Prompt r -> State.State ([[PlayerId.PlayerId]], [[ObjectId.ObjectId]]) r
        recording p = case p of
          Prompt.ChooseOpponent _ _ _ offer -> State.modify' (\(ps, os) -> (ps <> [NonEmpty.toList offer], os)) >> pure (NonEmpty.head offer)
          Prompt.ChooseAttachment _ _ _ offer -> State.modify' (\(ps, os) -> (ps, os <> [NonEmpty.toList offer])) >> pure (NonEmpty.head offer)
          _ -> pure (S.identityAnswer p)
        offered gs = State.execState (Engine.runGame recording (S.runPure S.identityAnswer (onMain gs) (S.cast S.alice spellId)) Engine.priorityLoop) ([], [])
    Spec.assertEqWith s "CR 801.5a at range 1 Archnemesis is not offered carol" (fst (offered (S.withRange 1 board))) [[S.bob, S.dave]]
    Spec.assertEqWith s "CR 801.5a at range 1 Pacifism is not offered carol's Piker" (snd (offered (S.withRange 1 board))) [[bobs, daves]]
    Spec.assertEqWith s "at an unlimited range both offers hold carol" (offered board) ([[S.bob, S.carol, S.dave]], [[bobs, carols, daves]])

  -- CR 801.10 for a chosen player: alice's Stuffy Doll chose erin, beside her
  -- at five seats, and bob now controls it. Bob's {T} has the Doll deal 1 to
  -- itself, and its trigger -- bob's -- would deal 1 to erin, two seats from bob.
  Spec.it s "CR 801.10 a chosen player outside the controller's range is dealt no damage" $ do
    doll <- S.printingOf s registry "Stuffy Doll"
    let (dollId, g0) = S.addPermanent doll S.alice (Setup.emptyGame (S.alice NonEmpty.:| [S.bob, S.carol, S.dave, erin]))
        chosen = g0 {GameState.objects = Map.adjust (\o -> o {Object.chosenPlayer = Just erin}) dollId (GameState.objects g0)}
        stolen = S.giveControl dollId S.bob chosen
        tapped gs =
          let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.bob dollId (theAbility doll))
           in resolveAll (snd (Engine.runGamePure S.identityAnswer activated Engine.settleForPriority))
    Spec.assertBool s (Game.inRangeOf S.alice erin (S.withRange 1 stolen) && not (Game.inRangeOf S.bob erin (S.withRange 1 stolen))) "erin is in alice's range and outside bob's"
    Spec.assertEqWith s "CR 801.10 at range 1 erin stays at 20" (S.lifeOf erin (tapped (S.withRange 1 stolen))) (Just 20)
    Spec.assertEqWith s "and the Doll was dealt its 1" (fmap Object.damage (Game.lookupObject dollId (tapped (S.withRange 1 stolen)))) (Just 1)
    Spec.assertEqWith s "at an unlimited range erin takes 1" (S.lifeOf erin (tapped stolen)) (Just 19)

  -- CR 801.10 cuts whom an effect AFFECTS, not who decides: alice's Cut the
  -- Tethers ("For each Spirit, return it to its owner's hand unless that player
  -- pays {3}.") over carol's Accursed Spirit, which bob controls. The Spirit is
  -- in alice's range by its controller (CR 801.2d) and its owner carol is not;
  -- carol, with no mana, cannot pay (CR 118.3), so the Spirit returns.
  Spec.it s "CR 801.10 a payer outside the controller's range is still offered the payment" $ do
    island <- S.printingOf s registry "Island"
    tethers <- S.printingOf s registry "Cut the Tethers"
    spirit <- S.printingOf s registry "Accursed Spirit"
    let (spiritId, g0) = S.addPermanent spirit S.carol (S.landsFor island S.alice 4 S.fourPlayerGame)
        (spellId, board) = S.addHandCard tethers S.alice (S.giveControl spiritId S.bob g0)
        cast gs = resolveAll (S.runPure S.identityAnswer (onMain gs) (S.cast S.alice spellId))
        carolsHand gs = length (Game.zoneMembers Zone.Hand S.carol gs)
    Spec.assertBool s (not (Game.inRangeOf S.alice S.carol (S.withRange 1 board))) "carol is outside alice's range"
    Spec.assertEqWith s "CR 118.12 at range 1 carol does not pay, so the Spirit returns to her hand" (Set.member spiritId (GameState.battlefield (cast (S.withRange 1 board))), carolsHand (cast (S.withRange 1 board))) (False, 1)
    Spec.assertEqWith s "and the same at an unlimited range" (Set.member spiritId (GameState.battlefield (cast board)), carolsHand (cast board)) (False, 1)

  -- CR 104.2b / 801.14: alice's Felidar Sovereign ("At the beginning of your
  -- upkeep, if you have 40 or more life, you win the game.") at 40 life. At range
  -- 1 only bob and dave, her opponents in range, lose; carol plays on.
  Spec.it s "CR 801.14 a player who wins makes only their opponents within range lose" $ do
    sovereign <- S.printingOf s registry "Felidar Sovereign"
    let (_, g0) = S.addPermanent sovereign S.alice S.fourPlayerGame
        atLife n = g0 {GameState.players = Map.adjust (\p -> p {Player.life = n}) S.alice (GameState.players g0)}
        limited = upkeepOf S.alice (S.withRange 1 (atLife 40))
    Spec.assertEqWith s "CR 801.14 at range 1 alice and carol are still playing" (Game.stillPlaying limited) [S.alice, S.carol]
    Spec.assertEqWith s "and the game goes on" (GameState.result limited) Nothing
    Spec.assertEqWith s "CR 104.2b at an unlimited range alice wins" (GameState.result (upkeepOf S.alice (atLife 40))) (Just (Result.Won S.alice))
    Spec.assertEqWith s "CR 603.4 at 39 life nothing happens" (Game.stillPlaying (upkeepOf S.alice (atLife 39))) [S.alice, S.bob, S.carol, S.dave]

  -- CR 801.16: Pawl.GameSpec's CR 104.4b loop -- alice's Sporemound mints a
  -- Saproling, a Life and Limb makes it a Forest land, and another player's
  -- Aether Flash buries it -- at SIX seats, so that two players can sit outside
  -- every loop player's range and play on together. With bob's Aether Flash the
  -- draw takes alice and bob and their neighbours frank and carol. Dave and erin
  -- are still in a game that has no result: the gap restarts for them rather
  -- than drawing them at the next check.
  --
  -- Aether Flash is never alice's: two triggers of hers on one Saproling would
  -- be hers to order (CR 603.3b), a choice, and then no loop is mandatory.
  Spec.it s "CR 801.16 a mandatory loop draws its players and those within their range" $ do
    flash <- S.printingOf s registry "Aether Flash"
    limb <- S.printingOf s registry "Life and Limb"
    sporemound <- S.printingOf s registry "Sporemound"
    forest <- S.printingOf s registry "Forest"
    hermit <- S.printingOf s registry "Thelonite Hermit"
    necrosynthesis <- S.printingOf s registry "Necrosynthesis"
    let loopedBeside extra limbOwner flashOwner ranged = resolveAll (ranged (loopBoard flash limb sporemound forest extra limbOwner flashOwner))
        loopedWith = loopedBeside (\_ gs -> gs)
        looped = loopedWith S.alice
        limited = looped S.bob (S.withRange 1)
    Spec.assertEqWith s "CR 801.16 at range 1 dave and erin are still playing" (Game.stillPlaying limited) [S.dave, erin]
    Spec.assertEqWith s "and the game goes on" (GameState.result limited) Nothing
    -- The same board with frank's Aether Flash: it is his triggers in the loop
    -- now, so his neighbour erin draws in carol's place. (An Aether Flash out of
    -- alice's range sees no Saproling enter, CR 801.7, and is in no loop.)
    Spec.assertEqWith s "CR 801.16 with frank's Aether Flash carol and dave are still playing" (Game.stillPlaying (looped frank (S.withRange 1))) [S.carol, S.dave]
    -- The first board with frank's Life and Limb: no trigger of his is in the
    -- loop, but his static ability is what makes each Saproling a land for
    -- Sporemound to see, so his neighbour erin draws too.
    Spec.assertEqWith s "CR 801.16 with frank's Life and Limb only dave is still playing" (Game.stillPlaying (loopedWith frank S.bob (S.withRange 1))) [S.dave]
    -- The same with a second Life and Limb of frank's: either is enough on
    -- its own, so neither is needed alone, but both are objects of frank's in
    -- the loop and erin still draws.
    let secondLimb _ gs = snd (S.addPermanent limb frank gs)
    Spec.assertEqWith s "CR 801.16 with two of frank's Life and Limbs only dave is still playing" (Game.stillPlaying (loopedBeside secondLimb frank S.bob (S.withRange 1))) [S.dave]
    -- The first board with frank's face-up Thelonite Hermit ("All Saprolings
    -- get +1/+1."): each Saproling is a 2/2 that Aether Flash still buries, and
    -- without the Hermit Sporemound and Aether Flash would trigger just the
    -- same, so the Hermit is in no loop and erin plays on.
    let withHermit _ gs = snd (S.addPermanent hermit frank gs)
    Spec.assertEqWith s "CR 801.16 with frank's Thelonite Hermit dave and erin are still playing" (Game.stillPlaying (loopedBeside withHermit S.alice S.bob (S.withRange 1))) [S.dave, erin]
    -- The first board with frank's Necrosynthesis on alice's Sporemound: its
    -- granted "Whenever another creature dies" triggers as each Saproling is
    -- buried, an ability of alice's that exists only through frank's Aura, so
    -- his neighbour erin draws too.
    let withNecrosynthesis sporemoundId gs = let (auraId, g) = S.addPermanent necrosynthesis frank gs in S.attach auraId sporemoundId g
    Spec.assertEqWith s "CR 801.16 with frank's Necrosynthesis on Sporemound only dave is still playing" (Game.stillPlaying (loopedBeside withNecrosynthesis S.alice S.bob (S.withRange 1))) [S.dave]
    -- The first board with frank's Hill Giant, and then with alice's Synthetic
    -- Spore Tithe (Sporemound, plus "and put a +1/+1 counter on each creature
    -- your opponents control") in Sporemound's place: nothing of frank's
    -- triggers or shapes a trigger, but the Tithe acts on his Giant every
    -- cycle, so the Giant is in the loop and his neighbour erin draws too.
    giant <- S.printingOf s registry "Hill Giant"
    tithe <- S.printingOf s registry "Synthetic Spore Tithe"
    let withGiant _ gs = snd (S.addPermanent giant frank gs)
        tithed = resolveAll (S.withRange 1 (loopBoard flash limb tithe forest withGiant S.alice S.bob))
    Spec.assertEqWith s "CR 801.16 with alice's Spore Tithe on frank's Hill Giant only dave is still playing" (Game.stillPlaying tithed) [S.dave]
    Spec.assertEqWith s "CR 801.16 with Sporemound beside frank's Hill Giant dave and erin are still playing" (Game.stillPlaying (loopedBeside withGiant S.alice S.bob (S.withRange 1))) [S.dave, erin]
    Spec.assertEqWith s "CR 104.4b at an unlimited range the game is a draw" (GameState.result (looped S.bob id)) (Just Result.Drawn)

  -- CR 801.16's "involved in that loop", at the guard itself: carol's stamp
  -- predates the gap's later half, which a loop's every cycle reaches, so it
  -- was a lead-in rather than the loop. Only alice's players draw.
  Spec.it s "CR 801.16 a player stamped only before the loop is not in it" $ do
    let limit = Engine.mandatoryLoopLimit
        board =
          (S.withRange 1 S.fourPlayerGame)
            { GameState.nextTimestamp = Timestamp.MkTimestamp limit,
              GameState.lastChoice = Timestamp.MkTimestamp 0,
              GameState.loopInvolvement = Map.fromList [(S.alice, Timestamp.MkTimestamp (limit - 1)), (S.carol, Timestamp.MkTimestamp (div limit 2 - 1))]
            }
        after = S.runPure S.identityAnswer board Engine.checkMandatoryLoop
    Spec.assertEqWith s "CR 801.16 carol, the last one playing, wins" (GameState.result after) (Just (Result.Won S.carol))
  where
    -- Pawl.GameSpec's loopBoard at six seats: alice, active, in her precombat
    -- main phase, with Sporemound and a tapped Forest entering, `limbOwner`'s
    -- Life and Limb, `flashOwner`'s Aether Flash, and whatever `extra` adds
    -- given Sporemound's id. Seeded ten events short of the limit.
    loopBoard flash limb sporemound forest extra limbOwner flashOwner =
      let base = Setup.emptyGame (S.alice NonEmpty.:| [S.bob, S.carol, S.dave, erin, frank])
          (_, gs1) = S.addPermanent flash flashOwner base
          (_, gs2) = S.addPermanent limb limbOwner gs1
          (sporemoundId, gs3) = S.addPermanent sporemound S.alice gs2
          (forestId, gs4) = S.entersWithTrigger forest S.alice (extra sporemoundId gs3)
          seeded =
            gs4
              { GameState.phase = Phase.PrecombatMain,
                GameState.remaining = Seq.empty,
                GameState.nextTimestamp = Timestamp.MkTimestamp (Engine.mandatoryLoopLimit - 10),
                GameState.lastChoice = Timestamp.MkTimestamp 0
              }
       in S.tapObject forestId seeded
    erin = PlayerId.MkPlayerId 4
    frank = PlayerId.MkPlayerId 5
    resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
    -- Pawl.LifeTriggerSpec's entry staging: the permanent is placed, its Moved
    -- event recorded, and the CR 603.6a scan runs at the next settle.
    entering oid gs =
      let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
          staged = S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project oid gs))] gs
       in resolveAll (snd (Engine.runGamePure S.identityAnswer staged Engine.settleForPriority))
    -- A main phase with alice holding priority, for an instant she casts.
    onMain gs = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
    -- CR 510.2: these creatures, attacking `defender` on `active`'s turn, deal
    -- their combat damage.
    strike active attackers defender gs =
      S.runPure
        S.identityAnswer
        gs
          { GameState.activePlayer = active,
            GameState.phase = Phase.Combat CombatStep.CombatDamage,
            GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [defender], Combat.Type.attackers = Map.fromList (fmap (\oid -> (oid, AttackTarget.OfPlayer defender)) attackers)}
          }
        (Monad.void Damage.dealCombatDamage)
    -- CR 503.1: `pid`'s upkeep begins and its triggers resolve.
    upkeepOf pid gs =
      let upkeep = Phase.Beginning BeginningStep.Upkeep
          began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
       in resolveAll (S.runPure S.identityAnswer began Engine.settleForPriority)
