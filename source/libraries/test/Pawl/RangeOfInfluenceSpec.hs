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
-- worldVictims (CR 801.12); and CR 801.2c's turn-start seating,
-- Pawl.Types.GameState's departedThisTurn.
--
-- FOUR SEATS, at range 1 unless a case says otherwise, turn order [alice, bob,
-- carol, dave]: bob and dave sit next to alice and carol sits two seats away.
-- Three seats would cut nothing, every seat being adjacent there. Each negative
-- is paired with the same board at an unlimited range.
module Pawl.RangeOfInfluenceSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence
import qualified Pawl.Types.Recipient as Recipient

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

  -- CR 801.4 through a target slot's offer. Ravenous Rats, {1}{B} Rat: "When this
  -- creature enters, target opponent discards a card." The offer is the engine's
  -- own output, read as Pawl.TeamSpec's teammate case reads it.
  Spec.it s "CR 801.4 a target opponent slot does not offer an opponent outside the controller's range" $ do
    rats <- S.printingOf s registry "Ravenous Rats"
    swamp <- S.printingOf s registry "Swamp"
    let lands = S.landsFor swamp S.alice 2 S.fourPlayerGame
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
            pure (S.preferring (const True) sets)
          _ -> pure (S.identityAnswer p)
        offered gs = State.execState (Engine.runGame recording (S.runPure S.identityAnswer gs (S.cast S.alice held)) Engine.priorityLoop) []
    Spec.assertEqWith
      s
      "CR 801.4 at range 1 only bob and dave are offered"
      (offered (S.withRange 1 board))
      [[Recipient.ToPlayer S.bob, Recipient.ToPlayer S.dave]]
    Spec.assertEqWith
      s
      "and at an unlimited range carol is too"
      (offered board)
      [[Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol, Recipient.ToPlayer S.dave]]

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

  -- CR 801.2c and its example: bob concedes during alice's turn, and carol,
  -- two seats from alice across bob's emptied seat, stays out of alice's range
  -- for the rest of that turn, then comes into it as the next turn begins. The
  -- Ravenous Rats offer is read as the CR 801.4 case above reads it: alice's on
  -- her own turn, then carol's on hers, which the handoff reaches past bob.
  Spec.it s "CR 801.2c a seat emptied mid-turn closes up only when the next turn begins" $ do
    rats <- S.printingOf s registry "Ravenous Rats"
    swamp <- S.printingOf s registry "Swamp"
    let lands = S.landsFor swamp S.carol 2 (S.landsFor swamp S.alice 2 S.fourPlayerGame)
        (alices, g0) = S.addHandCard rats S.alice lands
        (carols, g1) = S.addHandCard rats S.carol g0
        board =
          S.withRange
            1
            g1
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
        conceded = S.runPure S.identityAnswer board (Departure.leaveGame Departure.Type.Conceded S.bob)
        carolsTurn = S.runPure S.identityAnswer conceded Engine.handoffTurn
        carolsMain = carolsTurn {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.carol}
        recording :: Prompt.Prompt r -> State.State [[Recipient.Recipient]] r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.modify' (<> fmap (Set.toAscList . snd) (Map.elems sets))
            pure (S.preferring (const True) sets)
          _ -> pure (S.identityAnswer p)
        offered pid held gs = State.execState (Engine.runGame recording (S.runPure S.identityAnswer gs (S.cast pid held)) Engine.priorityLoop) []
    Spec.assertEqWith
      s
      "CR 801.2c for the rest of alice's turn carol is still two seats away, so only dave is offered"
      (offered S.alice alices conceded)
      [[Recipient.ToPlayer S.dave]]
    Spec.assertEqWith
      s
      "CR 801.2c from carol's turn on bob's seat has closed up, so alice is offered beside dave"
      (offered S.carol carols carolsMain)
      [[Recipient.ToPlayer S.alice, Recipient.ToPlayer S.dave]]
    Spec.assertEqWith s "the handoff skipped bob's seat" (GameState.activePlayer carolsTurn) S.carol
