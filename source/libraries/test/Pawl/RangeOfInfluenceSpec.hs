{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

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
-- playerRefPlayers, zoneScopePlayers and battlefieldMatching, Pawl.Engine.Count's
-- playersFor and the choice offers Game.reachableBy feeds (CR 801.5a, 801.10,
-- 801.11), and Pawl.Engine.Resolve.Effect's WinGame and DrawGame (CR 801.14,
-- 801.15); and CR 801.2c's turn-start seating, Pawl.Types.GameState's
-- departedThisTurn.
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
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.StepBegan as StepBegan
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

  -- CR 801.7 for a player the event involves: alice's Exquisite Blood
  -- ("Whenever an opponent loses life, you gain that much life.") while carol,
  -- two seats away, pays 2 life for her own Greed ("{B}, Pay 2 life: Draw a
  -- card.") -- a loss nothing of alice's causes, so CR 801.10 has no part in it.
  -- Bob, one seat away, pays for his own on the same board.
  Spec.it s "CR 801.7 a triggered ability does not trigger on a player outside its controller's range" $ do
    swamp <- S.printingOf s registry "Swamp"
    blood <- S.printingOf s registry "Exquisite Blood"
    greed <- S.printingOf s registry "Greed"
    case Face.activatedAbilities (S.combinedFace greed) of
      [] -> Spec.assertFailure s "Greed should carry an activated ability"
      ability : _ -> do
        let (_, g0) = S.addPermanent blood S.alice S.fourPlayerGame
            stock pid gs =
              let (_, withSwamp) = S.addPermanent swamp pid gs
                  (greedId, withGreed) = S.addPermanent greed pid withSwamp
                  (_, withLibrary) = S.addLibraryCard swamp pid withGreed
               in (greedId, withLibrary)
            (bobsGreed, g1) = stock S.bob g0
            (carolsGreed, g2) = stock S.carol g1
            board = g2 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
            pays pid greedId gs = resolveAll (S.runPure S.identityAnswer gs (Activate.activateAbility pid greedId ability))
            carolPaid = pays S.carol carolsGreed (S.withRange 1 board)
        Spec.assertEqWith s "carol really paid 2 at range 1" (S.lifeOf S.carol carolPaid) (Just 18)
        Spec.assertEqWith s "CR 801.7 so alice gains nothing" (S.lifeOf S.alice carolPaid) (Just 20)
        Spec.assertEqWith s "at an unlimited range she gains 2" (S.lifeOf S.alice (pays S.carol carolsGreed board)) (Just 22)
        Spec.assertEqWith s "and at range 1 bob's payment, in range, gains her 2" (S.lifeOf S.alice (pays S.bob bobsGreed (S.withRange 1 board))) (Just 22)
        -- CR 801.7a: bob pays his last 2 life and leaves the game to CR 704.5a
        -- before the trigger is put on the stack, but he was in range as he
        -- lost it.
        let lastLife = board {GameState.players = Map.adjust (\p -> p {Player.life = 2}) S.bob (GameState.players board)}
            bobLeft = pays S.bob bobsGreed (S.withRange 1 lastLife)
        Spec.assertBool s (notElem S.bob (Game.stillPlaying bobLeft)) "bob lost the game"
        Spec.assertEqWith s "CR 801.7a and alice still gains the 2 he lost" (S.lifeOf S.alice bobLeft) (Just 22)

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

  -- CR 801.7 on a CR 603.7 delayed ability: alice's False Cure ("Until end of
  -- turn, whenever a player gains life, that player loses 2 life for each 1
  -- life they gained.") while carol's Radiant Fountain ("When this land enters,
  -- you gain 2 life.") enters under her.
  Spec.it s "CR 801.7 a delayed triggered ability does not trigger outside its controller's range" $ do
    swamp <- S.printingOf s registry "Swamp"
    falseCure <- S.printingOf s registry "False Cure"
    fountain <- S.printingOf s registry "Radiant Fountain"
    let lands = S.landsFor swamp S.alice 2 S.fourPlayerGame
        (spellId, g0) = S.addHandCard falseCure S.alice lands
        board = g0 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
        carolAfter gs =
          let armed = resolveAll (S.runPure S.identityAnswer gs (S.cast S.alice spellId))
              (entrant, placed) = S.addPermanent fountain S.carol armed
           in S.lifeOf S.carol (entering entrant placed)
    Spec.assertEqWith s "CR 801.7 at range 1 carol keeps the 2 she gained" (carolAfter (S.withRange 1 board)) (Just 22)
    Spec.assertEqWith s "at an unlimited range she loses 4 for them" (carolAfter board) (Just 18)

  -- CR 801.13b's third case: alice's Fog ("Prevent all combat damage that would
  -- be dealt this turn.") names neither source nor recipient, so it reaches
  -- only damage whose source and recipient are both in alice's range. Carol's
  -- Goblin Piker attacks dave; bob's attacks alice, then carol.
  Spec.it s "CR 801.13b a prevention naming neither source nor recipient needs both in range" $ do
    forest <- S.printingOf s registry "Forest"
    fog <- S.printingOf s registry "Fog"
    piker <- S.printingOf s registry "Goblin Piker"
    let (spellId, g0) = S.addHandCard fog S.alice (S.landsFor forest S.alice 1 S.fourPlayerGame)
        (carols, g1) = S.addPermanent piker S.carol g0
        (bobs, g2) = S.addPermanent piker S.bob g1
        fogged ranged = castResolved S.identityAnswer S.alice spellId (ranged (onMain g2))
    Spec.assertEqWith s "CR 801.13b at range 1 carol's Piker still deals dave 2" (S.lifeOf S.dave (strike S.carol [carols] S.dave (fogged (S.withRange 1)))) (Just 18)
    Spec.assertEqWith s "at an unlimited range Fog prevents it" (S.lifeOf S.dave (strike S.carol [carols] S.dave (fogged id))) (Just 20)
    Spec.assertEqWith s "and at range 1 it prevents bob's, in range, to alice" (S.lifeOf S.alice (strike S.bob [bobs] S.alice (fogged (S.withRange 1)))) (Just 20)
    Spec.assertEqWith s "but not bob's to carol, whom alice does not reach" (S.lifeOf S.carol (strike S.bob [bobs] S.carol (fogged (S.withRange 1)))) (Just 18)

  -- CR 801.13b's first case: alice's Luminesce ("Prevent all damage that black
  -- sources and red sources would deal this turn.") names the source, so an
  -- in-range source is enough. Bob's red Piker attacks carol, whom alice does
  -- not reach; carol's attacks dave.
  Spec.it s "CR 801.13b a prevention naming the source needs only the source in range" $ do
    plains <- S.printingOf s registry "Plains"
    luminesce <- S.printingOf s registry "Luminesce"
    piker <- S.printingOf s registry "Goblin Piker"
    let (spellId, g0) = S.addHandCard luminesce S.alice (S.landsFor plains S.alice 1 S.fourPlayerGame)
        (carols, g1) = S.addPermanent piker S.carol g0
        (bobs, g2) = S.addPermanent piker S.bob g1
        cast ranged = castResolved S.identityAnswer S.alice spellId (ranged (onMain g2))
    Spec.assertEqWith s "CR 801.13b at range 1 bob's Piker deals carol nothing" (S.lifeOf S.carol (strike S.bob [bobs] S.carol (cast (S.withRange 1)))) (Just 20)
    Spec.assertEqWith s "but carol's, out of range, deals dave 2" (S.lifeOf S.dave (strike S.carol [carols] S.dave (cast (S.withRange 1)))) (Just 18)
    Spec.assertEqWith s "at an unlimited range it deals him nothing" (S.lifeOf S.dave (strike S.carol [carols] S.dave (cast id))) (Just 20)

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

  -- CR 801.13a: alice's Turn the Tables ("All combat damage that would be dealt
  -- to you this turn is dealt to target attacking creature instead.") targets
  -- one of bob's two attacking Goblin Pikers, and carol then gains control of
  -- it, taking it out of combat and out of alice's range. The other Piker's
  -- damage has nowhere in range to go, so it stays on alice.
  Spec.it s "CR 801.13a a redirection to a destination outside its controller's range does nothing" $ do
    plains <- S.printingOf s registry "Plains"
    tables <- S.printingOf s registry "Turn the Tables"
    piker <- S.printingOf s registry "Goblin Piker"
    let (spellId, g0) = S.addHandCard tables S.alice (S.landsFor plains S.alice 5 S.fourPlayerGame)
        (aimed, g1) = S.addPermanent piker S.bob g0
        (other, g2) = S.addPermanent piker S.bob g1
        attacking =
          (onMain g2)
            { GameState.activePlayer = S.bob,
              GameState.phase = Phase.Combat CombatStep.DeclareBlockers,
              GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.alice], Combat.Type.attackers = Map.fromList [(aimed, AttackTarget.OfPlayer S.alice), (other, AttackTarget.OfPlayer S.alice)]}
            }
        aim :: Prompt.Prompt r -> r
        aim p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature aimed))) sets
          _ -> S.identityAnswer p
        stolen ranged = S.giveControl aimed S.carol (castResolved aim S.alice spellId (ranged attacking))
        after ranged = S.settleSba (strike S.bob [other] S.alice (stolen ranged))
    Spec.assertEqWith s "CR 801.13a at range 1 alice takes the 2" (S.lifeOf S.alice (after (S.withRange 1))) (Just 18)
    Spec.assertBool s (S.onBattlefield aimed (after (S.withRange 1))) "and carol's Piker is untouched"
    Spec.assertEqWith s "at an unlimited range alice takes nothing" (S.lifeOf S.alice (after id)) (Just 20)
    Spec.assertBool s (not (S.onBattlefield aimed (after id))) "and the 2 kills carol's Piker"

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

  -- CR 801.10 for objects and for players: alice's Day of Judgment ("Destroy all
  -- creatures.") over a Goblin Piker each for bob, one seat away, and carol, two
  -- seats away, and over her own Zulaport Cutthroat ("Whenever this creature or
  -- another creature you control dies, each opponent loses 1 life and you gain 1
  -- life.").
  Spec.it s "CR 801.10 a resolving spell or ability does not affect an object or player outside its controller's range" $ do
    plains <- S.printingOf s registry "Plains"
    judgment <- S.printingOf s registry "Day of Judgment"
    piker <- S.printingOf s registry "Goblin Piker"
    cutthroat <- S.printingOf s registry "Zulaport Cutthroat"
    let (spellId, g0) = S.addHandCard judgment S.alice (S.landsFor plains S.alice 4 S.fourPlayerGame)
        (_, g1) = S.addPermanent cutthroat S.alice g0
        (bobs, g2) = S.addPermanent piker S.bob g1
        (carols, g3) = S.addPermanent piker S.carol g2
        cast ranged = castResolved S.identityAnswer S.alice spellId (ranged (onMain g3))
        limited = cast (S.withRange 1)
    Spec.assertBool s (S.onBattlefield carols limited) "CR 801.10 at range 1 carol's Piker survives"
    Spec.assertBool s (not (S.onBattlefield bobs limited)) "and bob's, in range, is destroyed"
    Spec.assertBool s (not (S.onBattlefield carols (cast id))) "at an unlimited range carol's is destroyed too"
    Spec.assertEqWith s "CR 801.10 at range 1 the Cutthroat costs carol nothing" (S.lifeOf S.carol limited) (Just 20)
    Spec.assertEqWith s "and bob, in range, 1" (S.lifeOf S.bob limited) (Just 19)
    Spec.assertEqWith s "at an unlimited range it costs carol 1" (S.lifeOf S.carol (cast id)) (Just 19)

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

  -- CR 801.11: alice's Malignus ("Malignus's power and toughness are each equal
  -- to half the highest life total among your opponents, rounded up.") while
  -- carol, two seats away, is at 40 and bob and dave, one seat away, at 20.
  Spec.it s "CR 801.11 an ability gets no information from outside its controller's range" $ do
    malignus <- S.printingOf s registry "Malignus"
    let (creature, g0) = S.addPermanent malignus S.alice S.fourPlayerGame
        board = g0 {GameState.players = Map.adjust (\p -> p {Player.life = 40}) S.carol (GameState.players g0)}
    Spec.assertEqWith s "CR 801.11 at range 1 Malignus reads only bob's and dave's 20" (S.powerToughnessOf creature (S.withRange 1 board)) (Just (10, 10))
    Spec.assertEqWith s "at an unlimited range it reads carol's 40" (S.powerToughnessOf creature board) (Just (20, 20))

  -- CR 801.5a for a player: alice casts True-Name Nemesis ("As this creature
  -- enters, choose a player."), and the choice offers only the players in her
  -- range.
  Spec.it s "CR 801.5a a choice of player offers only players within the chooser's range" $ do
    nemesis <- S.printingOf s registry "True-Name Nemesis"
    island <- S.printingOf s registry "Island"
    let (spellId, board) = S.addHandCard nemesis S.alice (S.landsFor island S.alice 3 S.fourPlayerGame)
        recording :: Prompt.Prompt r -> State.State [[PlayerId.PlayerId]] r
        recording p = case p of
          Prompt.ChoosePlayer _ _ _ offer -> State.modify' (<> [NonEmpty.toList offer]) >> pure (NonEmpty.head offer)
          _ -> pure (S.identityAnswer p)
        offered gs = State.execState (Engine.runGame recording (S.runPure S.identityAnswer (onMain gs) (S.cast S.alice spellId)) Engine.priorityLoop) []
    Spec.assertEqWith s "CR 801.5a at range 1 carol is not offered" (offered (S.withRange 1 board)) [[S.alice, S.bob, S.dave]]
    Spec.assertEqWith s "at an unlimited range she is" (offered board) [[S.alice, S.bob, S.carol, S.dave]]

  -- CR 801.5a for an opponent: alice's Pulling Teeth ("Clash with an opponent.
  -- ...") offers only the opponents in her range.
  Spec.it s "CR 801.5a a choice of opponent offers only opponents within the chooser's range" $ do
    swamp <- S.printingOf s registry "Swamp"
    teeth <- S.printingOf s registry "Pulling Teeth"
    let (spellId, board) = S.addHandCard teeth S.alice (S.landsFor swamp S.alice 2 S.fourPlayerGame)
        recording :: Prompt.Prompt r -> State.State [[PlayerId.PlayerId]] r
        recording p = case p of
          Prompt.ChooseOpponent _ _ _ offer -> State.modify' (<> [NonEmpty.toList offer]) >> pure (NonEmpty.head offer)
          _ -> pure (S.identityAnswer p)
        offered gs = State.execState (Engine.runGame recording (S.runPure S.identityAnswer (onMain gs) (S.cast S.alice spellId)) Engine.priorityLoop) []
    Spec.assertEqWith s "CR 801.5a at range 1 carol is not offered" (offered (S.withRange 1 board)) [[S.bob, S.dave]]
    Spec.assertEqWith s "at an unlimited range she is" (offered board) [[S.bob, S.carol, S.dave]]

  -- CR 801.10 for proliferate: alice's Steady Progress ("Proliferate. Draw a
  -- card.") while bob, one seat away, and carol, two seats away, each have a
  -- poison counter.
  Spec.it s "CR 801.10 proliferate offers only players within its controller's range" $ do
    island <- S.printingOf s registry "Island"
    progress <- S.printingOf s registry "Steady Progress"
    let (spellId, g0) = S.addHandCard progress S.alice (S.landsFor island S.alice 3 S.fourPlayerGame)
        (_, g1) = S.addLibraryCard island S.alice g0
        poisoned = Set.fromList [S.bob, S.carol]
        board = g1 {GameState.players = Map.mapWithKey (\pid p -> if Set.member pid poisoned then p {Player.counters = Map.singleton PlayerCounterKind.Poison 1} else p) (GameState.players g1)}
        recording :: Prompt.Prompt r -> State.State [[PlayerId.PlayerId]] r
        recording p = case p of
          Prompt.ChooseProliferate _ _ _ players -> State.modify' (<> [players]) >> pure (Set.empty, Set.fromList players)
          _ -> pure (S.identityAnswer p)
        offered gs = State.execState (Engine.runGame recording (S.runPure S.identityAnswer (onMain gs) (S.cast S.alice spellId)) Engine.priorityLoop) []
    Spec.assertEqWith s "CR 801.10 at range 1 only bob is offered" (offered (S.withRange 1 board)) [[S.bob]]
    Spec.assertEqWith s "at an unlimited range carol is too" (offered board) [[S.bob, S.carol]]

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

  -- CR 104.4c / 801.15: alice casts Divine Intervention ("This enchantment enters
  -- with two intervention counters on it. At the beginning of your upkeep, remove
  -- an intervention counter from this enchantment. When you remove the last
  -- intervention counter from this enchantment, the game is a draw.") and two of
  -- her upkeeps pass. At range 1 the draw takes alice, bob and dave out; carol,
  -- the last one playing, wins by CR 104.2a.
  Spec.it s "CR 801.15 a draw is a draw for its controller and the players within their range" $ do
    plains <- S.printingOf s registry "Plains"
    intervention <- S.printingOf s registry "Divine Intervention"
    let (spellId, g0) = S.addHandCard intervention S.alice (S.landsFor plains S.alice 8 S.fourPlayerGame)
        played ranged = castResolved S.identityAnswer S.alice spellId (ranged (onMain g0))
        twice gs = upkeepOf S.alice (upkeepOf S.alice gs)
        limited = twice (played (S.withRange 1))
    Spec.assertEqWith s "one upkeep leaves the game running" (GameState.result (upkeepOf S.alice (played id))) Nothing
    Spec.assertEqWith s "CR 801.15 at range 1 only carol is still playing" (Game.stillPlaying limited) [S.carol]
    Spec.assertEqWith s "CR 104.4c at an unlimited range the game is a draw" (GameState.result (twice (played id))) (Just Result.Drawn)
  where
    resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
    -- Pawl.LifeTriggerSpec's entry staging: the permanent is placed, its Moved
    -- event recorded, and the CR 603.6a scan runs at the next settle.
    entering oid gs =
      let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
          staged = S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project oid gs))] gs
       in resolveAll (snd (Engine.runGamePure S.identityAnswer staged Engine.settleForPriority))
    -- A main phase with alice holding priority, for an instant she casts.
    onMain gs = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
    castResolved :: (forall r. Prompt.Prompt r -> r) -> PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
    castResolved answer pid spellId gs = snd (Engine.runGamePure answer (S.runPure answer gs (S.cast pid spellId)) Engine.priorityLoop)
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
