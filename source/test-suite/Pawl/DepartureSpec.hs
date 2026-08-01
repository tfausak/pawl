-- Covers Pawl.Engine.Departure: who is still in the game, and the CR 104.2a/104.3
-- consequences of leaving it.
module Pawl.DepartureSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Zone as Zone

statusOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe Status.Status
statusOf pid gs = fmap Player.status (Map.lookup pid (GameState.players gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Departure" $ do
  Spec.it s "CR 104.3a a conceding player leaves immediately, with Conceded as the reason" $ do
    let gs = Setup.emptyGame S.bothPlayers
        after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
    Spec.assertEq s (statusOf S.alice after) . Just $ Status.Departed Departure.Type.Conceded

  Spec.it s "CR 104.2a the last player standing wins, without waiting for a state-based action check" $ do
    -- leaveGame settles the outcome itself. Nothing runs an SBA pass here.
    let gs = Setup.emptyGame S.bothPlayers
        after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
    Spec.assertEq s (GameState.result after) . Just $ Result.Won S.bob

  Spec.it s "an already-decided result is not overwritten" $ do
    let gs = (Setup.emptyGame S.bothPlayers) {GameState.result = Just Result.Drawn}
        after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
    Spec.assertEq s (GameState.result after) $ Just Result.Drawn

  -- #142: the SAME precedence, through the OTHER door. Pawl.Engine.Sba settles its
  -- own outcome at the end of a state-based-action pass; before this it used
  -- the opposite order from leaveGame, so a pass could replace a result the
  -- game had already reached. Set up a decided draw alongside a player who
  -- would lose to CR 704.5a, so the pass computes a DIFFERENT outcome
  -- (Won bob) and the two orderings disagree about which survives.
  Spec.it s "CR 104.1 a state-based-action pass does not overwrite a decided result" $ do
    let dying player = player {Player.life = 0}
        gs =
          (Setup.emptyGame S.bothPlayers)
            { GameState.result = Just Result.Drawn,
              GameState.players = Map.adjust dying S.alice (GameState.players (Setup.emptyGame S.bothPlayers))
            }
        after = S.runPure S.identityAnswer gs Sba.performStateBasedActions
    Spec.assertEqWith s "the decided draw stands" (GameState.result after) (Just Result.Drawn)
    Spec.assertEqWith s "alice still left the game (CR 704.5a is unaffected)" (statusOf S.alice after) (Just (Status.Departed Departure.Type.Lost))

  Spec.it s "stillPlaying omits a departed player" $ do
    let gs = Setup.emptyGame S.bothPlayers
        after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
    Spec.assertEq s (Game.stillPlaying after) [S.bob]

  Spec.it s "CR 800.4a/800.4m turnOrder is the SEATING roster: a departure does not shorten it" $ do
    -- The whole of M5.6a rests on this. CR 800.4m needs the departed seat to
    -- know when their turn WOULD have begun, and CR 800.4a's last sentence
    -- needs their position to find their successor. Pruning turnOrder makes
    -- both impossible; every read filters through stillPlaying instead.
    let after = S.runPure S.identityAnswer S.threePlayerGame (Departure.leaveGame Departure.Type.Conceded S.bob)
    Spec.assertEqWith s "bob keeps his seat" (GameState.turnOrder after) [S.alice, S.bob, S.carol]
    Spec.assertEqWith s "but is no longer playing" (Game.stillPlaying after) [S.alice, S.carol]

  Spec.it s "CR 104.2a one departure does not decide a three-player game" $ do
    let after = S.runPure S.identityAnswer S.threePlayerGame (Departure.leaveGame Departure.Type.Conceded S.bob)
        andAnother = S.runPure S.identityAnswer after (Departure.leaveGame Departure.Type.Conceded S.carol)
    Spec.assertEqWith s "two survivors, no result" (GameState.result after) Nothing
    Spec.assertEqWith s "one survivor, alice wins" (GameState.result andAnother) (Just (Result.Won S.alice))

  -- CR 725.4: "If the monarch leaves the game, the active player becomes the
  -- monarch at the same time as that player leaves the game."
  Spec.it s "CR 725.4 the monarch departs on someone else's turn: the active player takes the crown" $ do
    let board = S.withMonarch S.bob S.threePlayerGame
        gone = Departure.depart Departure.Type.Conceded S.bob board
    Spec.assertEqWith s "alice is the active player on this board" (GameState.activePlayer board) S.alice
    Spec.assertEqWith s "so alice is the monarch" (GameState.monarch gone) (Just S.alice)

  -- CR 725.4: "If the active player is leaving the game or if there is no
  -- active player, the next player in turn order who can become the monarch
  -- becomes the monarch."
  Spec.it s "CR 725.4 the monarch departs on their own turn: the next seat in turn order takes the crown" $ do
    let board = S.withMonarch S.alice S.threePlayerGame
        gone = Departure.depart Departure.Type.Conceded S.alice board
    Spec.assertEqWith s "bob, the seat after alice's" (GameState.monarch gone) (Just S.bob)

  Spec.it s "CR 725.4 the walk past the active player's seat skips a seat that has already departed" $ do
    -- alice is the active player and has already left, so there is no active
    -- player to crown; carol is the monarch and leaves too. This pins that a
    -- departed active player falls through to the walk at all (the first
    -- assertion) and that the result on THIS three-seat board is bob (the
    -- second). It does NOT pin which seat the walk is anchored on: with only
    -- one still-playing seat left (bob), any starting point in a circular
    -- scan finds it first -- a walk anchored on the DEPARTING MONARCH's seat
    -- instead of the active player's would also land on bob. The four-seat
    -- case below is what actually discriminates the two anchor readings.
    let board = S.withMonarch S.carol (Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame)
        gone = Departure.depart Departure.Type.Conceded S.carol board
    Spec.assertEqWith s "alice is still the active player's seat (CR 800.4j)" (GameState.activePlayer gone) S.alice
    Spec.assertEqWith s "bob takes the crown" (GameState.monarch gone) (Just S.bob)

  -- CR 725.4: the anchor-discriminating case. Four seats, not three: once the
  -- active player and the monarch have both departed, three seats leave only
  -- one still-playing candidate (see the previous case's comment), so any
  -- anchor lands on it. Here, after alice (active) and carol (monarch) both
  -- leave, TWO seats are still playing (bob, dave), and the two readings
  -- disagree: anchored on the ACTIVE player's seat (alice's), the walk is
  -- [bob, carol, dave] and bob is first still playing; anchored on the
  -- DEPARTING MONARCH's seat (carol's) instead, the walk is [dave, alice, bob]
  -- and dave is first still playing. Swapping this module's
  -- `List.break (== active)` for `List.break (== leaving)` makes this fail
  -- with "expected: Just bob, got: Just dave".
  Spec.it s "CR 725.4 four seats: the walk is anchored on the active player's seat, not the departing monarch's" $ do
    let aliceGone = Departure.depart Departure.Type.Conceded S.alice S.fourPlayerGame
        board = S.withMonarch S.carol aliceGone
        gone = Departure.depart Departure.Type.Conceded S.carol board
    Spec.assertEqWith s "bob, the seat after alice's -- dave would be the seat after carol's" (GameState.monarch gone) (Just S.bob)

  -- CR 725.4: "If no player still in the game can become the monarch, the
  -- game continues with no monarch."
  Spec.it s "CR 725.4 the last player standing is the monarch and leaves: no monarch, and no partial head" $ do
    let twoGone =
          Departure.depart
            Departure.Type.Conceded
            S.bob
            (Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame)
        board = S.withMonarch S.carol twoGone
        gone = Departure.depart Departure.Type.Conceded S.carol board
    Spec.assertEqWith s "nobody is left to become the monarch" (GameState.monarch gone) Nothing
    Spec.assertEqWith s "and the roster is untouched" (GameState.turnOrder gone) [S.alice, S.bob, S.carol]

  Spec.it s "CR 800.4a a departing player's objects leave the game, from every zone that can hold one" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (onField, g1) = S.addCreature piker S.bob S.threePlayerGame
        (inHand, g2) = S.addHandCard mountain S.bob g1
        (inLibrary, g3) = S.addLibraryCard mountain S.bob g2
        (inGraveyard, g4) = S.addGraveyardCard mountain S.bob g3
        (onStack, g5) = S.spellOnStack piker S.bob g4
        (aliceKeeps, g6) = S.addCreature piker S.alice g5
        gone = Departure.depart Departure.Type.Conceded S.bob g6
    Spec.assertEqWith s "bob's battlefield permanent is gone" (Game.lookupObject onField gone) Nothing
    Spec.assertEqWith s "bob's hand card is gone" (Game.lookupObject inHand gone) Nothing
    Spec.assertEqWith s "bob's library card is gone" (Game.lookupObject inLibrary gone) Nothing
    Spec.assertEqWith s "bob's graveyard card is gone" (Game.lookupObject inGraveyard gone) Nothing
    Spec.assertEqWith s "bob's spell on the stack is gone" (Game.lookupObject onStack gone) Nothing
    Spec.assertEqWith s "the shared battlefield set no longer names it" (Set.member onField (GameState.battlefield gone)) False
    Spec.assertEqWith s "the stack list no longer names it" (filter (== onStack) (GameState.stack gone)) []
    -- Only the zones this fixture actually PUT something in: Exile and
    -- Command hold nothing here, so including them would pass vacuously --
    -- Game.zoneMembers filters through ownedBy, which is False for a
    -- deleted id regardless of whether it was ever a member.
    Spec.assertEqWith s "nothing of bob's is left in any zone that received an object" (concatMap (\z -> Game.zoneMembers z S.bob gone) [Zone.Library, Zone.Hand, Zone.Graveyard, Zone.Battlefield, Zone.Stack]) []
    Spec.assertEqWith s "and alice's permanent is untouched -- the sweep is keyed to the OWNER" (fmap Object.owner (Game.lookupObject aliceKeeps gone)) (Just S.alice)

  Spec.it s "CR 510.4 a departing player's id is dropped from the struckFirst snapshot" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (onField, g1) = S.addCreature piker S.bob S.threePlayerGame
        snapshotted = g1 {GameState.combat = (GameState.combat g1) {Combat.Type.struckFirst = Just (Set.singleton onField)}}
        gone = Departure.depart Departure.Type.Conceded S.bob snapshotted
    Spec.assertEqWith s "bob's id is pruned from the CR 510.4 first-strike snapshot" (Combat.Type.struckFirst (GameState.combat gone)) (Just Set.empty)

  Spec.it s "CR 725 an exiledUntilMonarch entry KEYED on the departing player's own object is dropped" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (onField, g1) = S.addCreature piker S.bob S.threePlayerGame
        exiled = g1 {GameState.exiledUntilMonarch = Map.singleton onField (MonarchWatch.MkMonarchWatch {MonarchWatch.controller = S.alice, MonarchWatch.lastMonarch = Nothing})}
        gone = Departure.depart Departure.Type.Conceded S.bob exiled
    Spec.assertEqWith s "the entry keyed on bob's own (now-gone) object is dropped" (GameState.exiledUntilMonarch gone) Map.empty

  -- CR 509.1h's last sentence: "A creature remains blocked even if all the
  -- creatures blocking it are removed from combat." The blocker here is
  -- removed from the game entirely, not merely from combat, but the same
  -- rule applies -- the attacker's KEY in Combat.blockers is the record of
  -- blocked-ness (Combat.isBlocked), and Damage.attackerAssignment filters the
  -- gone id out at damage ASSIGNMENT time rather than here.
  Spec.it s "CR 509.1h a blocked attacker stays blocked when its blocker's OWNER departs" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (attacker, g1) = S.addCreature piker S.alice S.threePlayerGame
        (blocker, g2) = S.addCreature piker S.bob g1
        combat =
          (GameState.combat g2)
            { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.bob),
              Combat.Type.blockers = Map.singleton attacker (Set.singleton blocker)
            }
        blocked = g2 {GameState.combat = combat}
        gone = Departure.depart Departure.Type.Conceded S.bob blocked
    Spec.assertEqWith s "the blocker's object is gone (it was bob's)" (Game.lookupObject blocker gone) Nothing
    Spec.assertEqWith s "but the attacker is still recorded as blocked" (Combat.blockersOf attacker gone) (Set.singleton blocker)
    Spec.assertBool s (Combat.isBlocked attacker gone) "Combat.isBlocked agrees"

  Spec.it s "CR 800.1 a two-player game is not a multiplayer game, so CR 800.4a's object removal does not apply" $ do
    -- CR 800.1: "A multiplayer game is a game that begins with more than two
    -- players." CR 104.2a ends a two-player game the moment a player leaves,
    -- so nothing INSIDE that game can see the difference -- but a two-player
    -- SUBGAME is read after it ends. CR 729.5 has each player take the cards
    -- they own in the subgame into their main-game library, and
    -- Setup.funnelBack does that from the finished subgame's object pool. If
    -- the loser's cards left the game, funnelBack would have nothing to
    -- return and they would be destroyed.
    piker <- S.printingOf s registry "Goblin Piker"
    let (onField, twoSeats) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (alsoOnField, threeSeats) = S.addCreature piker S.bob (Setup.emptyGame S.threePlayers)
        twoGone = Departure.depart Departure.Type.Conceded S.bob twoSeats
        threeGone = Departure.depart Departure.Type.Conceded S.bob threeSeats
    Spec.assertEqWith s "two seats: bob's Piker stays in the game" (fmap Object.owner (Game.lookupObject onField twoGone)) (Just S.bob)
    Spec.assertEqWith s "three seats: it does not" (Game.lookupObject alsoOnField threeGone) Nothing
    Spec.assertEqWith s "and the seam agrees" (fmap Departure.continuesAfterDeparture [twoSeats, threeSeats]) [False, True]

  -- CR 800.4a: "any effects which give that player control of any objects or
  -- players end." Mindslaver's opcode is Effect.ControlPlayerNextTurn, whose
  -- effect lives in GameState.pendingControl until the controlled player's
  -- turn begins (CR 723.1b) and in GameState.activeControl during it
  -- (CR 723.1/723.3). Both are effects giving the departing player control of
  -- a PLAYER, so both end.
  Spec.it s "CR 800.4a a Mindslaver controller departing releases their victim, pending and active alike" $ do
    let pending = S.threePlayerGame {GameState.pendingControl = Map.singleton S.carol (Decider.MkDecider S.bob)}
        duringTurn =
          S.threePlayerGame
            { GameState.activePlayer = S.carol,
              GameState.activeControl = Just (Decider.MkDecider S.bob)
            }
        pendingGone = Departure.depart Departure.Type.Conceded S.bob pending
        activeGone = Departure.depart Departure.Type.Conceded S.bob duringTurn
    Spec.assertEqWith s "the scheduled control is gone" (GameState.pendingControl pendingGone) Map.empty
    Spec.assertEqWith s "the live control is gone" (GameState.activeControl activeGone) Nothing
    Spec.assertEqWith s "and a control held by someone still playing is untouched" (GameState.activeControl (Departure.depart Departure.Type.Conceded S.bob (duringTurn {GameState.activeControl = Just (Decider.MkDecider S.alice)}))) (Just (Decider.MkDecider S.alice))

  Spec.it s "CR 800.4a nothing in the game is owned or controlled by a player who has left it" $ do
    -- The postcondition CR 800.4a's four clauses exist to guarantee, on a
    -- board that reaches all of them: bob owns two permanents (a Goblin
    -- Piker and a Mindslaver, both added via S.addCreature) and a spell on
    -- the stack; he has also stolen control of alice's Darksteel Myr, a
    -- permanent she still owns, with a stored SetController.
    piker <- S.printingOf s registry "Goblin Piker"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    mindslaver <- S.printingOf s registry "Mindslaver"
    let (bobsPiker, g1) = S.addCreature piker S.bob S.threePlayerGame
        (aliceMyr, g2) = S.addCreature darksteelMyr S.alice g1
        (bobsSpell, g3) = S.spellOnStack piker S.bob g2
        (bobsSlaver, g4) = S.addCreature mindslaver S.bob g3
        g5 = S.giveControl aliceMyr S.bob g4
        gone = Departure.depart Departure.Type.Conceded S.bob g5
        ownedBy who = Map.keys (Map.filter (\obj -> Object.owner obj == who) (GameState.objects gone))
        controlledBy who = filter (\oid -> Projection.controllerOf oid gone == Just who) (Map.keys (GameState.objects gone))
    Spec.assertBool s (all (\oid -> Map.member oid (GameState.objects g5)) [bobsPiker, bobsSpell, bobsSlaver]) "the fixture put bob's permanent, spell, and Mindslaver into play before he left"
    Spec.assertEqWith s "bob really controlled alice's Myr before he left" (Projection.controllerOf aliceMyr g5) (Just S.bob)
    Spec.assertEqWith s "bob owns nothing" (ownedBy S.bob) []
    Spec.assertEqWith s "bob controls nothing" (controlledBy S.bob) []
    Spec.assertEqWith s "nothing of his is left on the stack -- his spell IS a card, so clause 1 is what removed it" (GameState.stack gone) []
    Spec.assertEqWith s "alice's Myr survived and reverted to her" (Projection.controllerOf aliceMyr gone) (Just S.alice)
    Spec.assertEqWith s "CR 104.2a: two survivors, so the game continues" (GameState.result gone) Nothing

  -- CR 800.4a with control from a STATIC ability (CR 613.1b) rather than a
  -- stored effect -- the third source of control, which the clause-3 and
  -- clause-4 proofs in Pawl.Engine.Departure have to survive. Alice owns AND
  -- controls the Control Magic, so clause 1 carries it out of the game with
  -- her and the static ability goes with its source (CR 611.3b); clause 2
  -- never has to look at it.
  Spec.it s "CR 800.4a: a departing player's own Control Magic leaves with her and releases the creature" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    let (creature, withCreature) = S.addCreature piker S.bob S.threePlayerGame
        (aura, withAura) = S.addCreature controlMagic S.alice withCreature
        attached = S.attach aura creature withAura
        after = Departure.depart Departure.Type.Conceded S.alice attached
    Spec.assertEqWith s "alice controlled it before she left" (Projection.controllerOf creature attached) (Just S.alice)
    Spec.assertEqWith s "the Aura left the game with her" (Game.lookupObject aura after) Nothing
    Spec.assertEqWith s "and bob has his creature back" (Projection.controllerOf creature after) (Just S.bob)
    Spec.assertEqWith s "which is control returning, not an exile -- clause 4 found nothing" (fmap Object.zone (Game.lookupObject creature after)) (Just Zone.Battlefield)

  -- The case clause 1 does NOT cover: the departing player CONTROLS a
  -- control-granting source they do not OWN. Bob has stolen alice's Control
  -- Magic (a stored SetController), and it is enchanting carol's creature, so
  -- CR 109.5's "you" for the Aura's static ability reads as bob. Clause 1
  -- removes nothing here -- bob owns neither object. Clause 2 ends the stored
  -- SetController naming bob, which hands the Aura back to alice, and CR
  -- 611.3a's "isn't locked in" then re-derives the grant to alice. So the
  -- creature is never "still controlled by" bob and clause 4 does not exile
  -- it -- which is the chain nonCardStackObjectsCease and
  -- remainingControlledExiled rely on being closed.
  Spec.it s "CR 800.4a: a departing player's stolen Control Magic reverts to its owner, taking the creature with it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    let (creature, withCreature) = S.addCreature piker S.carol S.threePlayerGame
        (aura, withAura) = S.addCreature controlMagic S.alice withCreature
        attached = S.attach aura creature withAura
        stolen = S.giveControl aura S.bob attached
        after = Departure.depart Departure.Type.Conceded S.bob stolen
    Spec.assertEqWith s "bob controlled the Aura he does not own" (Projection.controllerOf aura stolen) (Just S.bob)
    Spec.assertEqWith s "and so controlled carol's creature through it" (Projection.controllerOf creature stolen) (Just S.bob)
    Spec.assertEqWith s "the Aura is alice's again -- she owns it and clause 1 did not touch it" (Projection.controllerOf aura after) (Just S.alice)
    Spec.assertEqWith s "so the static ability now grants the creature to alice" (Projection.controllerOf creature after) (Just S.alice)
    Spec.assertEqWith s "neither was exiled: nothing was still controlled by bob" (fmap Object.zone (Game.lookupObject aura after), fmap Object.zone (Game.lookupObject creature after)) (Just Zone.Battlefield, Just Zone.Battlefield)
    Spec.assertEqWith s "bob controls nothing" (Projection.controls S.bob after) []
