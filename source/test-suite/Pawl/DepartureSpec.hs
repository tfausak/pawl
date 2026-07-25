-- Covers Pawl.Departure: who is still in the game, and the CR 104.2a/104.3
-- consequences of leaving it.
module Pawl.DepartureSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Combat as Combat
import qualified Pawl.Departure as Departure
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.Combat as Combat.Type
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.Departure as Departure.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

statusOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe Status.Status
statusOf pid gs = fmap Player.status (Map.lookup pid (GameState.players gs))

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Departure"
    [ HU.testCase "CR 104.3a a conceding player leaves immediately, with Conceded as the reason" $
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "alice departed by conceding" (Just (Status.Departed Departure.Type.Conceded)) (statusOf S.alice after),
      HU.testCase "CR 104.2a the last player standing wins, without waiting for a state-based action check" $
        -- leaveGame settles the outcome itself. Nothing runs an SBA pass here.
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "bob wins on the spot" (Just (Result.Won S.bob)) (GameState.result after),
      HU.testCase "an already-decided result is not overwritten" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.result = Just Result.Drawn}
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "the first result stands" (Just Result.Drawn) (GameState.result after),
      -- #142: the SAME precedence, through the OTHER door. Pawl.Sba settles its
      -- own outcome at the end of a state-based-action pass; before this it used
      -- the opposite order from leaveGame, so a pass could replace a result the
      -- game had already reached. Set up a decided draw alongside a player who
      -- would lose to CR 704.5a, so the pass computes a DIFFERENT outcome
      -- (Won bob) and the two orderings disagree about which survives.
      HU.testCase "CR 104.1 a state-based-action pass does not overwrite a decided result" $
        let dying player = player {Player.life = 0}
            gs =
              (Setup.emptyGame S.bothPlayers)
                { GameState.result = Just Result.Drawn,
                  GameState.players = Map.adjust dying S.alice (GameState.players (Setup.emptyGame S.bothPlayers))
                }
            after = S.runPure S.identityAnswer gs Sba.performStateBasedActions
         in do
              HU.assertEqual "the decided draw stands" (Just Result.Drawn) (GameState.result after)
              HU.assertEqual "alice still left the game (CR 704.5a is unaffected)" (Just (Status.Departed Departure.Type.Lost)) (statusOf S.alice after),
      HU.testCase "stillPlaying omits a departed player" $
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "only bob remains" [S.bob] (Departure.stillPlaying after),
      HU.testCase "CR 800.4a/800.4m turnOrder is the SEATING roster: a departure does not shorten it" $
        -- The whole of M5.6a rests on this. CR 800.4m needs the departed seat to
        -- know when their turn WOULD have begun, and CR 800.4a's last sentence
        -- needs their position to find their successor. Pruning turnOrder makes
        -- both impossible; every read filters through stillPlaying instead.
        let after = S.runPure S.identityAnswer S.threePlayerGame (Departure.leaveGame Departure.Type.Conceded S.bob)
         in do
              HU.assertEqual "bob keeps his seat" [S.alice, S.bob, S.carol] (GameState.turnOrder after)
              HU.assertEqual "but is no longer playing" [S.alice, S.carol] (Departure.stillPlaying after),
      HU.testCase "CR 104.2a one departure does not decide a three-player game" $
        let after = S.runPure S.identityAnswer S.threePlayerGame (Departure.leaveGame Departure.Type.Conceded S.bob)
            andAnother = S.runPure S.identityAnswer after (Departure.leaveGame Departure.Type.Conceded S.carol)
         in do
              HU.assertEqual "two survivors, no result" Nothing (GameState.result after)
              HU.assertEqual "one survivor, alice wins" (Just (Result.Won S.alice)) (GameState.result andAnother),
      -- CR 725.4: "If the monarch leaves the game, the active player becomes the
      -- monarch at the same time as that player leaves the game."
      HU.testCase "CR 725.4 the monarch departs on someone else's turn: the active player takes the crown" $
        let board = S.withMonarch S.bob S.threePlayerGame
            gone = Departure.depart Departure.Type.Conceded S.bob board
         in do
              HU.assertEqual "alice is the active player on this board" S.alice (GameState.activePlayer board)
              HU.assertEqual "so alice is the monarch" (Just S.alice) (GameState.monarch gone),
      -- CR 725.4: "If the active player is leaving the game or if there is no
      -- active player, the next player in turn order who can become the monarch
      -- becomes the monarch."
      HU.testCase "CR 725.4 the monarch departs on their own turn: the next seat in turn order takes the crown" $
        let board = S.withMonarch S.alice S.threePlayerGame
            gone = Departure.depart Departure.Type.Conceded S.alice board
         in HU.assertEqual "bob, the seat after alice's" (Just S.bob) (GameState.monarch gone),
      HU.testCase "CR 725.4 the walk past the active player's seat skips a seat that has already departed" $
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
         in do
              HU.assertEqual "alice is still the active player's seat (CR 800.4j)" S.alice (GameState.activePlayer gone)
              HU.assertEqual "bob takes the crown" (Just S.bob) (GameState.monarch gone),
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
      HU.testCase "CR 725.4 four seats: the walk is anchored on the active player's seat, not the departing monarch's" $
        let aliceGone = Departure.depart Departure.Type.Conceded S.alice S.fourPlayerGame
            board = S.withMonarch S.carol aliceGone
            gone = Departure.depart Departure.Type.Conceded S.carol board
         in HU.assertEqual "bob, the seat after alice's -- dave would be the seat after carol's" (Just S.bob) (GameState.monarch gone),
      -- CR 725.4: "If no player still in the game can become the monarch, the
      -- game continues with no monarch."
      HU.testCase "CR 725.4 the last player standing is the monarch and leaves: no monarch, and no partial head" $
        let twoGone =
              Departure.depart
                Departure.Type.Conceded
                S.bob
                (Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame)
            board = S.withMonarch S.carol twoGone
            gone = Departure.depart Departure.Type.Conceded S.carol board
         in do
              HU.assertEqual "nobody is left to become the monarch" Nothing (GameState.monarch gone)
              HU.assertEqual "and the roster is untouched" [S.alice, S.bob, S.carol] (GameState.turnOrder gone),
      HU.testCase "CR 800.4a a departing player's objects leave the game, from every zone that can hold one" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (onField, g1) = S.addCreature piker S.bob S.threePlayerGame
            (inHand, g2) = S.addHandCard mountain S.bob g1
            (inLibrary, g3) = S.addLibraryCard mountain S.bob g2
            (inGraveyard, g4) = S.addGraveyardCard mountain S.bob g3
            (onStack, g5) = S.spellOnStack piker S.bob g4
            (aliceKeeps, g6) = S.addCreature piker S.alice g5
            gone = Departure.depart Departure.Type.Conceded S.bob g6
        HU.assertEqual "bob's battlefield permanent is gone" Nothing (Game.lookupObject onField gone)
        HU.assertEqual "bob's hand card is gone" Nothing (Game.lookupObject inHand gone)
        HU.assertEqual "bob's library card is gone" Nothing (Game.lookupObject inLibrary gone)
        HU.assertEqual "bob's graveyard card is gone" Nothing (Game.lookupObject inGraveyard gone)
        HU.assertEqual "bob's spell on the stack is gone" Nothing (Game.lookupObject onStack gone)
        HU.assertEqual "the shared battlefield set no longer names it" False (Set.member onField (GameState.battlefield gone))
        HU.assertEqual "the stack list no longer names it" [] (filter (== onStack) (GameState.stack gone))
        -- Only the zones this fixture actually PUT something in: Exile and
        -- Command hold nothing here, so including them would pass vacuously --
        -- Game.zoneMembers filters through ownedBy, which is False for a
        -- deleted id regardless of whether it was ever a member.
        HU.assertEqual "nothing of bob's is left in any zone that received an object" [] (concatMap (\z -> Game.zoneMembers z S.bob gone) [Zone.Library, Zone.Hand, Zone.Graveyard, Zone.Battlefield, Zone.Stack])
        HU.assertEqual "and alice's permanent is untouched -- the sweep is keyed to the OWNER" (Just S.alice) (fmap Object.owner (Game.lookupObject aliceKeeps gone)),
      HU.testCase "CR 510.4 a departing player's id is dropped from the struckFirst snapshot" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (onField, g1) = S.addCreature piker S.bob S.threePlayerGame
            snapshotted = g1 {GameState.combat = (GameState.combat g1) {Combat.Type.struckFirst = Just (Set.singleton onField)}}
            gone = Departure.depart Departure.Type.Conceded S.bob snapshotted
        HU.assertEqual "bob's id is pruned from the CR 510.4 first-strike snapshot" (Just Set.empty) (Combat.Type.struckFirst (GameState.combat gone)),
      HU.testCase "CR 725 an exiledUntilMonarch entry KEYED on the departing player's own object is dropped" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (onField, g1) = S.addCreature piker S.bob S.threePlayerGame
            exiled = g1 {GameState.exiledUntilMonarch = Map.singleton onField S.alice}
            gone = Departure.depart Departure.Type.Conceded S.bob exiled
        HU.assertEqual "the entry keyed on bob's own (now-gone) object is dropped" Map.empty (GameState.exiledUntilMonarch gone),
      -- CR 509.1h's last sentence: "A creature remains blocked even if all the
      -- creatures blocking it are removed from combat." The blocker here is
      -- removed from the game entirely, not merely from combat, but the same
      -- rule applies -- Damage.attackerAssignment reads Combat.blockers as the
      -- record of blocked-ness and prunes it only at damage ASSIGNMENT time.
      HU.testCase "CR 509.1h a blocked attacker stays blocked when its blocker's OWNER departs" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (attacker, g1) = S.addCreature piker S.alice S.threePlayerGame
            (blocker, g2) = S.addCreature piker S.bob g1
            combat =
              (GameState.combat g2)
                { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.bob),
                  Combat.Type.blockers = Map.singleton attacker (Set.singleton blocker)
                }
            blocked = g2 {GameState.combat = combat}
            gone = Departure.depart Departure.Type.Conceded S.bob blocked
        HU.assertEqual "the blocker's object is gone (it was bob's)" Nothing (Game.lookupObject blocker gone)
        HU.assertEqual "but the attacker is still recorded as blocked" (Set.singleton blocker) (Combat.blockersOf attacker gone)
        HU.assertBool "Combat.isBlocked agrees" (Combat.isBlocked attacker gone),
      HU.testCase "CR 800.1 a two-player game is not a multiplayer game, so CR 800.4a's object removal does not apply" $ do
        -- CR 800.1: "A multiplayer game is a game that begins with more than two
        -- players." CR 104.2a ends a two-player game the moment a player leaves,
        -- so nothing INSIDE that game can see the difference -- but a two-player
        -- SUBGAME is read after it ends. CR 729.5 has each player take the cards
        -- they own in the subgame into their main-game library, and
        -- Setup.funnelBack does that from the finished subgame's object pool. If
        -- the loser's cards left the game, funnelBack would have nothing to
        -- return and they would be destroyed.
        piker <- Registry.printing registry "Goblin Piker"
        let (onField, twoSeats) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            (alsoOnField, threeSeats) = S.addCreature piker S.bob (Setup.emptyGame S.threePlayers)
            twoGone = Departure.depart Departure.Type.Conceded S.bob twoSeats
            threeGone = Departure.depart Departure.Type.Conceded S.bob threeSeats
        HU.assertEqual "two seats: bob's Piker stays in the game" (Just S.bob) (fmap Object.owner (Game.lookupObject onField twoGone))
        HU.assertEqual "three seats: it does not" Nothing (Game.lookupObject alsoOnField threeGone)
        HU.assertEqual "and the seam agrees" [False, True] (fmap Departure.continuesAfterDeparture [twoSeats, threeSeats]),
      -- CR 800.4a: "any effects which give that player control of any objects or
      -- players end." Mindslaver's opcode is Effect.ControlPlayerNextTurn, whose
      -- effect lives in GameState.pendingControl until the controlled player's
      -- turn begins (CR 723.1b) and in GameState.activeControl during it
      -- (CR 723.1/723.3). Both are effects giving the departing player control of
      -- a PLAYER, so both end.
      HU.testCase "CR 800.4a a Mindslaver controller departing releases their victim, pending and active alike" $
        let pending = S.threePlayerGame {GameState.pendingControl = Map.singleton S.carol (Decider.MkDecider S.bob)}
            duringTurn =
              S.threePlayerGame
                { GameState.activePlayer = S.carol,
                  GameState.activeControl = Just (Decider.MkDecider S.bob)
                }
            pendingGone = Departure.depart Departure.Type.Conceded S.bob pending
            activeGone = Departure.depart Departure.Type.Conceded S.bob duringTurn
         in do
              HU.assertEqual "the scheduled control is gone" Map.empty (GameState.pendingControl pendingGone)
              HU.assertEqual "the live control is gone" Nothing (GameState.activeControl activeGone)
              HU.assertEqual "and a control held by someone still playing is untouched" (Just (Decider.MkDecider S.alice)) (GameState.activeControl (Departure.depart Departure.Type.Conceded S.bob (duringTurn {GameState.activeControl = Just (Decider.MkDecider S.alice)}))),
      HU.testCase "CR 800.4a nothing in the game is owned or controlled by a player who has left it" $ do
        -- The postcondition CR 800.4a's four clauses exist to guarantee, on a
        -- board that reaches all of them: bob owns two permanents (a Goblin
        -- Piker and a Mindslaver, both added via S.addCreature) and a spell on
        -- the stack; he has also stolen control of alice's Darksteel Myr, a
        -- permanent she still owns, with a stored SetController.
        piker <- Registry.printing registry "Goblin Piker"
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        mindslaver <- Registry.printing registry "Mindslaver"
        let (bobsPiker, g1) = S.addCreature piker S.bob S.threePlayerGame
            (aliceMyr, g2) = S.addCreature darksteelMyr S.alice g1
            (bobsSpell, g3) = S.spellOnStack piker S.bob g2
            (bobsSlaver, g4) = S.addCreature mindslaver S.bob g3
            g5 = S.giveControl aliceMyr S.bob g4
            gone = Departure.depart Departure.Type.Conceded S.bob g5
            ownedBy who = Map.keys (Map.filter (\obj -> Object.owner obj == who) (GameState.objects gone))
            controlledBy who = filter (\oid -> Projection.controllerOf oid gone == Just who) (Map.keys (GameState.objects gone))
        HU.assertBool "the fixture put bob's permanent, spell, and Mindslaver into play before he left" (all (\oid -> Map.member oid (GameState.objects g5)) [bobsPiker, bobsSpell, bobsSlaver])
        HU.assertEqual "bob really controlled alice's Myr before he left" (Just S.bob) (Projection.controllerOf aliceMyr g5)
        HU.assertEqual "bob owns nothing" [] (ownedBy S.bob)
        HU.assertEqual "bob controls nothing" [] (controlledBy S.bob)
        HU.assertEqual "nothing of his is left on the stack -- his spell IS a card, so clause 1 is what removed it" [] (GameState.stack gone)
        HU.assertEqual "alice's Myr survived and reverted to her" (Just S.alice) (Projection.controllerOf aliceMyr gone)
        HU.assertEqual "CR 104.2a: two survivors, so the game continues" Nothing (GameState.result gone)
    ]
