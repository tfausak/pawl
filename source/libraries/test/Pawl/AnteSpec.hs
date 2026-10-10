{-# LANGUAGE GADTs #-}

-- Covers CR 407's ante: Pawl.Engine.Setup's CR 407.2 step, Effect.Ante's CR
-- 407.4 owner check (Pawl.Engine.Resolve.Effect), CR 800.4n in
-- Pawl.Engine.Departure, and Pawl.Engine.Ante's CR 407.3 bar, CR 407.2 payout
-- and CR 108.3 ownership report, with the card identity
-- (Pawl.Types.CardIdentity) the report reads.
module Pawl.AnteSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Ante as Ante
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Interchangeable as Interchangeable
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardIdentity as CardIdentity
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.OutsideDestination as OutsideDestination
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TeamId as TeamId
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.Zone as Zone

anteGame :: GameSettings.GameSettings
anteGame = GameSettings.plain {GameSettings.ante = True}

-- Keeps every hand and records each CR 407.2 draw's candidates, answering with
-- the LAST candidate, pinned by position.
anteDraws :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
anteDraws p = case p of
  Prompt.RandomObject candidates -> do
    State.modify' (NonEmpty.toList candidates :)
    pure (NonEmpty.last candidates)
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  _ -> pure (S.identityAnswer p)

-- The whole of Setup.newGame over this matchup under these settings, with what
-- anteDraws was asked, in order.
startedWith :: GameSettings.GameSettings -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (GameState.GameState, [[ObjectId.ObjectId]])
startedWith settings matchup =
  let ((_, gs), asked) = State.runState (Engine.runGame anteDraws (Setup.gameWith settings (fmap fst matchup)) (Setup.newGame S.performer matchup)) []
   in (gs, reverse asked)

-- Takes every "may" and records who was asked, in order (CR 603.5).
acceptingAll :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
acceptingAll p = case p of
  Prompt.ChooseOptional _ pid _ _ _ _ -> do
    State.modify' (pid :)
    pure OptionalDecision.Exercises
  _ -> pure (S.castAnswer p)

anteOf :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
anteOf = Game.zoneMembers Zone.Ante

ownedBy :: PlayerId.PlayerId -> GameState.GameState -> Int
ownedBy pid gs = Map.size (Map.filter (\obj -> Object.owner obj == pid) (GameState.objects gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Ante" $ do
  -- CR 407.2: one random card from each deck, after the starting player is
  -- determined and before any draw. Ten cards a deck, so a draw asked AFTER the
  -- opening hands would offer three candidates where this one offers ten.
  Spec.it s "CR 407.2 an ante game antes one card from each library before the opening hands" $ do
    mountain <- S.printingOf s registry "Mountain"
    let deck = Deck.fromCards (Map.singleton mountain 10)
        matchup = (S.alice, deck) NonEmpty.:| [(S.bob, deck)]
        (anted, asked) = startedWith anteGame matchup
        (plain, plainAsked) = startedWith GameSettings.plain matchup
    Spec.assertEqWith s "CR 407.2 each player has one card in the ante" (fmap (\pid -> length (anteOf pid anted)) [S.alice, S.bob]) [1, 1]
    Spec.assertEqWith s "CR 407.2 drawn from the whole library, before any draw" (fmap length asked) [10, 10]
    Spec.assertEqWith s "CR 103.5 and the opening hands are still seven" (fmap (\pid -> S.handSize pid anted) [S.alice, S.bob]) [7, 7]
    Spec.assertEqWith s "CR 407.1 a game not played for ante antes nothing" (GameState.ante plain, plainAsked) (Set.empty, [])
  -- An empty library antes nothing and asks nothing; a lone card is not a draw.
  Spec.it s "CR 407.2 an empty library antes nothing and a one-card library is not asked" $ do
    mountain <- S.printingOf s registry "Mountain"
    let matchup = (S.alice, Deck.fromCards (Map.singleton mountain 1)) NonEmpty.:| [(S.bob, Deck.fromCards Map.empty)]
        (anted, asked) = startedWith anteGame matchup
    Spec.assertEqWith s "CR 407.2 alice's one card is in the ante, bob has none" (length (anteOf S.alice anted), length (anteOf S.bob anted)) (1, 0)
    Spec.assertEqWith s "and nobody was asked to draw at random" asked []
  -- Three seats, each library a different size so each draw's candidates name
  -- whose library they came from.
  Spec.it s "CR 407.2 three seats each ante one card of their own, in turn order" $ do
    mountain <- S.printingOf s registry "Mountain"
    let deckOf n = Deck.fromCards (Map.singleton mountain n)
        matchup = (S.alice, deckOf 10) NonEmpty.:| [(S.bob, deckOf 11), (S.carol, deckOf 12)]
        (anted, asked) = startedWith anteGame matchup
    Spec.assertEqWith s "CR 407.2 each seat's own card is in the ante" (fmap (\pid -> length (anteOf pid anted)) [S.alice, S.bob, S.carol]) [1, 1, 1]
    Spec.assertEqWith s "drawn from each seat's own library, in turn order" (fmap length asked) [10, 11, 12]
  -- Two different cards, so WHICH one the draw landed on is readable by printing.
  -- The answer is the last candidate offered: a draw that ignored it and took
  -- the offered set's front antes the other card.
  Spec.it s "CR 407.2 the card the random draw lands on is the one anted" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let bare = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addLibraryCard mountain S.alice bare {GameState.settings = anteGame}
        (_, before) = S.addLibraryCard forest S.alice g1
        drawingLast :: Prompt.Prompt r -> r
        drawingLast p = case p of
          Prompt.RandomObject candidates -> NonEmpty.last candidates
          _ -> S.identityAnswer p
        printingsIn zone gs = fmap (`Game.printingOfObject` gs) (Game.zoneMembers zone S.alice gs)
        after = S.runPure drawingLast before (Setup.anteFromLibraries [S.alice])
    case reverse (printingsIn Zone.Library before) of
      drawn : kept -> do
        Spec.assertEqWith s "CR 407.2 the drawn card is in the ante" (printingsIn Zone.Ante after) [drawn]
        Spec.assertEqWith s "and the other stayed in her library" (printingsIn Zone.Library after) kept
      [] -> Spec.assertFailure s "alice's library is empty"
  -- CR 727.2 rebuilds every card, the anted ones among them, so the restart's
  -- own CR 407.2 step must not see the old ante. Twenty cards a deck: the old
  -- ante card is rebuilt below the eight the restart antes and draws, so a stale
  -- id would still name a library card at the end.
  Spec.it s "CR 727.2/407.2 a restart returns the ante to the libraries and antes one card each again" $ do
    mountain <- S.printingOf s registry "Mountain"
    let deck = Deck.fromCards (Map.singleton mountain 20)
        (anted, _) = startedWith anteGame ((S.alice, deck) NonEmpty.:| [(S.bob, deck)])
        restarted = S.runPure S.identityAnswer anted (Setup.restartGame S.performer Set.empty S.alice)
        inAnte oid = fmap Object.zone (Game.lookupObject oid restarted) == Just Zone.Ante
    Spec.assertEqWith s "CR 407.2 two cards in the ante again, each really there" (Set.size (GameState.ante restarted), all inAnte (Set.toList (GameState.ante restarted))) (2, True)
    Spec.assertEqWith s "CR 727.2 and nobody's cards went missing" (ownedBy S.alice restarted, ownedBy S.bob restarted) (20, 20)
  -- CR 727.1: a restarted game has no winner, so nothing is paid; CR 727.2:
  -- ownership does not change, so a card alice took before the restart (the
  -- write Darkpact makes) is still hers, and still reported as bob's to begin.
  Spec.it s "CR 727.1/727.2 a restart pays nothing, and an ownership change outlives it" $ do
    mountain <- S.printingOf s registry "Mountain"
    let deck = Deck.fromCards (Map.singleton mountain 20)
        (anted, _) = startedWith anteGame ((S.alice, deck) NonEmpty.:| [(S.bob, deck)])
    case anteOf S.bob anted of
      [bobs] -> do
        let taken = Game.setOwner bobs S.alice anted
            restarted = S.runPure S.identityAnswer taken (Setup.restartGame S.performer Set.empty S.alice)
        Spec.assertEqWith s "CR 727.2 the card alice took is reported, bob's to begin and hers now" (Map.elems (Ante.ownershipChanges restarted)) [(S.bob, S.alice)]
        Spec.assertEqWith s "CR 727.1 nobody won the restarted game" (GameState.result restarted) Nothing
        Spec.assertEqWith s "CR 727.2 and nothing else changed hands" (ownedBy S.alice restarted, ownedBy S.bob restarted) (21, 19)
      other -> Spec.assertFailure s ("bob anted " <> show (length other) <> " cards")
  -- CR 729.2 moves the main-game libraries, and a seat whose library is empty
  -- brings nothing to ante.
  Spec.it s "CR 729.2/407.2 a subgame seat with no library antes nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    let parent0 = Setup.gameWith anteGame S.bothPlayers
        parent = snd (S.addLibraryCard mountain S.alice (snd (S.addLibraryCard mountain S.alice parent0)))
        sub0 = Setup.subgameStateFrom S.alice parent
        sub = S.runPure S.identityAnswer sub0 (Setup.startGameFromCards S.performer Set.empty)
    Spec.assertEqWith s "CR 729.2 the subgame starts with no ante of its own" (GameState.ante sub0) Set.empty
    Spec.assertEqWith s "CR 407.2 alice antes one of her two cards, bob nothing" (length (anteOf S.alice sub), length (anteOf S.bob sub)) (1, 0)
  -- CR 800.4n: three seats so bob's concession leaves a game to stay in, and
  -- a card of his in exile that CR 800.4a does take.
  Spec.it s "CR 800.4n a departed player's card in the ante stays in the game" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let g0 = Setup.gameWith anteGame S.threePlayers
        (bobsAnte, g1) = S.addObjectIn Zone.Ante piker S.bob g0
        (bobsExile, g2) = S.addObjectIn Zone.Exile piker S.bob g1
        after = S.departs Departure.Type.Conceded S.bob g2
    Spec.assertEqWith s "CR 800.4n bob's ante card is still in the ante" (Set.member bobsAnte (GameState.ante after), fmap Object.zone (Game.lookupObject bobsAnte after)) (True, Just Zone.Ante)
    Spec.assertEqWith s "CR 800.4a while his exiled card left the game with him" (fmap Object.zone (Game.lookupObject bobsExile after)) Nothing
  -- CR 800.4n / 729.5: bob concedes a subgame played for ante. His subgame
  -- ante card stays behind him (CR 800.4n), and at the subgame's end his
  -- main-game library comes back whole -- once, not with the ante card twice.
  -- Three seats, because a two-seat concession ends the game first.
  Spec.it s "CR 800.4n/729.5 a player who leaves a subgame played for ante gets their main-game library back once" $ do
    mountain <- S.printingOf s registry "Mountain"
    let stock pid gs0 = foldr (\_ gs -> snd (S.addLibraryCard mountain pid gs)) gs0 (replicate 9 ())
        parent = stock S.carol (stock S.bob (stock S.alice (Setup.gameWith anteGame S.threePlayers)))
        sub = S.runPure S.identityAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty)
        left = S.departs Departure.Type.Conceded S.bob sub
        back = Setup.funnelBack left parent
    Spec.assertEqWith s "CR 729.5 bob's main-game library is whole again, and not one card more" (length (Game.zoneMembers Zone.Library S.bob back)) 9
    Spec.assertEqWith s "CR 800.4n bob's subgame ante card stayed behind him" (length (anteOf S.bob left)) 1
  -- CR 407.3 / 729.5: alice takes bob's subgame ante card (Darkpact's write,
  -- through the function its opcode calls), then bob concedes. The card goes
  -- to alice's main-game library, and bob's is rebuilt without it.
  Spec.it s "CR 729.5 a card whose owner changed in a subgame goes home once, to its new owner" $ do
    mountain <- S.printingOf s registry "Mountain"
    let stock pid gs0 = foldr (\_ gs -> snd (S.addLibraryCard mountain pid gs)) gs0 (replicate 9 ())
        parent = stock S.carol (stock S.bob (stock S.alice (Setup.gameWith anteGame S.threePlayers)))
        sub = S.runPure S.identityAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty)
    case anteOf S.bob sub of
      [bobs] -> do
        let back = Setup.funnelBack (S.departs Departure.Type.Conceded S.bob (Game.setOwner bobs S.alice sub)) parent
        Spec.assertEqWith s "CR 729.5 bob's main-game library is rebuilt without it" (length (Game.zoneMembers Zone.Library S.bob back)) 8
        Spec.assertEqWith s "CR 729.5 and alice's holds it" (length (Game.zoneMembers Zone.Library S.alice back)) 10
      other -> Spec.assertFailure s ("bob anted " <> show (length other) <> " cards")
  -- CR 108.3 / 729.4a: a card alice took from bob in the main game (its
  -- identity says bob began the game with it) is wished into a subgame from
  -- her hand. It is the same card in there, so the report still lists it once
  -- it comes home (CR 729.5).
  Spec.it s "CR 729.4a a card brought into a subgame from the main game keeps its identity" $ do
    mountain <- S.printingOf s registry "Mountain"
    bears <- S.printingOf s registry "Grizzly Bears"
    let stock pid gs0 = foldr (\_ gs -> snd (S.addLibraryCard mountain pid gs)) gs0 (replicate 9 ())
        (bearsId, g1) = S.addObjectIn Zone.Hand bears S.alice (stock S.carol (stock S.bob (stock S.alice (Setup.gameWith anteGame S.threePlayers))))
        bobs obj = obj {Object.identity = fmap (\i -> i {CardIdentity.startingOwner = S.bob}) (Object.identity obj)}
        parent = g1 {GameState.objects = Map.adjust bobs bearsId (GameState.objects g1)}
        sub = S.runPure S.identityAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty)
        (_, crossed) = Event.bringInFrom OutsideDestination.Hand S.alice bearsId sub
        back = Setup.funnelBack crossed (Setup.applyCrossings crossed parent)
    Spec.assertEqWith s "CR 108.3 the Bears are still bob's to begin and alice's now" (Map.elems (Ante.ownershipChanges back)) [(S.bob, S.alice)]
  -- CR 800.4n / 729.5: bob wishes his own main-game Jeweled Bird into a
  -- three-seat subgame, antes it, and concedes. The Bird stays in the subgame,
  -- and at its end goes to his main-game library with the rest of his cards.
  Spec.it s "CR 729.5 a departed player's ante card from the main game goes to their main-game library" $ do
    mountain <- S.printingOf s registry "Mountain"
    bird <- S.printingOf s registry "Jeweled Bird"
    let stock pid gs0 = foldr (\_ gs -> snd (S.addLibraryCard mountain pid gs)) gs0 (replicate 9 ())
        (birdId, parent) = S.addObjectIn Zone.Hand bird S.bob (stock S.carol (stock S.bob (stock S.alice (Setup.gameWith anteGame S.threePlayers))))
        sub = S.runPure S.identityAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty)
    case Event.bringInFrom OutsideDestination.Hand S.bob birdId sub of
      (Just (inSub NonEmpty.:| []), crossed) -> do
        let anted = S.departs Departure.Type.Conceded S.bob (S.runPure S.identityAnswer crossed (Event.changeZone inSub Zone.Ante))
            back = Setup.funnelBack anted (Setup.applyCrossings anted parent)
        Spec.assertEqWith s "CR 729.5 bob's main-game library holds his nine and the Bird" (length (Game.zoneMembers Zone.Library S.bob back)) 10
      _ -> Spec.assertFailure s "the Bird was not brought in"
  -- The same from bob's sideboard (CR 400.11a): the Bird goes to his library,
  -- and so is no longer in his pool.
  Spec.it s "CR 729.5 a departed player's ante card from their sideboard goes to their main-game library, not back to the pool" $ do
    mountain <- S.printingOf s registry "Mountain"
    bird <- S.printingOf s registry "Jeweled Bird"
    let stock pid gs0 = foldr (\_ gs -> snd (S.addLibraryCard mountain pid gs)) gs0 (replicate 9 ())
        (birdPrinting, g1) = Game.intern bird (stock S.carol (stock S.bob (stock S.alice (Setup.gameWith anteGame S.threePlayers))))
        pooled p = p {Player.outsideTheGame = Map.singleton birdPrinting 1}
        parent = g1 {GameState.players = Map.adjust pooled S.bob (GameState.players g1)}
        sub = S.runPure S.identityAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty)
        (inSub, brought) = Event.bringIn OutsideDestination.Hand S.bob birdPrinting sub
        anted = S.departs Departure.Type.Conceded S.bob (S.runPure S.identityAnswer brought (Event.changeZone inSub Zone.Ante))
        back = Setup.funnelBack anted (Setup.applyCrossings anted parent)
        poolOf pid gs = foldMap Player.outsideTheGame (Map.lookup pid (GameState.players gs))
    Spec.assertEqWith s "CR 729.5 bob's main-game library holds his nine and the Bird" (length (Game.zoneMembers Zone.Library S.bob back)) 10
    Spec.assertEqWith s "CR 400.11a and his pool no longer does" (poolOf S.bob back) Map.empty
  -- CR 729.5: a subgame played for ante antes one card from each library, and
  -- at its end each still-playing owner's ante card goes into their main-game
  -- library with the rest of their cards; the main game's own ante is untouched.
  Spec.it s "CR 729.5 a subgame ante card goes back to its owner's main-game library" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let stock pid gs0 = foldr (\_ gs -> snd (S.addLibraryCard mountain pid gs)) gs0 (replicate 9 ())
        (mainAnte, parent) = S.addObjectIn Zone.Ante piker S.alice (stock S.bob (stock S.alice (Setup.gameWith anteGame S.bothPlayers)))
        sub = S.runPure S.identityAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty)
        back = Setup.funnelBack sub parent
    Spec.assertEqWith s "CR 729.5 each main-game library is whole again, the anted card in it" (fmap (\pid -> length (Game.zoneMembers Zone.Library pid back)) [S.alice, S.bob]) [9, 9]
    Spec.assertEqWith s "CR 407.2 the subgame had anted one card from each library" (fmap (\pid -> length (anteOf pid sub)) [S.alice, S.bob]) [1, 1]
    Spec.assertEqWith s "CR 729.1a the main game's own ante is as it was" (GameState.ante back) (Set.singleton mainAnte)
  -- CR 800.4n / 727.2: bob's ante card stayed when he left, and a restart
  -- involves every card in the game. bob is not in the new game and has no
  -- library for it, so the card begins the new game where it was.
  Spec.it s "CR 800.4n/727.2 a departed player's ante card stays in the ante through a restart" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (bobsAnte, g1) = S.addObjectIn Zone.Ante piker S.bob (Setup.gameWith anteGame S.threePlayers)
        left = S.departs Departure.Type.Conceded S.bob g1
        restarted = S.runPure S.identityAnswer left (Setup.restartGame S.performer Set.empty S.alice)
    Spec.assertEqWith s "CR 727.2 bob's ante card is still in the ante" (GameState.ante restarted, fmap Object.zone (Game.lookupObject bobsAnte restarted)) (Set.singleton bobsAnte, Just Zone.Ante)
  -- CR 608.2d: "the player can't choose an option that's ... impossible". carol
  -- has no library, so she cannot ante, is not asked, and does not go to 20.
  -- Distinct starting lives so a wrong 20 cannot hide.
  Spec.it s "CR 608.2d Rebirth does not ask a seat with an empty library, and its life stays" $ do
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    rebirth <- S.printingOf s registry "Rebirth"
    let g0 = S.landsFor forest S.alice 6 (Setup.gameWith anteGame S.threePlayers)
        g1 = snd (S.addLibraryCard mountain S.alice (snd (S.addLibraryCard mountain S.alice g0)))
        g2 = snd (S.addLibraryCard mountain S.bob (snd (S.addLibraryCard mountain S.bob g1)))
        (rebirthId, g3) = S.addHandCard rebirth S.alice g2
        lives = Map.fromList [(S.alice, 5 :: Integer), (S.bob, 6), (S.carol, 7)]
        before =
          g3
            { GameState.players = Map.mapWithKey (\pid p -> p {Player.life = Map.findWithDefault (Player.life p) pid lives}) (GameState.players g3),
              GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        run = do
          S.cast S.alice rebirthId
          Stack.resolveTop
        (((), after), asked) = State.runState (Engine.runGame acceptingAll before run) []
    Spec.assertEqWith s "CR 608.2d carol keeps her 7 life" (S.lifeOf S.carol after) (Just 7)
    Spec.assertEqWith s "CR 407.4 alice and bob anted, and went to 20" (S.lifeOf S.alice after, S.lifeOf S.bob after, length (anteOf S.alice after), length (anteOf S.bob after)) (Just 20, Just 20, 1, 1)
    Spec.assertEqWith s "CR 608.2d carol was never asked" (reverse asked) [S.alice, S.bob]
  -- CR 400.1 / 400.3: between an ownership change and the move that follows it
  -- (Tempest Efreet), a card sits in a pile its owner does not hold. It leaves
  -- the pile that holds it and arrives in its owner's.
  Spec.it s "CR 400.1 a card leaves the pile that holds it, not its owner's" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (card, g1) = S.addObjectIn Zone.Graveyard piker S.alice (Setup.gameWith anteGame S.bothPlayers)
        g2 = g1 {GameState.objects = Map.adjust (\obj -> obj {Object.owner = S.bob}) card (GameState.objects g1)}
        after = S.runPure S.identityAnswer g2 (Event.changeZone card Zone.Hand)
    Spec.assertEqWith s "CR 400.1 alice's graveyard holds bob's card" (Game.pileHolderOf card g2) (Just S.alice)
    Spec.assertEqWith s "CR 400.1 alice's graveyard no longer holds it" (Game.zoneMembers Zone.Graveyard S.alice after) []
    Spec.assertEqWith s "CR 400.3 it is in bob's hand" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
  -- CR 400.7 / Tempest Efreet's "from anywhere": a card is followed through
  -- every move it made this turn to the object it is now, and is lost once it
  -- leaves the game (CR 800.4a). Three seats so alice's concession leaves a game.
  Spec.it s "CR 400.7 from anywhere follows a card moved twice, and loses it once it leaves the game" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (start, g1) = S.addObjectIn Zone.Battlefield piker S.alice (Setup.gameWith anteGame S.threePlayers)
        twice = do
          arrived <- Event.changeZoneReturning start Zone.Graveyard
          Monad.forM_ arrived (\gy -> Event.changeZone gy Zone.Exile)
        after = S.runPure S.identityAnswer g1 twice
        zoneNow gs = fmap Object.zone (Game.currentIncarnation start gs >>= \oid -> Game.lookupObject oid gs)
    Spec.assertEqWith s "CR 400.7 it is followed into exile" (zoneNow after) (Just Zone.Exile)
    Spec.assertEqWith s "CR 800.4a and lost once it has left the game" (Game.currentIncarnation start (S.departs Departure.Type.Conceded S.alice after)) Nothing
  -- CR 108.3: every card a game begins with is one card through every move,
  -- and the player who began the game with it is its starting owner. Only
  -- cards carry an identity, and no two share one.
  Spec.it s "CR 108.3 every card a game starts with carries its own identity" $ do
    mountain <- S.printingOf s registry "Mountain"
    let deck = Deck.fromCards (Map.singleton mountain 10)
        (started, _) = startedWith anteGame ((S.alice, deck) NonEmpty.:| [(S.bob, deck)])
        cards = filter (\obj -> case Object.source obj of Source.OfCard _ -> True; _ -> False) (Map.elems (GameState.objects started))
        identities = fmap Object.identity cards
        serials = fmap (fmap CardIdentity.serial) identities
    Spec.assertEqWith s "CR 108.3 each card's starting owner is its owner" (fmap (fmap CardIdentity.startingOwner) identities) (fmap (Just . Object.owner) cards)
    Spec.assertEqWith s "and no two cards share an identity" (Set.size (Set.fromList serials)) 20
  -- A serial is bookkeeping no rule reads, so two Mountains alice began the
  -- game with stay interchangeable and a choice between them is elided. One
  -- bob began the game with does not: the ownership report tells it apart.
  Spec.it s "CR 108.3 two cards differing only in their identity's serial are interchangeable, and not when their starting owners differ" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (a, g1) = S.addObjectIn Zone.Hand mountain S.alice (Setup.gameWith anteGame S.bothPlayers)
        (b, g2) = S.addObjectIn Zone.Hand mountain S.alice g1
        (c, g3) = S.addObjectIn Zone.Hand mountain S.alice g2
        bobs obj = obj {Object.identity = fmap (\i -> i {CardIdentity.startingOwner = S.bob}) (Object.identity obj)}
        g4 = g3 {GameState.objects = Map.adjust bobs c (GameState.objects g3)}
        alike = Interchangeable.objects (Projection.projectAll g4) g4
    Spec.assertEqWith s "two Mountains alice began the game with are interchangeable" (alike a b) True
    Spec.assertEqWith s "one bob began the game with is not" (alike a c) False
  -- CR 104.4a: two players who lose at once draw, and a draw has no winner,
  -- so CR 407.2 pays nobody.
  Spec.it s "CR 104.4a/407.2 a drawn game pays out no ante card" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (alices, g1) = S.addObjectIn Zone.Ante piker S.alice (Setup.gameWith anteGame S.bothPlayers)
        (bobs, g2) = S.addObjectIn Zone.Ante piker S.bob g1
        drawn = S.runPure S.identityAnswer g2 (Departure.leaveGameTogether Departure.Type.Lost [S.alice, S.bob])
        ownerOf oid = fmap Object.owner (Game.lookupObject oid drawn)
    Spec.assertEqWith s "CR 407.2 nobody won, so each ante card keeps its owner" (ownerOf alices, ownerOf bobs) (Just S.alice, Just S.bob)
    Spec.assertEqWith s "CR 104.4a the game is a draw" (GameState.result drawn) (Just Result.Drawn)
  -- bob draws and leaves while alice and carol play on (CR 801.16's partial
  -- draw is a departure); CR 800.4n keeps his ante card in the game, and when
  -- carol concedes, alice, the winner, owns all three.
  Spec.it s "CR 800.4n/407.2 a player who draws leaves their ante card to the eventual winner" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let stake pid (ids, g) = let (oid, g') = S.addObjectIn Zone.Ante piker pid g in (ids <> [oid], g')
        (staked, g1) = stake S.carol (stake S.bob (stake S.alice ([], Setup.gameWith anteGame S.threePlayers)))
        drew = S.runPure S.identityAnswer g1 (Departure.leaveGameTogether Departure.Type.Drew [S.bob])
        won = S.runPure S.identityAnswer drew (Departure.leaveGame Departure.Type.Conceded S.carol)
    Spec.assertEqWith s "CR 407.2 alice, the winner, owns every ante card" (fmap (\oid -> fmap Object.owner (Game.lookupObject oid won)) staked) (replicate 3 (Just S.alice))
    Spec.assertEqWith s "CR 104.2a alice won" (GameState.result won) (Just (Result.Won S.alice))
    Spec.assertEqWith s "and bob's draw had decided nothing" (GameState.result drew) Nothing
  -- alice and bob are a team; carol concedes and the team wins (CR 104.2c).
  -- CR 407.2's one winner says nothing of a team.
  Spec.it s "CR 104.2c a team's win pays out no ante card" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let teamed = anteGame {GameSettings.teams = Teams.MkTeams (Map.fromList [(S.alice, TeamId.MkTeamId 0), (S.bob, TeamId.MkTeamId 0), (S.carol, TeamId.MkTeamId 1)])}
        (carols, g1) = S.addObjectIn Zone.Ante piker S.carol (Setup.gameWith teamed S.threePlayers)
        won = S.runPure S.identityAnswer g1 (Departure.leaveGame Departure.Type.Conceded S.carol)
    Spec.assertEqWith s "carol's ante card is still hers" (fmap Object.owner (Game.lookupObject carols won)) (Just S.carol)
    Spec.assertEqWith s "CR 104.2c the team won" (GameState.result won) (Just (Result.TeamWon (TeamId.MkTeamId 0)))
