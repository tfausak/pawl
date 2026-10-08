{-# LANGUAGE GADTs #-}

-- Covers CR 407's ante: Pawl.Engine.Setup's CR 407.2 step, Effect.Ante's CR
-- 407.4 owner check (Pawl.Engine.Resolve.Effect), CR 800.4n in
-- Pawl.Engine.Departure, and Pawl.Engine.Ante's CR 407.3 bar.
module Pawl.AnteSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
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
