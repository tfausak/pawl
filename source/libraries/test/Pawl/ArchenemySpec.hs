{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | CR 904, the Archenemy variant: Pawl.Engine.Archenemy and Pawl.Engine.Scheme,
-- with Pawl.Types.TriggerCondition's SetInMotion and Pawl.Types.Effect's
-- Abandon.
module Pawl.ArchenemySpec where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Archenemy as Archenemy
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Archenemy" $ do
  -- CR 904.3 / 904.4 / 904.5 through the whole of Setup.newGame: the scheme
  -- deck starts in the command zone, face down, and its owner at 40 life.
  Spec.it s "CR 904.5 the archenemy starts at 40 life with the scheme deck face down" $ do
    forest <- S.printingOf s registry "Forest"
    look <- S.printingOf s registry "Look Skyward and Despair"
    soil <- S.printingOf s registry "The Very Soil Shall Shake"
    let alices = (Deck.fromCards (Map.singleton forest 20)) {Deck.schemes = Map.fromList [(look, 2), (soil, 1)]}
        bobs = Deck.fromCards (Map.singleton forest 20)
        started = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.newGame (Resolve.performHandAction Resolve.noSubgame) ((S.alice, alices) NonEmpty.:| [(S.bob, bobs)]))
    Spec.assertEqWith s "CR 904.5 alice has 40 life and bob 20" (S.lifeOf S.alice started, S.lifeOf S.bob started) (Just 40, Just 20)
    Spec.assertEqWith s "CR 904.4 all three schemes are in her scheme deck" (length (Archenemy.deckOf S.alice started)) 3
    Spec.assertEqWith s "none is face up" (Archenemy.faceUp started) []
    Spec.assertEqWith s "and none is in her library" (length (Game.zoneMembers Zone.Library S.alice started)) 13

  -- CR 904.6 / 904.2 through Setup.newGame: carol, seated last, brings the only
  -- scheme deck. The pair differs only in bob bringing one too, CR 904.12's
  -- Supervillain Rumble: a free-for-all from the head of the order.
  Spec.it s "CR 904.6 the archenemy takes the first turn, and CR 904.2 the rest are one team sharing turns" $ do
    forest <- S.printingOf s registry "Forest"
    look <- S.printingOf s registry "Look Skyward and Despair"
    let plain = Deck.fromCards (Map.singleton forest 20)
        schemer = plain {Deck.schemes = Map.singleton look 2}
        started bobs = S.runPure S.identityAnswer (Setup.emptyGame S.threePlayers) (Setup.newGame (Resolve.performHandAction Resolve.noSubgame) ((S.alice, plain) NonEmpty.:| [(S.bob, bobs), (S.carol, schemer)]))
        sides gs = (fmap (`Game.opponentsOf` gs) [S.alice, S.bob, S.carol], Turn.sharesTurn gs S.alice S.bob)
    Spec.assertEqWith s "CR 904.6 carol takes the first turn, the cyclic order kept" (GameState.activePlayer (started plain), GameState.turnOrder (started plain)) (S.carol, [S.carol, S.alice, S.bob])
    Spec.assertEqWith s "CR 904.2 alice and bob are teammates sharing turns, each carol's opponent" (sides (started plain)) ([[S.carol], [S.carol], [S.alice, S.bob]], True)
    Spec.assertEqWith s "CR 904.12c with two archenemies alice starts a free-for-all" (GameState.activePlayer (started schemer), sides (started schemer)) (S.alice, ([[S.bob, S.carol], [S.alice, S.carol], [S.alice, S.bob]], False))

  -- CR 904.13b / 904.13c through Setup.newGame: a three-seat Commander game in
  -- which alice brings the scheme deck and bob and carol are the opposing team,
  -- the teams CR 904.2 derives from the scheme deck alone. The pair of starts
  -- differs only in GameSettings.sharedTeamLife. Bob loses
  -- 7, which carol's total shows (CR 810.9a). From there, carol taking ten
  -- poison loses alone, and bob losing the other 53 takes the whole team.
  Spec.it s "CR 904.13b the opposing team shares one 60-point life total, and CR 904.13c keeps poison per player" $ do
    forest <- S.printingOf s registry "Forest"
    look <- S.printingOf s registry "Look Skyward and Despair"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let commanderDeck = (Deck.fromCards (Map.singleton forest 20)) {Deck.commander = Set.singleton shimatsu}
        alices = commanderDeck {Deck.schemes = Map.singleton look 2}
        teamed shared =
          let gs = Setup.emptyGame S.threePlayers
           in gs {GameState.settings = (GameState.settings gs) {GameSettings.sharedTeamLife = shared}}
        started shared = S.runPure S.identityAnswer (teamed shared) (Setup.newGame (Resolve.performHandAction Resolve.noSubgame) ((S.alice, alices) NonEmpty.:| [(S.bob, commanderDeck), (S.carol, commanderDeck)]))
        lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
        struck = S.runPure S.identityAnswer (started True) (Event.changeLife S.bob (-7))
        poisoned = S.settleSba (S.addPlayerCounter PlayerCounterKind.Poison 10 S.carol struck)
        felled = S.settleSba (S.runPure S.identityAnswer struck (Event.changeLife S.bob (-53)))
    Spec.assertEqWith s "CR 904.13b alice starts at 60, and bob and carol at their shared 60" (lives (started True)) (Just 60, Just 60, Just 60)
    Spec.assertEqWith s "CR 810.9 bob's loss of 7 is carol's too" (lives struck) (Just 60, Just 53, Just 53)
    Spec.assertEqWith s "CR 904.13c carol loses at ten poison and bob plays on" (Game.stillPlaying poisoned) [S.alice, S.bob]
    Spec.assertEqWith s "CR 810.8c a team at 0 life loses together" (Game.stillPlaying felled) [S.alice]
    Spec.assertEqWith s "without the shared total each opponent starts at CR 903.7's 40" (lives (started False)) (Just 60, Just 40, Just 40)

  -- CR 703.4e / 904.9 / 704.6e: at the start of her precombat main phase alice
  -- sets Look Skyward and Despair in motion; its "When you set this scheme in
  -- motion" makes a 5/5 Dragon, and once that has resolved the plain scheme
  -- goes to the bottom of her scheme deck.
  Spec.it s "CR 904.9 the archenemy sets a scheme in motion, and CR 704.6e abandons it once its trigger resolves" $ do
    board <- schemeBoard s registry ["Look Skyward and Despair", "The Very Soil Shall Shake"]
    let triggered = S.runPure S.identityAnswer (inMain S.alice board) (Engine.runTurnBasedActions Phase.PrecombatMain >> Engine.settleForPriority)
        resolved = S.runPure S.identityAnswer triggered Engine.priorityLoop
        bobsTurn = S.runPure S.identityAnswer (inMain S.bob board) (Engine.runTurnBasedActions Phase.PrecombatMain >> Engine.priorityLoop)
    Spec.assertEqWith s "CR 904.9 its trigger made a 5/5 flying Dragon for alice" (fmap (\oid -> (S.powerToughnessOf oid resolved, Projection.hasKeyword Keyword.Flying oid resolved)) (creaturesOf S.alice resolved)) [(Just (5, 5), True)]
    Spec.assertEqWith s "CR 701.32b Look Skyward and Despair is face up while its trigger waits" (names (Archenemy.faceUp triggered) triggered) ["Look Skyward and Despair"]
    Spec.assertEqWith s "CR 704.6e and the spent scheme is on the bottom of her deck" (names (Archenemy.deckOf S.alice resolved) resolved) ["The Very Soil Shall Shake", "Look Skyward and Despair"]
    Spec.assertEqWith s "with nothing face up" (Archenemy.faceUp resolved) []
    Spec.assertEqWith s "on bob's turn nothing is set in motion" (Archenemy.faceUp bobsTurn, creaturesOf S.alice bobsTurn) ([], [])

  -- CR 205.4h / 904.11 / 904.8: The Very Soil Shall Shake is ongoing, so it
  -- stays face up, and its "Creatures you control get +2/+2 and have trample"
  -- functions from the command zone -- alice's, not bob's.
  Spec.it s "CR 205.4h an ongoing scheme stays face up and its static ability applies" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    board <- schemeBoard s registry ["The Very Soil Shall Shake", "Look Skyward and Despair"]
    let (alicesPiker, b1) = S.addPermanent piker S.alice board
        (bobsPiker, b2) = S.addPermanent piker S.bob b1
        set = S.runPure S.identityAnswer (inMain S.alice b2) (Engine.runTurnBasedActions Phase.PrecombatMain >> Engine.priorityLoop)
    Spec.assertEqWith s "CR 205.4h it is still face up" (names (Archenemy.faceUp set) set) ["The Very Soil Shall Shake"]
    Spec.assertEqWith s "CR 904.8 alice's Piker is a 4/3 with trample" (S.powerToughnessOf alicesPiker set, Projection.hasKeyword Keyword.Trample alicesPiker set) (Just (4, 3), True)
    Spec.assertEqWith s "and bob's is its printed 2/1" (S.powerToughnessOf bobsPiker set) (Just (2, 1))

  -- CR 701.33: "When a creature you control dies, abandon this scheme." The
  -- ongoing scheme goes to the bottom of the deck and its pump ends.
  Spec.it s "CR 701.33 an ongoing scheme's own trigger abandons it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    board <- schemeBoard s registry ["The Very Soil Shall Shake", "Look Skyward and Despair"]
    let (victim, b1) = S.addPermanent piker S.alice board
        (survivor, b2) = S.addPermanent piker S.alice b1
        set = S.runPure S.identityAnswer (inMain S.alice b2) (Engine.runTurnBasedActions Phase.PrecombatMain >> Engine.priorityLoop)
        died = S.runPure S.identityAnswer set (Event.destroy Regenerability.Regenerable [victim] >> Engine.priorityLoop)
    Spec.assertEqWith s "CR 701.33b the scheme is no longer face up" (Archenemy.faceUp died) []
    Spec.assertEqWith s "and is on the bottom of alice's scheme deck" (names (Archenemy.deckOf S.alice died) died) ["Look Skyward and Despair", "The Very Soil Shall Shake"]
    Spec.assertEqWith s "so the surviving Piker is its printed 2/1 again" (S.powerToughnessOf survivor died) (Just (2, 1))

  -- CR 500.7 / 611.2a / 101.2: All in Good Time ("When you set this scheme in
  -- motion, take an extra turn after this one. Schemes can't be set in motion
  -- that turn." -- Oracle verified on Scryfall 2026-09-28). Look Skyward and
  -- Despair is next in the deck: the extra turn's CR 904.9 action sets nothing
  -- in motion, and the prohibition ends with that turn, so alice's next
  -- ordinary turn sets Look Skyward in motion and makes its Dragon.
  Spec.it s "CR 904.9 no scheme is set in motion during All in Good Time's extra turn, and one is on alice's next turn" $ do
    board <- schemeBoard s registry ["All in Good Time", "Look Skyward and Despair"]
    let start = (inMain S.alice board) {GameState.remaining = S.phasesAfter Phase.PrecombatMain}
        atExtra = throughTurn start
        afterExtra = throughTurn atExtra
        afterNext = throughTurn (throughTurn afterExtra)
    Spec.assertEqWith s "during the extra turn nothing was set in motion, so alice has no Dragon" (creaturesOf S.alice afterExtra) []
    Spec.assertEqWith s "and Look Skyward and Despair is still on top" (names (Archenemy.deckOf S.alice afterExtra) afterExtra) ["Look Skyward and Despair", "All in Good Time"]
    Spec.assertEqWith s "on alice's next turn it is set in motion and makes its 5/5 Dragon" (fmap (`S.powerToughnessOf` afterNext) (creaturesOf S.alice afterNext)) [Just (5, 5)]
    Spec.assertEqWith s "the extra turn is turn 2 and alice's, and the last read follows alice's turn 4" (fmap (\g -> (GameState.turnNumber g, GameState.activePlayer g)) [atExtra, afterNext]) [(2, S.alice), (5, S.bob)]

-- A two-player board with alice's scheme deck stacked in the order named, top
-- first, and twenty Forests in each library. No opening hands are drawn.
schemeBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [String] -> m GameState.GameState
schemeBoard s registry order = do
  forest <- S.printingOf s registry "Forest"
  schemes <- traverse (S.printingOf s registry) order
  let alices = (Deck.fromCards (Map.singleton forest 20)) {Deck.schemes = Map.fromList (fmap (\p -> (p, 1)) schemes)}
      bobs = Deck.fromCards (Map.singleton forest 20)
      built = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice alices >> Setup.createDeck S.bob bobs)
      rank oid = maybe (length order) (\face -> Maybe.fromMaybe (length order) (List.elemIndex (Face.name face) (fmap (CardName.MkCardName . Text.pack) order))) (Game.faceOf oid built)
  pure built {GameState.schemeDecks = Map.adjust (Seq.sortOn rank) S.alice (GameState.schemeDecks built)}

-- Step the game until the turn under way has ended and the next has begun.
-- Bounded, so a schedule that never hands off stops rather than looping.
throughTurn :: GameState.GameState -> GameState.GameState
throughTurn gs =
  let go n g =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || GameState.turnNumber g /= GameState.turnNumber gs
          then g
          else go (n - 1) (S.runPure S.identityAnswer g Engine.runStep)
   in go 64 gs

-- `pid`'s precombat main phase with priority and an empty stack.
inMain :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
inMain pid gs = gs {GameState.activePlayer = pid, GameState.phase = Phase.PrecombatMain, GameState.priority = Just pid}

names :: [ObjectId] -> GameState.GameState -> [String]
names oids gs = Maybe.mapMaybe (\oid -> fmap (Text.unpack . CardName.unwrap . Face.name) (Game.faceOf oid gs)) oids

creaturesOf :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId]
creaturesOf pid gs = filter (`Projection.isCreatureOf` gs) (Projection.controls pid gs)
