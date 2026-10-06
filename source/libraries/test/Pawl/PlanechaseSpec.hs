{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | CR 901, the Planechase variant: Pawl.Engine.Planechase and Pawl.Engine.Plane,
-- with Pawl.Types.Action's RollPlanarDie, Pawl.Types.TriggerCondition's
-- ChaosEnsues and Pawl.Types.Effect's Planeswalk.
module Pawl.PlanechaseSpec where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.CombatRestriction as CombatRestriction
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Planechase as Planechase
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Planechase" $ do
  -- CR 103.7 through the whole of Setup.newGame: after the opening hands, the
  -- starting player's top planar card is face up and the rest stay in the deck.
  Spec.it s "CR 103.7 a new game begins with the starting player's top plane face up" $ do
    forest <- S.printingOf s registry "Forest"
    academy <- S.printingOf s registry "Academy at Tolaria West"
    tazeem <- S.printingOf s registry "Tazeem"
    let alices = (Deck.fromCards (Map.singleton forest 20)) {Deck.planes = Set.fromList [academy, tazeem]}
        bobs = Deck.fromCards (Map.singleton forest 20)
        started = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.newGame (Resolve.performHandAction Resolve.noSubgame) ((S.alice, alices) NonEmpty.:| [(S.bob, bobs)]))
    Spec.assertEqWith s "one plane is face up" (length (Planechase.faceUp started)) 1
    Spec.assertEqWith s "and the other is still alice's planar deck" (length (Planechase.deckOf S.alice started)) 1
    Spec.assertEqWith s "and neither is in her library" (length (Game.zoneMembers Zone.Library S.alice started)) 13

  -- CR 311.4 / 901.7 and CR 901.6: Academy at Tolaria West ("At the beginning of
  -- your end step, if you have no cards in hand, draw seven cards.") is alice's,
  -- but its controller is the planar controller -- the active player -- so on
  -- bob's turn it is bob who draws.
  Spec.it s "CR 901.6 the face-up plane's end-step trigger is the active player's" $ do
    board <- planarBoard s registry ["Academy at Tolaria West"]
    let alicesEnd = endStep S.alice board
        bobsEnd = endStep S.bob board
    Spec.assertEqWith s "CR 311.4 at alice's end step alice draws seven" (S.handSize S.alice alicesEnd, S.handSize S.bob alicesEnd) (7, 0)
    Spec.assertEqWith s "CR 901.6 at bob's end step bob draws seven, though alice owns the plane" (S.handSize S.alice bobsEnd, S.handSize S.bob bobsEnd) (0, 7)

  -- CR 311.4: a face-up plane's static abilities affect the game. Tazeem's
  -- "Creatures can't block" reaches bob's creature; in the planar deck it does
  -- not.
  Spec.it s "CR 311.4 a face-up Tazeem stops creatures from blocking" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    board <- planarBoard s registry ["Tazeem", "Academy at Tolaria West"]
    buried <- planarBoard s registry ["Academy at Tolaria West", "Tazeem"]
    let (pikerId, withPiker) = S.addPermanent piker S.bob board
        (buriedPiker, withBuried) = S.addPermanent piker S.bob buried
    Spec.assertBool s (Set.member pikerId (CombatRestriction.cantBlock (Just S.bob) [pikerId] withPiker)) "CR 311.4 with Tazeem face up bob's creature can't block"
    Spec.assertBool s (not (Set.member buriedPiker (CombatRestriction.cantBlock (Just S.bob) [buriedPiker] withBuried))) "with Tazeem in the planar deck it can"

  -- CR 116.2i / 901.9: the active player, in a main phase with an empty stack,
  -- may roll the planar die; the first roll of the turn is free and the second
  -- costs {1}.
  Spec.it s "CR 901.9 the planar die is the active player's special action, and each roll costs one more" $ do
    forest <- S.printingOf s registry "Forest"
    board <- planarBoard s registry ["Academy at Tolaria West"]
    let main = inMain S.alice board
        rolledOnce = S.runPure (rolling 1) main (Planechase.roll S.manaPerformer S.alice)
        (_, withForest) = S.addPermanent forest S.alice rolledOnce
    Spec.assertBool s (elem Action.Type.RollPlanarDie (Action.legalActions S.alice main)) "CR 116.2i the active player may roll"
    Spec.assertBool s (notElem Action.Type.RollPlanarDie (Action.legalActions S.bob (main {GameState.priority = Just S.bob}))) "and a player whose turn it isn't may not"
    Spec.assertBool s (elem (GameEvent.DiceRolled S.alice) (S.eventsOf rolledOnce)) "CR 901.9d the roll is a die roll"
    Spec.assertBool s (notElem Action.Type.RollPlanarDie (Action.legalActions S.alice rolledOnce)) "CR 901.9 with no mana the second roll is not offered"
    Spec.assertBool s (elem Action.Type.RollPlanarDie (Action.legalActions S.alice withForest)) "and with a Forest to pay {1} it is"

  -- CR 901.9b / 311.7: the chaos symbol sets off Academy at Tolaria West's
  -- "Whenever chaos ensues, discard your hand." A blank face does nothing.
  Spec.it s "CR 311.7 rolling the chaos symbol triggers the plane's chaos ability" $ do
    forest <- S.printingOf s registry "Forest"
    board <- planarBoard s registry ["Academy at Tolaria West"]
    let (_, one) = S.addHandCard forest S.alice (inMain S.alice board)
        (_, stocked) = S.addHandCard forest S.alice one
        chaos = rollAndResolve 5 stocked
        blank = rollAndResolve 1 stocked
    Spec.assertEqWith s "CR 311.7 chaos: alice discards her hand" (S.handSize S.alice chaos) 0
    Spec.assertEqWith s "CR 901.9a a blank face: she keeps it" (S.handSize S.alice blank) 2

  -- CR 311.7 / 901.6 again, on Tazeem's "draw a card for each land you
  -- control": bob rolls on his own turn, so "you" is bob.
  Spec.it s "CR 901.6 the chaos ability's you is the planar controller" $ do
    forest <- S.printingOf s registry "Forest"
    board <- planarBoard s registry ["Tazeem"]
    let (_, b1) = S.addPermanent forest S.bob board
        (_, b2) = S.addPermanent forest S.bob b1
        (_, b3) = S.addPermanent forest S.alice b2
        chaos = S.runPure (rolling 5) (inMain S.bob b3) (Planechase.roll S.manaPerformer S.bob >> Engine.priorityLoop)
    Spec.assertEqWith s "bob draws one card for each of his two Forests, alice none" (S.handSize S.bob chaos, S.handSize S.alice chaos) (2, 0)

  -- CR 901.8 / 701.31b: the Planeswalker symbol triggers the planeswalking
  -- ability, which puts Academy on the bottom as a new object (CR 311.6) and
  -- turns Tazeem face up.
  Spec.it s "CR 901.8 rolling the Planeswalker symbol planeswalks to the next plane" $ do
    board <- planarBoard s registry ["Academy at Tolaria West", "Tazeem"]
    let main = inMain S.alice board
        walked = rollAndResolve 6 main
        blank = rollAndResolve 1 main
        names gs = fmap (\oid -> fmap Face.name (Game.faceOf oid gs)) (Planechase.faceUp gs)
        academyBefore = Planechase.faceUp main
    Spec.assertEqWith s "CR 701.31b Tazeem is the face-up plane" (names walked) [Just (CardName.MkCardName (Text.pack "Tazeem"))]
    Spec.assertEqWith s "and Academy is on the bottom of the planar deck" (fmap (\oid -> fmap Face.name (Game.faceOf oid walked)) (Planechase.deckOf S.alice walked)) [Just (CardName.MkCardName (Text.pack "Academy at Tolaria West"))]
    Spec.assertBool s (all (\oid -> Maybe.isNothing (Game.lookupObject oid walked)) academyBefore) "CR 311.6 as a new object"
    Spec.assertEqWith s "a blank face leaves Academy face up" (names blank) [Just (CardName.MkCardName (Text.pack "Academy at Tolaria West"))]

  -- CR 901.10 / 311.5 / 800.4p: bob, the active player, concedes in his main
  -- phase while his Academy is the face-up plane. It leaves the game with him,
  -- carol -- the next seat in turn order, not alice, the first by id -- becomes
  -- planar controller, and her top planar card is turned up at once.
  Spec.it s "CR 311.5 a departing active player's plane is replaced from the next seat's planar deck" $ do
    board <- seatedPlanarBoard s registry S.bob [(S.alice, ["Tazeem", "Academy at Tolaria West"]), (S.bob, ["Academy at Tolaria West"]), (S.carol, ["Tazeem", "Academy at Tolaria West"])]
    let gone = S.runPure (conceding S.bob) (inMain S.bob board) Engine.priorityLoop
        tazeem = CardName.MkCardName (Text.pack "Tazeem")
    Spec.assertEqWith s "CR 901.10 carol's Tazeem is face up in place of bob's Academy" (fmap (\oid -> (fmap Face.name (Game.faceOf oid gone), fmap Object.owner (Game.lookupObject oid gone))) (Planechase.faceUp gone)) [(Just tazeem, Just S.carol)]
    Spec.assertEqWith s "CR 311.5 carol controls it" (fmap (`Projection.controllerOf` gone) (Planechase.faceUp gone)) [Just S.carol]
    Spec.assertEqWith s "and alice's planar deck is untouched" (length (Planechase.deckOf S.alice gone), length (Planechase.deckOf S.carol gone)) (2, 1)

  -- CR 901.10a: bob rolls the Planeswalker symbol, and while his planeswalking
  -- ability is on the stack alice, whose Academy is face up, concedes. bob turns
  -- up his Tazeem (CR 901.10) and the ability ceases to exist, so it does not
  -- planeswalk him on to his Academy.
  Spec.it s "CR 901.10a a plane leaving the game ends a pending planeswalking ability" $ do
    board <- seatedPlanarBoard s registry S.alice [(S.alice, ["Academy at Tolaria West"]), (S.bob, ["Tazeem", "Academy at Tolaria West"]), (S.carol, [])]
    let answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.RollDie _ -> 6
          _ -> conceding S.alice p
        gone = S.runPure answer (inMain S.bob board) (Planechase.roll S.manaPerformer S.bob >> Engine.priorityLoop)
        names = fmap (\oid -> fmap Face.name (Game.faceOf oid gone)) (Planechase.faceUp gone)
    Spec.assertEqWith s "CR 901.10a bob's Tazeem is still the face-up plane" names [Just (CardName.MkCardName (Text.pack "Tazeem"))]
    Spec.assertEqWith s "and his Academy is still in his planar deck" (length (Planechase.deckOf S.bob gone)) 1
    Spec.assertBool s (notElem S.alice (Game.stillPlaying gone)) "alice has left the game"

-- A two-player board with alice's planar deck stacked in the order named, top
-- first, the top one face up (CR 103.7), and twenty Forests in each library.
-- No opening hands are drawn, so both hands are empty.
planarBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [String] -> m GameState.GameState
planarBoard s registry order = do
  forest <- S.printingOf s registry "Forest"
  planes <- traverse (S.printingOf s registry) order
  let alices = (Deck.fromCards (Map.singleton forest 20)) {Deck.planes = Set.fromList planes}
      bobs = Deck.fromCards (Map.singleton forest 20)
      built = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice alices >> Setup.createDeck S.bob bobs)
      rank oid = maybe (length order) (\face -> Maybe.fromMaybe (length order) (List.elemIndex (Face.name face) (fmap (CardName.MkCardName . Text.pack) order))) (Game.faceOf oid built)
      stacked = built {GameState.planarDecks = Map.adjust (Seq.sortOn rank) S.alice (GameState.planarDecks built)}
  pure (S.runPure S.identityAnswer stacked (Planechase.setStartingPlane S.alice))

-- A three-player board, each seat's planar deck stacked in the order named,
-- top first, and `starter`'s top card face up (CR 103.7). Twenty Forests in
-- each library; no opening hands.
seatedPlanarBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> PlayerId.PlayerId -> [(PlayerId.PlayerId, [String])] -> m GameState.GameState
seatedPlanarBoard s registry starter seats = do
  forest <- S.printingOf s registry "Forest"
  decks <- traverse (\(pid, order) -> (\planes -> (pid, (Deck.fromCards (Map.singleton forest 20)) {Deck.planes = Set.fromList planes})) <$> traverse (S.printingOf s registry) order) seats
  let built = S.runPure S.identityAnswer (Setup.emptyGame S.threePlayers) (mapM_ (uncurry Setup.createDeck) decks)
      rank order oid = maybe (length order) (\face -> Maybe.fromMaybe (length order) (List.elemIndex (Face.name face) (fmap (CardName.MkCardName . Text.pack) order))) (Game.faceOf oid built)
      stacked = built {GameState.planarDecks = Map.mapWithKey (\pid deck -> maybe deck (\order -> Seq.sortOn (rank order) deck) (List.lookup pid seats)) (GameState.planarDecks built)}
  pure (S.runPure S.identityAnswer stacked (Planechase.setStartingPlane starter))

-- `who` concedes when asked; everyone else plays on, and every other prompt is
-- answered by identity.
conceding :: PlayerId.PlayerId -> Prompt.Prompt r -> r
conceding who p = case p of
  Prompt.Concede asked -> if asked == who then Concession.Concedes else Concession.Continues
  _ -> S.identityAnswer p

-- `pid`'s precombat main phase with priority and an empty stack.
inMain :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
inMain pid gs = gs {GameState.activePlayer = pid, GameState.phase = Phase.PrecombatMain, GameState.priority = Just pid}

-- `pid`'s end step begins and every trigger resolves.
endStep :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
endStep pid gs =
  let step = Phase.Ending EndingStep.EndStep
      begun = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan step pid)) (gs {GameState.activePlayer = pid, GameState.phase = step, GameState.priority = Just pid})
   in S.runPure S.identityAnswer begun Engine.priorityLoop

-- Answer the planar die with this number and everything else by identity.
rolling :: Natural -> Prompt.Prompt r -> r
rolling n p = case p of
  Prompt.RollDie _ -> n
  _ -> S.identityAnswer p

-- alice rolls the planar die showing `n`, and whatever it triggers resolves.
rollAndResolve :: Natural -> GameState.GameState -> GameState.GameState
rollAndResolve n gs = S.runPure (rolling n) gs (Planechase.roll S.manaPerformer S.alice >> Engine.priorityLoop)
