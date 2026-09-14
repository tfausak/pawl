{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 701.61 FORAGE -- Pawl.Engine.Forage, Effect.Forage's two arms in
-- Pawl.Engine.Resolve.Effect (the executing one and effectIsImpossible's), and
-- Prompt.ChooseForage.
--
-- Treetop Sentries ({3}{G} Creature -- Squirrel Archer 2/4, "Reach. When this
-- creature enters, you may forage. If you do, draw a card.") is the fixture: its
-- forage clause states nothing the rulebook does not, and the draw hanging off
-- CR 608.2c's "if you do" is what makes the ANSWER to the "may" observable
-- separately from what the forage moved.
--
-- THE BOARD SHAPE that makes the cases discriminating: alice's graveyard holds
-- FIVE cards of five different printings, so the three she exiles are three she
-- CHOSE and the two left behind are the proof -- a forage that exiled the first
-- three, or all five, reads differently. Five and not three: at exactly three
-- the choice is forced and no prompt is raised, which would make the chooser's
-- answer unobservable. The Food is a Golden Egg, the one permanent on the board
-- with the subtype, so a sacrifice cannot be mistaken for anything else leaving.
--
-- Asserted by NAME and not by ObjectId: CR 400.7 makes the exiled card a new
-- object, so the id the board handed out no longer names it. The library card is
-- a Mountain, of no printing in the graveyard, so a drawn card cannot be
-- mistaken for a card that stayed put.
module Pawl.ForageSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ForageMode as ForageMode
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- alice's board: `buried` printings in her graveyard, `foods` Golden Eggs on her
-- battlefield, one Mountain in her library to draw, and Treetop Sentries
-- entering with its CR 603.6a event alongside it.
sentriesBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> Int -> GameState.GameState
sentriesBoard sentries egg library buried foods =
  let withGraveyard = List.foldl' (\gs printing -> snd (S.addGraveyardCard printing S.alice gs)) (Setup.emptyGame S.bothPlayers) buried
      withFoods = List.foldl' (\gs _ -> snd (S.addPermanent egg S.alice gs)) withGraveyard [1 .. foods]
      -- Something to draw: an empty library makes the draw a no-op and a CR
      -- 104.3c loss, which would hide whether the "if you do" clause ran.
      (_, withLibrary) = S.addLibraryCard library S.alice withFoods
   in snd (S.entersWithTrigger sentries S.alice withLibrary)

-- The entering trigger placed and resolved under one answerer, with every prompt
-- it raised recorded in order.
--
-- Recorded rather than counted: the cases below turn on WHICH question was
-- asked, and a count cannot tell a branch offer from a card chooser.
foraged :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ([Text.Text], GameState.GameState)
foraged answer gs =
  let recording :: Prompt.Prompt r -> State.State [Text.Text] r
      recording p = do
        State.modify (<> [S.promptKind p])
        pure (answer p)
      (after, asked) = State.runState (Engine.runGame recording gs (Engine.placePendingTriggers *> Stack.resolveTop)) []
   in (asked, snd after)

-- Take the "may", and exile the graveyard cards at these positions in the
-- offered list. PINNED by position rather than searched: an answerer that took
-- whatever was legal would find three cards again after a mutation and keep the
-- case green.
takingExiles :: [Int] -> Prompt.Prompt r -> r
takingExiles positions p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.ChooseForage {} -> ForageMode.ExileCards
  Prompt.ChooseExilesFromGraveyard _ _ _ candidates _ -> Set.fromList (fmap (candidates !!) positions)
  _ -> S.identityAnswer p

-- Take the "may" and the Food half of rule 701.61a.
takingFood :: Prompt.Prompt r -> r
takingFood p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.ChooseForage {} -> ForageMode.SacrificeFood
  _ -> S.identityAnswer p

-- The names of the cards in one of alice's zones, sorted.
namesIn :: Zone.Zone -> GameState.GameState -> [CardName.CardName]
namesIn zone gs = List.sort (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone S.alice gs))

-- The five graveyard printings, and the two of them a forage taking positions
-- 0, 2 and 4 leaves behind.
names :: [Printing.Printing] -> [CardName.CardName]
names = List.sort . fmap S.printingName

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Forage" $ do
  Spec.it s "CR 701.61a foraging exiles the three cards the forager chose" $ do
    sentries <- S.printingOf s registry "Treetop Sentries"
    egg <- S.printingOf s registry "Golden Egg"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    cow <- S.printingOf s registry "Bartered Cow"
    let gs = sentriesBoard sentries egg mountain [forest, spider, piker, bolt, cow] 0
        (asked, after) = foraged (takingExiles [0, 2, 4]) gs
    -- The gameplay reading first, and both directions of it: exactly the three
    -- chosen cards left the graveyard for exile, and the two she kept did not.
    Spec.assertEqWith s "CR 701.61a the three chosen cards are in exile" (namesIn Zone.Exile after) (names [forest, piker, cow])
    Spec.assertEqWith s "CR 701.61a the two the forager kept are still in the graveyard" (namesIn Zone.Graveyard after) (names [spider, bolt])
    -- CR 608.2c's "if you do": the forage happened, so the draw did.
    Spec.assertEqWith s "CR 608.2c the draw hanging off the forage happened" (S.handSize S.alice after) 1
    Spec.assertBool s (elem (Text.pack "ChooseExilesFromGraveyard") asked) "CR 701.61a five candidates for three cards: the forager chose"
    -- Nothing to ask about the branch: she controls no Food.
    Spec.assertBool s (notElem (Text.pack "ChooseForage") asked) "CR 608.2d with no Food there is no branch to offer"
  Spec.it s "CR 701.61a a forager who cannot exile three cards sacrifices the Food" $ do
    sentries <- S.printingOf s registry "Treetop Sentries"
    egg <- S.printingOf s registry "Golden Egg"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    let gs = sentriesBoard sentries egg mountain [forest, spider] 1
        (asked, after) = foraged (takingExiles [0]) gs
    -- The Egg in the graveyard beside the two untouched cards is the whole
    -- action: CR 701.21a's sacrifice happened and rule 701.61a's exile did not.
    Spec.assertEqWith s "CR 701.21a the Food was sacrificed and the two graveyard cards stayed" (namesIn Zone.Graveyard after) (names [forest, spider, egg])
    Spec.assertEqWith s "CR 701.61a nothing was exiled" (namesIn Zone.Exile after) []
    Spec.assertEqWith s "CR 608.2c the draw hanging off the forage happened" (S.handSize S.alice after) 1
    Spec.assertBool s (notElem (Text.pack "ChooseForage") asked) "CR 608.2d one half cannot be carried out, so there is no branch to offer"
    Spec.assertBool s (notElem (Text.pack "ChooseExilesFromGraveyard") asked) "and no cards were offered for exile"
  Spec.it s "CR 701.61a with both halves available the forager's answer decides which" $ do
    sentries <- S.printingOf s registry "Treetop Sentries"
    egg <- S.printingOf s registry "Golden Egg"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    cow <- S.printingOf s registry "Bartered Cow"
    let gs = sentriesBoard sentries egg mountain [forest, spider, piker, bolt, cow] 1
        (asked, exiling) = foraged (takingExiles [0, 1, 2]) gs
        (_, eating) = foraged takingFood gs
    Spec.assertBool s (elem (Text.pack "ChooseForage") asked) "CR 701.61a both halves can be carried out, so the forager is asked which"
    -- One board, two answers, opposite boards afterwards.
    Spec.assertEqWith s "CR 701.61a exiling takes the three cards and leaves the Food alone" (namesIn Zone.Exile exiling) (names [forest, spider, piker])
    Spec.assertEqWith s "CR 701.61a and sacrificing leaves the graveyard alone" (namesIn Zone.Graveyard eating) (names [forest, spider, piker, bolt, cow, egg])
    Spec.assertEqWith s "CR 701.21a the sacrifice took the Food" (S.countOnBattlefieldByName (S.printingName egg) S.alice eating) 0
    Spec.assertEqWith s "CR 701.61a and the exile left it on the battlefield" (S.countOnBattlefieldByName (S.printingName egg) S.alice exiling) 1
  Spec.it s "CR 608.2d a forager who can do neither half is not offered the forage" $ do
    sentries <- S.printingOf s registry "Treetop Sentries"
    egg <- S.printingOf s registry "Golden Egg"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    let gs = sentriesBoard sentries egg mountain [forest, spider] 0
        (asked, after) = foraged (takingExiles [0]) gs
    Spec.assertEqWith s "CR 608.2d nothing was drawn, so the forage never happened" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and the graveyard is untouched" (namesIn Zone.Graveyard after) (names [forest, spider])
    Spec.assertBool s (notElem (Text.pack "ChooseOptional") asked) "CR 608.2d an impossible option is not offered"
