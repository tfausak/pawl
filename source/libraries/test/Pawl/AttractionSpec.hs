{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 717 Attractions -- Pawl.Engine.Attraction, CR 701.51's
-- Effect.OpenAttraction, CR 701.52a's roll to visit (Resolve.rollToVisit, run by
-- Engine.runTurnBasedActions as CR 703.4g's action), CR 702.159a's
-- TriggerCondition.Visit, CR 717.6's junkyard (Event.toJunkyard), and the
-- Attraction deck across CR 727's restart and CR 729's subgames.
--
-- Deadbeat Attendant ("When this creature enters, open an Attraction") and
-- Bumper Cars ("Visit -- Target creature must be blocked this turn if able") are
-- the producers. Bumper Cars is printed with six light patterns (CR 717.1);
-- 202a lights 2, 3 and 6 and 202f lights 4, 5 and 6, so a 3 visits the one and
-- not the other.
module Pawl.AttractionSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Attraction as Attraction
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Phase as Phase
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Attraction" $ do
  openSpec s registry
  visitSpec s registry
  junkyardSpec s registry
  newGameSpec s registry

bumperCars :: CardName.CardName
bumperCars = CardName.MkCardName (Text.pack "Bumper Cars")

-- A printing of `card` with these lights (CR 717.1).
lit :: [Natural] -> Printing.Printing -> Printing.Printing
lit numbers card = card {Printing.lights = Set.fromList numbers}

-- The two Bumper Cars printings the file uses.
carsA, carsF :: Printing.Printing -> Printing.Printing
carsA = lit [2, 3, 6]
carsF = lit [4, 5, 6]

-- `pid` brings these Attractions, and nothing else.
withAttractions :: PlayerId -> [Printing.Printing] -> GameState.GameState -> GameState.GameState
withAttractions pid printings gs =
  S.runPure S.identityAnswer gs (Setup.createDeck pid (Deck.fromCards Map.empty) {Deck.attractions = Map.fromListWith (+) [(p, 1) | p <- printings]})

-- CR 701.51: opening an Attraction.
openSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
openSpec s registry = Spec.describe s "Open an Attraction" $ do
  Spec.it s "CR 701.51b Deadbeat Attendant puts the top of the Attraction deck onto the battlefield" $ do
    attendant <- S.printingOf s registry "Deadbeat Attendant"
    swamp <- S.printingOf s registry "Swamp"
    cars <- S.printingOf s registry "Bumper Cars"
    -- createDeck deals the deck in printing order, so 202a is on top and 202f
    -- beneath it: the lights of what entered say WHICH card was opened.
    let (gs, spellId) = S.handOne attendant (withAttractions S.alice [carsA cars, carsF cars] (S.landsInPlay swamp 2))
        cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
        after = S.runPure S.identityAnswer cast Engine.priorityLoop
        opened = filter (\oid -> Game.lightsOf oid after /= Set.empty) (Set.toList (GameState.battlefield after))
    -- THE GAMEPLAY ASSERTION: the top card, 202a, is on the battlefield.
    Spec.assertEqWith s "CR 701.51b: the top card entered, lit 2, 3 and 6" (fmap (`Game.lightsOf` after) opened) [Set.fromList [2, 3, 6]]
    Spec.assertEqWith s "under alice's control" (S.countOnBattlefieldByName bumperCars S.alice after) 1
    Spec.assertEqWith s "and 202f is left in the deck" (fmap (`Game.lightsOf` after) (Attraction.deckOf S.alice after)) [Set.fromList [4, 5, 6]]
    Spec.assertEqWith s "which was two cards before" (length (Attraction.deckOf S.alice gs)) 2
  Spec.it s "CR 609.3 an empty Attraction deck opens nothing" $ do
    attendant <- S.printingOf s registry "Deadbeat Attendant"
    swamp <- S.printingOf s registry "Swamp"
    let (gs, spellId) = S.handOne attendant (S.landsInPlay swamp 2)
        after = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast S.alice spellId)) Engine.priorityLoop
    Spec.assertEqWith s "the Attendant resolved" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Deadbeat Attendant")) S.alice after) 1
    Spec.assertEqWith s "and no Attraction entered" (S.countOnBattlefieldByName bumperCars S.alice after) 0
  Spec.it s "CR 717.2 / 103.3a the Attraction deck is shuffled before the game begins" $ do
    swamp <- S.printingOf s registry "Swamp"
    cars <- S.printingOf s registry "Bumper Cars"
    let (gs, shuffles) = started swamp [carsA cars, carsF cars]
        deck = Attraction.deckOf S.alice gs
    -- Dealt 202a then 202f; the shuffle's reversed answer is what stands.
    Spec.assertEqWith s "CR 103.3a: the shuffle's order is the deck's" (fmap (`Game.lightsOf` gs) deck) [Set.fromList [4, 5, 6], Set.fromList [2, 3, 6]]
    Spec.assertEqWith s "asked once, over exactly the deck" (filter ((== Set.fromList deck) . Set.fromList) shuffles) [reverse deck]
    Spec.assertEqWith s "and three shuffles in all: two libraries and the one deck" (length shuffles) 3
  Spec.it s "CR 717.2 the Attraction deck is in the command zone, not the library or the starting deck" $ do
    cars <- S.printingOf s registry "Bumper Cars"
    let gs = withAttractions S.alice [carsA cars] (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "one card in the deck" (length (Attraction.deckOf S.alice gs)) 1
    Spec.assertEqWith s "whose zone is the command zone" (fmap (\oid -> Game.zoneOf oid gs) (Attraction.deckOf S.alice gs)) [Just Zone.Command]
    Spec.assertEqWith s "but which is not face up there" (GameState.command gs) Set.empty
    Spec.assertEqWith s "nothing in the library" (Game.zoneMembers Zone.Library S.alice gs) []

-- Start a game in which alice brings `attractions` beside ten Swamps, answering
-- every shuffle by reversing it. Answers the final board and every shuffle asked.
started :: Printing.Printing -> [Printing.Printing] -> (GameState.GameState, [[ObjectId]])
started swamp attractions =
  let deck = (Deck.fromCards (Map.singleton swamp 10)) {Deck.attractions = Map.fromListWith (+) [(p, 1) | p <- attractions]}
      answering :: Prompt.Prompt r -> State.State [[ObjectId]] r
      answering p = case p of
        Prompt.Shuffle ids -> State.modify' (ids :) >> pure (reverse ids)
        _ -> pure (S.identityAnswer p)
      ((_, after), shuffles) = State.runState (Engine.runGame answering (Setup.emptyGame S.bothPlayers) (Setup.newGame Resolve.performHandAction ((S.alice, deck) NonEmpty.:| [(S.bob, Deck.fromCards (Map.singleton swamp 10))]))) []
   in (after, reverse shuffles)

-- The board every visit case starts from: alice controls `attraction` and a
-- Goblin Piker, bob controls a Goblin Piker, and it is alice's precombat main
-- phase. Answers alice's Piker, then bob's, then the Attraction.
visitBoard :: Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId, ObjectId, ObjectId)
visitBoard piker attraction =
  let (mine, g1) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
      (theirs, g2) = S.addPermanent piker S.bob g1
      (attractionId, g3) = S.addPermanent attraction S.alice g2
   in (g3 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}, mine, theirs, attractionId)

-- Run `game` answering each die from `rolls` in turn, every target slot with
-- `target` alone, and everything else as S.identityAnswer does. Answers the
-- final board and the sizes of the dice asked for.
rolling :: [Natural] -> ObjectId -> GameState.GameState -> Game a -> (GameState.GameState, [Natural])
rolling rolls target gs game =
  let answering :: Prompt.Prompt r -> State.State ([Natural], [Natural]) r
      answering p = case p of
        Prompt.RollDie sides -> do
          (pending, sizes) <- State.get
          case pending of
            face : rest -> State.put (rest, sides : sizes) >> pure face
            [] -> State.put ([], sides : sizes) >> pure 1
        -- FILTERED, NOT BUILT: the offered recipient that is `target`.
        Prompt.ChooseTargets _ _ _ slots -> pure (fmap (Set.filter ((== Just target) . Recipient.objectOf) . snd) slots)
        _ -> pure (S.identityAnswer p)
      ((_, after), (_, asked)) = State.runState (Engine.runGame answering gs game) (rolls, [])
   in (after, reverse asked)

-- The roll to visit, then everything it triggered resolved.
visitWith :: [Natural] -> ObjectId -> GameState.GameState -> (GameState.GameState, [Natural])
visitWith rolls target gs =
  rolling rolls target gs (Engine.runTurnBasedActions Phase.PrecombatMain >> Engine.priorityLoop)

-- Could bob decline to block once alice attacks with everything -- her Piker,
-- the one creature she controls? The gameplay question Bumper Cars'
-- requirement answers (CR 509.1c).
bobMayDecline :: GameState.GameState -> Bool
bobMayDecline gs =
  let board = gs {GameState.phase = Phase.Combat CombatStep.DeclareAttackers, GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]}}
      attacked = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
   in Combat.legalBlockDeclaration S.bob Map.empty attacked

-- CR 703.4g / 701.52a / 702.159a: rolling to visit.
visitSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
visitSpec s registry = Spec.describe s "Roll to visit" $ do
  Spec.it s "CR 702.159a a result lit up on the Attraction triggers its visit ability" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    let (gs, mine, _, _) = visitBoard piker (carsA cars)
        (hit, _) = visitWith [3] mine gs
        (miss, _) = visitWith [4] mine gs
    -- THE GAMEPLAY ASSERTION: a 3 is lit on 202a, so the visit ability resolved
    -- and alice's Piker must be blocked; a 4 is not, so nothing triggered.
    Spec.assertBool s (not (bobMayDecline hit)) "CR 509.1c: after a 3, bob must block the Piker"
    Spec.assertBool s (bobMayDecline miss) "CR 701.52a: after a 4, nothing was visited"
    Spec.assertBool s (bobMayDecline gs) "and before the roll, nothing required it"
  Spec.it s "CR 717.1 the lights are the printing's" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    -- The pair of boards above, differing only in the printing: 202f lights 4
    -- and not 3.
    let (gs, mine, _, _) = visitBoard piker (carsF cars)
    Spec.assertBool s (bobMayDecline (fst (visitWith [3] mine gs))) "a 3 visits nothing lit 4, 5 and 6"
    Spec.assertBool s (not (bobMayDecline (fst (visitWith [4] mine gs)))) "and a 4 visits it"
  -- The copy tripwire: a Phyrexian Metamorph copying Bumper Cars is an
  -- Attraction named Bumper Cars with the visit ability, but its card lights
  -- nothing (CR 109.3, 707.2) and has a traditional back (CR 717.6).
  Spec.it s "CR 707.2 a copy of an Attraction rolls but lights nothing, and is no Astrotorium card" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    metamorph <- S.printingOf s registry "Phyrexian Metamorph"
    let (board, mine, _, original) = visitBoard piker (carsA cars)
        copyAnswer :: Prompt.Prompt r -> r
        copyAnswer p = case p of
          Prompt.ChooseCopyTarget {} -> Just original
          _ -> S.identityAnswer p
        (_, staged) = S.spellOnStack metamorph S.alice board
        entered = S.runPure copyAnswer staged Stack.resolveTop
        -- The original leaves, so the copy is alice's only Attraction.
        gs = S.runPure S.identityAnswer entered (Event.changeZone original Zone.Exile)
        printedMetamorph oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Phyrexian Metamorph"))
        copyId = Maybe.fromMaybe original (List.find printedMetamorph (Set.toList (GameState.battlefield gs)))
    Spec.assertEqWith s "CR 707.2: the copy is a Bumper Cars" (Projection.namesOf copyId gs) (Set.singleton bumperCars)
    Spec.assertEqWith s "CR 717.4: it is an Attraction, so alice rolls" (snd (visitWith [3] mine gs)) [6]
    Spec.assertBool s (bobMayDecline (fst (visitWith [3] mine gs))) "CR 109.3: a 3 visits nothing, the copy's card lighting nothing"
    let binned = S.runPure S.identityAnswer gs (Event.changeZone copyId Zone.Graveyard)
    Spec.assertEqWith s "CR 717.6: the Metamorph card goes to the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice binned)) 1
  Spec.it s "CR 717.4 a player who controls no Attraction does not roll" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    let (gs, mine, _, _) = visitBoard piker (carsA cars)
        -- bob's turn: alice's Attraction is not his.
        bobs = gs {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
    Spec.assertEqWith s "alice rolls one d6 on her turn" (snd (visitWith [3] mine gs)) [6]
    Spec.assertEqWith s "bob rolls nothing on his" (snd (visitWith [3] mine bobs)) []
  Spec.it s "CR 701.52a the roll to visit is a die roll: Pixie Guide adds a die and CR 706.6 ignores the lower" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    guide <- S.printingOf s registry "Pixie Guide"
    let (gs, mine, _, _) = visitBoard piker (carsA cars)
        guarded = snd (S.addPermanent guide S.alice gs)
    -- THE GAMEPLAY ASSERTION: 1 then 3 under the Guide keeps the 3, which is
    -- lit; without it the 1 is the result, which is not.
    Spec.assertBool s (not (bobMayDecline (fst (visitWith [1, 3] mine guarded)))) "CR 706.6: the 1 is ignored and the 3 visits"
    Spec.assertBool s (bobMayDecline (fst (visitWith [1, 3] mine gs))) "CR 706.1: without the Guide the 1 stands"
    Spec.assertEqWith s "two dice under the Guide" (snd (visitWith [1, 3] mine guarded)) [6, 6]
  Spec.it s "CR 703.4g the turn machinery itself rolls as the precombat main phase begins" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    let (gs, mine, _, _) = visitBoard piker (carsA cars)
        (_, asked) = rolling [3] mine gs Engine.runStep
    Spec.assertEqWith s "one d6" asked [6]

-- CR 717.6: the junkyard.
junkyardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
junkyardSpec s registry = Spec.describe s "The junkyard" $ do
  Spec.it s "CR 717.6 an Attraction bound for the graveyard goes to the command zone" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    let (gs, mine, _, attraction) = visitBoard piker (carsA cars)
        binned = S.runPure S.identityAnswer gs (Event.changeZone attraction Zone.Graveyard)
        exiled = S.runPure S.identityAnswer gs (Event.changeZone attraction Zone.Exile)
        creature = S.runPure S.identityAnswer gs (Event.changeZone mine Zone.Graveyard)
        faceUp = Set.toList (GameState.command binned)
    Spec.assertEqWith s "CR 717.6: not in the graveyard" (Game.zoneMembers Zone.Graveyard S.alice binned) []
    Spec.assertEqWith s "CR 717.6a: face up in the command zone, still lit" (fmap (`Game.lightsOf` binned) faceUp) [Set.fromList [2, 3, 6]]
    Spec.assertEqWith s "owned by alice" (fmap (fmap Object.owner . (`Game.lookupObject` binned)) faceUp) [Just S.alice]
    Spec.assertEqWith s "CR 717.6: exile is not redirected" (length (Game.zoneMembers Zone.Exile S.alice exiled)) 1
    Spec.assertEqWith s "and a creature card still dies" (length (Game.zoneMembers Zone.Graveyard S.alice creature)) 1
  Spec.it s "CR 717.6 an Attraction bound for its owner's hand goes to the command zone" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    let (gs, _, _, attraction) = visitBoard piker (carsA cars)
        bounced = S.runPure S.identityAnswer gs (Event.changeZone attraction Zone.Hand)
    Spec.assertEqWith s "not in hand" (Game.zoneMembers Zone.Hand S.alice bounced) []
    Spec.assertEqWith s "but in the command zone" (Set.size (GameState.command bounced)) 1
  Spec.it s "CR 717.6a a junkyard card does not function" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    let (gs, mine, _, attraction) = visitBoard piker (carsA cars)
        binned = S.runPure S.identityAnswer gs (Event.changeZone attraction Zone.Graveyard)
    Spec.assertEqWith s "no roll: alice controls no Attraction" (snd (visitWith [3] mine binned)) []

-- CR 727.2 / 729.2a / 729.5a: a new game.
newGameSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
newGameSpec s registry = Spec.describe s "A new game" $ do
  Spec.it s "CR 727.2 a restart puts every Attraction card back in its owner's Attraction deck" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    let (board, _, _, _) = visitBoard piker (carsA cars)
        gs = withAttractions S.alice [carsF cars] board
        restarted = S.runPure S.identityAnswer gs (Setup.restartGame Resolve.performHandAction Set.empty S.alice)
        lights = Set.fromList (fmap (`Game.lightsOf` restarted) (Attraction.deckOf S.alice restarted))
    Spec.assertEqWith s "both Bumper Cars are in alice's Attraction deck" lights (Set.fromList [Set.fromList [2, 3, 6], Set.fromList [4, 5, 6]])
    Spec.assertEqWith s "and neither is on the battlefield" (S.countOnBattlefieldByName bumperCars S.alice restarted) 0
    Spec.assertEqWith s "the Piker went to the library, the Attraction did not" (length (Game.zoneMembers Zone.Library S.alice restarted) + length (Game.zoneMembers Zone.Hand S.alice restarted)) 1
  Spec.it s "CR 729.2a a subgame takes the Attraction deck, and CR 729.5a gives it back" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    cars <- S.printingOf s registry "Bumper Cars"
    let (board, _, _, faceUp) = visitBoard piker (carsA cars)
        parent = withAttractions S.alice [carsF cars] board
        sub = S.runPure S.identityAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards Resolve.performHandAction Set.empty)
        back = Setup.funnelBack sub parent
    Spec.assertEqWith s "CR 729.2a: the deck is in the subgame" (fmap (`Game.lightsOf` sub) (Attraction.deckOf S.alice sub)) [Set.fromList [4, 5, 6]]
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject faceUp sub)) "and the face-up one stayed behind"
    Spec.assertEqWith s "CR 729.5a: the deck is back" (fmap (`Game.lightsOf` back) (Attraction.deckOf S.alice back)) [Set.fromList [4, 5, 6]]
    Spec.assertEqWith s "not in the library" (filter (\oid -> Game.lightsOf oid back /= Set.empty) (Game.zoneMembers Zone.Library S.alice back)) []
    Spec.assertEqWith s "and the face-up one never moved" (Game.zoneOf faceUp back) (Just Zone.Battlefield)
