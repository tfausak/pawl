{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Vanguard, and the CR 902 readings its four callers make:
-- Pawl.Engine.Setup's starting life and command zone, Pawl.Engine.Mulligan's
-- starting hand size, Pawl.Engine.PlayerEffect's maximum hand size, and the two
-- walks that let a vanguard's abilities function from the command zone
-- (Pawl.Engine.Event's triggered, Pawl.Engine.Projection's static).
module Pawl.VanguardSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- A deck of thirty Mountains, plus whichever vanguard card the leg brings. Every
-- leg below is a pair of boards built from this and differing in exactly that
-- one argument.
deckOf :: Printing.Printing -> Maybe Printing.Printing -> Deck.Deck
deckOf mountain vanguard =
  Deck.MkDeck
    { Deck.cards = Map.singleton mountain 30,
      Deck.commander = Nothing,
      Deck.vanguard = vanguard,
      Deck.dungeons = Set.empty,
      Deck.sideboard = Map.empty
    }

-- Alice brings `vanguard`, bob brings none. Bob's deck is built too, so his
-- library is stocked for the opening draws (CR 104.3c would otherwise deck him)
-- and he is the in-board control for every per-player assertion.
built :: Printing.Printing -> Maybe Printing.Printing -> GameState.GameState
built mountain vanguard =
  S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) $ do
    Setup.createDeck S.alice (deckOf mountain vanguard)
    Setup.createDeck S.bob (deckOf mountain Nothing)

-- Keep the first hand, so the opening draw is what the hand size assertions read.
keepAnswer :: Prompt.Prompt r -> r
keepAnswer p = case p of
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Mulligan exactly once, then keep, bottoming whatever the engine offers first.
mulliganOnceAnswer :: Prompt.Prompt r -> r
mulliganOnceAnswer p = case p of
  Prompt.DeclareMulligan _ pid offer ->
    if pid == S.alice && MulliganOffer.taken offer == 0 then MulliganDecision.Mulligan else MulliganDecision.Keep
  _ -> S.identityAnswer p

opened :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
opened answer gs = S.runPure answer gs (Mulligan.openingHands S.performer [S.alice, S.bob])

handSize :: GameState.GameState -> Int
handSize gs = length (Game.zoneMembers Zone.Hand S.alice gs)

commandNames :: GameState.GameState -> [CardName.CardName]
commandNames gs = fmap (\oid -> maybe (CardName.MkCardName (Text.pack "?")) S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Command S.alice gs)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Vanguard" $ do
  -- CR 902.3 / CR 313.2: the card is placed beside the library before the game
  -- begins, which is the command zone, and it is NOT one of the deck's cards --
  -- the library is the thirty Mountains and nothing more.
  Spec.it s "CR 902.3 a vanguard card begins the game face up in the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    let with = built mountain (Just gerrard)
        without = built mountain Nothing
    Spec.assertEqWith s "the vanguard is in alice's command zone" (commandNames with) [CardName.MkCardName (Text.pack "Gerrard")]
    Spec.assertEqWith s "face up" (fmap (\oid -> fmap Object.facing (Game.lookupObject oid with)) (Game.zoneMembers Zone.Command S.alice with)) [Just Facing.FaceUp]
    Spec.assertEqWith s "and not among the thirty cards of her library" (length (Game.zoneMembers Zone.Library S.alice with)) 30
    Spec.assertEqWith s "bob, who brought none, has an empty command zone" (Game.zoneMembers Zone.Command S.bob with) []
    Spec.assertEqWith s "and neither has alice on the deck that differs only in that field" (commandNames without) []

  -- CR 902.4 / CR 313.7: twenty plus or minus the life modifier. Three legs on
  -- one deck: Selenia's +7, Gerrard's printed +0 -- which must NOT be read as
  -- "no vanguard" -- and no vanguard at all.
  Spec.it s "CR 902.4 the starting life total is twenty plus the life modifier" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    selenia <- S.printingOf s registry "Selenia"
    Spec.assertEqWith s "Selenia's +7" (S.lifeOf S.alice (built mountain (Just selenia))) (Just 27)
    Spec.assertEqWith s "Gerrard's +0" (S.lifeOf S.alice (built mountain (Just gerrard))) (Just 20)
    Spec.assertEqWith s "no vanguard at all" (S.lifeOf S.alice (built mountain Nothing)) (Just 20)
    Spec.assertEqWith s "and bob, on the same board, keeps CR 103.4's twenty" (S.lifeOf S.bob (built mountain (Just selenia))) (Just 20)

  -- CR 902.5 / CR 313.6: seven cards, as modified. Gerrard's -4 and Selenia's +1
  -- move it in both directions off the same board.
  Spec.it s "CR 902.5 the opening hand is seven cards as modified by the hand modifier" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    selenia <- S.printingOf s registry "Selenia"
    Spec.assertEqWith s "Gerrard's -4 draws three" (handSize (opened keepAnswer (built mountain (Just gerrard)))) 3
    Spec.assertEqWith s "Selenia's +1 draws eight" (handSize (opened keepAnswer (built mountain (Just selenia)))) 8
    Spec.assertEqWith s "no vanguard draws CR 103.5's seven" (handSize (opened keepAnswer (built mountain Nothing))) 7
    Spec.assertEqWith s "and bob draws seven beside her" (length (Game.zoneMembers Zone.Hand S.bob (opened keepAnswer (built mountain (Just gerrard))))) 7

  -- CR 902.5a: a mulligan "draws a new hand equal to their starting hand size",
  -- which is the modified number and not seven. At two seats no mulligan is free
  -- (CR 103.5c), so the first one bottoms one card: three drawn less one bottomed
  -- against seven drawn less one bottomed.
  Spec.it s "CR 902.5a a mulligan redraws to the modified starting hand size" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    Spec.assertEqWith s "Gerrard: three redrawn, one bottomed" (handSize (opened mulliganOnceAnswer (built mountain (Just gerrard)))) 2
    Spec.assertEqWith s "no vanguard: seven redrawn, one bottomed" (handSize (opened mulliganOnceAnswer (built mountain Nothing))) 6

  -- CR 902.5b / CR 402.2: the maximum hand size takes the same modifier, and CR
  -- 514.1's cleanup discard is where a player pays it. Driven through
  -- Engine.discardToHandSize rather than asserted off
  -- PlayerEffect.maximumHandSize, so the number is read where the rule spends it.
  Spec.it s "CR 902.5b the maximum hand size is seven as modified" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    let -- Both legs enter the cleanup holding SEVEN, which is what makes the pair
        -- differ in one thing: Gerrard's opening hand is three, so it is topped up
        -- to the seven the control draws on its own.
        sevenInHand vanguard =
          let board = opened keepAnswer (built mountain vanguard)
           in List.foldl' (\g _ -> snd (S.addHandCard mountain S.alice g)) board (replicate (7 - handSize board) ())
        trimmed vanguard = handSize (S.runPure S.identityAnswer (sevenInHand vanguard) (Engine.discardToHandSize S.alice))
    Spec.assertEqWith s "both legs hold seven going in" (fmap (handSize . sevenInHand) [Just gerrard, Nothing]) [7, 7]
    Spec.assertEqWith s "Gerrard's -4 trims her to three" (trimmed (Just gerrard)) 3
    Spec.assertEqWith s "and without him CR 402.2's seven takes nothing" (trimmed Nothing) 7
    Spec.assertEqWith s "the number CR 402.2's fold seeds itself with, for Gerrard" (PlayerEffect.maximumHandSize S.alice (built mountain (Just gerrard))) (Just 3)
    Spec.assertEqWith s "and for bob beside him" (PlayerEffect.maximumHandSize S.bob (built mountain (Just gerrard))) (Just 7)

  -- CR 902.7 / CR 313.4: "its triggered abilities may trigger" from the command
  -- zone. Gerrard's draw-step trigger against the turn-based draw of CR 504.1:
  -- one draw is the step's own, two is the step's plus his.
  Spec.it s "CR 902.7 a vanguard's triggered ability triggers from the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    let drawStep vanguard =
          let board = opened keepAnswer (built mountain vanguard)
              before = handSize board
              -- Turn two, not turn one: CR 103.8a has the starting player skip the
              -- draw of their FIRST turn, which would leave the control leg drawing
              -- nothing and the difference resting on that rule instead of this one.
              after = S.runPure S.identityAnswer (board {GameState.phase = Phase.Beginning BeginningStep.DrawStep, GameState.turnNumber = 2}) Engine.runStep
           in handSize after - before
    Spec.assertEqWith s "with Gerrard, alice draws two" (drawStep (Just gerrard)) 2
    Spec.assertEqWith s "without him, CR 504.1's one" (drawStep Nothing) 1

  -- CR 902.7 / CR 313.4 again: "its static abilities affect the game" from the
  -- command zone. Selenia's grant reaches alice's creature and not bob's, so the
  -- ability's controller is CR 902.6's owner and the walk is not handing the
  -- keyword to the board.
  Spec.it s "CR 902.7 a vanguard's static ability functions from the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    selenia <- S.printingOf s registry "Selenia"
    let place vanguard =
          let (mine, g1) = S.addCreature piker S.alice (built mountain vanguard)
              (theirs, g2) = S.addCreature piker S.bob g1
           in (mine, theirs, g2)
        (aliceCreature, bobCreature, board) = place (Just selenia)
        (aliceControl, _, control) = place Nothing
    Spec.assertBool s (Projection.hasKeyword Keyword.Vigilance aliceCreature board) "alice's creature has vigilance"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Vigilance bobCreature board)) "bob's, on the same board, does not"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Vigilance aliceControl control)) "and neither does alice's on the board without the vanguard"

  -- CR 313.2 against CR 727.2: a restart puts every card into its owner's new
  -- library, and the vanguard card is the exception -- it stays where it is, and
  -- rule 902.4's life total is dealt out again from it.
  Spec.it s "CR 313.2 / 727.1 a restarted game keeps the vanguard in the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    selenia <- S.printingOf s registry "Selenia"
    let restarted = S.runPure keepAnswer (built mountain (Just selenia)) (Setup.restartGame S.performer Set.empty S.alice)
    Spec.assertEqWith s "still in the command zone" (commandNames restarted) [CardName.MkCardName (Text.pack "Selenia")]
    Spec.assertEqWith s "and CR 902.4's twenty-seven is dealt out again" (S.lifeOf S.alice restarted) (Just 27)
    Spec.assertEqWith s "her opening hand is eight again" (handSize restarted) 8

  -- CR 729.2b and CR 729.5b: the vanguard card crosses into a subgame and comes
  -- back out of it, which is the pair CR 729.2c and CR 729.5c make for a
  -- commander.
  Spec.it s "CR 729.2b / 729.5b a subgame takes the vanguard card and gives it back" $ do
    mountain <- S.printingOf s registry "Mountain"
    selenia <- S.printingOf s registry "Selenia"
    let parent = built mountain (Just selenia)
        sub = S.runPure keepAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty)
        back = Setup.funnelBack sub parent
    Spec.assertEqWith s "the subgame's command zone holds it" (commandNames sub) [CardName.MkCardName (Text.pack "Selenia")]
    Spec.assertEqWith s "and CR 902.4 applies inside the subgame too" (S.lifeOf S.alice sub) (Just 27)
    Spec.assertEqWith s "the main game has it back when the subgame ends" (commandNames back) [CardName.MkCardName (Text.pack "Selenia")]
