{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Conspiracy and the CR 315 readings its callers make:
-- Pawl.Engine.Setup's CR 315.2 placement and CR 315.3 restart hold, and
-- Pawl.Engine.Vanguard.functionsFromCommandZone's conspiracy arm (CR 113.6p /
-- 315.5). Pawl.OutsideTheGameSpec's Ring of Ma'rûf pair covers CR 315.3's last
-- sentence.
--
-- Sentinel Dispatch (Conspiracy, no mana cost; "(Start the game with this
-- conspiracy face up in the command zone.) At the beginning of the first upkeep,
-- create a 1/1 colorless Construct artifact creature token with defender." --
-- name, type line and Oracle text checked against api.scryfall.com 2026-09-22,
-- paper printing `cns`) is the producer. Its "the first upkeep" is transcribed
-- as an each-upkeep trigger that triggers only once: CR 315.3 keeps the card in
-- the command zone from before the first upkeep to the end of the game, so its
-- first triggering is at the game's first upkeep.
--
-- Power Play (Conspiracy, no mana cost; "(Start the game with this conspiracy
-- face up in the command zone.) You are the starting player. If multiple players
-- would be the starting player, one of those players is chosen at random." --
-- checked against api.scryfall.com 2026-09-29, paper printing `cns`) is CR
-- 103.1c's card, and Pawl.Engine.Setup.claimStartingPlayer its reader.
module Pawl.ConspiracySpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RestartSignal as RestartSignal
import qualified Pawl.Types.Zone as Zone

-- Thirty Mountains, plus whichever conspiracies the leg brings.
deckOf :: Printing.Printing -> [Printing.Printing] -> Deck.Deck
deckOf mountain conspiracies =
  (Deck.fromCards (Map.singleton mountain 30))
    { Deck.conspiracies = Map.fromListWith (+) (fmap (\p -> (p, 1)) conspiracies)
    }

-- BOB brings the conspiracies and alice, the starting player, brings none: the
-- game's first upkeep is then alice's, so a trigger that fires there is not
-- reading "your first upkeep". Both libraries are stocked for the opening draws
-- (CR 104.3c).
built :: Printing.Printing -> [Printing.Printing] -> GameState.GameState
built mountain conspiracies =
  S.runPure keepAnswer (Setup.emptyGame S.bothPlayers) $ do
    Setup.createDeck S.alice (deckOf mountain [])
    Setup.createDeck S.bob (deckOf mountain conspiracies)
    Mulligan.openingHands S.performer [S.alice, S.bob]

keepAnswer :: Prompt.Prompt r -> r
keepAnswer p = case p of
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

commandNames :: PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
commandNames pid gs = fmap (\oid -> maybe (CardName.MkCardName (Text.pack "?")) S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Command pid gs)

-- What a whole-setup run asked: who declared a mulligan, in order, and the
-- candidates of each CR 103.1c random draw.
data Asked
  = DeclaredBy PlayerId.PlayerId
  | DrewFrom [PlayerId.PlayerId]
  deriving (Eq, Show)

-- Keeps every hand and takes every offered CR 103.6 action, recording each ask.
-- A CR 103.1c draw is answered with the candidate at index `pick`, pinned by
-- position so the answer cannot go looking for the right player.
recording :: Int -> Prompt.Prompt r -> State.State [Asked] r
recording pick p = case p of
  Prompt.DeclareMulligan _ pid _ -> do
    State.modify' (DeclaredBy pid :)
    pure MulliganDecision.Keep
  Prompt.OpeningHandAction _ _ candidates -> pure (Maybe.listToMaybe candidates)
  Prompt.RandomFirstPlayer candidates -> do
    State.modify' (DrewFrom (NonEmpty.toList candidates) :)
    pure (Maybe.fromMaybe (NonEmpty.head candidates) (Maybe.listToMaybe (drop pick (NonEmpty.toList candidates))))
  _ -> pure (S.identityAnswer p)

-- The whole of Setup.newGame over this matchup, seated in its order -- so its
-- head is CR 103.1's determination -- with what `recording pick` was asked.
started :: Int -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (GameState.GameState, [Asked])
started pick matchup =
  let ((_, gs), asked) = State.runState (Engine.runGame (recording pick) (Setup.emptyGame (fmap fst matchup)) (Setup.newGame S.performer matchup)) []
   in (gs, reverse asked)

declarers :: [Asked] -> [PlayerId.PlayerId]
declarers asked = [pid | DeclaredBy pid <- asked]

draws :: [Asked] -> [[PlayerId.PlayerId]]
draws asked = [pids | DrewFrom pids <- asked]

-- Run the upkeep of the given turn, with the given player active.
upkeep :: PlayerId.PlayerId -> Natural -> GameState.GameState -> GameState.GameState
upkeep active turn gs =
  S.runPure
    S.identityAnswer
    ( gs
        { GameState.activePlayer = active,
          GameState.phase = Phase.Beginning BeginningStep.Upkeep,
          GameState.turnNumber = turn,
          GameState.restartSignal = RestartSignal.Playing
        }
    )
    Engine.runStep

-- The Construct tokens on the battlefield, by who CONTROLS them (CR 110.2).
constructsControlledBy :: PlayerId.PlayerId -> GameState.GameState -> Int
constructsControlledBy pid gs =
  length
    [ oid
    | oid <- Set.toList (GameState.battlefield gs),
      fmap S.nameOf (Game.cardOf oid gs) == Just (CardName.MkCardName (Text.pack "Construct Token")),
      Projection.controllerOf oid gs == Just pid
    ]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Conspiracy" $ do
  -- CR 315.2 / 315.6: the conspiracy starts face up in its owner's command zone
  -- and is not one of the deck's cards.
  Spec.it s "CR 315.2 a conspiracy begins the game face up in the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    dispatch <- S.printingOf s registry "Sentinel Dispatch"
    let with = built mountain [dispatch]
    Spec.assertEqWith s "the conspiracy is in bob's command zone" (commandNames S.bob with) [CardName.MkCardName (Text.pack "Sentinel Dispatch")]
    Spec.assertEqWith s "face up" (fmap (\oid -> fmap Object.facing (Game.lookupObject oid with)) (Game.zoneMembers Zone.Command S.bob with)) [Just Facing.FaceUp]
    Spec.assertEqWith s "and not among the thirty cards of his library and hand" (length (Game.zoneMembers Zone.Library S.bob with) + length (Game.zoneMembers Zone.Hand S.bob with)) 30
    Spec.assertEqWith s "alice, who brought none, has an empty command zone" (Game.zoneMembers Zone.Command S.alice with) []

  -- CR 315.5 / 113.6p: the conspiracy's triggered ability triggers from the
  -- command zone, at the game's first upkeep -- alice's, not bob's -- and at no
  -- upkeep after it. The control leg differs only in the conspiracy.
  Spec.it s "CR 315.5 Sentinel Dispatch creates its Construct at the first upkeep and never again" $ do
    mountain <- S.printingOf s registry "Mountain"
    dispatch <- S.printingOf s registry "Sentinel Dispatch"
    let first = upkeep S.alice 1 (built mountain [dispatch])
        second = upkeep S.bob 2 first
        control = upkeep S.alice 1 (built mountain [])
    Spec.assertEqWith s "at alice's first upkeep, bob gets a Construct" (constructsControlledBy S.bob first) 1
    Spec.assertEqWith s "and at bob's own upkeep after it, no second one" (constructsControlledBy S.bob second) 1
    Spec.assertEqWith s "without the conspiracy, no Construct" (constructsControlledBy S.bob control) 0
    Spec.assertEqWith s "and alice, who has none, gets none" (constructsControlledBy S.alice second) 0

  -- CR 315.3 against CR 727.2: a restart puts every card into its owner's new
  -- library, and the conspiracy stays in the command zone -- where the new game's
  -- first upkeep triggers it again.
  Spec.it s "CR 315.3 / 727.1 a restarted game keeps the conspiracy in the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    dispatch <- S.printingOf s registry "Sentinel Dispatch"
    let played = upkeep S.bob 2 (upkeep S.alice 1 (built mountain [dispatch]))
        restarted = S.runPure keepAnswer played (Setup.restartGame S.performer Set.empty S.alice)
    Spec.assertEqWith s "still in bob's command zone" (commandNames S.bob restarted) [CardName.MkCardName (Text.pack "Sentinel Dispatch")]
    Spec.assertEqWith s "not shuffled into his thirty-card library" (length (Game.zoneMembers Zone.Library S.bob restarted) + length (Game.zoneMembers Zone.Hand S.bob restarted)) 30
    Spec.assertEqWith s "and the new game's first upkeep makes a Construct again" (constructsControlledBy S.bob (upkeep S.alice 1 restarted)) 1

  -- CR 103.1c: alice is seated first, so CR 103.1 makes her the starting player
  -- -- until bob's Power Play supersedes it. Gemstone Caverns is the observer:
  -- "if ... you're not the starting player" reads Quantity.IsStartingPlayer. Both
  -- decks are all Caverns, so the two seats differ only in the conspiracy, and
  -- the control leg differs from this one only in that.
  Spec.describe s "Power Play" $ do
    Spec.it s "CR 103.1c Power Play makes its controller the starting player" $ do
      caverns <- S.printingOf s registry "Gemstone Caverns"
      powerPlay <- S.printingOf s registry "Power Play"
      let leg conspiracies = started 0 ((S.alice, deckOf caverns []) NonEmpty.:| [(S.bob, deckOf caverns conspiracies)])
          (with, asked) = leg [powerPlay]
          (control, controlAsked) = leg []
          begunWith pid gs = not (null (Game.zoneMembers Zone.Battlefield pid gs))
      Spec.assertEqWith s "alice, no longer the starting player, begins with her Caverns" (begunWith S.alice with) True
      Spec.assertEqWith s "bob, now the starting player, does not" (begunWith S.bob with) False
      Spec.assertEqWith s "without Power Play, alice starts and has none" (begunWith S.alice control) False
      Spec.assertEqWith s "and bob has his" (begunWith S.bob control) True
      Spec.assertEqWith s "bob takes the first turn" (GameState.activePlayer with) S.bob
      Spec.assertEqWith s "the turn order begins with him" (GameState.turnOrder with) [S.bob, S.alice]
      -- CR 103.5: the starting player declares first.
      Spec.assertEqWith s "bob declares his mulligan first" (declarers asked) [S.bob, S.alice]
      Spec.assertEqWith s "the control leg is untouched" (declarers controlAsked) [S.alice, S.bob]
      -- One claimant is no draw.
      Spec.assertEqWith s "nothing is drawn at random" (draws asked) []

    -- Power Play's second sentence, and its 2014-05-29 ruling: the draw is over
    -- exactly the two claimants, and the order is rotated, not reseated, so the
    -- loser keeps their place. Two logs, one landing on each claimant; three
    -- seats so the non-claimant is a third player the draw must leave out.
    Spec.it s "CR 103.1c two Power Plays: one claimant is drawn at random" $ do
      mountain <- S.printingOf s registry "Mountain"
      powerPlay <- S.printingOf s registry "Power Play"
      let matchup = (S.alice, deckOf mountain []) NonEmpty.:| [(S.bob, deckOf mountain [powerPlay]), (S.carol, deckOf mountain [powerPlay])]
          (toCarol, carolAsked) = started 1 matchup
          (toBob, bobAsked) = started 0 matchup
      Spec.assertEqWith s "landing on carol, she starts and bob goes last" (GameState.turnOrder toCarol) [S.carol, S.alice, S.bob]
      Spec.assertEqWith s "landing on bob, he starts and alice goes last" (GameState.turnOrder toBob) [S.bob, S.carol, S.alice]
      Spec.assertEqWith s "carol takes the first turn" (GameState.activePlayer toCarol) S.carol
      Spec.assertEqWith s "one draw, over bob and carol alone" (draws carolAsked) [[S.bob, S.carol]]
      Spec.assertEqWith s "the same draw in the other log" (draws bobAsked) [[S.bob, S.carol]]

    -- CR 727.1a then CR 103.1c: alice restarts the game, and bob's Power Play,
    -- still in his command zone (CR 315.3), supersedes her claim.
    Spec.it s "CR 103.1c / 727.1a Power Play supersedes a restart's starting player" $ do
      mountain <- S.printingOf s registry "Mountain"
      powerPlay <- S.printingOf s registry "Power Play"
      let restart conspiracies = S.runPure keepAnswer (built mountain conspiracies) (Setup.restartGame S.performer Set.empty S.alice)
      Spec.assertEqWith s "with Power Play, bob starts the new game" (GameState.turnOrder (restart [powerPlay])) [S.bob, S.alice]
      Spec.assertEqWith s "without it, alice does" (GameState.turnOrder (restart [])) [S.alice, S.bob]
