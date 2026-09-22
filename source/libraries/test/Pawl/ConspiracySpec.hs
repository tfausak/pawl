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
module Pawl.ConspiracySpec where

import qualified Data.Map.Strict as Map
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
