module Pawl.Benchmark where

import qualified Control.Exception as Exception
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Script as Script
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Types.Deck as Deck
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import Pawl.Types.Result (Result)
import qualified Test.Tasty.Bench as Bench

-- Two players, seeded from the benchmark's argument.
playersFrom :: Natural -> NonEmpty.NonEmpty PlayerId
playersFrom n = PlayerId.MkPlayerId n NonEmpty.:| [PlayerId.MkPlayerId (n + 1)]

-- Takes the first player's id so the whole game genuinely depends on the
-- benchmark's argument; otherwise GHC floats the result out and times a cached
-- value rather than the game. NOINLINE keeps it from being folded back in.
goldfish :: Deck.Deck -> Natural -> Result
goldfish deck n =
  let players = playersFrom n
   in fst (Engine.runMatchPure Script.passing (Setup.mirror deck players))
{-# NOINLINE goldfish #-}

-- Parameterized for the same reason as 'goldfish'.
casting :: Deck.Deck -> Natural -> Result
casting deck n =
  let players = playersFrom n
   in fst (Engine.runMatchPure Script.casting (Setup.mirror deck players))
{-# NOINLINE casting #-}

fighting :: Deck.Deck -> Natural -> Result
fighting deck n =
  let players = playersFrom n
   in fst (Engine.runMatchPure Script.fighting (Setup.mirror deck players))
{-# NOINLINE fighting #-}

-- The redDeck recipe (name -> count), loaded from the registry -- the proof
-- that files -> parse -> a real game works end-to-end.
-- The benchmark is at IO and has no spec record to fold a failure into, so an
-- unloadable card is an exception. Pawl.Support.printingOf is the spec-side
-- adapter for the same Pawl.Cards deck lists.
fetchOrThrow :: Registry.Registry IO -> String -> IO Printing.Printing
fetchOrThrow registry name = do
  result <- Registry.named registry name
  case result of
    Nothing -> Exception.throwIO (userError ("no such card: " <> name))
    Just card -> pure (Printing.MkPrinting card)

loadRedDeck :: Registry.Registry IO -> IO Deck.Deck
loadRedDeck registry = do
  mountain <- fetchOrThrow registry "Mountain"
  piker <- fetchOrThrow registry "Goblin Piker"
  maiden <- fetchOrThrow registry "Bird Maiden"
  bolt <- fetchOrThrow registry "Lightning Bolt"
  pure (Deck.fromCards (Map.fromList [(mountain, 36), (piker, 12), (maiden, 8), (bolt, 4)]))

-- A deck that reliably attaches several Auras to a populated battlefield, so
-- Sba.fallsOff (per-Aura board re-derivation) and Projection.controllerOf
-- (walking the battlefield for control-granting statics, the hot path
-- Projection.controls calls at every SBA sweep) both get exercised here -- no
-- prior benchmark deck contained an Aura at all.
--
-- Setup.createDeck builds each player's library from 'Data.Map.Strict.toList',
-- so it is grouped by Printing's derived Ord -- primarily 'name' (Text, so
-- ordinary lexicographic order) -- NOT interleaved, and Prompt.Shuffle is the
-- identity in every answer function above, so the library is never actually
-- shuffled. "Control Magic" < "Darksteel Myr" < "Island" alphabetically, so the
-- library is exactly [Control Magic x3][Darksteel Myr x4][Island x53] with no
-- randomness at all. The opening hand is the library's first 7 cards -- all 3
-- Control Magic and all 4 Darksteel Myr, i.e. the entire non-land half of the
-- deck -- and every card kept in a hand at the maximum size stays there until
-- something makes room, per discardNewest above (CR 514.1 discards the newest
-- cards first, keeping this same original 7). Nothing else ever gets a turn at
-- being kept: every OTHER card this deck draws arrives during a mana-less
-- stretch with nothing to spend it on, so it is discarded again by the next
-- cleanup step -- which is why this deck carries not one spare copy of either
-- spell. Darksteel Myr (colourless, {3}) is castable off the Islands alone, so
-- once enough of the 53 Islands have entered play as one-a-turn land drops
-- (CR 305.2), all 4 Myr resolve as legal Cast targets for Control Magic
-- ({2}{U}{U}, also Island-payable) to enchant, in turn making all 6 Control
-- Magic across the mirror match legal casts. Observed (via 'cabal repl',
-- reading 'GameState.turnNumber' after 'Engine.runMatchPure'): this deck
-- decks out on turn 108. Darksteel Myr is 0/1 Indestructible, so once all 8
-- are on the battlefield, "fighting 2p aura" runs a full 4v4 attack/block/
-- damage cycle on essentially every one of those 108 turns -- nothing ever
-- dies, so combat never tapers off the way it does against the red mirror's
-- 1/2 Bird Maidens, which gang-block and kill the lead attacker.
loadControlDeck :: Registry.Registry IO -> IO Deck.Deck
loadControlDeck registry = do
  island <- fetchOrThrow registry "Island"
  myr <- fetchOrThrow registry "Darksteel Myr"
  control <- fetchOrThrow registry "Control Magic"
  pure (Deck.fromCards (Map.fromList [(island, 53), (myr, 4), (control, 3)]))

-- loadControlDeck's PAIRED CONTROL: the same 60 cards and the same 4 Darksteel
-- Myr, with the 3 Control Magic replaced by 3 more Islands -- so the only
-- difference between the two scenarios is the Aura (#200).
--
-- Without a control, "fighting 2p aura" is a baseline rather than a diagnostic:
-- its number folds together what an Aura costs the engine (per-Aura
-- re-derivation in Sba.fallsOff, and the control-grant walk
-- Projection.controlGrants adds to every controllerOf) with what its WORKLOAD
-- costs (a 4v4 board that never dies, over 108 turns). Only the difference
-- between the two attributes anything.
--
-- Deck size and creature count are held fixed on purpose, because both drive
-- the workload. 60 cards means the same number of turns: both decks are observed
-- (via 'cabal repl bench:pawl-benchmark', reading 'GameState.turnNumber' after
-- 'Engine.runMatchPure Script.fighting') to deck out on turn 108, and to leave a
-- battlefield of comparable size -- 115 permanents here against 120 there. And 4
-- Darksteel Myr a side is the same attack/block/damage cycle, since a 0/1
-- indestructible never dies and so never stops attacking.
--
-- What the missing Aura does change is WHO controls those 8 Myr. Measured the
-- same way: 4 and 4 at the end here, 5 and 3 there, because each Control Magic
-- that resolves steals one. That drift is the Aura's own doing and is part of
-- what the pair measures, not a flaw in the control.
loadNoAuraDeck :: Registry.Registry IO -> IO Deck.Deck
loadNoAuraDeck registry = do
  island <- fetchOrThrow registry "Island"
  myr <- fetchOrThrow registry "Darksteel Myr"
  pure (Deck.fromCards (Map.fromList [(island, 56), (myr, 4)]))

-- The only deck here that reaches CR 613.8's dependency ordering at all. Every
-- other scenario above runs Projection.projectDeciding's tight fold instead:
-- that loop is entered only at a layer in `movableLayers`, which needs a
-- candidate whose affected set is `staticallyMovable` (a Matching* or
-- AttachedPlayerControls filter) or one whose magnitude counts its own layer,
-- and no printing in the decks above carries a static ability except Control
-- Magic, whose one ability is Affected.Attached.
--
-- Ashaya, Soul of the Wild ({3}{G}{G}) and Kormus Bell ({4}) are a CR 613.8a
-- clause (b) pair at layer 4, in both directions. Ashaya's affected set --
-- nontoken creatures you control -- reads card types, and Kormus Bell's layer-4
-- modification writes one (AddCardType Creature onto every Swamp), so a Swamp
-- the Bell animates joins Ashaya's set. Back the other way, Kormus Bell's set
-- reads a subtype and Ashaya writes one (AddLandSubtype Forest), so the aspect
-- screen clears there too and `changesAt` runs on that pair as well. Both cards ADD a
-- subtype rather than setting one, so CR 305.7 never strips Ashaya's
-- characteristic-defining ability -- which is what rules Blood Moon out as the
-- partner even though it makes the same cross-object edge: with a set land
-- subtype in the pairing, every Ashaya cast over a whole match was observed in a
-- graveyard at the end of it, so the edge would exist for a turn rather than for
-- the game.
--
-- Deck order is the same forced sequence loadControlDeck describes: "Ashaya,
-- Soul of the Wild" < "Darksteel Myr" < "Forest" < "Kormus Bell" < "Swamp", so
-- the opening hand is the Ashaya, both Myr and four of the five Forests and the
-- Bell is the second card drawn, so each is castable as soon as its own land
-- count is down rather than arriving among the last draws. Both are on the
-- battlefield when the game ends.
--
-- Thirteen cards, not sixty, and that is the whole reason: `resolve` re-projects
-- every other battlefield object at every movable layer, so the cost climbs far
-- faster than the deck does. Wall clock for one process running this match and
-- its control back to back, measured at several deck sizes on the engine as it
-- stood BEFORE the CR 613.8a gate #2641 added: about 3s at 12 cards, 6s at 13,
-- 14s at 14, 24s at 15, 74s at 18, and no result inside twelve minutes at 30.
-- That gate took the 13-card point from 5.696s to 3.394s of tasty-bench mean; the
-- shape of the ladder is unmeasured since, and the deck stays at 13.
--
-- Observed by running the match and reading the final GameState: both decks deck
-- out on turn 14, and the battlefields differ only by the four permanents the
-- statics themselves are -- 21 here against 17 there, both boards carrying the
-- same 10 Forests, 3 Swamps and 4 Darksteel Myr.
loadDependentDeck :: Registry.Registry IO -> IO Deck.Deck
loadDependentDeck registry = do
  ashaya <- fetchOrThrow registry "Ashaya, Soul of the Wild"
  myr <- fetchOrThrow registry "Darksteel Myr"
  forest <- fetchOrThrow registry "Forest"
  bell <- fetchOrThrow registry "Kormus Bell"
  swamp <- fetchOrThrow registry "Swamp"
  pure (Deck.fromCards (Map.fromList [(ashaya, 1), (myr, 2), (forest, 5), (bell, 1), (swamp, 4)]))

-- loadDependentDeck's PAIRED CONTROL: the same 13 cards and the same 2 Darksteel
-- Myr, with Ashaya and the Bell replaced by two more Swamps, so the only
-- difference between the two scenarios is the movable layer.
--
-- Without it "casting 2p dependent" is a baseline rather than a diagnostic, for
-- the reason loadNoAuraDeck gives: its number folds what the dependency scan
-- costs together with what the board costs. Deck size holds the game length
-- fixed (both deck out on turn 14) and the land count holds the workload fixed
-- (10 Forests and 3 Swamps across the two seats either way).
loadIndependentDeck :: Registry.Registry IO -> IO Deck.Deck
loadIndependentDeck registry = do
  myr <- fetchOrThrow registry "Darksteel Myr"
  forest <- fetchOrThrow registry "Forest"
  swamp <- fetchOrThrow registry "Swamp"
  pure (Deck.fromCards (Map.fromList [(myr, 2), (forest, 5), (swamp, 6)]))

main :: IO ()
main = do
  root <- Registry.defaultRoot
  registry <- Registry.fileRegistry root
  deck <- loadRedDeck registry
  controlDeck <- loadControlDeck registry
  noAuraDeck <- loadNoAuraDeck registry
  dependentDeck <- loadDependentDeck registry
  independentDeck <- loadIndependentDeck registry
  Bench.defaultMain
    [ Bench.bench "goldfish 2p" $ Bench.whnf (goldfish deck) 0,
      Bench.bench "casting 2p" $ Bench.whnf (casting deck) 0,
      Bench.bench "fighting 2p" $ Bench.whnf (fighting deck) 0,
      Bench.bench "fighting 2p aura" $ Bench.whnf (fighting controlDeck) 0,
      -- The line above's paired control: same board, no Aura (loadNoAuraDeck).
      Bench.bench "fighting 2p no aura" $ Bench.whnf (fighting noAuraDeck) 0,
      Bench.bench "casting 2p dependent" $ Bench.whnf (casting dependentDeck) 0,
      -- The line above's paired control: same 13 cards, no movable layer
      -- (loadIndependentDeck).
      Bench.bench "casting 2p independent" $ Bench.whnf (casting independentDeck) 0
    ]
