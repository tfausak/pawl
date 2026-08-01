-- The decks the random-game properties and setup tests play with. This module is
-- only the four hand-tuned 60-card lists, whose comments explain the swap-ins
-- that keep each at 60 cards (and so the CR 400.7 conservation count at 120).
--
-- Each takes HOW TO FETCH a printing rather than a registry, because its two
-- kinds of caller disagree about what to do with a failure: a spec case folds it
-- into an assertion, and Pawl.PropertySpec -- which is tasty-based and has no
-- spec record -- throws. One deck list, two adapters.
module Pawl.Cards where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Printing as Printing

type Fetch m = String -> m Printing.Printing

redDeck :: (Monad m) => Fetch m -> m Deck.Deck
redDeck fetch = do
  mountain <- fetch "Mountain"
  piker <- fetch "Goblin Piker"
  birdMaiden <- fetch "Bird Maiden"
  bolt <- fetch "Lightning Bolt"
  blaze <- fetch "Blaze"
  dragonFodder <- fetch "Dragon Fodder"
  chaosCharm <- fetch "Chaos Charm"
  pure . Deck.MkDeck $
    Map.fromList
      [ (mountain, 36),
        (piker, 4),
        (birdMaiden, 4),
        (bolt, 4),
        -- Blaze swaps in for four Pikers to keep the deck at 60 (so the CR 400.7
        -- conservation counts stay 120); the variable red cost gives the random
        -- red matchup its X-payment coverage (M4a spec §6).
        (blaze, 4),
        -- Dragon Fodder swaps in for four Pikers to keep the deck at 60 (so the
        -- card-backed conservation count stays 120) and give random red games their
        -- token-churn coverage: creation from nothing and CR 704.5d cease-to-exist.
        (dragonFodder, 4),
        -- Chaos Charm swaps in for four Bird Maidens to keep the deck at 60 (so
        -- the card-backed conservation count stays 120) and give random red games
        -- modal-choice coverage; Pikers and the remaining Bird Maidens stay on
        -- board so the damage/haste modes have legal targets.
        (chaosCharm, 4)
      ]

greenDeck :: (Monad m) => Fetch m -> m Deck.Deck
greenDeck fetch = do
  forest <- fetch "Forest"
  warMammoth <- fetch "War Mammoth"
  fog <- fetch "Fog"
  giantGrowth <- fetch "Giant Growth"
  serpentsGift <- fetch "Serpent's Gift"
  battlegrowth <- fetch "Battlegrowth"
  pure . Deck.MkDeck $
    Map.fromList
      [ (forest, 36),
        (warMammoth, 8),
        -- Fog swaps in for four War Mammoths to keep the deck at 60 (card-backed
        -- conservation stays 120) and give random green games combat-damage
        -- prevention coverage (CR 615).
        (fog, 4),
        (giantGrowth, 4),
        (serpentsGift, 4),
        -- Battlegrowth swaps in for four War Mammoths (deck stays 60; card-backed
        -- conservation stays 120) so random green games exercise +1/+1 counters.
        (battlegrowth, 4)
      ]

-- Blue, no creatures: Divination accelerates its own deck-out, Unsummon bounces
-- the opponent's creatures, Tome Scour mills them. Gives bounce/draw/mill random
-- coverage (M4b fast follow).
blueDeck :: (Monad m) => Fetch m -> m Deck.Deck
blueDeck fetch = do
  island <- fetch "Island"
  unsummon <- fetch "Unsummon"
  divination <- fetch "Divination"
  tomeScour <- fetch "Tome Scour"
  pure . Deck.MkDeck $
    Map.fromList
      [ (island, 40),
        (unsummon, 8),
        (divination, 8),
        (tomeScour, 4)
      ]

blackDeck :: (Monad m) => Fetch m -> m Deck.Deck
blackDeck fetch = do
  swamp <- fetch "Swamp"
  typhoidRats <- fetch "Typhoid Rats"
  drudgeSkeletons <- fetch "Drudge Skeletons"
  murder <- fetch "Murder"
  mindRot <- fetch "Mind Rot"
  instillInfection <- fetch "Instill Infection"
  pure . Deck.MkDeck $
    Map.fromList
      [ (swamp, 36),
        (typhoidRats, 8),
        -- Drudge Skeletons swaps in for four Typhoid Rats (deck stays 60) so random
        -- black games exercise regeneration against Murder's destroy (CR 701.19a).
        (drudgeSkeletons, 4),
        -- Murder and Mind Rot give Destroy and Discard random-play coverage.
        (murder, 4),
        (mindRot, 4),
        -- Instill Infection swaps in for four Typhoid Rats (deck stays 60) so random
        -- black games exercise -1/-1 counters and the CR 704.5q annihilation SBA.
        (instillInfection, 4)
      ]
