-- The decks the random-game properties and setup tests play with. Cards
-- themselves come from Pawl.Registry, one file per card, loaded on demand; this
-- module is only the four hand-tuned 60-card lists, whose comments explain the
-- swap-ins that keep each at 60 cards (and so the CR 400.7 conservation count
-- at 120).
module Pawl.Cards where

import qualified Data.Map.Strict as Map
import qualified Pawl.Registry as Registry
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Registry as Registry.Type

redDeck :: Registry.Type.Registry -> IO Deck.Deck
redDeck registry = do
  mountain <- Registry.printing registry "Mountain"
  piker <- Registry.printing registry "Goblin Piker"
  birdMaiden <- Registry.printing registry "Bird Maiden"
  bolt <- Registry.printing registry "Lightning Bolt"
  blaze <- Registry.printing registry "Blaze"
  dragonFodder <- Registry.printing registry "Dragon Fodder"
  chaosCharm <- Registry.printing registry "Chaos Charm"
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

greenDeck :: Registry.Type.Registry -> IO Deck.Deck
greenDeck registry = do
  forest <- Registry.printing registry "Forest"
  warMammoth <- Registry.printing registry "War Mammoth"
  fog <- Registry.printing registry "Fog"
  giantGrowth <- Registry.printing registry "Giant Growth"
  serpentsGift <- Registry.printing registry "Serpent's Gift"
  battlegrowth <- Registry.printing registry "Battlegrowth"
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
blueDeck :: Registry.Type.Registry -> IO Deck.Deck
blueDeck registry = do
  island <- Registry.printing registry "Island"
  unsummon <- Registry.printing registry "Unsummon"
  divination <- Registry.printing registry "Divination"
  tomeScour <- Registry.printing registry "Tome Scour"
  pure . Deck.MkDeck $
    Map.fromList
      [ (island, 40),
        (unsummon, 8),
        (divination, 8),
        (tomeScour, 4)
      ]

blackDeck :: Registry.Type.Registry -> IO Deck.Deck
blackDeck registry = do
  swamp <- Registry.printing registry "Swamp"
  typhoidRats <- Registry.printing registry "Typhoid Rats"
  drudgeSkeletons <- Registry.printing registry "Drudge Skeletons"
  murder <- Registry.printing registry "Murder"
  mindRot <- Registry.printing registry "Mind Rot"
  instillInfection <- Registry.printing registry "Instill Infection"
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
