-- The decks the played-out-game and setup tests play with. This module is only
-- the four hand-tuned 60-card lists, whose comments explain the swap-ins that
-- keep each at 60 cards (and so the CR 400.7 conservation count at 120).
--
-- Each takes HOW TO FETCH a printing rather than a registry, because its two
-- kinds of caller disagree about what to do with a failure: a spec case folds it
-- into an assertion (Pawl.Support.printingOf), and the benchmark, which has no
-- spec record, throws (its own fetchOrThrow). One deck list, two adapters.
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
        -- Blaze swaps in for four Pikers to keep the deck at 60, so the CR 400.7
        -- conservation counts stay 120. Its own X-payment coverage is CastSpec's.
        (blaze, 4),
        -- Dragon Fodder swaps in for four Pikers to keep the deck at 60, so the
        -- card-backed conservation count stays 120. Its tokens are also why that
        -- count excludes Source.OfToken: they legitimately come and go.
        (dragonFodder, 4),
        -- Chaos Charm swaps in for four Bird Maidens to keep the deck at 60, so
        -- the card-backed conservation count stays 120. Its modal coverage is
        -- ModalSpec's.
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
        -- Fog swaps in for four War Mammoths to keep the deck at 60, so
        -- card-backed conservation stays 120. Its CR 615 prevention coverage is
        -- ResolveSpec's and ReplacementSpec's.
        (fog, 4),
        (giantGrowth, 4),
        (serpentsGift, 4),
        -- Battlegrowth swaps in for four War Mammoths, so the deck stays 60 and
        -- card-backed conservation stays 120. Its +1/+1 counter coverage is
        -- ResolveSpec's.
        (battlegrowth, 4)
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
        -- Drudge Skeletons swaps in for four Typhoid Rats, so the deck stays 60.
        -- Its CR 701.19a regeneration coverage is CostSpec's and DamageSpec's.
        (drudgeSkeletons, 4),
        -- Murder and Mind Rot are the deck's Destroy and Discard.
        (murder, 4),
        (mindRot, 4),
        -- Instill Infection swaps in for four Typhoid Rats, so the deck stays 60.
        -- Its -1/-1 counter and CR 704.5q coverage is ResolveSpec's.
        (instillInfection, 4)
      ]
