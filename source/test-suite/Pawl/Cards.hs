{-# LANGUAGE TemplateHaskell #-}

-- The M3.5 flip: every printing is spliced from its committed data/cards
-- JSON at compile time (via the temporary Pawl.Cards.Load TH shim), so the
-- files are the single source of truth and the hand-written MkCard literals
-- are gone. The decks stay here (they are built from card values).
module Pawl.Cards where

import qualified Data.Map.Strict as Map
import qualified Pawl.Cards.Load as Load
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.Printing as Printing

mountainPrinting :: Printing.Printing
mountainPrinting = $(Load.loadPrinting "mountain")

swampPrinting :: Printing.Printing
swampPrinting = $(Load.loadPrinting "swamp")

forestPrinting :: Printing.Printing
forestPrinting = $(Load.loadPrinting "forest")

pikerPrinting :: Printing.Printing
pikerPrinting = $(Load.loadPrinting "goblin-piker")

birdMaidenPrinting :: Printing.Printing
birdMaidenPrinting = $(Load.loadPrinting "bird-maiden")

nimbleBirdstickerPrinting :: Printing.Printing
nimbleBirdstickerPrinting = $(Load.loadPrinting "nimble-birdsticker")

ogreSentryPrinting :: Printing.Printing
ogreSentryPrinting = $(Load.loadPrinting "ogre-sentry")

windseekerCentaurPrinting :: Printing.Printing
windseekerCentaurPrinting = $(Load.loadPrinting "windseeker-centaur")

goblinChariotPrinting :: Printing.Printing
goblinChariotPrinting = $(Load.loadPrinting "goblin-chariot")

sabretoothTigerPrinting :: Printing.Printing
sabretoothTigerPrinting = $(Load.loadPrinting "sabretooth-tiger")

ridgetopRaptorPrinting :: Printing.Printing
ridgetopRaptorPrinting = $(Load.loadPrinting "ridgetop-raptor")

typhoidRatsPrinting :: Printing.Printing
typhoidRatsPrinting = $(Load.loadPrinting "typhoid-rats")

warMammothPrinting :: Printing.Printing
warMammothPrinting = $(Load.loadPrinting "war-mammoth")

lightningBoltPrinting :: Printing.Printing
lightningBoltPrinting = $(Load.loadPrinting "lightning-bolt")

giantGrowthPrinting :: Printing.Printing
giantGrowthPrinting = $(Load.loadPrinting "giant-growth")

humilityPrinting :: Printing.Printing
humilityPrinting = $(Load.loadPrinting "humility")

serpentsGiftPrinting :: Printing.Printing
serpentsGiftPrinting = $(Load.loadPrinting "serpent-s-gift")

bloodMoonPrinting :: Printing.Printing
bloodMoonPrinting = $(Load.loadPrinting "blood-moon")

urborgPrinting :: Printing.Printing
urborgPrinting = $(Load.loadPrinting "urborg-tomb-of-yawgmoth")

opalescencePrinting :: Printing.Printing
opalescencePrinting = $(Load.loadPrinting "opalescence")

islandPrinting :: Printing.Printing
islandPrinting = $(Load.loadPrinting "island")

magicalHackPrinting :: Printing.Printing
magicalHackPrinting = $(Load.loadPrinting "magical-hack")

landformPrinting :: Printing.Printing
landformPrinting = $(Load.loadPrinting "landform")

prodigalSorcererPrinting :: Printing.Printing
prodigalSorcererPrinting = $(Load.loadPrinting "prodigal-sorcerer")

llanowarElvesPrinting :: Printing.Printing
llanowarElvesPrinting = $(Load.loadPrinting "llanowar-elves")

evolvingWildsPrinting :: Printing.Printing
evolvingWildsPrinting = $(Load.loadPrinting "evolving-wilds")

restInPeacePrinting :: Printing.Printing
restInPeacePrinting = $(Load.loadPrinting "rest-in-peace")

plainsPrinting :: Printing.Printing
plainsPrinting = $(Load.loadPrinting "plains")

mindslaverPrinting :: Printing.Printing
mindslaverPrinting = $(Load.loadPrinting "mindslaver")

panglacialWurmPrinting :: Printing.Printing
panglacialWurmPrinting = $(Load.loadPrinting "panglacial-wurm")

-- The registry the dataflow lint and future golden tests iterate. A printing
-- not listed here escapes the hygiene net -- add every new printing.
allPrintings :: [Printing.Printing]
allPrintings =
  [ mountainPrinting,
    swampPrinting,
    forestPrinting,
    pikerPrinting,
    birdMaidenPrinting,
    nimbleBirdstickerPrinting,
    ogreSentryPrinting,
    windseekerCentaurPrinting,
    goblinChariotPrinting,
    sabretoothTigerPrinting,
    ridgetopRaptorPrinting,
    typhoidRatsPrinting,
    warMammothPrinting,
    lightningBoltPrinting,
    giantGrowthPrinting,
    humilityPrinting,
    serpentsGiftPrinting,
    bloodMoonPrinting,
    urborgPrinting,
    opalescencePrinting,
    islandPrinting,
    magicalHackPrinting,
    landformPrinting,
    prodigalSorcererPrinting,
    llanowarElvesPrinting,
    evolvingWildsPrinting,
    restInPeacePrinting,
    plainsPrinting,
    mindslaverPrinting,
    panglacialWurmPrinting
  ]

-- The concrete decks, relocated from Pawl.Setup (M3.5): they are built from card
-- values, so they live with the cards, not in the engine library.
redDeck :: Deck.Deck
redDeck =
  Deck.MkDeck $
    Map.fromList
      [ (mountainPrinting, 36),
        (pikerPrinting, 12),
        (birdMaidenPrinting, 8),
        (lightningBoltPrinting, 4)
      ]

greenDeck :: Deck.Deck
greenDeck =
  Deck.MkDeck $
    Map.fromList
      [ (forestPrinting, 36),
        (warMammothPrinting, 16),
        (giantGrowthPrinting, 4),
        (serpentsGiftPrinting, 4)
      ]

blackDeck :: Deck.Deck
blackDeck =
  Deck.MkDeck $
    Map.fromList
      [ (swampPrinting, 36),
        (typhoidRatsPrinting, 24)
      ]
