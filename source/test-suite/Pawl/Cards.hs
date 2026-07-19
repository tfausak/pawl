-- The card pool, loaded from the committed data/cards JSON at test-suite start
-- (M3.5's named expiry, cashed): every fixture reads the files -- the source of
-- truth -- through the same codec the benchmark uses, so the pure top-level
-- splices and the TH shim are gone. The loaded 'Cards' record is threaded into
-- every 'tests' tree from 'Main'; the decks and 'allPrintings' are functions of
-- it. A malformed or missing file fails loudly in IO, never 'error' in pure code.
module Pawl.Cards where

import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TextIO
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.Printing as Printing

data Cards = MkCards
  { mountainPrinting :: Printing.Printing,
    swampPrinting :: Printing.Printing,
    forestPrinting :: Printing.Printing,
    pikerPrinting :: Printing.Printing,
    birdMaidenPrinting :: Printing.Printing,
    nimbleBirdstickerPrinting :: Printing.Printing,
    ogreSentryPrinting :: Printing.Printing,
    windseekerCentaurPrinting :: Printing.Printing,
    goblinChariotPrinting :: Printing.Printing,
    sabretoothTigerPrinting :: Printing.Printing,
    ridgetopRaptorPrinting :: Printing.Printing,
    typhoidRatsPrinting :: Printing.Printing,
    warMammothPrinting :: Printing.Printing,
    lightningBoltPrinting :: Printing.Printing,
    giantGrowthPrinting :: Printing.Printing,
    humilityPrinting :: Printing.Printing,
    serpentsGiftPrinting :: Printing.Printing,
    bloodMoonPrinting :: Printing.Printing,
    urborgPrinting :: Printing.Printing,
    opalescencePrinting :: Printing.Printing,
    islandPrinting :: Printing.Printing,
    magicalHackPrinting :: Printing.Printing,
    landformPrinting :: Printing.Printing,
    prodigalSorcererPrinting :: Printing.Printing,
    llanowarElvesPrinting :: Printing.Printing,
    evolvingWildsPrinting :: Printing.Printing,
    restInPeacePrinting :: Printing.Printing,
    plainsPrinting :: Printing.Printing,
    mindslaverPrinting :: Printing.Printing,
    panglacialWurmPrinting :: Printing.Printing,
    blazePrinting :: Printing.Printing,
    darksteelMyrPrinting :: Printing.Printing,
    murderPrinting :: Printing.Printing,
    unsummonPrinting :: Printing.Printing
  }
  deriving (Eq, Show)

loadPrinting :: String -> IO Printing.Printing
loadPrinting slug = do
  contents <- TextIO.readFile ("data/cards/" <> slug <> ".json")
  case Json.parse contents >>= Codec.jsonToPrinting of
    Right p -> pure p
    Left err -> ioError (userError ("card " <> slug <> ": " <> show err))

loadCards :: IO Cards
loadCards = do
  mountainPrinting_ <- loadPrinting "mountain"
  swampPrinting_ <- loadPrinting "swamp"
  forestPrinting_ <- loadPrinting "forest"
  pikerPrinting_ <- loadPrinting "goblin-piker"
  birdMaidenPrinting_ <- loadPrinting "bird-maiden"
  nimbleBirdstickerPrinting_ <- loadPrinting "nimble-birdsticker"
  ogreSentryPrinting_ <- loadPrinting "ogre-sentry"
  windseekerCentaurPrinting_ <- loadPrinting "windseeker-centaur"
  goblinChariotPrinting_ <- loadPrinting "goblin-chariot"
  sabretoothTigerPrinting_ <- loadPrinting "sabretooth-tiger"
  ridgetopRaptorPrinting_ <- loadPrinting "ridgetop-raptor"
  typhoidRatsPrinting_ <- loadPrinting "typhoid-rats"
  warMammothPrinting_ <- loadPrinting "war-mammoth"
  lightningBoltPrinting_ <- loadPrinting "lightning-bolt"
  giantGrowthPrinting_ <- loadPrinting "giant-growth"
  humilityPrinting_ <- loadPrinting "humility"
  serpentsGiftPrinting_ <- loadPrinting "serpent-s-gift"
  bloodMoonPrinting_ <- loadPrinting "blood-moon"
  urborgPrinting_ <- loadPrinting "urborg-tomb-of-yawgmoth"
  opalescencePrinting_ <- loadPrinting "opalescence"
  islandPrinting_ <- loadPrinting "island"
  magicalHackPrinting_ <- loadPrinting "magical-hack"
  landformPrinting_ <- loadPrinting "landform"
  prodigalSorcererPrinting_ <- loadPrinting "prodigal-sorcerer"
  llanowarElvesPrinting_ <- loadPrinting "llanowar-elves"
  evolvingWildsPrinting_ <- loadPrinting "evolving-wilds"
  restInPeacePrinting_ <- loadPrinting "rest-in-peace"
  plainsPrinting_ <- loadPrinting "plains"
  mindslaverPrinting_ <- loadPrinting "mindslaver"
  panglacialWurmPrinting_ <- loadPrinting "panglacial-wurm"
  blazePrinting_ <- loadPrinting "blaze"
  darksteelMyrPrinting_ <- loadPrinting "darksteel-myr"
  murderPrinting_ <- loadPrinting "murder"
  unsummonPrinting_ <- loadPrinting "unsummon"
  pure
    MkCards
      { mountainPrinting = mountainPrinting_,
        swampPrinting = swampPrinting_,
        forestPrinting = forestPrinting_,
        pikerPrinting = pikerPrinting_,
        birdMaidenPrinting = birdMaidenPrinting_,
        nimbleBirdstickerPrinting = nimbleBirdstickerPrinting_,
        ogreSentryPrinting = ogreSentryPrinting_,
        windseekerCentaurPrinting = windseekerCentaurPrinting_,
        goblinChariotPrinting = goblinChariotPrinting_,
        sabretoothTigerPrinting = sabretoothTigerPrinting_,
        ridgetopRaptorPrinting = ridgetopRaptorPrinting_,
        typhoidRatsPrinting = typhoidRatsPrinting_,
        warMammothPrinting = warMammothPrinting_,
        lightningBoltPrinting = lightningBoltPrinting_,
        giantGrowthPrinting = giantGrowthPrinting_,
        humilityPrinting = humilityPrinting_,
        serpentsGiftPrinting = serpentsGiftPrinting_,
        bloodMoonPrinting = bloodMoonPrinting_,
        urborgPrinting = urborgPrinting_,
        opalescencePrinting = opalescencePrinting_,
        islandPrinting = islandPrinting_,
        magicalHackPrinting = magicalHackPrinting_,
        landformPrinting = landformPrinting_,
        prodigalSorcererPrinting = prodigalSorcererPrinting_,
        llanowarElvesPrinting = llanowarElvesPrinting_,
        evolvingWildsPrinting = evolvingWildsPrinting_,
        restInPeacePrinting = restInPeacePrinting_,
        plainsPrinting = plainsPrinting_,
        mindslaverPrinting = mindslaverPrinting_,
        panglacialWurmPrinting = panglacialWurmPrinting_,
        blazePrinting = blazePrinting_,
        darksteelMyrPrinting = darksteelMyrPrinting_,
        murderPrinting = murderPrinting_,
        unsummonPrinting = unsummonPrinting_
      }

allPrintings :: Cards -> [Printing.Printing]
allPrintings cards =
  [ mountainPrinting cards,
    swampPrinting cards,
    forestPrinting cards,
    pikerPrinting cards,
    birdMaidenPrinting cards,
    nimbleBirdstickerPrinting cards,
    ogreSentryPrinting cards,
    windseekerCentaurPrinting cards,
    goblinChariotPrinting cards,
    sabretoothTigerPrinting cards,
    ridgetopRaptorPrinting cards,
    typhoidRatsPrinting cards,
    warMammothPrinting cards,
    lightningBoltPrinting cards,
    giantGrowthPrinting cards,
    humilityPrinting cards,
    serpentsGiftPrinting cards,
    bloodMoonPrinting cards,
    urborgPrinting cards,
    opalescencePrinting cards,
    islandPrinting cards,
    magicalHackPrinting cards,
    landformPrinting cards,
    prodigalSorcererPrinting cards,
    llanowarElvesPrinting cards,
    evolvingWildsPrinting cards,
    restInPeacePrinting cards,
    plainsPrinting cards,
    mindslaverPrinting cards,
    panglacialWurmPrinting cards,
    blazePrinting cards,
    darksteelMyrPrinting cards,
    murderPrinting cards,
    unsummonPrinting cards
  ]

redDeck :: Cards -> Deck.Deck
redDeck cards =
  Deck.MkDeck $
    Map.fromList
      [ (mountainPrinting cards, 36),
        (pikerPrinting cards, 8),
        (birdMaidenPrinting cards, 8),
        (lightningBoltPrinting cards, 4),
        -- Blaze swaps in for four Pikers to keep the deck at 60 (so the CR 400.7
        -- conservation counts stay 120); the variable red cost gives the random
        -- red matchup its X-payment coverage (M4a spec §6).
        (blazePrinting cards, 4)
      ]

greenDeck :: Cards -> Deck.Deck
greenDeck cards =
  Deck.MkDeck $
    Map.fromList
      [ (forestPrinting cards, 36),
        (warMammothPrinting cards, 16),
        (giantGrowthPrinting cards, 4),
        (serpentsGiftPrinting cards, 4)
      ]

blackDeck :: Cards -> Deck.Deck
blackDeck cards =
  Deck.MkDeck $
    Map.fromList
      [ (swampPrinting cards, 36),
        (typhoidRatsPrinting cards, 24)
      ]
