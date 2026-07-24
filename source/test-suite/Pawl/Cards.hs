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
import qualified Pawl.Registry as Registry
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Registry as Registry.Type

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
    syntheticRestartPrinting :: Printing.Printing,
    syntheticSubgamePrinting :: Printing.Printing,
    panglacialWurmPrinting :: Printing.Printing,
    blazePrinting :: Printing.Printing,
    darksteelMyrPrinting :: Printing.Printing,
    murderPrinting :: Printing.Printing,
    unsummonPrinting :: Printing.Printing,
    angelicEdictPrinting :: Printing.Printing,
    divinationPrinting :: Printing.Printing,
    tomeScourPrinting :: Printing.Printing,
    mindRotPrinting :: Printing.Printing,
    dragonFodderPrinting :: Printing.Printing,
    fogPrinting :: Printing.Printing,
    drudgeSkeletonsPrinting :: Printing.Printing,
    cancelPrinting :: Printing.Printing,
    battlegrowthPrinting :: Printing.Printing,
    instillInfectionPrinting :: Printing.Printing,
    chaosCharmPrinting :: Printing.Printing,
    wallOfStonePrinting :: Printing.Printing,
    syntheticModalActivatedPrinting :: Printing.Printing,
    aetherChannelerPrinting :: Printing.Printing,
    syntheticModalTriggerPrinting :: Printing.Printing,
    actOfTreasonPrinting :: Printing.Printing,
    clonePrinting :: Printing.Printing,
    devoidDronePrinting :: Printing.Printing,
    badMoonPrinting :: Printing.Printing,
    doomBladePrinting :: Printing.Printing,
    crimsonWispsPrinting :: Printing.Printing,
    aphoticWispsPrinting :: Printing.Printing,
    tarmogoyfPrinting :: Printing.Printing,
    innerCalmPrinting :: Printing.Printing,
    twistedImagePrinting :: Printing.Printing,
    barbarianOutcastPrinting :: Printing.Printing,
    khabalGhoulPrinting :: Printing.Printing,
    tidalWavePrinting :: Printing.Printing,
    sarcomancyPrinting :: Printing.Printing,
    hardenedScalesPrinting :: Printing.Printing,
    corpsejackMenacePrinting :: Printing.Printing,
    primalPlasmaPrinting :: Printing.Printing,
    doublingSeasonPrinting :: Printing.Printing,
    masterThiefPrinting :: Printing.Printing,
    palaceJailerPrinting :: Printing.Printing,
    hagOfInnerWeaknessPrinting :: Printing.Printing,
    ruleOfLawPrinting :: Printing.Printing,
    thaliaPrinting :: Printing.Printing,
    sapphireMedallionPrinting :: Printing.Printing,
    reliquaryTowerPrinting :: Printing.Printing,
    silencePrinting :: Printing.Printing,
    greedPrinting :: Printing.Printing,
    villageRitesPrinting :: Printing.Printing,
    fireblastPrinting :: Printing.Printing,
    terrorPrinting :: Printing.Printing,
    reprisalPrinting :: Printing.Printing,
    glistenerElfPrinting :: Printing.Printing,
    longtuskCubPrinting :: Printing.Printing,
    nightmarePrinting :: Printing.Printing,
    suddenImpactPrinting :: Printing.Printing
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
  serpentsGiftPrinting_ <- loadPrinting "serpents-gift"
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
  syntheticRestartPrinting_ <- loadPrinting "synthetic-restart"
  syntheticSubgamePrinting_ <- loadPrinting "synthetic-subgame"
  panglacialWurmPrinting_ <- loadPrinting "panglacial-wurm"
  blazePrinting_ <- loadPrinting "blaze"
  darksteelMyrPrinting_ <- loadPrinting "darksteel-myr"
  murderPrinting_ <- loadPrinting "murder"
  unsummonPrinting_ <- loadPrinting "unsummon"
  angelicEdictPrinting_ <- loadPrinting "angelic-edict"
  divinationPrinting_ <- loadPrinting "divination"
  tomeScourPrinting_ <- loadPrinting "tome-scour"
  mindRotPrinting_ <- loadPrinting "mind-rot"
  dragonFodderPrinting_ <- loadPrinting "dragon-fodder"
  fogPrinting_ <- loadPrinting "fog"
  drudgeSkeletonsPrinting_ <- loadPrinting "drudge-skeletons"
  cancelPrinting_ <- loadPrinting "cancel"
  battlegrowthPrinting_ <- loadPrinting "battlegrowth"
  instillInfectionPrinting_ <- loadPrinting "instill-infection"
  chaosCharmPrinting_ <- loadPrinting "chaos-charm"
  wallOfStonePrinting_ <- loadPrinting "wall-of-stone"
  syntheticModalActivatedPrinting_ <- loadPrinting "synthetic-modal-activator"
  aetherChannelerPrinting_ <- loadPrinting "aether-channeler"
  syntheticModalTriggerPrinting_ <- loadPrinting "synthetic-modal-trigger"
  actOfTreasonPrinting_ <- loadPrinting "act-of-treason"
  clonePrinting_ <- loadPrinting "clone"
  devoidDronePrinting_ <- loadPrinting "synthetic-devoid-drone"
  badMoonPrinting_ <- loadPrinting "bad-moon"
  doomBladePrinting_ <- loadPrinting "doom-blade"
  crimsonWispsPrinting_ <- loadPrinting "crimson-wisps"
  aphoticWispsPrinting_ <- loadPrinting "aphotic-wisps"
  tarmogoyfPrinting_ <- loadPrinting "tarmogoyf"
  innerCalmPrinting_ <- loadPrinting "inner-calm-outer-strength"
  twistedImagePrinting_ <- loadPrinting "twisted-image"
  barbarianOutcastPrinting_ <- loadPrinting "barbarian-outcast"
  khabalGhoulPrinting_ <- loadPrinting "khabal-ghoul"
  tidalWavePrinting_ <- loadPrinting "tidal-wave"
  sarcomancyPrinting_ <- loadPrinting "sarcomancy"
  hardenedScalesPrinting_ <- loadPrinting "hardened-scales"
  corpsejackMenacePrinting_ <- loadPrinting "corpsejack-menace"
  primalPlasmaPrinting_ <- loadPrinting "primal-plasma"
  doublingSeasonPrinting_ <- loadPrinting "doubling-season"
  masterThiefPrinting_ <- loadPrinting "master-thief"
  palaceJailerPrinting_ <- loadPrinting "palace-jailer"
  hagOfInnerWeaknessPrinting_ <- loadPrinting "hag-of-inner-weakness"
  ruleOfLawPrinting_ <- loadPrinting "rule-of-law"
  thaliaPrinting_ <- loadPrinting "thalia-guardian-of-thraben"
  sapphireMedallionPrinting_ <- loadPrinting "sapphire-medallion"
  reliquaryTowerPrinting_ <- loadPrinting "reliquary-tower"
  silencePrinting_ <- loadPrinting "silence"
  greedPrinting_ <- loadPrinting "greed"
  villageRitesPrinting_ <- loadPrinting "village-rites"
  fireblastPrinting_ <- loadPrinting "fireblast"
  terrorPrinting_ <- loadPrinting "terror"
  reprisalPrinting_ <- loadPrinting "reprisal"
  glistenerElfPrinting_ <- loadPrinting "glistener-elf"
  longtuskCubPrinting_ <- loadPrinting "longtusk-cub"
  nightmarePrinting_ <- loadPrinting "nightmare"
  suddenImpactPrinting_ <- loadPrinting "sudden-impact"
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
        syntheticRestartPrinting = syntheticRestartPrinting_,
        syntheticSubgamePrinting = syntheticSubgamePrinting_,
        panglacialWurmPrinting = panglacialWurmPrinting_,
        blazePrinting = blazePrinting_,
        darksteelMyrPrinting = darksteelMyrPrinting_,
        murderPrinting = murderPrinting_,
        unsummonPrinting = unsummonPrinting_,
        angelicEdictPrinting = angelicEdictPrinting_,
        divinationPrinting = divinationPrinting_,
        tomeScourPrinting = tomeScourPrinting_,
        mindRotPrinting = mindRotPrinting_,
        dragonFodderPrinting = dragonFodderPrinting_,
        fogPrinting = fogPrinting_,
        drudgeSkeletonsPrinting = drudgeSkeletonsPrinting_,
        cancelPrinting = cancelPrinting_,
        battlegrowthPrinting = battlegrowthPrinting_,
        instillInfectionPrinting = instillInfectionPrinting_,
        chaosCharmPrinting = chaosCharmPrinting_,
        wallOfStonePrinting = wallOfStonePrinting_,
        syntheticModalActivatedPrinting = syntheticModalActivatedPrinting_,
        aetherChannelerPrinting = aetherChannelerPrinting_,
        syntheticModalTriggerPrinting = syntheticModalTriggerPrinting_,
        actOfTreasonPrinting = actOfTreasonPrinting_,
        clonePrinting = clonePrinting_,
        devoidDronePrinting = devoidDronePrinting_,
        badMoonPrinting = badMoonPrinting_,
        doomBladePrinting = doomBladePrinting_,
        crimsonWispsPrinting = crimsonWispsPrinting_,
        aphoticWispsPrinting = aphoticWispsPrinting_,
        tarmogoyfPrinting = tarmogoyfPrinting_,
        innerCalmPrinting = innerCalmPrinting_,
        twistedImagePrinting = twistedImagePrinting_,
        barbarianOutcastPrinting = barbarianOutcastPrinting_,
        khabalGhoulPrinting = khabalGhoulPrinting_,
        tidalWavePrinting = tidalWavePrinting_,
        sarcomancyPrinting = sarcomancyPrinting_,
        hardenedScalesPrinting = hardenedScalesPrinting_,
        corpsejackMenacePrinting = corpsejackMenacePrinting_,
        primalPlasmaPrinting = primalPlasmaPrinting_,
        doublingSeasonPrinting = doublingSeasonPrinting_,
        masterThiefPrinting = masterThiefPrinting_,
        palaceJailerPrinting = palaceJailerPrinting_,
        hagOfInnerWeaknessPrinting = hagOfInnerWeaknessPrinting_,
        ruleOfLawPrinting = ruleOfLawPrinting_,
        thaliaPrinting = thaliaPrinting_,
        sapphireMedallionPrinting = sapphireMedallionPrinting_,
        reliquaryTowerPrinting = reliquaryTowerPrinting_,
        silencePrinting = silencePrinting_,
        greedPrinting = greedPrinting_,
        villageRitesPrinting = villageRitesPrinting_,
        fireblastPrinting = fireblastPrinting_,
        terrorPrinting = terrorPrinting_,
        reprisalPrinting = reprisalPrinting_,
        glistenerElfPrinting = glistenerElfPrinting_,
        longtuskCubPrinting = longtuskCubPrinting_,
        nightmarePrinting = nightmarePrinting_,
        suddenImpactPrinting = suddenImpactPrinting_
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
    syntheticRestartPrinting cards,
    syntheticSubgamePrinting cards,
    panglacialWurmPrinting cards,
    blazePrinting cards,
    darksteelMyrPrinting cards,
    murderPrinting cards,
    unsummonPrinting cards,
    angelicEdictPrinting cards,
    divinationPrinting cards,
    tomeScourPrinting cards,
    mindRotPrinting cards,
    dragonFodderPrinting cards,
    fogPrinting cards,
    drudgeSkeletonsPrinting cards,
    cancelPrinting cards,
    battlegrowthPrinting cards,
    instillInfectionPrinting cards,
    chaosCharmPrinting cards,
    wallOfStonePrinting cards,
    syntheticModalActivatedPrinting cards,
    aetherChannelerPrinting cards,
    syntheticModalTriggerPrinting cards,
    actOfTreasonPrinting cards,
    clonePrinting cards,
    devoidDronePrinting cards,
    badMoonPrinting cards,
    doomBladePrinting cards,
    crimsonWispsPrinting cards,
    aphoticWispsPrinting cards,
    tarmogoyfPrinting cards,
    innerCalmPrinting cards,
    twistedImagePrinting cards,
    barbarianOutcastPrinting cards,
    khabalGhoulPrinting cards,
    tidalWavePrinting cards,
    sarcomancyPrinting cards,
    hardenedScalesPrinting cards,
    corpsejackMenacePrinting cards,
    primalPlasmaPrinting cards,
    doublingSeasonPrinting cards,
    masterThiefPrinting cards,
    palaceJailerPrinting cards,
    hagOfInnerWeaknessPrinting cards,
    ruleOfLawPrinting cards,
    thaliaPrinting cards,
    sapphireMedallionPrinting cards,
    reliquaryTowerPrinting cards,
    silencePrinting cards,
    greedPrinting cards,
    villageRitesPrinting cards,
    fireblastPrinting cards,
    terrorPrinting cards,
    reprisalPrinting cards,
    glistenerElfPrinting cards,
    longtuskCubPrinting cards,
    nightmarePrinting cards,
    suddenImpactPrinting cards
  ]

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
