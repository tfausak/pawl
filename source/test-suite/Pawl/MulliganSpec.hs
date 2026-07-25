{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Mulligan: the CR 103.5 opening-hand / mulligan process.
module Pawl.MulliganSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Binding as Binding
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mulligan as Mulligan
import qualified Pawl.Registry as Registry
import qualified Pawl.Replay as Replay
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Departure as Departure.Type
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import qualified Pawl.Type.MulliganOffer as MulliganOffer
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Move every object a player owns onto their library (mirror of
-- SetupSpec.poolToLibrary), to craft a known-size, identity-shuffled library.
poolToLibrary :: PlayerId -> GameState.GameState -> GameState.GameState
poolToLibrary pid gs =
  let mine = Map.keys (Map.filter (\o -> Object.owner o == pid) (GameState.objects gs))
      onLibrary o = o {Object.zone = Zone.Library}
   in gs
        { GameState.objects = List.foldl' (flip (Map.adjust onLibrary)) (GameState.objects gs) mine,
          GameState.battlefield = Set.difference (GameState.battlefield gs) (Set.fromList mine),
          GameState.library = Map.insert pid (Seq.fromList mine) (GameState.library gs)
        }

-- n Mountains in each player's library, nothing elsewhere.
libraryGame :: Printing.Printing -> Int -> GameState.GameState
libraryGame mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany pid g = List.foldl' (\h _ -> snd (S.addCreature mountain pid h)) g (replicate n ())
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany S.bob (addMany S.alice g0)))

libSize :: PlayerId -> GameState.GameState -> Int
libSize pid gs = length (Game.zoneMembers Zone.Library pid gs)

libBottom :: Int -> PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
libBottom k pid gs = reverse (take k (reverse (Game.zoneMembers Zone.Library pid gs)))

-- Keep always: reproduces the pre-mulligan draw.
keepAnswer :: Prompt.Prompt r -> r
keepAnswer p = case p of
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Mulligan while fewer than `k` taken, then keep. Bottom the first `count`.
mulliganUpTo :: Int -> Prompt.Prompt r -> r
mulliganUpTo k p = case p of
  Prompt.DeclareMulligan _ _ offer ->
    if fromIntegral (MulliganOffer.taken offer) < k then MulliganDecision.Mulligan else MulliganDecision.Keep
  _ -> S.identityAnswer p

run :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
run answer gs = snd (Engine.runGamePure answer gs (Mulligan.openingHands S.performer [S.alice, S.bob]))

-- CR 800.1: the three-seat twin of libraryGame -- n Mountains in each of the
-- three players' libraries, nothing elsewhere. The SEAT COUNT is what CR 103.5c
-- reads (through Mulligan.freeMulligans), so this must be built from
-- S.threePlayers: a two-seat game with a third player's cards in it is not a
-- multiplayer game.
libraryGame3 :: Printing.Printing -> Int -> GameState.GameState
libraryGame3 mountain n =
  let g0 = Setup.emptyGame S.threePlayers
      addMany pid g = List.foldl' (\h _ -> snd (S.addCreature mountain pid h)) g (replicate n ())
   in poolToLibrary S.carol (poolToLibrary S.bob (poolToLibrary S.alice (addMany S.carol (addMany S.bob (addMany S.alice g0)))))

-- run, for three seats. CR 103.5: the declaration order is the turn order, so
-- the list is [alice, bob, carol] and not a set.
run3 :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
run3 answer gs = snd (Engine.runGamePure answer gs (Mulligan.openingHands S.performer [S.alice, S.bob, S.carol]))

-- Always mulligan, recording every player asked. How many times a player is
-- ASKED is how pawl expresses CR 103.5's limit ("A player can take mulligans
-- until their opening hand would be zero cards, after which they may not take
-- further mulligans"): a hand of zero is a forced keep and is never asked. So
-- counting asks is how CR 103.5c's second clause -- the free mulligan does not
-- count toward the number of mulligans a player may take -- becomes observable.
recordAlwaysMulligan :: Prompt.Prompt r -> State.State [PlayerId] r
recordAlwaysMulligan p = case p of
  Prompt.DeclareMulligan _ pid _ -> do
    State.modify' (pid :)
    pure MulliganDecision.Mulligan
  _ -> pure (S.identityAnswer p)

-- Alice's library in an explicit, non-uniform order of 20 pairwise-DISTINCT
-- printings, so an individual library card can be told apart by its printed
-- Card identity even after a CR 400.7 zone-change reincarnation:
-- Event.changeZone (Pawl.Event) mints every moved card a FRESH ObjectId --
-- mkObj there only resets zone/tapped/damage/sickness/bindings/counters/
-- timestamp -- so a card's pre-move id is never equal to its post-move id,
-- which is why an order test can't compare ObjectIds across the move. Source
-- (hence the printed Card, read back via Game.cardOf) is the one field mkObj
-- carries forward unchanged, so it survives the reincarnation and is what
-- this test tracks instead. Bob's library is uniform Mountains; only alice
-- mulligans in the accompanying test, so bob's composition never enters the
-- assertion.
distinctPrintings :: Registry.Type.Registry -> IO [Printing.Printing]
distinctPrintings registry =
  mapM
    (Registry.printing registry)
    [ "Mountain",
      "Swamp",
      "Forest",
      "Goblin Piker",
      "Bird Maiden",
      "Nimble Birdsticker",
      "Ogre Sentry",
      "Windseeker Centaur",
      "Goblin Chariot",
      "Sabretooth Tiger",
      "Ridgetop Raptor",
      "Typhoid Rats",
      "War Mammoth",
      "Lightning Bolt",
      "Giant Growth",
      "Humility",
      "Serpent's Gift",
      "Blood Moon",
      "Urborg, Tomb of Yawgmoth",
      "Opalescence"
    ]

orderedLibraryGame :: Printing.Printing -> [Printing.Printing] -> GameState.GameState
orderedLibraryGame mountain printings =
  let g0 = Setup.emptyGame S.bothPlayers
      addOrdered pid g = List.foldl' (\h p -> snd (S.addCreature p pid h)) g printings
      addUniform pid g = List.foldl' (\h _ -> snd (S.addCreature mountain pid h)) g printings
   in poolToLibrary S.bob (poolToLibrary S.alice (addUniform S.bob (addOrdered S.alice g0)))

-- Alice mulligans exactly twice then keeps; bob keeps at once. On each Bottom
-- prompt, bottoms the FIRST `count` cards of the (post-redraw) hand in
-- REVERSED order -- a deliberately NON-identity order -- so the test can
-- prove the library bottom honors that exact chosen order, not just the
-- chosen set.
bottomReversedAnswer :: Prompt.Prompt r -> r
bottomReversedAnswer p = case p of
  Prompt.DeclareMulligan _ pid offer -> if pid == S.alice && MulliganOffer.taken offer < 2 then MulliganDecision.Mulligan else MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> reverse (take (fromIntegral count) hand)
  _ -> S.identityAnswer p

-- alice's library: a Serum Powder on top, then `n` Mountains; bob's is uniform
-- Mountains. poolToLibrary orders a library by ascending ObjectId, which is
-- insertion order, so the Powder added first is the top card and is drawn into
-- her opening hand -- CR 103.5b's window reads the HAND, so it has to be drawn
-- and not merely owned.
powderGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
powderGame powder mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany powder S.alice 1 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob n withAlice))

-- Records every CR 103.5b offer's candidate list, declines it, and keeps.
recordWindow :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
recordWindow p = case p of
  Prompt.MulliganAction _ _ candidates -> do
    State.modify' (candidates :)
    pure Nothing
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  _ -> pure (S.identityAnswer p)

-- Takes the first offered CR 103.5b action whenever one is offered, and always
-- keeps. With a single Powder in the deck the window loop ends by itself: the
-- action exiles the Powder along with the rest of the hand, so the redrawn hand
-- offers nothing.
usePowder :: Prompt.Prompt r -> r
usePowder p = case p of
  Prompt.MulliganAction _ _ candidates -> Maybe.listToMaybe candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- alice's library: `above` Mountains, then a Serum Powder, then 20 more; bob's
-- is uniform. With `above` = 7 the Powder is NOT in the opening hand and is
-- drawn only after a mulligan, which is how CR 103.5b's "This need not be in
-- the first round of mulligans" becomes observable.
powderUnder :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
powderUnder powder mountain above =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice 20 (addMany powder S.alice 1 (addMany mountain S.alice above g0))
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob 20 withAlice))

-- alice's library: a Serum Powder, six Mountains, a SECOND Serum Powder, then
-- 20 Mountains. Her opening hand is the first Powder plus six Mountains; using
-- it exiles that hand and draws the second Powder, which is what makes CR
-- 103.5b's repeated action observable.
chainGame :: Printing.Printing -> Printing.Printing -> GameState.GameState
chainGame powder mountain =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice =
        addMany mountain S.alice 20 (addMany powder S.alice 1 (addMany mountain S.alice 6 (addMany powder S.alice 1 g0)))
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob 20 withAlice))

-- alice's whole library is a Serum Powder and six Mountains -- exactly one
-- opening hand, nothing left to redraw.
shortPowderGame :: Printing.Printing -> Printing.Printing -> GameState.GameState
shortPowderGame powder mountain =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice 6 (addMany powder S.alice 1 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob 20 withAlice))

-- Takes the first offered action, and mulligans exactly once (alice only), so a
-- test can prove the action did NOT count toward CR 103.5's bottom count.
powderThenMulliganOnce :: Prompt.Prompt r -> r
powderThenMulliganOnce p = case p of
  Prompt.MulliganAction _ _ candidates -> Maybe.listToMaybe candidates
  Prompt.DeclareMulligan _ pid offer ->
    if pid == S.alice && MulliganOffer.taken offer < 1 then MulliganDecision.Mulligan else MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Declines every window and mulligans once (alice only), recording how many
-- DECLARATIONS had already happened at each offer. An offer recorded after two
-- declarations is a second-round offer: round 1 is alice's declaration then
-- bob's.
recordWindowRound :: Prompt.Prompt r -> State.State (Int, [Int]) r
recordWindowRound p = case p of
  Prompt.MulliganAction {} -> do
    State.modify' (\(n, seen) -> (n, n : seen))
    pure Nothing
  Prompt.DeclareMulligan _ pid offer -> do
    State.modify' (\(n, seen) -> (n + 1, seen))
    pure (if pid == S.alice && MulliganOffer.taken offer < 1 then MulliganDecision.Mulligan else MulliganDecision.Keep)
  -- Bottoms the LAST card, not the first. S.identityAnswer bottoms the first,
  -- which here is the Powder the mulligan just drew -- it would land at the
  -- library bottom and round 2 would have nothing to offer, so the test would
  -- pass or fail on the fixture's draw order rather than on CR 103.5b.
  Prompt.Bottom _ _ hand count -> pure (reverse (take (fromIntegral count) (reverse hand)))
  _ -> pure (S.identityAnswer p)

-- alice keeps at once; bob mulligans twice then keeps. Every window is
-- declined, and each offer's player is recorded, so a test can prove a player
-- who has kept is never offered again.
recordWindowPlayers :: Prompt.Prompt r -> State.State [PlayerId] r
recordWindowPlayers p = case p of
  Prompt.MulliganAction _ pid _ -> do
    State.modify' (pid :)
    pure Nothing
  Prompt.DeclareMulligan _ pid offer ->
    pure (if pid == S.bob && MulliganOffer.taken offer < 2 then MulliganDecision.Mulligan else MulliganDecision.Keep)
  _ -> pure (S.identityAnswer p)

-- Records alice's offer at each of her declarations, chronologically reversed,
-- and mulligans her twice before keeping; everyone else keeps at once. What the
-- offer SAYS is the point: `taken` and the cards the next mulligan would bottom
-- are different numbers once CR 103.5c's free mulligan exists.
recordOffers :: Prompt.Prompt r -> State.State [(Natural, Natural)] r
recordOffers p = case p of
  Prompt.DeclareMulligan _ pid offer ->
    if pid == S.alice
      then do
        State.modify' ((MulliganOffer.taken offer, MulliganOffer.bottomCount offer) :)
        pure (if MulliganOffer.taken offer < 2 then MulliganDecision.Mulligan else MulliganDecision.Keep)
      else pure MulliganDecision.Keep
  _ -> pure (S.identityAnswer p)

-- Records each DeclareMulligan's pid, so the test can assert a kept player is
-- not asked again. Alice keeps immediately; bob mulligans twice then keeps.
recordAsks :: Prompt.Prompt r -> State.State [PlayerId] r
recordAsks p = case p of
  Prompt.DeclareMulligan _ pid offer -> do
    State.modify' (pid :)
    pure (if pid == S.bob && MulliganOffer.taken offer < 2 then MulliganDecision.Mulligan else MulliganDecision.Keep)
  _ -> pure (S.identityAnswer p)

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Mulligan"
    [ HU.testCase "CR 103.5b: a hand card granting an action is offered at the declaration" $ do
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = powderGame powder mountain 20
            ((_, _after), offered) = State.runState (Program.foldProgramM recordWindow (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
        HU.assertEqual "exactly one offer -- alice's, in the one round she declares" 1 (length offered)
        HU.assertEqual "and it offered exactly her Powder" [1] (fmap length offered),
      HU.testCase "CR 103.5b: taking the action exiles the whole hand and redraws that many" $ do
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let after = run usePowder (powderGame powder mountain 20)
        HU.assertEqual "alice's hand is a full seven again" 7 (S.handSize S.alice after)
        HU.assertEqual "her first seven are exiled" 7 (length (Game.zoneMembers Zone.Exile S.alice after))
        HU.assertEqual "and her library is seven shorter than after the opening draw" 7 (libSize S.alice after)
        HU.assertEqual "bob, with no Powder, is untouched" 7 (S.handSize S.bob after)
        HU.assertEqual "and exiles nothing" 0 (length (Game.zoneMembers Zone.Exile S.bob after)),
      HU.testCase "a hand action's effects can name their own card through the reserved self slot" $ do
        -- Resolve.performHandAction binds the granting card into
        -- Binding.triggerSource, which is how "this card" is expressible with no
        -- self-variant opcode (Effect.Sacrifice's own comment). Proved here on a
        -- settled opening hand so it does not depend on CR 103.6 existing yet.
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let drawn = run keepAnswer (powderGame powder mountain 20)
        case Game.zoneMembers Zone.Hand S.alice drawn of
          [] -> HU.assertFailure "expected a drawn opening hand to act from"
          oid : _ -> do
            let after = S.runPure S.identityAnswer drawn (Resolve.performHandAction oid S.alice [Effect.MoveToZone Binding.triggerSource Zone.Exile])
            HU.assertEqual "the named card left the hand" 6 (S.handSize S.alice after)
            HU.assertEqual "and is in exile" 1 (length (Game.zoneMembers Zone.Exile S.alice after)),
      HU.testCase "CR 103.5b: the action is not a mulligan -- it does not add to the bottom count" $ do
        -- alice takes the action and then mulligans ONCE. CR 103.5 bottoms a
        -- number equal to the mulligans she has taken, which is one -- so her
        -- opening hand is six. A five would mean the action had been counted.
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let after = run powderThenMulliganOnce (powderGame powder mountain 20)
        HU.assertEqual "one mulligan bottoms exactly one card" 6 (S.handSize S.alice after),
      HU.testCase "CR 103.5b: the window is offered in a later round, not just the first" $ do
        -- The Powder sits under alice's opening seven, so round 1 offers her
        -- nothing; she mulligans, redraws into it, and round 2 offers it. The
        -- recorded value is how many declarations preceded the offer: 2 (hers
        -- and bob's, both in round 1).
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = powderUnder powder mountain 7
            ((_, _after), (_, offers)) = State.runState (Program.foldProgramM recordWindowRound (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) (0, [])
        HU.assertEqual "exactly one offer, and it came after both first-round declarations" [2] offers,
      HU.testCase "CR 103.5b: the action may be taken more than once in one window" $ do
        -- Using the first Powder draws the second; the loop offers again and
        -- ends only when the redrawn hand holds none. Fourteen cards exiled is
        -- two uses.
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let after = run usePowder (chainGame powder mountain)
        HU.assertEqual "two hands exiled" 14 (length (Game.zoneMembers Zone.Exile S.alice after))
        HU.assertEqual "and the third hand is a full seven" 7 (S.handSize S.alice after),
      HU.testCase "CR 103.5b: a hand with no granting card is not asked" $ do
        -- Where the rules leave nothing to ask, don't prompt.
        mountain <- Registry.printing registry "Mountain"
        let gs0 = libraryGame mountain 20
            ((_, _after), offered) = State.runState (Program.foldProgramM recordWindow (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
        HU.assertEqual "no window prompt at all" [] offered,
      HU.testCase "CR 103.5b: a player who has kept is never offered the window again" $ do
        -- alice keeps in round 1 and leaves the pool; bob keeps the loop alive
        -- for two more rounds. She declares once, so she is offered once --
        -- CR 103.5b's window exists only "at a time they would declare".
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = powderGame powder mountain 20
            ((_, _after), offers) = State.runState (Program.foldProgramM recordWindowPlayers (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
        HU.assertEqual "offered in her one declaration round and never again" 1 (length (filter (== S.alice) offers)),
      HU.testCase "CR 103.5b: a game with a mulligan action replays deterministically" $ do
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = powderGame powder mountain 20
            ((_, recorded), responses) = Replay.record usePowder gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
            (_, replayed) = Replay.replay responses gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
        HU.assertEqual "alice hand matches" (S.handSize S.alice recorded) (S.handSize S.alice replayed)
        HU.assertEqual "alice exile matches" (Game.zoneMembers Zone.Exile S.alice recorded) (Game.zoneMembers Zone.Exile S.alice replayed)
        HU.assertEqual
          "alice library ORDER matches"
          (Game.zoneMembers Zone.Library S.alice recorded)
          (Game.zoneMembers Zone.Library S.alice replayed),
      HU.testCase "CR 727.3/729.3: a short deck still flags drewFromEmpty through the CR 103.5b action" $ do
        -- alice's whole library is one opening hand. Taking the action exiles
        -- all seven and redraws from an empty library, which flags the failed
        -- draw and leaves her with nothing -- a forced keep under CR 103.5's
        -- final sentence, so she is not asked to declare.
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let after = run usePowder (shortPowderGame powder mountain)
        HU.assertEqual "her hand is empty" 0 (S.handSize S.alice after)
        HU.assertBool "and she drew from an empty library" (Set.member S.alice (GameState.drewFromEmpty after)),
      HU.testCase "CR 103.5: all-Keep draws exactly seven, library shrinks by seven" $ do
        mountain <- Registry.printing registry "Mountain"
        let after = run keepAnswer (libraryGame mountain 20)
        HU.assertEqual "alice hand" 7 (S.handSize S.alice after)
        HU.assertEqual "alice library" 13 (libSize S.alice after),
      HU.testCase "CR 103.5: one mulligan bottoms one card (hand of six)" $ do
        mountain <- Registry.printing registry "Mountain"
        let after = run (mulliganUpTo 1) (libraryGame mountain 20)
        HU.assertEqual "alice hand" 6 (S.handSize S.alice after),
      HU.testCase "CR 103.5: two mulligans bottom two cards (hand of five)" $ do
        mountain <- Registry.printing registry "Mountain"
        let after = run (mulliganUpTo 2) (libraryGame mountain 20)
        HU.assertEqual "alice hand" 5 (S.handSize S.alice after),
      HU.testCase "CR 103.5 final sentence: mulligan floors at a zero-card hand" $ do
        -- A 7-card library: redraw is always 7, but the loop bottoms up to the
        -- growing count; the process deterministically terminates with alice's
        -- opening hand floored at zero cards (never negative).
        mountain <- Registry.printing registry "Mountain"
        let after = run (mulliganUpTo 100) (libraryGame mountain 7)
        HU.assertEqual "opening hand floored at zero" 0 (S.handSize S.alice after),
      HU.testCase "CR 103.5: bottomed cards are the ones NOT in the opening hand" $ do
        -- One mulligan: hand is 6, and the 1 bottomed card is at the library
        -- bottom, distinct (by id) from the six in hand.
        mountain <- Registry.printing registry "Mountain"
        let after = run (mulliganUpTo 1) (libraryGame mountain 20)
            handIds = Set.fromList (Game.zoneMembers Zone.Hand S.alice after)
            bottom1 = libBottom 1 S.alice after
        HU.assertEqual "the bottomed card is not in hand" [] (filter (`Set.member` handIds) bottom1),
      HU.testCase "CR 103.5: the chosen bottom ORDER is honored, not just the set" $ do
        -- alice's deck (top to bottom) is 20 distinct printings, index 0 drawn
        -- first. Opening hand takes indices 0..6; round 1 (taken 0->1) returns
        -- them to the bottom, redraws indices 7..13, and bottoms (reversed)
        -- index 7 alone -- a singleton, no order to prove yet. Round 2 (taken
        -- 1->2) returns the surviving hand (8..13) to the bottom, redraws
        -- indices 14..19,0, and bottoms count=2 reversed: [15,14] -- humility
        -- then giantGrowth, a genuinely non-identity order. Round 3 (taken=2)
        -- keeps, ending the process.
        mountain <- Registry.printing registry "Mountain"
        printings <- distinctPrintings registry
        humility <- Registry.printing registry "Humility"
        giantGrowth <- Registry.printing registry "Giant Growth"
        let gs0 = orderedLibraryGame mountain printings
            after = run bottomReversedAnswer gs0
            bottomCards = fmap (\oid -> Game.cardOf oid after) (libBottom 2 S.alice after)
            -- The round-2 bottoming returns indices [15, 14] of distinctPrintings
            -- (humility, then giantGrowth); named directly to avoid a partial `!!`.
            expectedCards = fmap (Just . Printing.card) [humility, giantGrowth]
        HU.assertEqual "library bottom equals the chosen order exactly (humility, then giantGrowth)" expectedCards bottomCards,
      HU.testCase "CR 103.5: keeping is terminal -- a kept player is not asked again" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs0 = libraryGame mountain 20
            ((_, _after), asked) = State.runState (Program.foldProgramM recordAsks (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
        HU.assertEqual "alice kept round 1, so was asked exactly once" 1 (length (filter (== S.alice) asked))
        HU.assertBool "bob mulliganed twice then kept, so was asked more than once" (length (filter (== S.bob) asked) > 1),
      HU.testCase "CR 103.5: a game with mulligans replays deterministically" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs0 = libraryGame mountain 20
            ((_, recorded), responses) = Replay.record (mulliganUpTo 2) gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
            (_, replayed) = Replay.replay responses gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
        HU.assertEqual "alice hand matches" (S.handSize S.alice recorded) (S.handSize S.alice replayed)
        HU.assertEqual "alice library matches" (libSize S.alice recorded) (libSize S.alice replayed)
        HU.assertEqual
          "alice library ORDER matches"
          (Game.zoneMembers Zone.Library S.alice recorded)
          (Game.zoneMembers Zone.Library S.alice replayed),
      HU.testCase "CR 727.3/729.3: a short library still flags drewFromEmpty through the mulligan path" $ do
        -- bob has a 5-card library; his 7-card opening draw empties it and flags
        -- the failed draw. Driven by an actual mulligan (mulliganUpTo 1) so the
        -- flag is shown to SURVIVE the shuffle-back-and-redraw path, not merely
        -- the initial draw -- "regardless of any mulligans" (CR 727.3 / 729.3).
        mountain <- Registry.printing registry "Mountain"
        let g0 = Setup.emptyGame S.bothPlayers
            addMany pid n g = List.foldl' (\h _ -> snd (S.addCreature mountain pid h)) g (replicate n ())
            gs0 = poolToLibrary S.bob (poolToLibrary S.alice (addMany S.bob 5 (addMany S.alice 20 g0)))
            after = run (mulliganUpTo 1) gs0
        HU.assertBool "bob drew from an empty library" (Set.member S.bob (GameState.drewFromEmpty after)),
      HU.testCase "CR 103.5c/800.6: one free mulligan at three seats, none at two" $ do
        HU.assertEqual "three seats: the first mulligan is free" 1 (Mulligan.freeMulligans S.threePlayerGame)
        HU.assertEqual "two seats: none is" 0 (Mulligan.freeMulligans (Setup.emptyGame S.bothPlayers)),
      HU.testCase "CR 800.1: a game that BEGAN with three players keeps its free mulligan after a departure" $
        -- CR 800.1: "A multiplayer game is a game that begins with more than two
        -- players." Begins with, not currently has. GameState.turnOrder is the
        -- permanent seating roster, so this stays 1 with two survivors; an
        -- implementation that counted Departure.stillPlaying would answer 0.
        HU.assertEqual
          "still one free mulligan with two survivors"
          1
          (Mulligan.freeMulligans (Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame)),
      HU.testCase "CR 103.5c: the first mulligan bottoms nothing and the second bottoms one" $ do
        -- Two runs over the same three-seat board. Today both bottom one more
        -- card than they should: takeMulligan bottoms `count` unconditionally,
        -- so the first mulligan leaves a hand of 6 and the second a hand of 5.
        mountain <- Registry.printing registry "Mountain"
        let once = run3 (mulliganUpTo 1) (libraryGame3 mountain 20)
            twice = run3 (mulliganUpTo 2) (libraryGame3 mountain 20)
        HU.assertEqual "alice's free first mulligan keeps all seven" 7 (S.handSize S.alice once)
        HU.assertEqual "bob's too" 7 (S.handSize S.bob once)
        HU.assertEqual "carol's too" 7 (S.handSize S.carol once)
        HU.assertEqual "and nothing went to the bottom of alice's library" 13 (libSize S.alice once)
        HU.assertEqual "the second mulligan bottoms exactly one" 6 (S.handSize S.alice twice)
        HU.assertEqual "two-player: unchanged, the first mulligan bottoms one" 6 (S.handSize S.alice (run (mulliganUpTo 1) (libraryGame mountain 20))),
      HU.testCase "CR 103.5c second clause: the free mulligan does not count toward the limit either" $ do
        -- Hand + library is 20 throughout, so the redraw is always a full seven
        -- and the process ends exactly when the bottomed count reaches seven.
        -- Two seats: hands run 6,5,4,3,2,1,0 -- seven asks. Three seats: the
        -- first is free, so 7,6,5,4,3,2,1,0 -- eight asks. Today both give
        -- seven, because the free allowance does not exist.
        mountain <- Registry.printing registry "Mountain"
        let asksIn owners gs0 = snd (State.runState (Program.foldProgramM recordAlwaysMulligan (State.runStateT (Mulligan.openingHands S.performer owners) gs0)) [])
            three = asksIn [S.alice, S.bob, S.carol] (libraryGame3 mountain 20)
            two = asksIn [S.alice, S.bob] (libraryGame mountain 20)
        HU.assertEqual "three seats: alice may take eight mulligans" 8 (length (filter (== S.alice) three))
        HU.assertEqual "two seats: seven, unchanged" 7 (length (filter (== S.alice) two)),
      HU.testCase "CR 103.5c: the declaration says what the mulligan COSTS, not just how many were taken" $ do
        -- The two seat counts agree on `taken` at every step and disagree on the
        -- cost at every step: three seats get CR 103.5c's free first mulligan,
        -- so alice's opening declaration offers (taken 0, bottom 0) where a
        -- two-player game offers (taken 0, bottom 1). An answerer holding only
        -- the raw count cannot tell those apart, which is what #176 was about.
        mountain <- Registry.printing registry "Mountain"
        let offersIn owners gs0 = reverse (snd (State.runState (Program.foldProgramM recordOffers (State.runStateT (Mulligan.openingHands S.performer owners) gs0)) []))
        HU.assertEqual
          "three seats: the first mulligan is free, so it bottoms nothing"
          [(0, 0), (1, 1), (2, 2)]
          (offersIn [S.alice, S.bob, S.carol] (libraryGame3 mountain 20))
        HU.assertEqual
          "two seats: every mulligan costs, from the first"
          [(0, 1), (1, 2), (2, 3)]
          (offersIn [S.alice, S.bob] (libraryGame mountain 20)),
      HU.testCase "CR 103.5c: a mulligan that bottoms nothing asks nothing" $ do
        -- Where the rules leave nothing to ask, don't prompt: choosing zero of
        -- seven cards has exactly one legal answer. Today the free mulligan is
        -- not free, so each of the three players is asked to bottom one card and
        -- three Response.PutOnBottom entries are recorded.
        mountain <- Registry.printing registry "Mountain"
        let bottomsIn owners gs0 =
              let (_, log_) = Replay.record (mulliganUpTo 1) gs0 (Mulligan.openingHands S.performer owners)
                  isBottom r = case r of
                    Response.PutOnBottom _ -> True
                    _ -> False
               in length (filter isBottom log_)
        HU.assertEqual "three seats: no bottom choice is asked for a free mulligan" 0 (bottomsIn [S.alice, S.bob, S.carol] (libraryGame3 mountain 20))
        HU.assertEqual "two seats: both players are still asked" 2 (bottomsIn [S.alice, S.bob] (libraryGame mountain 20))
    ]
