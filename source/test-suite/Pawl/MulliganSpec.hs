{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Mulligan: the CR 103.5 opening-hand / mulligan process.
module Pawl.MulliganSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HandActionIndex as HandActionIndex
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Zone as Zone

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
mulliganUpTo :: Natural -> Prompt.Prompt r -> r
mulliganUpTo k p = case p of
  Prompt.DeclareMulligan _ _ offer ->
    if MulliganOffer.taken offer < k then MulliganDecision.Mulligan else MulliganDecision.Keep
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
-- Event.changeZone (Pawl.Engine.Event) mints every moved card a FRESH ObjectId --
-- mkObj there only resets zone/tapped/damage/sickness/bindings/counters/
-- timestamp -- so a card's pre-move id is never equal to its post-move id,
-- which is why an order test can't compare ObjectIds across the move. Source
-- (hence the printed Card, read back via Game.cardOf) is the one field mkObj
-- carries forward unchanged, so it survives the reincarnation and is what
-- this test tracks instead. Bob's library is uniform Mountains; only alice
-- mulligans in the accompanying test, so bob's composition never enters the
-- assertion.
distinctPrintings :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m [Printing.Printing]
distinctPrintings s registry =
  mapM
    (S.printingOf s registry)
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
  Prompt.Bottom _ _ hand count -> reverse (List.genericTake count hand)
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
recordWindow :: Prompt.Prompt r -> State.State [[(ObjectId.ObjectId, HandActionIndex.HandActionIndex)]] r
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

-- Takes the offered CR 103.5b action whose index is `i`, and always keeps.
-- Answering by INDEX is the whole point: the fixture card grants two actions, so
-- the ObjectId alone would not say which of them was taken.
useActionAt :: Natural -> Prompt.Prompt r -> r
useActionAt i p = case p of
  Prompt.MulliganAction _ _ candidates ->
    List.find (\(_, index) -> index == HandActionIndex.MkHandActionIndex i) candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- alice's library: a Synthetic Twofold Powder on top, then `n` Mountains; bob's
-- is uniform. The Powder is drawn into her opening hand, which is where CR
-- 103.5b reads from.
twofoldGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
twofoldGame twofold mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany twofold S.alice 1 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob n withAlice))

-- alice's library: a No-Regrets Egret on top, then `n` Mountains; bob's is
-- uniform. The Egret is drawn into her opening hand, which is where CR 103.5b
-- reads from -- and unlike the Powder its action leaves it there.
egretGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
egretGame egret mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany egret S.alice 1 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob n withAlice))

-- Takes the first offered CR 103.5b action for the first `k` offers, declines
-- the next, and keeps; every offer's candidate list is recorded.
--
-- The bound is the ANSWERER's, deliberately. handWindow re-offers an action
-- that stays in hand for as long as the player keeps taking it (CR 103.5b caps
-- nothing, CR 104.4b leaves an optional loop alone), so a test that answered
-- "take" forever would hang rather than fail. Counting the offers instead makes
-- the failure this is for -- a window that stops re-offering a repeatable
-- action -- an assertion that fails in milliseconds. The opposite failure, a
-- window that ignores the decline, is a real hang and can only ever show up as
-- a TIMEOUT; nothing here changes that.
takeThenDecline :: Int -> Prompt.Prompt r -> State.State [[(ObjectId.ObjectId, HandActionIndex.HandActionIndex)]] r
takeThenDecline k p = case p of
  Prompt.MulliganAction _ _ candidates -> do
    seen <- State.get
    State.put (candidates : seen)
    pure (if length seen < k then Maybe.listToMaybe candidates else Nothing)
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  _ -> pure (S.identityAnswer p)

-- The names of the cards alice REVEALED (CR 701.20a), in event order. CR
-- 701.20e's look is not one of them -- it is private and records no event --
-- which is what keeps this a count of the reveals alone.
revealedNames :: GameState.GameState -> [String]
revealedNames gs = Maybe.mapMaybe revealedName (S.eventsOf gs)
  where
    revealedName event = case event of
      GameEvent.Revealed (Revealed.MkRevealed pid _ _ pc)
        | pid == S.alice ->
            fmap (Text.unpack . CardName.unwrap) (Maybe.listToMaybe (Set.toList (PC.names pc)))
      _ -> Nothing

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
  Prompt.Bottom _ _ hand count -> pure (reverse (List.genericTake count (reverse hand)))
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

-- alice's library with a Leyline of the Void on top and `n` Mountains under it;
-- bob's is uniform Mountains. The Leyline is drawn into her opening hand, which
-- is where CR 103.6 reads from.
leylineGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
leylineGame leyline mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany leyline S.alice 1 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob n withAlice))

-- Both players open with a Leyline on top, so a test can watch turn order.
leylineBothGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
leylineBothGame leyline mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany leyline S.alice 1 g0)
      withBoth = addMany mountain S.bob n (addMany leyline S.bob 1 withAlice)
   in poolToLibrary S.bob (poolToLibrary S.alice withBoth)

-- alice opens with TWO Leylines on top, so CR 103.6's "any such actions in any
-- order" is observable: a single ask cannot place both.
twoLeylineGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
twoLeylineGame leyline mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany leyline S.alice 2 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob n withAlice))

-- Takes every offered CR 103.6 action; keeps every hand.
useOpeningAction :: Prompt.Prompt r -> r
useOpeningAction p = case p of
  Prompt.OpeningHandAction _ _ candidates -> Maybe.listToMaybe candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Declines every CR 103.6 action; keeps every hand.
declineOpeningAction :: Prompt.Prompt r -> r
declineOpeningAction p = case p of
  Prompt.OpeningHandAction {} -> Nothing
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Records the prompt stream as tags, so a test can prove CR 103.6's window opens
-- only once the whole CR 103.5 process is complete, and in turn order.
recordOpeningOrder :: Prompt.Prompt r -> State.State [(Text.Text, PlayerId)] r
recordOpeningOrder p = case p of
  Prompt.DeclareMulligan _ pid _ -> do
    State.modify' ((Text.pack "declare", pid) :)
    pure MulliganDecision.Keep
  Prompt.OpeningHandAction _ pid _ -> do
    State.modify' ((Text.pack "opening", pid) :)
    pure Nothing
  _ -> pure (S.identityAnswer p)

-- Records each DeclareMulligan's pid, so the test can assert a kept player is
-- not asked again. Alice keeps immediately; bob mulligans twice then keeps.
recordAsks :: Prompt.Prompt r -> State.State [PlayerId] r
recordAsks p = case p of
  Prompt.DeclareMulligan _ pid offer -> do
    State.modify' (pid :)
    pure (if pid == S.bob && MulliganOffer.taken offer < 2 then MulliganDecision.Mulligan else MulliganDecision.Keep)
  _ -> pure (S.identityAnswer p)

-- Mulligans `n` times as alice, then keeps -- so the bottoming step runs with a
-- known count.
mulliganingTwice :: Prompt.Prompt r -> Maybe r
mulliganingTwice p = case p of
  Prompt.DeclareMulligan _ pid offer ->
    Just (if pid == S.alice && MulliganOffer.taken offer < 2 then MulliganDecision.Mulligan else MulliganDecision.Keep)
  _ -> Nothing

-- Names the first card of the hand it is ACTUALLY offered, twice. Reading the
-- offered hand is what makes this a dedupe test: a fixed list computed elsewhere
-- would name ids that are not in the post-mulligan hand and be rejected by the
-- membership filter instead, never reaching the duplicate check.
bottomDuplicatingFirst :: Prompt.Prompt r -> r
bottomDuplicatingFirst p = case mulliganingTwice p of
  Just r -> r
  Nothing -> case p of
    Prompt.Bottom _ _ hand _ -> case hand of
      h : _ -> [h, h]
      [] -> []
    _ -> S.identityAnswer p

-- Names two ids that were never in any hand.
bottomInventing :: Prompt.Prompt r -> r
bottomInventing p = case mulliganingTwice p of
  Just r -> r
  Nothing -> case p of
    Prompt.Bottom {} -> [ObjectId.MkObjectId 9998, ObjectId.MkObjectId 9999]
    _ -> S.identityAnswer p

-- #222: an answer that is not a permutation of the library must not become the
-- library. CR 701.24a defines a shuffle as randomising the order of the cards
-- that are there -- not as replacing them.
-- Answers Prompt.Shuffle with a fixed list, whatever was offered -- the lying
-- interpreter #222 is about. Top-level so it stays polymorphic in the prompt's
-- result type; a `let` binding would not.
shuffleAnswering :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
shuffleAnswering answer p = case p of
  Prompt.Shuffle _ -> answer
  _ -> S.identityAnswer p

-- An HONEST interpreter: a genuine permutation of what it was offered.
reversingShuffle :: Prompt.Prompt r -> r
reversingShuffle p = case p of
  Prompt.Shuffle ids -> reverse ids
  _ -> S.identityAnswer p

trustedAnswerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trustedAnswerSpec s registry =
  Spec.describe s "TrustedAnswers" $ do
    -- CR 103.5: after mulliganing twice, alice bottoms exactly two cards and
    -- keeps five. An interpreter naming the same card twice must not bottom it
    -- twice -- the second move is a no-op on an id that has already left, so the
    -- hand would silently end up one card too big.
    --
    -- TWO mulligans, not one: with n = 1 the duplicate is truncated away by
    -- `take n` before the dedupe is ever consulted, and the test would pass
    -- against the bug it exists to catch.
    Spec.it s "#222 a bottoming answer that repeats a card still bottoms two" $ do
      mountain <- S.printingOf s registry "Mountain"
      let after = run bottomDuplicatingFirst (libraryGame mountain 20)
      Spec.assertEqWith s "alice mulliganed twice, so she keeps five" (length (Game.zoneMembers Zone.Hand S.alice after)) 5
    Spec.it s "#222 a bottoming answer naming cards not in hand still bottoms from the hand" $ do
      mountain <- S.printingOf s registry "Mountain"
      let after = run bottomInventing (libraryGame mountain 20)
      Spec.assertEqWith s "alice still ends on five" (length (Game.zoneMembers Zone.Hand S.alice after)) 5
      Spec.assertBool s (List.notElem (ObjectId.MkObjectId 9999) (Game.zoneMembers Zone.Library S.alice after)) "and no invented card reached her library"
    Spec.it s "#222 a shuffle answer that duplicates a card is refused" $ do
      forest <- S.printingOf s registry "Forest"
      let (a, g1) = S.addLibraryCard forest S.alice (Setup.emptyGame S.bothPlayers)
          (b, gs) = S.addLibraryCard forest S.alice g1
          -- The interpreter returns one card twice: a library that would gain a
          -- card and lose one.
          after = S.runPure (shuffleAnswering [a, a]) gs (Mulligan.shuffleLibrary S.alice)
      Spec.assertEqWith s "the library still holds both cards, once each" (List.sort (Game.zoneMembers Zone.Library S.alice after)) [a, b]
    Spec.it s "#222 a shuffle answer naming a card that was never there is refused" $ do
      forest <- S.printingOf s registry "Forest"
      let (a, g1) = S.addLibraryCard forest S.alice (Setup.emptyGame S.bothPlayers)
          (b, gs) = S.addLibraryCard forest S.alice g1
          phantom = ObjectId.MkObjectId 9999
          after = S.runPure (shuffleAnswering [a, phantom]) gs (Mulligan.shuffleLibrary S.alice)
      Spec.assertEqWith s "no invented card entered the library" (List.sort (Game.zoneMembers Zone.Library S.alice after)) [a, b]
    -- The control: an honest permutation IS honoured, so the guard cannot pass
    -- by ignoring every answer.
    Spec.it s "#222 an honest permutation is honoured" $ do
      forest <- S.printingOf s registry "Forest"
      let (a, g1) = S.addLibraryCard forest S.alice (Setup.emptyGame S.bothPlayers)
          (b, gs) = S.addLibraryCard forest S.alice g1
          before = Game.zoneMembers Zone.Library S.alice gs
          after = S.runPure reversingShuffle gs (Mulligan.shuffleLibrary S.alice)
      Spec.assertEqWith s "the fixture really has two cards" (List.sort before) [a, b]
      Spec.assertEqWith s "the order is the reversal the interpreter asked for" (Game.zoneMembers Zone.Library S.alice after) (reverse before)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry =
  Spec.describe s "Pawl.Engine.Mulligan" $ do
    Spec.it s "CR 103.5b: a hand card granting an action is offered at the declaration" $ do
      powder <- S.printingOf s registry "Serum Powder"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = powderGame powder mountain 20
          ((_, _after), offered) = State.runState (Engine.runGame recordWindow gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) []
      Spec.assertEqWith s "exactly one offer -- alice's, in the one round she declares" (length offered) 1
      Spec.assertEqWith s "and it offered exactly her Powder" (fmap length offered) [1]
    Spec.it s "CR 103.5b: taking the action exiles the whole hand and redraws that many" $ do
      powder <- S.printingOf s registry "Serum Powder"
      mountain <- S.printingOf s registry "Mountain"
      let after = run usePowder (powderGame powder mountain 20)
      Spec.assertEqWith s "alice's hand is a full seven again" (S.handSize S.alice after) 7
      Spec.assertEqWith s "her first seven are exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 7
      Spec.assertEqWith s "and her library is seven shorter than after the opening draw" (libSize S.alice after) 7
      Spec.assertEqWith s "bob, with no Powder, is untouched" (S.handSize S.bob after) 7
      Spec.assertEqWith s "and exiles nothing" (length (Game.zoneMembers Zone.Exile S.bob after)) 0
    Spec.it s "a hand action's effects can name their own card through the reserved self slot" $ do
      -- Resolve.performHandAction binds the granting card into
      -- Binding.triggerSource, which is how "this card" is expressible with no
      -- self-variant opcode (Effect.Sacrifice's own comment). Proved here on a
      -- settled opening hand so it does not depend on CR 103.6 existing yet.
      powder <- S.printingOf s registry "Serum Powder"
      mountain <- S.printingOf s registry "Mountain"
      let drawn = run keepAnswer (powderGame powder mountain 20)
      case Game.zoneMembers Zone.Hand S.alice drawn of
        [] -> Spec.assertFailure s "expected a drawn opening hand to act from"
        oid : _ -> do
          let after = S.runPure S.identityAnswer drawn (Resolve.performHandAction oid S.alice [Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.triggerSource) Zone.Exile EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue)])
          Spec.assertEqWith s "the named card left the hand" (S.handSize S.alice after) 6
          Spec.assertEqWith s "and is in exile" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
    Spec.it s "CR 103.5b: the action is not a mulligan -- it does not add to the bottom count" $ do
      -- alice takes the action and then mulligans ONCE. CR 103.5 bottoms a
      -- number equal to the mulligans she has taken, which is one -- so her
      -- opening hand is six. A five would mean the action had been counted.
      powder <- S.printingOf s registry "Serum Powder"
      mountain <- S.printingOf s registry "Mountain"
      let after = run powderThenMulliganOnce (powderGame powder mountain 20)
      Spec.assertEqWith s "one mulligan bottoms exactly one card" (S.handSize S.alice after) 6
    Spec.it s "CR 103.5b: the window is offered in a later round, not just the first" $ do
      -- The Powder sits under alice's opening seven, so round 1 offers her
      -- nothing; she mulligans, redraws into it, and round 2 offers it. The
      -- recorded value is how many declarations preceded the offer: 2 (hers
      -- and bob's, both in round 1).
      powder <- S.printingOf s registry "Serum Powder"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = powderUnder powder mountain 7
          ((_, _after), (_, offers)) = State.runState (Engine.runGame recordWindowRound gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) (0, [])
      Spec.assertEqWith s "exactly one offer, and it came after both first-round declarations" offers [2]
    Spec.it s "CR 103.5b: the action may be taken more than once in one window" $ do
      -- Using the first Powder draws the second; the loop offers again and
      -- ends only when the redrawn hand holds none. Fourteen cards exiled is
      -- two uses.
      powder <- S.printingOf s registry "Serum Powder"
      mountain <- S.printingOf s registry "Mountain"
      let after = run usePowder (chainGame powder mountain)
      Spec.assertEqWith s "two hands exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 14
      Spec.assertEqWith s "and the third hand is a full seven" (S.handSize S.alice after) 7
    Spec.it s "CR 103.5b: an action that leaves its card in hand is offered again" $ do
      -- No-Regrets Egret only reveals itself, so nothing leaves the hand and
      -- the same entry comes back. The answerer takes it twice and then
      -- declines, so THREE offers is the whole assertion: one or two would mean
      -- the window stopped re-offering a repeatable action, and that is what
      -- this case is here to catch. It cannot hang -- the answerer bounds the
      -- loop, so the failure arrives as a number rather than as a timeout.
      egret <- S.printingOf s registry "No-Regrets Egret"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = egretGame egret mountain 20
          ((_, after), offers) = State.runState (Engine.runGame (takeThenDecline 2) gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) []
      Spec.assertEqWith s "taken, taken, declined" (length offers) 3
      Spec.assertEqWith s "each offering the one Egret" (fmap length offers) [1, 1, 1]
      Spec.assertEqWith s "CR 701.20b: it never left her hand" (S.handSize S.alice after) 7
      Spec.assertEqWith s "CR 701.20a: revealed once per taking, and the look revealed nothing" (revealedNames after) ["No-Regrets Egret", "No-Regrets Egret"]
      -- 21 cards less the opening seven. CR 701.20e's look at the top two puts
      -- nothing anywhere, so two takings leave the library exactly as the draw
      -- left it.
      Spec.assertEqWith s "and the look moved no card" (libSize S.alice after) 14
    Spec.it s "CR 103.5b: one card granting two actions offers both" $ do
      -- Nothing in CR 103 caps how many such actions a card grants, so the two
      -- are two offers with the same granting card and different indices. A
      -- single offer here would mean the second action is unreachable.
      twofold <- S.printingOf s registry "Synthetic Twofold Powder"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = twofoldGame twofold mountain 20
          ((_, _after), offered) = State.runState (Engine.runGame recordWindow gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) []
      Spec.assertEqWith s "exactly one offer -- alice's, in the one round she declares" (length offered) 1
      Spec.assertEqWith s "and it offered two actions" (fmap length offered) [2]
      Spec.assertEqWith
        s
        "both from the same card, as its first and second action"
        (fmap (fmap snd) offered)
        [[HandActionIndex.MkHandActionIndex 0, HandActionIndex.MkHandActionIndex 1]]
      Spec.assertEqWith
        s
        "and the two entries name one card, not two"
        (fmap (length . List.nub . fmap fst) offered)
        [1]
    Spec.it s "CR 103.5b: taking the FIRST of two actions does the first one's thing" $ do
      twofold <- S.printingOf s registry "Synthetic Twofold Powder"
      mountain <- S.printingOf s registry "Mountain"
      let after = run (useActionAt 0) (twofoldGame twofold mountain 20)
      Spec.assertEqWith s "her opening seven are exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 7
      Spec.assertEqWith s "her hand is a full seven again" (S.handSize S.alice after) 7
      Spec.assertEqWith s "and nothing went to the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 0
    Spec.it s "CR 103.5b: taking the SECOND of two actions does the second one's thing" $ do
      -- The discriminating case: the same card, the same window, the other
      -- index. Exiling nothing and putting exactly the granting card onto the
      -- battlefield is what tells the second action apart from the first.
      twofold <- S.printingOf s registry "Synthetic Twofold Powder"
      mountain <- S.printingOf s registry "Mountain"
      let after = run (useActionAt 1) (twofoldGame twofold mountain 20)
      Spec.assertEqWith s "the granting card is on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 1
      Spec.assertEqWith s "her hand is one smaller" (S.handSize S.alice after) 6
      Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 0
    Spec.it s "CR 103.5b: a two-action card replays deterministically" $ do
      twofold <- S.printingOf s registry "Synthetic Twofold Powder"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = twofoldGame twofold mountain 20
          ((_, recorded), responses) = Replay.record (useActionAt 1) gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
          ((_, replayed), desync) = Replay.replay responses gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
      Spec.assertEqWith s "the transcript answered every prompt" desync Nothing
      Spec.assertEqWith s "the battlefield the second action made matches" (Game.zoneMembers Zone.Battlefield S.alice recorded) (Game.zoneMembers Zone.Battlefield S.alice replayed)
      Spec.assertEqWith s "and the replay exiled nothing either" (length (Game.zoneMembers Zone.Exile S.alice replayed)) 0
    Spec.it s "CR 103.5b: a hand with no granting card is not asked" $ do
      -- Where the rules leave nothing to ask, don't prompt.
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = libraryGame mountain 20
          ((_, _after), offered) = State.runState (Engine.runGame recordWindow gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) []
      Spec.assertEqWith s "no window prompt at all" offered []
    Spec.it s "CR 103.5b: a player who has kept is never offered the window again" $ do
      -- alice keeps in round 1 and leaves the pool; bob keeps the loop alive
      -- for two more rounds. She declares once, so she is offered once --
      -- CR 103.5b's window exists only "at a time they would declare".
      powder <- S.printingOf s registry "Serum Powder"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = powderGame powder mountain 20
          ((_, _after), offers) = State.runState (Engine.runGame recordWindowPlayers gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) []
      Spec.assertEqWith s "offered in her one declaration round and never again" (length (filter (== S.alice) offers)) 1
    Spec.it s "CR 103.5b: a game with a mulligan action replays deterministically" $ do
      powder <- S.printingOf s registry "Serum Powder"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = powderGame powder mountain 20
          ((_, recorded), responses) = Replay.record usePowder gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
          ((_, replayed), desync) = Replay.replay responses gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
      Spec.assertEqWith s "the transcript answered every prompt" desync Nothing
      Spec.assertEqWith s "alice hand matches" (S.handSize S.alice recorded) (S.handSize S.alice replayed)
      Spec.assertEqWith s "alice exile matches" (Game.zoneMembers Zone.Exile S.alice recorded) (Game.zoneMembers Zone.Exile S.alice replayed)
      Spec.assertEqWith
        s
        "alice library ORDER matches"
        (Game.zoneMembers Zone.Library S.alice recorded)
        (Game.zoneMembers Zone.Library S.alice replayed)
    Spec.it s "CR 727.3/729.3: a short deck still flags drewFromEmpty through the CR 103.5b action" $ do
      -- alice's whole library is one opening hand. Taking the action exiles
      -- all seven and redraws from an empty library, which flags the failed
      -- draw and leaves her with nothing -- a forced keep under CR 103.5's
      -- final sentence, so she is not asked to declare.
      powder <- S.printingOf s registry "Serum Powder"
      mountain <- S.printingOf s registry "Mountain"
      let after = run usePowder (shortPowderGame powder mountain)
      Spec.assertEqWith s "her hand is empty" (S.handSize S.alice after) 0
      Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty after)) "and she drew from an empty library"
    Spec.it s "CR 103.5: all-Keep draws exactly seven, library shrinks by seven" $ do
      mountain <- S.printingOf s registry "Mountain"
      let after = run keepAnswer (libraryGame mountain 20)
      Spec.assertEqWith s "alice hand" (S.handSize S.alice after) 7
      Spec.assertEqWith s "alice library" (libSize S.alice after) 13
    Spec.it s "CR 103.5: one mulligan bottoms one card (hand of six)" $ do
      mountain <- S.printingOf s registry "Mountain"
      let after = run (mulliganUpTo 1) (libraryGame mountain 20)
      Spec.assertEqWith s "alice hand" (S.handSize S.alice after) 6
    Spec.it s "CR 103.5: two mulligans bottom two cards (hand of five)" $ do
      mountain <- S.printingOf s registry "Mountain"
      let after = run (mulliganUpTo 2) (libraryGame mountain 20)
      Spec.assertEqWith s "alice hand" (S.handSize S.alice after) 5
    Spec.it s "CR 103.5 final sentence: mulligan floors at a zero-card hand" $ do
      -- A 7-card library: redraw is always 7, but the loop bottoms up to the
      -- growing count; the process deterministically terminates with alice's
      -- opening hand floored at zero cards (never negative).
      mountain <- S.printingOf s registry "Mountain"
      let after = run (mulliganUpTo 100) (libraryGame mountain 7)
      Spec.assertEqWith s "opening hand floored at zero" (S.handSize S.alice after) 0
    Spec.it s "CR 103.5: bottomed cards are the ones NOT in the opening hand" $ do
      -- One mulligan: hand is 6, and the 1 bottomed card is at the library
      -- bottom, distinct (by id) from the six in hand.
      mountain <- S.printingOf s registry "Mountain"
      let after = run (mulliganUpTo 1) (libraryGame mountain 20)
          handIds = Set.fromList (Game.zoneMembers Zone.Hand S.alice after)
          bottom1 = libBottom 1 S.alice after
      Spec.assertEqWith s "the bottomed card is not in hand" (filter (`Set.member` handIds) bottom1) []
    Spec.it s "CR 103.5: the chosen bottom ORDER is honored, not just the set" $ do
      -- alice's deck (top to bottom) is 20 distinct printings, index 0 drawn
      -- first. Opening hand takes indices 0..6; round 1 (taken 0->1) returns
      -- them to the bottom, redraws indices 7..13, and bottoms (reversed)
      -- index 7 alone -- a singleton, no order to prove yet. Round 2 (taken
      -- 1->2) returns the surviving hand (8..13) to the bottom, redraws
      -- indices 14..19,0, and bottoms count=2 reversed: [15,14] -- humility
      -- then giantGrowth, a genuinely non-identity order. Round 3 (taken=2)
      -- keeps, ending the process.
      mountain <- S.printingOf s registry "Mountain"
      printings <- distinctPrintings s registry
      humility <- S.printingOf s registry "Humility"
      giantGrowth <- S.printingOf s registry "Giant Growth"
      let gs0 = orderedLibraryGame mountain printings
          after = run bottomReversedAnswer gs0
          bottomCards = fmap (\oid -> Game.cardOf oid after) (libBottom 2 S.alice after)
          -- The round-2 bottoming returns indices [15, 14] of distinctPrintings
          -- (humility, then giantGrowth); named directly to avoid a partial `!!`.
          expectedCards = fmap (Just . Printing.card) [humility, giantGrowth]
      Spec.assertEqWith s "library bottom equals the chosen order exactly (humility, then giantGrowth)" bottomCards expectedCards
    Spec.it s "CR 103.5: keeping is terminal -- a kept player is not asked again" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = libraryGame mountain 20
          ((_, _after), asked) = State.runState (Engine.runGame recordAsks gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) []
      Spec.assertEqWith s "alice kept round 1, so was asked exactly once" (length (filter (== S.alice) asked)) 1
      Spec.assertBool s (length (filter (== S.bob) asked) > 1) "bob mulliganed twice then kept, so was asked more than once"
    Spec.it s "CR 103.5: a game with mulligans replays deterministically" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = libraryGame mountain 20
          ((_, recorded), responses) = Replay.record (mulliganUpTo 2) gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
          ((_, replayed), desync) = Replay.replay responses gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
      Spec.assertEqWith s "the transcript answered every prompt" desync Nothing
      Spec.assertEqWith s "alice hand matches" (S.handSize S.alice recorded) (S.handSize S.alice replayed)
      Spec.assertEqWith s "alice library matches" (libSize S.alice recorded) (libSize S.alice replayed)
      Spec.assertEqWith
        s
        "alice library ORDER matches"
        (Game.zoneMembers Zone.Library S.alice recorded)
        (Game.zoneMembers Zone.Library S.alice replayed)
    Spec.it s "CR 727.3/729.3: a short library still flags drewFromEmpty through the mulligan path" $ do
      -- bob has a 5-card library; his 7-card opening draw empties it and flags
      -- the failed draw. Driven by an actual mulligan (mulliganUpTo 1) so the
      -- flag is shown to SURVIVE the shuffle-back-and-redraw path, not merely
      -- the initial draw -- "regardless of any mulligans" (CR 727.3 / 729.3).
      mountain <- S.printingOf s registry "Mountain"
      let g0 = Setup.emptyGame S.bothPlayers
          addMany pid n g = List.foldl' (\h _ -> snd (S.addCreature mountain pid h)) g (replicate n ())
          gs0 = poolToLibrary S.bob (poolToLibrary S.alice (addMany S.bob 5 (addMany S.alice 20 g0)))
          after = run (mulliganUpTo 1) gs0
      Spec.assertBool s (Set.member S.bob (GameState.drewFromEmpty after)) "bob drew from an empty library"
    Spec.it s "CR 103.5c/800.6: one free mulligan at three seats, none at two" $ do
      Spec.assertEqWith s "three seats: the first mulligan is free" (Mulligan.freeMulligans S.threePlayerGame) 1
      Spec.assertEqWith s "two seats: none is" (Mulligan.freeMulligans (Setup.emptyGame S.bothPlayers)) 0
    Spec.it s "CR 800.1: a game that BEGAN with three players keeps its free mulligan after a departure" $
      -- CR 800.1: "A multiplayer game is a game that begins with more than two
      -- players." Begins with, not currently has. GameState.turnOrder is the
      -- permanent seating roster, so this stays 1 with two survivors; an
      -- implementation that counted Game.stillPlaying would answer 0.
      Spec.assertEqWith
        s
        "still one free mulligan with two survivors"
        (Mulligan.freeMulligans (Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame))
        1
    Spec.it s "CR 103.5c: the first mulligan bottoms nothing and the second bottoms one" $ do
      -- Two runs over the same three-seat board. Today both bottom one more
      -- card than they should: takeMulligan bottoms `count` unconditionally,
      -- so the first mulligan leaves a hand of 6 and the second a hand of 5.
      mountain <- S.printingOf s registry "Mountain"
      let once = run3 (mulliganUpTo 1) (libraryGame3 mountain 20)
          twice = run3 (mulliganUpTo 2) (libraryGame3 mountain 20)
      Spec.assertEqWith s "alice's free first mulligan keeps all seven" (S.handSize S.alice once) 7
      Spec.assertEqWith s "bob's too" (S.handSize S.bob once) 7
      Spec.assertEqWith s "carol's too" (S.handSize S.carol once) 7
      Spec.assertEqWith s "and nothing went to the bottom of alice's library" (libSize S.alice once) 13
      Spec.assertEqWith s "the second mulligan bottoms exactly one" (S.handSize S.alice twice) 6
      Spec.assertEqWith s "two-player: unchanged, the first mulligan bottoms one" (S.handSize S.alice (run (mulliganUpTo 1) (libraryGame mountain 20))) 6
    Spec.it s "CR 103.5c second clause: the free mulligan does not count toward the limit either" $ do
      -- Hand + library is 20 throughout, so the redraw is always a full seven
      -- and the process ends exactly when the bottomed count reaches seven.
      -- Two seats: hands run 6,5,4,3,2,1,0 -- seven asks. Three seats: the
      -- first is free, so 7,6,5,4,3,2,1,0 -- eight asks. Today both give
      -- seven, because the free allowance does not exist.
      mountain <- S.printingOf s registry "Mountain"
      let asksIn owners gs0 = snd (State.runState (Engine.runGame recordAlwaysMulligan gs0 (Mulligan.openingHands S.performer owners)) [])
          three = asksIn [S.alice, S.bob, S.carol] (libraryGame3 mountain 20)
          two = asksIn [S.alice, S.bob] (libraryGame mountain 20)
      Spec.assertEqWith s "three seats: alice may take eight mulligans" (length (filter (== S.alice) three)) 8
      Spec.assertEqWith s "two seats: seven, unchanged" (length (filter (== S.alice) two)) 7
    Spec.it s "CR 103.5c: the declaration says what the mulligan COSTS, not just how many were taken" $ do
      -- The two seat counts agree on `taken` at every step and disagree on the
      -- cost at every step: three seats get CR 103.5c's free first mulligan,
      -- so alice's opening declaration offers (taken 0, bottom 0) where a
      -- two-player game offers (taken 0, bottom 1). An answerer holding only
      -- the raw count cannot tell those apart, which is what #176 was about.
      mountain <- S.printingOf s registry "Mountain"
      let offersIn owners gs0 = reverse (snd (State.runState (Engine.runGame recordOffers gs0 (Mulligan.openingHands S.performer owners)) []))
      Spec.assertEqWith
        s
        "three seats: the first mulligan is free, so it bottoms nothing"
        (offersIn [S.alice, S.bob, S.carol] (libraryGame3 mountain 20))
        [(0, 0), (1, 1), (2, 2)]
      Spec.assertEqWith
        s
        "two seats: every mulligan costs, from the first"
        (offersIn [S.alice, S.bob] (libraryGame mountain 20))
        [(0, 1), (1, 2), (2, 3)]
    Spec.it s "CR 103.6: the window opens only once the mulligan process is complete" $ do
      leyline <- S.printingOf s registry "Leyline of the Void"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = leylineGame leyline mountain 20
          (_, tags) = State.runState (Engine.runGame recordOpeningOrder gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) []
      Spec.assertEqWith
        s
        "both declarations, then alice's opening-hand window"
        (reverse tags)
        [(Text.pack "declare", S.alice), (Text.pack "declare", S.bob), (Text.pack "opening", S.alice)]
    Spec.it s "CR 103.6a: taking the action puts the card onto the battlefield" $ do
      leyline <- S.printingOf s registry "Leyline of the Void"
      mountain <- S.printingOf s registry "Mountain"
      let after = run useOpeningAction (leylineGame leyline mountain 20)
      Spec.assertEqWith s "alice's hand is one smaller" (S.handSize S.alice after) 6
      Spec.assertEqWith s "and the Leyline is on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 1
    Spec.it s "CR 103.6: declining leaves the card in hand" $ do
      leyline <- S.printingOf s registry "Leyline of the Void"
      mountain <- S.printingOf s registry "Mountain"
      let after = run declineOpeningAction (leylineGame leyline mountain 20)
      Spec.assertEqWith s "a full opening hand" (S.handSize S.alice after) 7
      Spec.assertEqWith s "and nothing on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 0
    Spec.it s "CR 103.6: the starting player's window comes before the other player's" $ do
      leyline <- S.printingOf s registry "Leyline of the Void"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = leylineBothGame leyline mountain 20
          (_, tags) = State.runState (Engine.runGame recordOpeningOrder gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) []
          openings = reverse (fmap snd (filter (\(tag, _) -> tag == Text.pack "opening") tags))
      Spec.assertEqWith s "alice first, then bob" openings [S.alice, S.bob]
    Spec.it s "CR 103.6: 'any such actions in any order' -- the window re-offers until declined" $ do
      leyline <- S.printingOf s registry "Leyline of the Void"
      mountain <- S.printingOf s registry "Mountain"
      let after = run useOpeningAction (twoLeylineGame leyline mountain 20)
      Spec.assertEqWith s "both Leylines are on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 2
      Spec.assertEqWith s "and the hand is two smaller" (S.handSize S.alice after) 5
    Spec.it s "CR 103.6: no granting card means no prompt" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = libraryGame mountain 20
          (_, tags) = State.runState (Engine.runGame recordOpeningOrder gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])) []
      Spec.assertEqWith s "no opening-hand prompt at all" (filter (\(tag, _) -> tag == Text.pack "opening") tags) []
    Spec.it s "CR 103.6: a game with an opening-hand action replays deterministically" $ do
      leyline <- S.printingOf s registry "Leyline of the Void"
      mountain <- S.printingOf s registry "Mountain"
      let gs0 = leylineGame leyline mountain 20
          ((_, recorded), responses) = Replay.record useOpeningAction gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
          ((_, replayed), desync) = Replay.replay responses gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
      Spec.assertEqWith s "the transcript answered every prompt" desync Nothing
      Spec.assertEqWith s "battlefield matches" (Game.zoneMembers Zone.Battlefield S.alice recorded) (Game.zoneMembers Zone.Battlefield S.alice replayed)
      Spec.assertEqWith s "hand matches" (S.handSize S.alice recorded) (S.handSize S.alice replayed)
    Spec.it s "CR 103.5c: a mulligan that bottoms nothing asks nothing" $ do
      -- Where the rules leave nothing to ask, don't prompt: choosing zero of
      -- seven cards has exactly one legal answer. Today the free mulligan is
      -- not free, so each of the three players is asked to bottom one card and
      -- three Response.PutOnBottom entries are recorded.
      mountain <- S.printingOf s registry "Mountain"
      let bottomsIn owners gs0 =
            let (_, log_) = Replay.record (mulliganUpTo 1) gs0 (Mulligan.openingHands S.performer owners)
                isBottom r = case r of
                  Response.PutOnBottom _ -> True
                  _ -> False
             in length (filter isBottom log_)
      Spec.assertEqWith s "three seats: no bottom choice is asked for a free mulligan" (bottomsIn [S.alice, S.bob, S.carol] (libraryGame3 mountain 20)) 0
      Spec.assertEqWith s "two seats: both players are still asked" (bottomsIn [S.alice, S.bob] (libraryGame mountain 20)) 2
    trustedAnswerSpec s registry
