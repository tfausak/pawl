{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.OutsideTheGame (CR 400.11's pool and the road into the
-- game), the two fields that carry it -- Pawl.Types.Deck's sideboard (CR 103.2a)
-- and Pawl.Types.Player's outsideTheGame -- Pawl.Engine.Setup.createDeck's copy
-- between them, and Pawl.Engine.Resolve's two arms:
-- Effect.RevealFromOutsideTheGame and Effect.ExileThisSpell (CR 608.2n).
--
-- Gameplay-level throughout but for the setup case at the end: every other case
-- casts the printed Burning Wish ({1}{R} sorcery, "You may reveal a sorcery card
-- you own from outside the game and put it into your hand. Exile Burning Wish.")
-- and resolves it through the stack, so what is asserted is the whole path from
-- card JSON to the card in hand.
module Pawl.OutsideTheGameSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- alice, two Mountains untapped, Burning Wish in hand, and the given multiset of
-- printings outside the game.
--
-- The pool is written onto the player directly, Pawl.DungeonSpec's posture for
-- Player.dungeons: what CR 103.2a's road from a Deck does is the last case below,
-- and every case in between is about what a card can do with a pool that exists.
-- The printings are interned first, since Player.outsideTheGame names them by id.
wishBoard :: Printing.Printing -> Printing.Printing -> [(Printing.Printing, Natural.Natural)] -> (GameState.GameState, ObjectId.ObjectId)
wishBoard mountain wish sideboard =
  -- FOUR Mountains rather than two: the cases that cast a second wish off the
  -- board the first one left need mana the first cast did not tap.
  let (board, wishId) = S.handOne wish (S.landsInPlay mountain 4)
      -- Interned in LIST ORDER, so the ids ascend with the list and the case
      -- below can name what Prompt.ChooseFromOutsideTheGame offers last.
      intern (printing, n) (acc, gs) = let (printingId, gs') = Game.intern printing gs in (acc <> [(printingId, n)], gs')
      (entries, interned) = List.foldl' (flip intern) ([], board) sideboard
      stock p = p {Player.outsideTheGame = Map.fromList entries}
   in (interned {GameState.players = Map.adjust stock S.alice (GameState.players interned)}, wishId)

-- Burning Wish's printed "You may", taken. S.identityAnswer declines every
-- optional clause (CR 608.2d), so a case that wants the wish to do anything says
-- so here; the two cases that assert nothing happens still go through it, which
-- is what keeps "the filter admitted nothing" apart from "she declined".
exercising :: Prompt.Prompt r -> r
exercising p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- Cast the wish and resolve it, under the given answer.
resolveWish :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
resolveWish answer wishId gs =
  let cast = snd (Engine.runGamePure answer gs (S.cast S.alice wishId))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- The printings of the cards in a zone, so a case can say WHICH card arrived
-- rather than only how many did.
printingsIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Printing.Printing]
printingsIn zone pid gs = Maybe.mapMaybe (\oid -> Game.printingOfObject oid gs) (Game.zoneMembers zone pid gs)

-- What is left in a player's pool, by printing.
poolOf :: PlayerId.PlayerId -> GameState.GameState -> Map.Map PrintingId.PrintingId Natural.Natural
poolOf pid gs = maybe Map.empty Player.outsideTheGame (Map.lookup pid (GameState.players gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.OutsideTheGame" $ do
  -- CR 400.11c and CR 701.20a: the gate. A card outside the game reaches the hand
  -- and nothing else does.
  Spec.it s "CR 400.11c Burning Wish puts the sorcery it revealed from outside the game into her hand" $ do
    mountain <- S.printingOf s registry "Mountain"
    wish <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (gs, wishId) = wishBoard mountain wish [(signInBlood, 1)]
        resolved = resolveWish exercising wishId gs
    Spec.assertEqWith s "her hand starts with the wish alone" (printingsIn Zone.Hand S.alice gs) [wish]
    Spec.assertEqWith s "and holds the sorcery from outside the game afterwards" (printingsIn Zone.Hand S.alice resolved) [signInBlood]
    -- CR 608.2n: the second sentence. The wish is in EXILE and not in the
    -- graveyard, which is the whole of what Effect.ExileThisSpell does.
    Spec.assertEqWith s "the wish itself was exiled" (printingsIn Zone.Exile S.alice resolved) [wish]
    Spec.assertEqWith s "and no card reached her graveyard" (printingsIn Zone.Graveyard S.alice resolved) []
  -- CR 400.11b: "those cards remain in the game until the game ends". The copy
  -- brought in is gone from the pool, so a second wish cannot find it again.
  Spec.it s "CR 400.11b the pool is spent: a second Burning Wish finds nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    wish <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (gs, firstWish) = wishBoard mountain wish [(signInBlood, 1)]
        once = resolveWish exercising firstWish gs
        -- A second wish, cast off the board the first one left: same lands, same
        -- pool, and the pool is what has changed. ADDED to the hand rather than
        -- S.handOne's replacement of it, so the card the first wish put there is
        -- still in hand to be counted.
        (secondWish, again) = S.addHandCard wish S.alice once
        twice = resolveWish exercising secondWish again
    -- THE BEHAVIOUR first, and the pool's own state after it: an assertion about
    -- the pool ordered ahead would absorb a mutation the hand is what observes.
    Spec.assertEqWith s "the second wish brings in nothing: the one card in hand is the first wish's" (printingsIn Zone.Hand S.alice twice) [signInBlood]
    Spec.assertEqWith s "and the first wish had emptied the pool" (Map.size (poolOf S.alice once)) 0
    -- The second wish still resolved and still exiled itself, which is what tells
    -- "found nothing" apart from "did not resolve".
    Spec.assertEqWith s "and exiled itself all the same" (length (printingsIn Zone.Exile S.alice twice)) 2
  -- CR 100.4a's sideboard is a multiset, which is the one place this pool parts
  -- from Player.dungeons' set: two copies are two cards.
  Spec.it s "CR 100.4a two copies of one printing are two cards: the second wish finds the second copy" $ do
    mountain <- S.printingOf s registry "Mountain"
    wish <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (gs, firstWish) = wishBoard mountain wish [(signInBlood, 2)]
        once = resolveWish exercising firstWish gs
        (secondWish, again) = S.addHandCard wish S.alice once
        twice = resolveWish exercising secondWish again
    Spec.assertEqWith s "the second wish brings in the second copy" (List.sort (printingsIn Zone.Hand S.alice twice)) (List.sort [signInBlood, signInBlood])
    Spec.assertEqWith s "and one copy was left after the first wish" (Map.elems (poolOf S.alice once)) [1]
  -- CR 400.11c: what the effect ALLOWS is the card's own filter, so a pool that
  -- holds nothing the filter admits yields nothing.
  Spec.it s "CR 400.11c a pool holding no sorcery yields nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    wish <- S.printingOf s registry "Burning Wish"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    let (gs, wishId) = wishBoard mountain wish [(dragon, 1)]
        resolved = resolveWish exercising wishId gs
    Spec.assertEqWith s "the creature card stays outside the game" (Map.elems (poolOf S.alice resolved)) [1]
    Spec.assertEqWith s "and her hand is empty of it" (printingsIn Zone.Hand S.alice resolved) []
  -- CR 400.11c again, with a CHOICE: two eligible cards, so the player is asked
  -- which one, and the answer is what arrives.
  Spec.it s "CR 400.11c two eligible cards are a choice, and the answer decides which arrives" $ do
    mountain <- S.printingOf s registry "Mountain"
    wish <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    psychicMiasma <- S.printingOf s registry "Psychic Miasma"
    let (gs, wishId) = wishBoard mountain wish [(signInBlood, 1), (psychicMiasma, 1)]
        -- Answering with the LAST offered rather than the first, so the assertion
        -- fails if the answer is ignored and the head is taken.
        answeringLast :: Prompt.Prompt r -> r
        answeringLast p = case p of
          Prompt.ChooseFromOutsideTheGame _ _ candidates -> List.last (foldr (:) [] candidates)
          _ -> exercising p
        resolved = resolveWish answeringLast wishId gs
    -- Psychic Miasma is interned second, so it is what the prompt offers last and
    -- what the answer above names.
    Spec.assertEqWith s "the card she named is the one in her hand" (printingsIn Zone.Hand S.alice resolved) [psychicMiasma]
    Spec.assertEqWith s "and the other card is still outside the game" (Map.size (poolOf S.alice resolved)) 1
  -- CR 103.2a: the sideboard is set aside before the game, which is the one path
  -- every case above skips by writing Player.outsideTheGame directly.
  Spec.it s "CR 103.2a a deck's sideboard is recorded on its player and minted into no zone" $ do
    signInBlood <- S.printingOf s registry "Sign in Blood"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    let deck =
          Deck.MkDeck
            { Deck.cards = Map.empty,
              Deck.commander = Nothing,
              Deck.dungeons = Set.empty,
              Deck.sideboard = Map.fromList [(signInBlood, 2), (dragon, 1)]
            }
        after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice deck)
        heldBy pid = traverse (\(i, n) -> fmap (\p -> (p, n)) (Game.printingOf i after)) (Map.toList (poolOf pid after))
    Spec.assertEqWith s "alice set aside three cards, counted per printing" (fmap List.sort (heldBy S.alice)) (Just (List.sort [(signInBlood, 2), (dragon, 1)]))
    Spec.assertEqWith s "bob set aside none" (poolOf S.bob after) Map.empty
    -- CR 400.11a / 400.11: the sideboard is outside the game, and outside the game
    -- is not a zone -- so setup mints no object for any of it.
    Spec.assertEqWith s "and no object was minted for any of them" (Map.size (GameState.objects after)) 0
