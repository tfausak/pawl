{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.OutsideTheGame (CR 400.11's pool and the road into the
-- game), the two fields that carry it -- Pawl.Types.Deck's sideboard (CR 103.2a)
-- and Pawl.Types.Player's outsideTheGame -- Pawl.Engine.Setup.createDeck's copy
-- between them, and Pawl.Engine.Resolve's two arms:
-- Effect.RevealFromOutsideTheGame and Effect.ExileThisSpell (CR 608.2n). Also
-- CR 729.4's second place a card can be outside the game: the main game as a
-- subgame sees it, which is Pawl.Engine.Setup's subgameStateFrom and
-- applyCrossings with Pawl.Engine.Engine.playSubgame between them.
--
-- Gameplay-level but for three cases: the CR 103.2a setup case and the two CR
-- 729.4 cases before it call the unit under test directly. Every other case
-- casts a printed wish and resolves it through the stack, so what is asserted is
-- the whole path from card JSON to the card in hand -- Burning Wish ({1}{R}
-- sorcery, "You may reveal a sorcery card you own from outside the game and put
-- it into your hand. Exile Burning Wish.") for the pool, and Living Wish inside a
-- Shahrazad subgame for CR 729.4's main game, in the last case of all.
module Pawl.OutsideTheGameSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.OutsideTheGame as OutsideTheGame
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.OutsideCard as OutsideCard
import qualified Pawl.Types.Phase as Phase
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

-- n fresh objects of one printing at the back of a player's library: what CR
-- 729.2 moves into the subgame, and the one main-game zone that does move. Mints
-- through S.addHandCard and relocates, the way Pawl.GameSpec's own subgame
-- fixtures stock a library.
stockLibrary :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
stockLibrary printing n pid gs0 =
  let one gs =
        let (oid, gs1) = S.addHandCard printing pid gs
         in gs1
              { GameState.objects = Map.adjust (\o -> o {Object.zone = Zone.Library}) oid (GameState.objects gs1),
                GameState.hand = Map.adjust (Seq.filter (/= oid)) pid (GameState.hand gs1),
                GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs1)
              }
   in List.foldl' (\gs _ -> one gs) gs0 (replicate n ())

-- CR 122.1a: the +1/+1 counters on one object.
plusOneCounters :: ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
plusOneCounters oid gs =
  maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Map.lookup oid (GameState.objects gs))

-- Where in the log the first event a predicate admits sits, so a case can compare
-- WHEN two things were recorded and not merely that both were.
eventIndex :: (GameEvent.GameEvent -> Bool) -> GameState.GameState -> Maybe Int
eventIndex predicate gs = List.findIndex (predicate . LoggedEvent.event) (Foldable.toList (GameState.events gs))

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
  -- A sideboard is a multiset -- CR 100.4a caps it at fifteen CARDS and applies CR
  -- 100.2a's four-card limit across deck and sideboard together -- which is the one
  -- place this pool parts from Player.dungeons' set: two copies are two cards.
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
  -- CR 729.4: a subgame's own view of the main game. Alice's creature sits
  -- on the PARENT's battlefield; Setup.subgameStateFrom moves it into
  -- GameState.outsideObjects (CR 729.4, "all objects in the main game ... are
  -- considered outside the subgame"), and a wish cast IN the subgame can reach
  -- it just as it reaches the sideboard pool. Called at the unit level --
  -- `OutsideTheGame.reveal` directly -- rather than through a printed wish,
  -- since Living Wish's own casting is not this unit's concern.
  Spec.it s "CR 729.4/729.4a a wish cast in a subgame reaches a main-game creature" $ do
    bear <- S.printingOf s registry "Prodigal Sorcerer"
    let (bearId, parent) = S.addCreature bear S.alice (Setup.emptyGame S.bothPlayers)
        sub = Setup.subgameStateFrom S.alice parent
        predicate = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        after = snd (Engine.runGamePure S.identityAnswer sub (OutsideTheGame.reveal predicate bearId S.alice))
    Spec.assertEqWith s "CR 729.4/400.11c: the main-game creature arrives in her subgame hand" (printingsIn Zone.Hand S.alice after) [bear]
    Spec.assertEqWith s "CR 729.4a: the crossing is recorded for the outer frame to apply" (Foldable.toList (GameState.broughtIn after)) [bearId]
    Spec.assertEqWith s "and it is no longer offered" (Map.member bearId (GameState.outsideObjects after)) False
  -- CR 108.3b: the reach outside the game is scoped to the acting player's OWN
  -- cards. Bob's creature sits in the same outsideObjects map; alice's wish must
  -- not see it.
  Spec.it s "CR 108.3b a main-game creature owned by another player is not offered" $ do
    bear <- S.printingOf s registry "Prodigal Sorcerer"
    let (bobsBearId, parent) = S.addCreature bear S.bob (Setup.emptyGame S.bothPlayers)
        sub = Setup.subgameStateFrom S.alice parent
        predicate = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        after = snd (Engine.runGamePure S.identityAnswer sub (OutsideTheGame.reveal predicate bobsBearId S.alice))
    Spec.assertEqWith s "alice's hand stays empty: bob's creature is not hers to reach" (printingsIn Zone.Hand S.alice after) []
    Spec.assertEqWith s "and bob's card is untouched" (Map.member bobsBearId (GameState.outsideObjects after)) True
  -- CR 729.4 / 729.4a / 729.5, the whole road at gameplay level and the case #152
  -- is about. alice casts Shahrazad in the main game; inside the subgame she casts
  -- Living Wish ({1}{G} sorcery, "You may reveal a creature or land card you own
  -- from outside the game and put it into your hand. Exile Living Wish.") twice
  -- and takes one of her main-game Goblin Pikers each time. Each Piker leaves the
  -- MAIN GAME as it crosses (CR 729.4a), and bob's Super Shredder ("whenever
  -- another permanent leaves the battlefield, put a +1/+1 counter on Super
  -- Shredder") is the main-game ability that watches it go.
  --
  -- TWO crossings and not one. A single crossing cannot tell a batch read against
  -- one frozen board from a batch read against the running one (CR 608.2h), which
  -- is the distinction Setup.applyCrossings' fold draws; it is also what makes
  -- the wish be cast twice, so a pool spent by the first cast would show up here.
  --
  -- The sizing, which is what the fixture is built around: alice starts the
  -- subgame (CR 729.2), so she skips her first draw (CR 103.8a) and her turns are
  -- 1, 3 and 5. One land per turn (CR 305.2) puts her second land down on turn 3,
  -- which is the first turn the {1}{G} wish is castable, and the second wish waits
  -- for turn 5. Both libraries hold nine cards: seven go to an opening hand, so
  -- bob draws his last two on turns 2 and 4 and draws from an empty library on
  -- turn 6, which ends the subgame under CR 704.5b -- one turn after alice's
  -- second cast. Nine is also over CR 729.3's seven, so neither player decks
  -- during setup. alice's nine are the two wishes and seven Forests; whichever end
  -- the shuffle answer leaves them at she holds both wishes by turn 5, since seven
  -- opening cards plus her turn-3 and turn-5 draws are her whole library.
  --
  -- The Plains she cast Shahrazad with are LANDS she owns on the main-game
  -- battlefield, so CR 729.4 offers them to the wish alongside the Pikers; the
  -- answerer names a Piker rather than taking the head of the offer.
  Spec.it s "CR 729.4/729.4a/729.5 gameplay: Living Wish takes two main-game creatures out of a Shahrazad subgame, and the triggers wait for the main game" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    livingWish <- S.printingOf s registry "Living Wish"
    piker <- S.printingOf s registry "Goblin Piker"
    shredder <- S.printingOf s registry "Super Shredder"
    let g0 = Setup.emptyGame S.bothPlayers
        (firstPiker, g1) = S.addCreature piker S.alice g0
        (secondPiker, g2) = S.addCreature piker S.alice g1
        (shredderId, g3) = S.addCreature shredder S.bob g2
        g4 = S.landsFor plains S.alice 2 g3
        g5 = stockLibrary mountain 9 S.bob (stockLibrary forest 7 S.alice (stockLibrary livingWish 2 S.alice g4))
        (_shahrazadId, g6) = S.addHandCard shahrazad S.alice g5
        before =
          g6
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        isPiker candidate = case candidate of
          OutsideCard.InAnotherGame oid -> oid == firstPiker || oid == secondPiker
          OutsideCard.InPool _ -> False
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          -- Living Wish's printed "may" (CR 608.2d), taken both times.
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          Prompt.ChooseFromOutsideTheGame _ _ offered ->
            Maybe.fromMaybe (NonEmpty.head offered) (List.find isPiker (NonEmpty.toList offered))
          -- CR 729.2's roll, answered so the turn count above is the one played.
          Prompt.RandomFirstPlayer _ -> S.alice
          _ -> S.castAnswer p
        after = snd (Engine.runGamePure answer before Engine.priorityLoop)
        isLifeLoss e = case e of
          GameEvent.LifeLost {} -> True
          _ -> False
        isShredderCounter e = case e of
          GameEvent.CountersPut change -> CounterChange.object change == shredderId
          _ -> False
        -- CR 729.5's last sentence, read off the one log both are recorded in:
        -- Shahrazad's own LoseLife clause runs while it finishes resolving, and
        -- the trigger's counter cannot be placed until it has gone on the stack
        -- after that.
        counterFollowsTheLifeLoss = case (eventIndex isLifeLoss after, eventIndex isShredderCounter after) of
          (Just loss, Just counter) -> Just (counter > loss)
          _ -> Nothing
    -- The fixture's own preconditions, which the runner under test cannot redden.
    Spec.assertEqWith s "both of alice's creatures start on the main-game battlefield" (Set.member firstPiker (GameState.battlefield before), Set.member secondPiker (GameState.battlefield before)) (True, True)
    Spec.assertEqWith s "and bob's Super Shredder starts with no counters" (plusOneCounters shredderId before) 0
    Spec.assertEqWith s "CR 729.4: both main-game creatures left the main game for the subgame" (Set.member firstPiker (GameState.battlefield after), Set.member secondPiker (GameState.battlefield after)) (False, False)
    Spec.assertEqWith s "CR 729.4a: Super Shredder's leaves-the-battlefield trigger resolved once per crossing" (plusOneCounters shredderId after) 2
    Spec.assertEqWith s "CR 729.5: the triggers went on the stack only after Shahrazad finished resolving" counterFollowsTheLifeLoss (Just True)
    Spec.assertEqWith s "CR 729.5: the cards the wishes took come back to her main-game library" (length (filter (== piker) (printingsIn Zone.Library S.alice after))) 2
    Spec.assertEqWith s "CR 729.5: and the subgame's own cards -- the wishes it exiled included -- came back with them" (length (printingsIn Zone.Library S.alice after)) 11
    -- CR 729.1b: alice won the subgame (bob decked), so she is the one player the
    -- follow-on LoseLife excludes. 20 halves to 10.
    Spec.assertEqWith s "CR 729.1b: only bob paid, which is how the subgame ended" (S.lifeOf S.alice after, S.lifeOf S.bob after) (Just 20, Just 10)
    Spec.assertEqWith s "CR 729.1a: the subgame did not decide the main game" (GameState.result after) Nothing
