{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Event's CR 400.11 group -- `eligible`, `bringInto`,
-- `bringIn` and `bringInFrom`, the pool and the roads into the game -- the two
-- fields that carry it -- Pawl.Types.Deck's sideboard (CR 103.2a)
-- and Pawl.Types.Player's outsideTheGame -- Pawl.Engine.Setup.createDeck's copy
-- between them, and Pawl.Engine.Resolve's two arms:
-- Effect.FromOutsideTheGame and Effect.ExileThisSpell (CR 608.2n). Also
-- CR 729.4's second place a card can be outside the game: the main game as a
-- subgame sees it, which is Pawl.Engine.Setup's subgameStateFrom and
-- applyCrossings with Pawl.Engine.Engine.playSubgame between them.
--
-- Gameplay-level but for the CR 103.2a setup case and the CR 729.4 group that
-- follows it, which reach Event.eligible and Event.bringInto directly on a board
-- Setup.subgameStateFrom built -- the main game those cases
-- read is played out through the stack, but the wish inside the subgame is
-- called rather than cast. Every other case casts a printed wish and resolves it
-- through the stack, so what is asserted is the whole path from card JSON to the
-- card in the zone the wish names -- Burning Wish ({1}{R} sorcery, "You may
-- reveal a sorcery card you own from outside the game and put it into your hand.
-- Exile Burning Wish.")
-- for the pool, with Cunning Wish for the cycle's instant speed, Death Wish
-- for a card that prints no reveal, The Raven's Warning for a destination that
-- is not the hand, Research for a count other than one, and, in four cases, a Shahrazad
-- subgame for CR 729.4's main game -- Living Wish reaching two main-game
-- creatures, then Death Wish reaching a main-game Titania's Song (CR 604.2's
-- handover), then Burning Wish reaching the resolving Shahrazad itself (CR
-- 729.5), then Burning Wish reaching a main-game graveyard (CR 729.4a).
--
-- The last group takes the OTHER road in: Ring of Ma'rûf's CR 614.6 draw
-- replacement, whose rewrite reaches the same `bringInto` the resolution arm
-- does.
module Pawl.OutsideTheGameSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LeftTheGame as LeftTheGame
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.MergeComponent as MergeComponent
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.OutsideCard as OutsideCard
import qualified Pawl.Types.OutsideDestination as OutsideDestination
import qualified Pawl.Types.OutsideObject as OutsideObject
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.SearchPlace as SearchPlace
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- alice, four untapped lands of one printing, one wish in hand, and the given
-- multiset of printings outside the game.
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
      intern (printing, n) (acc, gs) = let (printingId, gs2) = Game.intern printing gs in (acc <> [(printingId, n)], gs2)
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

-- n fresh objects of one printing in a player's library: what CR 729.2 moves into
-- the subgame, and the one main-game zone that does move. Which END of the
-- library they land at is not a dial the case below turns -- CR 729.2 shuffles
-- each library as the subgame starts, and the sizing argument there holds for
-- either order.
stockLibrary :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
stockLibrary printing n pid gs0 =
  List.foldl' (\gs _ -> snd (S.addLibraryCard printing pid gs)) gs0 (replicate n ())

-- CR 122.1a: the +1/+1 counters on one object.
plusOneCounters :: ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
plusOneCounters oid gs =
  maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)

-- Where in the log the first event a predicate admits sits, so a case can compare
-- WHEN two things were recorded and not merely that both were.
eventIndex :: (GameEvent.GameEvent -> Bool) -> GameState.GameState -> Maybe Int
eventIndex predicate gs = List.findIndex (predicate . LoggedEvent.event) (Foldable.toList (GameState.events gs))

-- alice's Hanweir Battlements and Hanweir Garrison put onto `base` and melded
-- into Hanweir, the Writhing Township (CR 701.42a), answering the township's id
-- -- Nothing if the meld did not happen. Duplicated from Pawl.SetupSpec's
-- `meldedBoard` rather than hoisted into Pawl.Support, which rebuilds every
-- spec in the tree; the five Mountains pay the melding ability.
meldedBoard :: GameState.GameState -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (Maybe ObjectId.ObjectId, GameState.GameState)
meldedBoard base battlements garrison mountain =
  let (battlementsId, g1) = S.addPermanent battlements S.alice base
      (_, g2) = S.addPermanent garrison S.alice g1
      board =
        (S.landsFor mountain S.alice 5 g2)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      townshipName = CardName.MkCardName (Text.pack "Hanweir, the Writhing Township")
   in case Projection.abilitiesOf battlementsId board of
        [_, _, melding] ->
          let after = S.runPure (sparing battlementsId) board (do Activate.activateAbility S.alice battlementsId melding; Stack.resolveTop)
              township = filter (\oid -> fmap S.nameOf (Game.cardOf oid after) == Just townshipName) (Game.zoneMembers Zone.Battlefield S.alice after)
           in (Maybe.listToMaybe township, after)
        _ -> (Nothing, board)

-- Pays the melding ability without tapping Hanweir Battlements for mana, whose
-- {T} the ability still needs (CR 107.5). Duplicated from Pawl.SetupSpec.
sparing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
sparing oid p = case p of
  Prompt.ChooseManaSource _ _ candidates -> List.find (/= oid) (NonEmpty.toList candidates)
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Event (CR 400.11)" $ do
  -- CR 400.11c and CR 701.20a: the gate. The card the wish's filter admits
  -- reaches the hand its sentence names, and nothing else comes in with it.
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
          Prompt.ChooseFromOutsideTheGame _ _ candidates _ _ -> [List.last (foldr (:) [] candidates)]
          _ -> exercising p
        resolved = resolveWish answeringLast wishId gs
    -- Psychic Miasma is interned second, so it is what the prompt offers last and
    -- what the answer above names.
    Spec.assertEqWith s "the card she named is the one in her hand" (printingsIn Zone.Hand S.alice resolved) [psychicMiasma]
    Spec.assertEqWith s "and the other card is still outside the game" (Map.size (poolOf S.alice resolved)) 1
  -- CR 304.1 / 307.1: Cunning Wish ({2}{U} instant, "You may reveal an instant
  -- card you own from outside the game and put it into your hand. Exile Cunning
  -- Wish.") is the cycle's instant, so it is the one wish that reaches outside
  -- the game with another spell already on the stack.
  --
  -- A PAIR of castability readings off ONE board, differing only in which wish is
  -- asked about: both are in the same hand, both are payable from the same eight
  -- untapped lands, and the spell on the stack is the same spell. The type line is
  -- the only difference left, which is what CR 307.1 turns on.
  Spec.it s "CR 307.1 with a spell on the stack the cycle's instant is castable and its sorcery is not" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    cunning <- S.printingOf s registry "Cunning Wish"
    burning <- S.printingOf s registry "Burning Wish"
    cancel <- S.printingOf s registry "Cancel"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (board, occupantId) = wishBoard mountain burning [(cancel, 1), (signInBlood, 1)]
        withIslands = S.landsFor island S.alice 4 board
        (cunningId, withCunning) = S.addHandCard cunning S.alice withIslands
        (burningId, ready) = S.addHandCard burning S.alice withCunning
        -- The occupant is a Burning Wish cast at sorcery speed onto an EMPTY
        -- stack, which is what makes the stack non-empty for the two readings
        -- below without introducing a card the pool does not already hold.
        held = snd (Engine.runGamePure exercising ready (S.cast S.alice occupantId))
    Spec.assertEqWith s "the stack holds the spell cast first" (length (Game.zoneMembers Zone.Stack S.alice held)) 1
    Spec.assertEqWith s "CR 307.1: the sorcery in hand cannot be cast into that window" (S.castable S.alice burningId held) False
    -- The same card off the same board with an EMPTY stack, so the reading above
    -- cannot be passing for want of mana or for anything else about the card.
    Spec.assertEqWith s "and it was castable on that board before the stack held anything" (S.castable S.alice burningId ready) True
    Spec.assertEqWith s "CR 304.1: the instant can" (S.castable S.alice cunningId held) True
    -- Driven, not merely gated: the wish is actually cast in that window, and what
    -- arrives is the INSTANT its filter names rather than the sorcery sitting
    -- beside it in the pool -- which the wish underneath then takes in its turn.
    let stacked = snd (Engine.runGamePure exercising held (S.cast S.alice cunningId))
        once = snd (Engine.runGamePure exercising stacked Stack.resolveTop)
        after = snd (Engine.runGamePure exercising once Stack.resolveTop)
    Spec.assertEqWith s "both wishes are on the stack at once" (length (Game.zoneMembers Zone.Stack S.alice stacked)) 2
    Spec.assertEqWith s "CR 400.11c: Cunning Wish resolves first and takes the instant" (List.sort (printingsIn Zone.Hand S.alice once)) (List.sort [burning, cancel])
    Spec.assertEqWith s "and the sorcery wish underneath then takes the sorcery" (List.sort (printingsIn Zone.Hand S.alice after)) (List.sort [burning, cancel, signInBlood])
    Spec.assertEqWith s "CR 608.2n: both cast wishes exiled themselves" (List.sort (printingsIn Zone.Exile S.alice after)) (List.sort [burning, cunning])
  -- CR 701.20a is a keyword action of its own, and a card that does not print it
  -- shows nobody anything. Death Wish ({1}{B}{B} sorcery, "You may put a card you
  -- own from outside the game into your hand. You lose half your life, rounded up.
  -- Exile Death Wish.") is the producer: no reveal, and no stated quality either,
  -- so its filter is the empty And that admits everything.
  --
  -- A PAIR of boards differing in ONE thing, the wish. Same lands, same pool, same
  -- card arriving in hand -- so the Revealed entry is the only reading that moves.
  Spec.it s "CR 701.20a Death Wish brings the card in without revealing it, where Burning Wish reveals" $ do
    mountain <- S.printingOf s registry "Mountain"
    swamp <- S.printingOf s registry "Swamp"
    death <- S.printingOf s registry "Death Wish"
    burning <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let boardFor wish =
          let (base, wishId) = wishBoard mountain wish [(signInBlood, 1)]
           in (S.landsFor swamp S.alice 4 base, wishId)
        (deathBoard, deathId) = boardFor death
        (burningBoard, burningId) = boardFor burning
        afterDeath = resolveWish exercising deathId deathBoard
        afterBurning = resolveWish exercising burningId burningBoard
        isRevealed e = case e of
          GameEvent.Revealed _ -> True
          _ -> False
    -- THE BEHAVIOUR first. The card is in hand either way, so an assertion that
    -- it arrived cannot absorb the reveal reading below.
    Spec.assertEqWith s "CR 400.11c: Death Wish's empty filter admits the sorcery, which reaches her hand" (printingsIn Zone.Hand S.alice afterDeath) [signInBlood]
    Spec.assertEqWith s "CR 701.20a: and nothing was revealed" (eventIndex isRevealed afterDeath) Nothing
    Spec.assertEqWith s "where Burning Wish brings in the same card" (printingsIn Zone.Hand S.alice afterBurning) [signInBlood]
    Spec.assertEqWith s "and does reveal it" (Maybe.isJust (eventIndex isRevealed afterBurning)) True
    -- The rest of Death Wish's sentence, which is what tells a resolution apart
    -- from a fizzle: CR 119.3 and CR 107.1a, half of twenty rounded up.
    Spec.assertEqWith s "she lost half her life, rounded up" (S.lifeOf S.alice afterDeath) (Just 10)
    Spec.assertEqWith s "and Burning Wish cost her none" (S.lifeOf S.alice afterBurning) (Just 20)
  -- CR 103.2a: the sideboard is set aside before the game, which is the one path
  -- every case above skips by writing Player.outsideTheGame directly.
  Spec.it s "CR 103.2a a deck's sideboard is recorded on its player and minted into no zone" $ do
    signInBlood <- S.printingOf s registry "Sign in Blood"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    let deck =
          Deck.MkDeck
            { Deck.cards = Map.empty,
              Deck.commander = Set.empty,
              Deck.vanguard = Nothing,
              Deck.dungeons = Set.empty,
              Deck.sideboard = Map.fromList [(signInBlood, 2), (dragon, 1)],
              Deck.conspiracies = Map.empty,
              Deck.attractions = Map.empty,
              Deck.planes = Set.empty,
              Deck.schemes = Map.empty
            }
        after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice deck)
        heldBy pid = traverse (\(i, n) -> fmap (\p -> (p, n)) (Game.printingOf i after)) (Map.toList (poolOf pid after))
    Spec.assertEqWith s "alice set aside three cards, counted per printing" (fmap List.sort (heldBy S.alice)) (Just (List.sort [(signInBlood, 2), (dragon, 1)]))
    Spec.assertEqWith s "bob set aside none" (poolOf S.bob after) Map.empty
    -- CR 400.11a / 400.11: the sideboard is outside the game, and outside the game
    -- is not a zone -- so setup mints no object for any of it.
    Spec.assertEqWith s "and no object was minted for any of them" (Map.size (GameState.objects after)) 0
  -- CR 400.11c and CR 115.10a: what the filter CANNOT see. Filter.IsBound asks
  -- whether the candidate is an object the resolution bound, and a card outside
  -- the game is never one -- rule 400.11c keeps every spell and ability from
  -- affecting it, and nothing mints an object for it until `bringInto` does. So
  -- the bare Filter.contextFor `eligible` builds is honest: handing it the
  -- resolution's slots could not change an answer here. Pawl.CardSpec's
  -- "CR 400.11c no card asks
  -- IsBound in a wish's filter" is what keeps a card out of the position.
  --
  -- The pair differs in exactly one thing: the same board, the same bound slot,
  -- the same card-type conjunct, and the atom present or absent.
  Spec.it s "CR 400.11c a wish's filter cannot see what the resolution bound" $ do
    mountain <- S.printingOf s registry "Mountain"
    wish <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (board, wishId) = wishBoard mountain wish [(signInBlood, 1)]
        slot = SlotName.MkSlotName (Text.pack "target")
        -- The binding a resolution would have made, present on the board before
        -- either filter runs. It names the wish's own object because WHICH object
        -- it names cannot matter: no candidate outside the game is any of them.
        bind o = o {Object.bindings = Map.insert slot (Binding.toObject wishId) (Object.bindings o)}
        gs = board {GameState.objects = Map.adjust bind wishId (GameState.objects board)}
        bringing predicate = snd (Engine.runGamePure exercising gs (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True) wishId S.alice))
        atom = Filter.And [Filter.HasCardType CardType.Sorcery, Filter.IsBound slot]
    Spec.assertEqWith s "the slot names an object before either filter runs" (fmap Object.bindings (Game.lookupObject wishId gs)) (Just (Map.singleton slot (Binding.toObject wishId)))
    -- THE BEHAVIOUR: "a sorcery that IS the bound object" admits nothing, so the
    -- exclusion a card would write -- Not of the same atom -- is vacuously true.
    Spec.assertEqWith s "the wish brings in nothing" (printingsIn Zone.Hand S.alice (bringing atom)) [wish]
    Spec.assertEqWith s "where the same wish without the atom brings the sorcery in" (List.sort (printingsIn Zone.Hand S.alice (bringing (Filter.HasCardType CardType.Sorcery)))) (List.sort [wish, signInBlood])
    -- And the offer itself, one step nearer the atom: the atom is False for a
    -- candidate with no identity, so the conjunction admits nobody.
    Spec.assertEqWith s "nothing was even offered" (length (Event.eligible atom wishId S.alice gs)) 0
    Spec.assertEqWith s "where the sorcery filter offers the one card" (length (Event.eligible (Filter.HasCardType CardType.Sorcery) wishId S.alice gs)) 1
  -- CR 729.4: a subgame's own view of the main game. Alice's creature sits
  -- on the PARENT's battlefield; Setup.subgameStateFrom moves it into
  -- GameState.outsideObjects (CR 729.4, "all objects in the main game ... are
  -- considered outside the subgame"), and a wish cast IN the subgame can reach
  -- it just as it reaches the sideboard pool. Called at the unit level --
  -- `Event.bringInto` directly -- rather than through a printed wish,
  -- since Living Wish's own casting is not this unit's concern.
  Spec.it s "CR 729.4/729.4a a wish cast in a subgame reaches a main-game creature" $ do
    bear <- S.printingOf s registry "Prodigal Sorcerer"
    let (bearId, parent) = S.addPermanent bear S.alice (Setup.emptyGame S.bothPlayers)
        sub = Setup.subgameStateFrom S.alice parent
        predicate = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        after = snd (Engine.runGamePure S.identityAnswer sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True) bearId S.alice))
    Spec.assertEqWith s "CR 729.4/400.11c: the main-game creature arrives in her subgame hand" (printingsIn Zone.Hand S.alice after) [bear]
    Spec.assertEqWith s "CR 729.4a: the crossing is recorded for the outer frame to apply" (Foldable.toList (GameState.broughtIn after)) [bearId]
    Spec.assertEqWith s "and it is no longer offered" (Map.member bearId (GameState.outsideObjects after)) False
  -- The discrimination OutsideCard exists for: BOTH of CR 400.11c's sources on
  -- offer at once, and the answer names the pool one, not the main-game one.
  Spec.it s "CR 400.11c/729.4: the pool and the main game are offered together, and the answer picks the pool card" $ do
    bear <- S.printingOf s registry "Prodigal Sorcerer"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    let (bearId, parent0) = S.addPermanent bear S.alice (Setup.emptyGame S.bothPlayers)
        (dragonId, parent1) = Game.intern dragon parent0
        stock p = p {Player.outsideTheGame = Map.singleton dragonId 1}
        parent = parent1 {GameState.players = Map.adjust stock S.alice (GameState.players parent1)}
        sub = Setup.subgameStateFrom S.alice parent
        predicate = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        offered = Event.eligible predicate bearId S.alice sub
        isPool candidate = case candidate of
          OutsideCard.InPool _ -> True
          OutsideCard.InAnotherGame _ -> False
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseFromOutsideTheGame _ _ candidates _ _ -> [Maybe.fromMaybe (NonEmpty.head candidates) (List.find isPool (NonEmpty.toList candidates))]
          _ -> S.identityAnswer p
        after = snd (Engine.runGamePure answer sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True) bearId S.alice))
    Spec.assertEqWith s "the pool and the main game were both on offer" (length offered) 2
    Spec.assertEqWith s "the pool card is what arrived" (printingsIn Zone.Hand S.alice after) [dragon]
    Spec.assertEqWith s "the main-game creature is untouched: the answer did not take it" (Map.member bearId (GameState.outsideObjects after)) True
    Spec.assertEqWith s "and the pool entry was spent" (poolOf S.alice after) Map.empty
  -- CR 108.3b: the reach outside the game is scoped to the acting player's OWN
  -- cards. Bob's creature sits in the same outsideObjects map; alice's wish must
  -- not see it.
  Spec.it s "CR 108.3b a main-game creature owned by another player is not offered" $ do
    bear <- S.printingOf s registry "Prodigal Sorcerer"
    let (bobsBearId, parent) = S.addPermanent bear S.bob (Setup.emptyGame S.bothPlayers)
        sub = Setup.subgameStateFrom S.alice parent
        predicate = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        after = snd (Engine.runGamePure S.identityAnswer sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True) bobsBearId S.alice))
    Spec.assertEqWith s "alice's hand stays empty: bob's creature is not hers to reach" (printingsIn Zone.Hand S.alice after) []
    Spec.assertEqWith s "and bob's card is untouched" (Map.member bobsBearId (GameState.outsideObjects after)) True
  -- CR 708.2 over CR 729.4: what a subgame's wish sees of a FACE-DOWN main-game
  -- permanent. alice casts Soul Summons ({1}{W} sorcery, "Manifest the top card
  -- of your library") in the main game over Sign in Blood, so the permanent
  -- standing outside the subgame is a card whose PRINTED face and whose CR 708.2a
  -- face-down characteristics disagree about every card type it has: a sorcery
  -- underneath, a 2/2 creature with no name on the table.
  --
  -- TWO WISHES over the SAME board, which is what makes the reading falsifiable
  -- in both directions. Living Wish's creature-or-land filter must reach the
  -- manifest and did not before; Burning Wish's sorcery filter must NOT reach it
  -- and did before. The Soul Summons in alice's main-game graveyard is the
  -- control the second leg needs -- a FACE-UP sorcery outside the subgame, so
  -- "the sorcery wish brought nothing in" cannot pass for the right answer, and
  -- the answerer below asks for the manifest by id and settles for the graveyard
  -- card only because the manifest is not on offer.
  --
  -- The two Plains are lands outside the subgame, so the creature-or-land leg
  -- offers three candidates for one pick and cannot short-circuit. The Goblin
  -- Piker under Sign in Blood keeps CR 104.3c off the board and leaves the
  -- library non-empty for CR 729.2 to move.
  Spec.it s "CR 708.2/729.4 a manifested main-game sorcery is offered to a subgame's wish as a creature and not as a sorcery" $ do
    summons <- S.printingOf s registry "Soul Summons"
    plains <- S.printingOf s registry "Plains"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (g1, summonsId) = S.handOne summons (S.landsInPlay plains 2)
        (_, g2) = S.addLibraryCard piker S.alice g1
        (_, g3) = S.addLibraryCard signInBlood S.alice g2
        parent = S.runPure S.identityAnswer g3 (S.cast S.alice summonsId >> Stack.resolveTop)
        faceDownIds = filter (\oid -> maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid parent)) (Set.toList (GameState.battlefield parent))
        manifestedId = Maybe.fromMaybe summonsId (Maybe.listToMaybe faceDownIds)
        sub = Setup.subgameStateFrom S.alice parent
        creatureOrLand = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Land]
        -- Pinned by id, never by searching for a legal option: an answerer that
        -- picked whatever the filter offered would repair the assertion under a
        -- mutation. It falls back to the head only where the manifest is absent,
        -- which is the whole of what the sorcery leg asserts.
        preferManifest :: Prompt.Prompt r -> r
        preferManifest p = case p of
          Prompt.ChooseFromOutsideTheGame _ _ candidates _ _ -> [Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== OutsideCard.InAnotherGame manifestedId) (NonEmpty.toList candidates))]
          _ -> S.identityAnswer p
        wishing predicate = snd (Engine.runGamePure preferManifest sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand predicate True) manifestedId S.alice))
        offeredOuter predicate = Maybe.mapMaybe (\card -> case card of OutsideCard.InAnotherGame oid -> Just oid; _ -> Nothing) (Event.eligible predicate manifestedId S.alice sub)
    -- CR 708.2a's 2/2 creature is what the creature-or-land wish reaches, and CR
    -- 708.9's reveal is what arrives: the card itself, not the 2/2.
    Spec.assertEqWith s "CR 708.2a/729.4: the creature-or-land wish takes the manifest, and CR 708.9 hands it over as the sorcery card underneath" (printingsIn Zone.Hand S.alice (wishing creatureOrLand)) [signInBlood]
    -- The other arm of the same rule: "no characteristics other than those
    -- listed" leaves the manifest no card type but creature, so the sorcery wish
    -- has to settle for the face-up sorcery in the main-game graveyard.
    Spec.assertEqWith s "CR 708.2: the sorcery wish cannot reach the manifest and takes the face-up main-game sorcery instead" (printingsIn Zone.Hand S.alice (wishing (Filter.HasCardType CardType.Sorcery))) [summons]
    -- Preconditions, after the behaviour: the fixture really did manifest a
    -- sorcery, and the offer really did have a choice in it.
    Spec.assertEqWith s "the manifest is face down on the main-game battlefield" (length faceDownIds) 1
    Spec.assertEqWith s "and the card under it is Sign in Blood" (Game.printingOfObject manifestedId parent) (Just signInBlood)
    Spec.assertEqWith s "the creature-or-land wish was offered the manifest beside alice's two main-game Plains" (length (offeredOuter creatureOrLand), elem manifestedId (offeredOuter creatureOrLand)) (3, True)
    Spec.assertEqWith s "and the sorcery wish was offered the graveyard card alone" (elem manifestedId (offeredOuter (Filter.HasCardType CardType.Sorcery)), length (offeredOuter (Filter.HasCardType CardType.Sorcery))) (False, 1)
  -- CR 712.8g over CR 729.4: a melded main-game permanent is ONE object outside
  -- the subgame (CR 701.42a), offered by its combined face. Granted (Fae of
  -- Wishes' Adventure, "You may reveal a noncreature card you own from outside
  -- the game and put it into your hand.") is the one printed wish whose filter
  -- tells the combined face from the two front faces: Hanweir, the Writhing
  -- Township is a creature, while Hanweir Battlements is a land.
  --
  -- A PAIR differing in the meld alone, cast and resolved inside the subgame.
  -- Unmelded, Granted takes the Battlements; melded, the township is not on
  -- offer and no Hanweir card arrives. The answerer pins the Hanweir object by
  -- id and falls back to the head of the offer, which holds alice's main-game
  -- Mountains on both legs.
  Spec.it s "CR 712.8g/729.4 Granted cannot see a melded Hanweir, whose combined face is a creature, and takes an unmelded Battlements" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    fae <- S.printingOf s registry "Fae of Wishes"
    let (townshipId, melded) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        (battlementsId, u1) = S.addPermanent battlements S.alice (Setup.emptyGame S.bothPlayers)
        (_, u2) = S.addPermanent garrison S.alice u1
        unmelded = S.landsFor mountain S.alice 5 u2
        hanweir = Maybe.catMaybes [townshipId, Just battlementsId]
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          -- Granted's printed "may" (CR 608.2d), taken.
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ ->
            [Maybe.fromMaybe (NonEmpty.head offered) (List.find (\card -> any (\oid -> card == OutsideCard.InAnotherGame oid) hanweir) (NonEmpty.toList offered))]
          _ -> S.identityAnswer p
        granting parent =
          let (faeId, sub1) = S.addHandCard fae S.alice (S.landsFor island S.alice 4 (Setup.subgameStateFrom S.alice parent))
              sub2 = sub1 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
              granted = CardName.MkCardName (Text.pack "Granted")
           in (faeId, S.runPure answer sub2 (Cast.castSpell S.manaPerformer S.alice faeId granted Facing.FaceUp >> Stack.resolveTop))
        hanweirIn gs = filter (`elem` [battlements, garrison]) (printingsIn Zone.Hand S.alice gs)
        (_, afterMelded) = granting melded
        (_, afterUnmelded) = granting unmelded
    -- The gameplay-level claims, first.
    Spec.assertEqWith s "CR 712.8g: the melded township is a creature, so Granted brings no Hanweir card in" (hanweirIn afterMelded) []
    Spec.assertEqWith s "CR 729.4: the unmelded Battlements is a noncreature card, and Granted takes it" (hanweirIn afterUnmelded) [battlements]
    -- What each leg rests on.
    Spec.assertBool s (Maybe.isJust townshipId) "the meld happened"
    Spec.assertEqWith s "Granted resolved on the melded leg, taking a Mountain instead" (printingsIn Zone.Hand S.alice afterMelded) [mountain]
    Spec.assertEqWith s "CR 729.4: the township stays outside the subgame" (fmap (`Map.member` GameState.outsideObjects afterMelded) townshipId) (Just True)
  -- CR 712.21 over CR 729.4: what a wish takes of a melded permanent is the
  -- permanent, and the permanent leaving the battlefield is "two cards into
  -- the appropriate zone" -- here her subgame hand. Living Wish's creature
  -- filter admits the township's combined face.
  Spec.it s "CR 712.21/729.4 a wish cast in a subgame takes a melded main-game permanent, and both its cards arrive" $ do
    mountain <- S.printingOf s registry "Mountain"
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (townshipId, melded) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        sub = Setup.subgameStateFrom S.alice melded
        township = Maybe.fromMaybe (ObjectId.MkObjectId 0) townshipId
        creature = Filter.HasCardType CardType.Creature
        after = snd (Engine.runGamePure exercising sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand creature True) township S.alice))
    Spec.assertEqWith s "CR 712.21: both of Hanweir's cards arrive in her subgame hand" (List.sort (fmap S.nameOf (Maybe.mapMaybe (`Game.cardOf` after) (Game.zoneMembers Zone.Hand S.alice after)))) (List.sort [S.nameOf (Printing.card battlements), S.nameOf (Printing.card garrison)])
    Spec.assertEqWith s "CR 712.21e: one permanent crossed, under the township's id" (Foldable.toList (GameState.broughtIn after)) [township]
    Spec.assertBool s (Maybe.isJust townshipId) "the meld happened"
    Spec.assertEqWith s "CR 729.4: it was the one creature outside the subgame" (fmap (\oid -> [OutsideCard.InAnotherGame oid]) townshipId) (Just (Event.eligible creature township S.alice sub))
  -- CR 712.21a: "if a melded permanent is put into its owner's graveyard or
  -- library, that player may arrange the two cards in any order" -- and a wish
  -- naming the top of her library (The Raven's Warning's chapter III) is that.
  -- A PAIR of answers to Prompt.OrderComponentCards over the same board, which
  -- must leave different cards on top.
  Spec.it s "CR 712.21a a melded permanent a subgame puts on top of her library is arranged by her, and the answer decides the top card" $ do
    mountain <- S.printingOf s registry "Mountain"
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (townshipId, melded) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        sub = Setup.subgameStateFrom S.alice melded
        township = Maybe.fromMaybe (ObjectId.MkObjectId 0) townshipId
        creature = Filter.HasCardType CardType.Creature
        arranging :: ([Natural.Natural] -> [Natural.Natural]) -> Prompt.Prompt r -> r
        arranging order p = case p of
          Prompt.OrderComponentCards _ _ _ offered -> order (List.genericTake (length offered) [0 ..])
          _ -> exercising p
        topWith order =
          let after = snd (Engine.runGamePure (arranging order) sub (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.LibraryTop creature False) township S.alice))
              library = Game.zoneMembers Zone.Library S.alice after
           in (fmap S.nameOf (Maybe.listToMaybe library >>= (`Game.cardOf` after)), length library)
        (asOffered, offeredCount) = topWith id
        (reversedOrder, reversedCount) = topWith reverse
    Spec.assertBool s (asOffered /= reversedOrder) "CR 712.21a: the two arrangements leave different cards on top"
    Spec.assertEqWith s "and each top card is one of Hanweir's" (all (`elem` [Just (S.nameOf (Printing.card battlements)), Just (S.nameOf (Printing.card garrison))]) [asOffered, reversedOrder]) True
    Spec.assertEqWith s "CR 712.21: both cards arrive on each leg" (offeredCount, reversedCount) (2, 2)
    Spec.assertBool s (Maybe.isJust townshipId) "the meld happened"
  -- CR 730.3 over CR 729.4: a merged permanent rides the same door. Cubwarden
  -- merged over alice's Goblin Piker is one object outside the subgame, offered
  -- by its topmost component (CR 730.2a), and both cards arrive. Hand-built
  -- through Object.source rather than cast, since the merge itself is
  -- Pawl.MutateSpec's subject. Two more boards put a token on top, which CR
  -- 730.2d makes a token and so no card a wish can bring in, and a token
  -- underneath, which is no card to bring.
  Spec.it s "CR 730.3/729.4 a wish cast in a subgame takes a merged main-game permanent, and both its cards arrive" $ do
    cubwarden <- S.printingOf s registry "Cubwarden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (mergedId, g1) = S.addPermanent cubwarden S.alice (Setup.emptyGame S.bothPlayers)
        (pikerPrinting, g2) = Game.intern piker g1
        cubwardenPrinting = case fmap Object.source (Game.lookupObject mergedId g2) of
          Just (Source.OfCard printingId) -> printingId
          _ -> pikerPrinting
        withSource source gs = gs {GameState.objects = Map.adjust (\o -> o {Object.source = source}) mergedId (GameState.objects gs)}
        merged = withSource (Source.OfMerge (MergeComponent.OfCard cubwardenPrinting NonEmpty.:| [MergeComponent.OfCard pikerPrinting])) g2
        tokenTopped = withSource (Source.OfMerge (MergeComponent.OfToken pikerPrinting NonEmpty.:| [MergeComponent.OfCard cubwardenPrinting])) g2
        overToken = withSource (Source.OfMerge (MergeComponent.OfCard cubwardenPrinting NonEmpty.:| [MergeComponent.OfToken pikerPrinting])) g2
        creature = Filter.HasCardType CardType.Creature
        wishing parent = snd (Engine.runGamePure exercising (Setup.subgameStateFrom S.alice parent) (Event.bringInto (FromOutsideTheGame.MkFromOutsideTheGame 1 False OutsideDestination.Hand creature True) mergedId S.alice))
    Spec.assertEqWith s "CR 730.3: both of the merged permanent's cards arrive in her subgame hand" (List.sort (printingsIn Zone.Hand S.alice (wishing merged))) (List.sort [cubwarden, piker])
    Spec.assertEqWith s "CR 730.2d: a token on top makes it a token, and no card is brought in" (printingsIn Zone.Hand S.alice (wishing tokenTopped)) []
    Spec.assertEqWith s "CR 730.3/111.7: Cubwarden over a token brings the one card, the token being none" (printingsIn Zone.Hand S.alice (wishing overToken)) [cubwarden]
    Spec.assertEqWith s "the merged board's entry is offered by its topmost component" (fmap OutsideObject.printing (Map.lookup mergedId (GameState.outsideObjects (Setup.subgameStateFrom S.alice merged)))) (Just cubwardenPrinting)
  -- CR 712.21 / 729.4a / 729.5 at gameplay level: alice casts Shahrazad over a
  -- melded Hanweir, the Writhing Township, and inside the subgame Living Wish
  -- takes the township. One permanent leaves the main game (CR 712.21e), and
  -- its two cards are what the subgame hands back to her main-game library.
  --
  -- The sizing is the Death Wish case's: one wish and eight Forests, so she
  -- holds the wish by turn 5 with three lands down, and bob decks on turn 6.
  -- Her main-game Plains and Mountains are lands outside the subgame too, so
  -- the offer has a choice in it; the answerer pins the township by id.
  Spec.it s "CR 712.21/729.4a gameplay: Living Wish takes a melded Hanweir out of the main game, and both its cards come back to her library" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    livingWish <- S.printingOf s registry "Living Wish"
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (townshipId, melded) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        township = Maybe.fromMaybe (ObjectId.MkObjectId 0) townshipId
        g1 = S.landsFor plains S.alice 2 melded
        g2 = stockLibrary mountain 9 S.bob (stockLibrary forest 8 S.alice (stockLibrary livingWish 1 S.alice g1))
        (_shahrazadId, g3) = S.addHandCard shahrazad S.alice g2
        before =
          g3
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          -- Living Wish's printed "may" (CR 608.2d), taken.
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ ->
            [Maybe.fromMaybe (NonEmpty.head offered) (List.find (== OutsideCard.InAnotherGame township) (NonEmpty.toList offered))]
          -- CR 729.2's roll, answered so the turn count above is the one played.
          Prompt.RandomFirstPlayer _ -> S.alice
          _ -> S.castAnswer p
        after = snd (Engine.runGamePure answer before Engine.priorityLoop)
        leftTheGame = [LeftTheGame.object l | GameEvent.LeftTheGame l <- fmap LoggedEvent.event (Foldable.toList (GameState.events after))]
        hanweirIn zone = filter (`elem` [battlements, garrison]) (printingsIn zone S.alice after)
    -- The gameplay-level claims, first.
    Spec.assertEqWith s "CR 712.21/729.5: both of Hanweir's cards come back to her main-game library" (List.sort (hanweirIn Zone.Library)) (List.sort [battlements, garrison])
    Spec.assertEqWith s "CR 712.21e/729.4a: one permanent left the main game, the township" leftTheGame [township]
    -- What the claims rest on.
    Spec.assertBool s (Maybe.isJust townshipId) "the meld happened"
    Spec.assertEqWith s "the township started on the main-game battlefield" (Set.member township (GameState.battlefield before)) True
    Spec.assertEqWith s "CR 729.4a: it is gone from the main game" (Map.member township (GameState.objects after)) False
    Spec.assertEqWith s "and neither card is on her battlefield" (hanweirIn Zone.Battlefield) []
    Spec.assertEqWith s "CR 729.1b: alice won the subgame, so only bob paid" (S.lifeOf S.alice after, S.lifeOf S.bob after) (Just 20, Just 10)
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
        (firstPiker, g1) = S.addPermanent piker S.alice g0
        (secondPiker, g2) = S.addPermanent piker S.alice g1
        (shredderId, g3) = S.addPermanent shredder S.bob g2
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
          -- The fallback is unreachable while a Piker is still offered, and is not
          -- a silent one: taking the head of the offer takes a PLAINS, which
          -- crosses instead, so the battlefield and library assertions below both
          -- redden rather than the case passing on the wrong card.
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ ->
            [Maybe.fromMaybe (NonEmpty.head offered) (List.find isPiker (NonEmpty.toList offered))]
          -- CR 729.2's roll, answered so the turn count above is the one played.
          Prompt.RandomFirstPlayer _ -> S.alice
          _ -> S.castAnswer p
        after = snd (Engine.runGamePure answer before Engine.priorityLoop)
        -- Scoped to bob, the player CR 729.1b makes pay: today he is the only
        -- main-game life-loser, but an unscoped match would also catch alice's.
        isLifeLoss e = case e of
          GameEvent.LifeLost change -> LifeChange.player change == S.bob
          _ -> False
        isShredderCounter e = case e of
          GameEvent.CountersPut change -> CounterChange.object change == shredderId
          _ -> False
        -- CR 608.2n: "as the final part of an instant or sorcery spell's
        -- resolution, the spell is put into its owner's graveyard" -- so this is
        -- the moment CR 729.5 calls the spell FINISHING resolving, and the only
        -- stack-to-graveyard move in the main game's log (Living Wish exiles
        -- itself inside the subgame, whose log is its own, and a resolved ability
        -- ceases to exist rather than moving).
        isSpellFinishing e = case e of
          GameEvent.Moved (Moved.MkMoved zc _ _ _ _) -> ZoneChange.from zc == Zone.Stack && ZoneChange.to zc == Zone.Graveyard
          _ -> False
        -- CR 729.5's last sentence, read off the one log all three are recorded
        -- in. The chain: Shahrazad's own LoseLife clause runs during resolution,
        -- the spell then leaves the stack, and only after that can the trigger go
        -- on the stack and place its counter.
        resolutionThenTrigger = case (eventIndex isLifeLoss after, eventIndex isSpellFinishing after, eventIndex isShredderCounter after) of
          (Just loss, Just finished, Just counter) -> Just (loss < finished, finished < counter)
          _ -> Nothing
    -- The fixture's own preconditions, which the runner under test cannot redden.
    Spec.assertEqWith s "both of alice's creatures start on the main-game battlefield" (Set.member firstPiker (GameState.battlefield before), Set.member secondPiker (GameState.battlefield before)) (True, True)
    Spec.assertEqWith s "and bob's Super Shredder starts with no counters" (plusOneCounters shredderId before) 0
    Spec.assertEqWith s "CR 729.4: both main-game creatures left the main game for the subgame" (Set.member firstPiker (GameState.battlefield after), Set.member secondPiker (GameState.battlefield after)) (False, False)
    Spec.assertEqWith s "CR 729.4a: Super Shredder's leaves-the-battlefield trigger resolved once per crossing" (plusOneCounters shredderId after) 2
    Spec.assertEqWith s "CR 608.2n/729.5: the trigger's counter lands only after Shahrazad's last clause AND after the spell left the stack" resolutionThenTrigger (Just (True, True))
    Spec.assertEqWith s "CR 729.5: the cards the wishes took come back to her main-game library" (length (filter (== piker) (printingsIn Zone.Library S.alice after))) 2
    Spec.assertEqWith s "CR 729.5: and the subgame's own cards -- the wishes it exiled included -- came back with them" (length (printingsIn Zone.Library S.alice after)) 11
    -- CR 729.1b: alice won the subgame (bob decked), so she is the one player the
    -- follow-on LoseLife excludes. 20 halves to 10.
    Spec.assertEqWith s "CR 729.1b: only bob paid, which is how the subgame ended" (S.lifeOf S.alice after, S.lifeOf S.bob after) (Just 20, Just 10)
    Spec.assertEqWith s "CR 729.1a: the subgame did not decide the main game" (GameState.result after) Nothing
  -- CR 604.2's exception on the crossing road, and the case #2459 named.
  -- Titania's Song ({3}{G} enchantment, "Each noncreature artifact loses all
  -- abilities and becomes an artifact creature with power and toughness each
  -- equal to its mana value. If this enchantment leaves the battlefield, this
  -- effect continues until end of turn."): CR 604.2 ends a static ability's
  -- effect when its permanent leaves the battlefield, and this card's own text
  -- overrides that. CR 729.4a takes the enchantment out of the main game, which
  -- IS leaving the battlefield, so the effect has to go on applying in the main
  -- game the subgame was played inside -- exactly as it does on CR 800.4a's road
  -- out of the game (Pawl.DepartureSpec).
  --
  -- Death Wish ({1}{B}{B} sorcery, "You may put a card you own from outside the
  -- game into your hand. You lose half your life, rounded up. Exile Death Wish.")
  -- is the producer, and its unrestricted filter is why: #2459 was filed reading
  -- the pool's wishes as naming a card type apiece, but Death Wish names none, so
  -- CR 729.4 offers it a main-game ENCHANTMENT. The Death Wish case further up
  -- proves the same filter admits an ordinary sorcery out of the POOL.
  --
  -- Jade Statue ({4} artifact) is the probe: a noncreature artifact of mana value
  -- 4, so an animated one is a 4/4 and an un-animated one is no creature at all.
  --
  -- The sizing is the case above's, one wish instead of two: alice starts the
  -- subgame (CR 729.2) so her turns are 1, 3 and 5, and three Swamps are down on
  -- turn 5, which is the first turn the {1}{B}{B} wish is castable. Nine cards
  -- each: bob draws his last two on turns 2 and 4 and draws from an empty library
  -- on turn 6, ending the subgame one turn after alice's cast. alice's nine are
  -- the wish and eight Swamps, so whichever end the shuffle leaves the wish at
  -- she holds it by turn 5.
  Spec.it s "CR 604.2/729.4a gameplay: Death Wish takes Titania's Song out of the main game and its effect goes on applying there" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    deathWish <- S.printingOf s registry "Death Wish"
    titaniasSong <- S.printingOf s registry "Titania's Song"
    jadeStatue <- S.printingOf s registry "Jade Statue"
    let g0 = Setup.emptyGame S.bothPlayers
        (songId, g1) = S.addPermanent titaniasSong S.alice g0
        (statueId, g2) = S.addPermanent jadeStatue S.alice g1
        g3 = S.landsFor plains S.alice 2 g2
        g4 = stockLibrary mountain 9 S.bob (stockLibrary swamp 8 S.alice (stockLibrary deathWish 1 S.alice g3))
        (_shahrazadId, g5) = S.addHandCard shahrazad S.alice g4
        before =
          g5
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          -- Death Wish's printed "may" (CR 608.2d).
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          -- Pinned to the Song by id. The offer also holds her two Plains and the
          -- Jade Statue, so an answerer taking the head of it would cross a
          -- different card and every assertion below would redden rather than the
          -- case passing on the wrong one.
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ ->
            [Maybe.fromMaybe (NonEmpty.head offered) (List.find (== OutsideCard.InAnotherGame songId) (NonEmpty.toList offered))]
          -- CR 729.2's roll, answered so the turn count above is the one played.
          Prompt.RandomFirstPlayer _ -> S.alice
          _ -> S.castAnswer p
        after = snd (Engine.runGamePure answer before Engine.priorityLoop)
    -- The fixture's own preconditions, read off the board before anything runs,
    -- so the runner under test cannot redden them: the Song really was animating
    -- the Statue in the main game, which is what makes "continues" mean anything.
    Spec.assertEqWith s "CR 613.4b the Song animates the Statue as a 4/4 before the subgame" (S.powerToughnessOf statueId before) (Just (4, 4))
    Spec.assertEqWith s "and both are on the main game's battlefield" (Set.member songId (GameState.battlefield before), Set.member statueId (GameState.battlefield before)) (True, True)
    -- The gameplay-level claim, ahead of every proxy below it.
    Spec.assertEqWith s "CR 604.2: the Song's effect goes on applying in the main game, so the Statue is still a 4/4" (S.powerToughnessOf statueId after) (Just (4, 4))
    Spec.assertBool s (Projection.isCreatureOf statueId after) "CR 611.2a: and still an artifact creature"
    -- What the claim rests on: the Song genuinely left, and the handover is a
    -- stored effect rather than the Song still projecting from the battlefield.
    Spec.assertEqWith s "CR 729.4a: the wish took the Song out of the main game entirely" (Map.member songId (GameState.objects after)) False
    Spec.assertEqWith s "CR 604.2/611.2c: the continuing effect is stored, one per part of the ability" (length (GameState.continuousEffects after)) 4
    Spec.assertEqWith s "CR 729.5: the Song comes back to her main-game library" (List.elem titaniasSong (printingsIn Zone.Library S.alice after)) True
    Spec.assertEqWith s "CR 729.1b: alice won the subgame, so only bob paid" (S.lifeOf S.alice after, S.lifeOf S.bob after) (Just 20, Just 10)
  -- CR 729.5's second sentence, the one the case above does not reach: "the spell
  -- or ability that created the subgame finishes resolving, EVEN IF IT WAS
  -- CREATED BY A SPELL CARD THAT'S NO LONGER ON THE STACK". alice casts Shahrazad
  -- in the main game; inside the subgame she casts Burning Wish ({1}{R} sorcery,
  -- "You may reveal a sorcery card you own from outside the game and put it into
  -- your hand. Exile Burning Wish.") and names the Shahrazad that is resolving.
  -- CR 729.4 offers it -- the main game's stack is outside the subgame like every
  -- other main-game zone -- so Setup.applyCrossings deletes the main game's own
  -- Shahrazad object and funnelBack files the subgame's incarnation in alice's
  -- main-game library.
  --
  -- What CR 729.5 then requires is that Shahrazad's SECOND effect still runs with
  -- the winner it bound. bob decks, so alice wins, and "each player who doesn't
  -- win the subgame loses half their life" must reach bob alone.
  --
  -- THE DISCRIMINATING QUANTITY IS THE WINNER'S LIFE, not the loser's: bob halves
  -- under either reading. A resolution that lost its winner binding leaves
  -- PlayerRef.EachPlayerExcept excluding nobody, and alice halves too.
  --
  -- The sizing is the case above's, one wish instead of two. alice starts the
  -- subgame (CR 729.2) so she skips her first draw (CR 103.8a) and her turns are
  -- 1, 3 and 5; nine cards each means seven go to an opening hand and bob draws
  -- from an empty library on turn 6, which ends the subgame under CR 704.5b.
  -- alice's nine are one Burning Wish and eight Mountains, so by turn 5 she has
  -- drawn her whole library and holds the wish whatever the shuffle did, with
  -- three lands down against its {1}{R}. Nine is over CR 729.3's seven, so
  -- neither player decks during setup.
  Spec.it s "CR 729.5 gameplay: a wish that takes the resolving Shahrazad itself still finishes resolving with the winner it bound" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    burningWish <- S.printingOf s registry "Burning Wish"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = S.landsFor plains S.alice 2 g0
        g2 = stockLibrary mountain 9 S.bob (stockLibrary mountain 8 S.alice (stockLibrary burningWish 1 S.alice g1))
        (_shahrazadId, g3) = S.addHandCard shahrazad S.alice g2
        before =
          g3
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        isFromAnotherGame candidate = case candidate of
          OutsideCard.InAnotherGame _ -> True
          OutsideCard.InPool _ -> False
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          -- Burning Wish's printed "may" (CR 608.2d), taken.
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          -- The main-game Shahrazad is the only sorcery alice owns outside the
          -- subgame -- her main-game Plains are lands and her pool is empty -- so
          -- this names it. The fallback is not silent: taking the head of an offer
          -- that held anything else would leave Shahrazad's card in the main game
          -- rather than in her library, which the second assertion reddens on.
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ ->
            [Maybe.fromMaybe (NonEmpty.head offered) (List.find isFromAnotherGame (NonEmpty.toList offered))]
          -- CR 729.2's roll, answered so the turn count above is the one played.
          Prompt.RandomFirstPlayer _ -> S.alice
          _ -> S.castAnswer p
        after = snd (Engine.runGamePure answer before Engine.priorityLoop)
    Spec.assertEqWith s "the fixture starts both seats at twenty" (S.lifeOf S.alice before, S.lifeOf S.bob before) (Just 20, Just 20)
    Spec.assertEqWith s "CR 729.5: the resolution finishes with the winner it bound -- alice won and is excluded, bob alone halves" (S.lifeOf S.alice after, S.lifeOf S.bob after) (Just 20, Just 10)
    Spec.assertEqWith s "CR 729.4/729.5: the wish really took the resolving Shahrazad, which came back to alice's main-game library" (length (filter (== shahrazad) (printingsIn Zone.Library S.alice after))) 1
    Spec.assertEqWith s "CR 400.7/608.2n: and so it is in no main-game graveyard" (printingsIn Zone.Graveyard S.alice after) []
    Spec.assertEqWith s "CR 729.1a: the subgame did not decide the main game" (GameState.result after) Nothing

  -- CR 729.4a's wider half: main-game abilities "trigger on objects leaving a
  -- main-game ZONE", not only the battlefield. alice's Spirit Mascot ({R}{W}
  -- creature, "Whenever one or more cards leave your graveyard, put a +1/+1
  -- counter on this creature.") watches her main-game graveyard, which holds a
  -- Sign in Blood. Inside the subgame Burning Wish takes a sorcery she owns from
  -- outside the game, and CR 729.4 offers it two: the Sign in Blood, and the
  -- resolving Shahrazad on the main game's stack.
  --
  -- A PAIR differing in the answer alone. Taking the graveyard card is a card
  -- leaving her graveyard, and the Mascot gets its counter once the main game
  -- resumes; taking Shahrazad is a card leaving the STACK, and it gets none.
  --
  -- The sizing is the Shahrazad-taking case above's, which this board's
  -- second leg repeats.
  Spec.it s "CR 729.4a gameplay: Burning Wish takes a card out of a main-game graveyard and Spirit Mascot sees it leave" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    burningWish <- S.printingOf s registry "Burning Wish"
    mascot <- S.printingOf s registry "Spirit Mascot"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let g0 = Setup.emptyGame S.bothPlayers
        (mascotId, g1) = S.addPermanent mascot S.alice g0
        (signId, g2) = S.addGraveyardCard signInBlood S.alice g1
        g3 = S.landsFor plains S.alice 2 g2
        g4 = stockLibrary mountain 9 S.bob (stockLibrary mountain 8 S.alice (stockLibrary burningWish 1 S.alice g3))
        (_shahrazadId, g5) = S.addHandCard shahrazad S.alice g4
        before =
          g5
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        -- Pinned by id: the graveyard card, or the one other main-game sorcery
        -- offered, which is the resolving Shahrazad.
        playWith takeGraveyardCard =
          let wanted candidate = case candidate of
                OutsideCard.InAnotherGame oid -> (oid == signId) == takeGraveyardCard
                OutsideCard.InPool _ -> False
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                -- Burning Wish's printed "may" (CR 608.2d), taken.
                Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                Prompt.ChooseFromOutsideTheGame _ _ offered _ _ ->
                  [Maybe.fromMaybe (NonEmpty.head offered) (List.find wanted (NonEmpty.toList offered))]
                -- CR 729.2's roll, answered so the turn count above is the one played.
                Prompt.RandomFirstPlayer _ -> S.alice
                _ -> S.castAnswer p
           in snd (Engine.runGamePure answer before Engine.priorityLoop)
        tookGraveyardCard = playWith True
        tookShahrazad = playWith False
    -- The gameplay-level claims, first.
    Spec.assertEqWith s "CR 729.4a: a card left alice's main-game graveyard, so the Mascot got its counter" (plusOneCounters mascotId tookGraveyardCard) 1
    Spec.assertEqWith s "CR 729.4a: a card left the main game's stack instead, so it got none" (plusOneCounters mascotId tookShahrazad) 0
    -- What each leg took, which is the pair's one difference.
    Spec.assertEqWith s "the first leg's wish took the Sign in Blood out of her graveyard" (Map.member signId (GameState.objects tookGraveyardCard), printingsIn Zone.Graveyard S.alice tookGraveyardCard) (False, [shahrazad])
    Spec.assertEqWith s "the second leg's wish took Shahrazad, leaving the Sign in Blood where it was" (Map.member signId (GameState.objects tookShahrazad), length (filter (== shahrazad) (printingsIn Zone.Library S.alice tookShahrazad))) (True, 1)
    Spec.assertEqWith s "the Mascot starts with no counters" (plusOneCounters mascotId before) 0
    Spec.assertEqWith s "CR 729.1b: alice won the subgame on both legs, so only bob paid" (fmap (\gs -> (S.lifeOf S.alice gs, S.lifeOf S.bob gs)) [tookGraveyardCard, tookShahrazad]) [(Just 20, Just 10), (Just 20, Just 10)]

  -- CR 729.5 again, against the two readers that looked the resolving spell up
  -- by id: Synthetic Subgame Tithe ({W}{W} sorcery: players play a subgame,
  -- then roll a six-sided die, you gain life equal to the result, then 1 life
  -- for each player who didn't win). A synthetic because no printed subgame
  -- card reads an amount it bound or counts "each player except the winner";
  -- issue 2493 records the Scryfall query. Burning Wish inside the subgame takes
  -- the resolving Tithe, so both gains are read off a spell whose object is
  -- gone: the die's 4 through Quantity.InSlot, and bob's 1 through
  -- Count.playersFor's EachPlayerExcept arm.
  --
  -- A PAIR differing in the wish's "may" alone: declined, the Tithe stays on
  -- the stack and the same gains are read off a live object. The sizing is the
  -- Shahrazad-taking case above's.
  Spec.it s "CR 729.5 gameplay: a subgame spell whose card a wish took still reads the die it rolled and counts who didn't win" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    tithe <- S.printingOf s registry "Synthetic Subgame Tithe"
    burningWish <- S.printingOf s registry "Burning Wish"
    let g1 = S.landsFor plains S.alice 2 (Setup.emptyGame S.bothPlayers)
        g2 = stockLibrary mountain 9 S.bob (stockLibrary mountain 8 S.alice (stockLibrary burningWish 1 S.alice g1))
        (_titheId, g3) = S.addHandCard tithe S.alice g2
        before =
          g3
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        playWith decision =
          let answer :: Prompt.Prompt r -> r
              answer p = case p of
                -- Burning Wish's printed "may" (CR 608.2d): the pair's one difference.
                Prompt.ChooseOptional {} -> decision
                -- CR 706.2's result, distinct from every other number here.
                Prompt.RollDie _ -> 4
                -- CR 729.2's roll, answered so the turn count above is the one played.
                Prompt.RandomFirstPlayer _ -> S.alice
                _ -> S.castAnswer p
           in snd (Engine.runGamePure answer before Engine.priorityLoop)
        taken = playWith OptionalDecision.Exercises
        left = playWith OptionalDecision.Declines
    -- The gameplay-level claims, first: 20 + 4 for the die + 1 for bob.
    Spec.assertEqWith s "CR 729.5: with the Tithe's card taken, alice still gains the die's 4 and 1 for bob" (S.lifeOf S.alice taken) (Just 25)
    Spec.assertEqWith s "the same gains off a Tithe nothing took" (S.lifeOf S.alice left) (Just 25)
    -- What the pair rests on.
    Spec.assertEqWith s "CR 729.4/729.5: the wish took the Tithe, which came back to alice's main-game library" (length (filter (== tithe) (printingsIn Zone.Library S.alice taken)), length (filter (== tithe) (printingsIn Zone.Graveyard S.alice taken))) (1, 0)
    Spec.assertEqWith s "the declined wish left it to finish into her graveyard" (length (filter (== tithe) (printingsIn Zone.Graveyard S.alice left))) 1
    Spec.assertEqWith s "bob lost nothing either way" (S.lifeOf S.bob taken, S.lifeOf S.bob left) (Just 20, Just 20)

  -- CR 614.6 / 400.11c: Ring of Ma'rûf ({5} Artifact, "{5}, {T}, Exile this
  -- artifact: The next time you would draw a card this turn, instead put a card
  -- you own from outside the game into your hand." -- name, cost, type line and
  -- Oracle text checked against api.scryfall.com 2026-09-04, paper printing
  -- `arn`).
  --
  -- The second road into the game, and the only printing that takes it --
  -- Scryfall o:"outside the game" o:"would draw", 2026-09-04, one hit: an
  -- activated ability installs a floating row (CR 614.3) and the next draw is
  -- CANCELLED, so no card leaves the library and the card arrives from the pool
  -- instead.
  --
  -- A PAIR of boards differing in one thing -- whether the ability was activated
  -- -- so "the draw was replaced" and "she drew normally" are told apart by WHICH
  -- printing is in her hand, not merely by how many cards are.
  --
  -- Every number distinct: five lands, two library cards, one card in the pool.
  Spec.it s "CR 400.11c a draw replaced by a wish brings the card in from outside the game" $ do
    plains <- S.printingOf s registry "Plains"
    ring <- S.printingOf s registry "Ring of Ma'rûf"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (armed, ringId) = ringBoard plains ring piker signInBlood True
        after = S.runPure S.identityAnswer armed (Event.drawCard S.alice)
    -- The fixture's own precondition: the cost really exiled the artifact, which
    -- is what says the ability was activated and resolved at all.
    Spec.assertEqWith s "setup: the activation cost exiled the Ring, so the ability was paid for and resolved" (Set.member ringId (GameState.battlefield armed)) False
    -- THE BEHAVIOUR, ahead of every proxy: the card in hand is the one from the
    -- pool and not the one off the top of her library.
    Spec.assertEqWith s "CR 400.11c the card she owns outside the game is what reached her hand" (printingsIn Zone.Hand S.alice after) [signInBlood]
    Spec.assertEqWith s "CR 614.6 the draw never happened, so nothing left her library" (length (Game.zoneMembers Zone.Library S.alice after)) 2
    Spec.assertEqWith s "CR 400.11b and the pool is spent" (Map.size (poolOf S.alice after)) 0
  -- The CONTROL: the same board with the ability never activated, so the only
  -- difference is the floating row.
  Spec.it s "CR 121.1 with no row installed the same board draws off her library" $ do
    plains <- S.printingOf s registry "Plains"
    ring <- S.printingOf s registry "Ring of Ma'rûf"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let (unarmed, _) = ringBoard plains ring piker signInBlood False
        after = S.runPure S.identityAnswer unarmed (Event.drawCard S.alice)
    Spec.assertEqWith s "CR 121.1 she drew the library card" (printingsIn Zone.Hand S.alice after) [piker]
    Spec.assertEqWith s "which came off her library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
    Spec.assertEqWith s "CR 400.11b and nothing was taken out of the pool" (Map.size (poolOf S.alice after)) 1
  -- CR 315.3: "conspiracy cards that aren't in the game can't be brought into the
  -- game". The first case's board with Sentinel Dispatch in the pool instead of
  -- Sign in Blood: the draw is still replaced, and nothing arrives (CR 609.3).
  Spec.it s "CR 315.3 a conspiracy outside the game cannot be brought in" $ do
    plains <- S.printingOf s registry "Plains"
    ring <- S.printingOf s registry "Ring of Ma'rûf"
    piker <- S.printingOf s registry "Goblin Piker"
    dispatch <- S.printingOf s registry "Sentinel Dispatch"
    let (armed, _) = ringBoard plains ring piker dispatch True
        after = S.runPure S.identityAnswer armed (Event.drawCard S.alice)
    Spec.assertEqWith s "CR 315.3 the conspiracy did not reach her hand" (printingsIn Zone.Hand S.alice after) []
    Spec.assertEqWith s "CR 400.11b and it is still in the pool" (Map.size (poolOf S.alice after)) 1
    Spec.assertEqWith s "CR 614.6 the draw was still replaced" (length (Game.zoneMembers Zone.Library S.alice after)) 2
  -- CR 407.3: "when not playing for ante ... these cards can't be brought into
  -- the game from outside the game". Contract from Below is a sorcery, so
  -- Burning Wish's filter admits it; the pair differs only in the setting.
  Spec.it s "CR 407.3 Burning Wish cannot bring in an ante card when not playing for ante" $ do
    mountain <- S.printingOf s registry "Mountain"
    wish <- S.printingOf s registry "Burning Wish"
    contract <- S.printingOf s registry "Contract from Below"
    let (gs, wishId) = wishBoard mountain wish [(contract, 1)]
        forAnte = gs {GameState.settings = (GameState.settings gs) {GameSettings.ante = True}}
    Spec.assertEqWith s "CR 407.3 not playing for ante, her hand gets nothing" (printingsIn Zone.Hand S.alice (resolveWish exercising wishId gs)) []
    Spec.assertEqWith s "playing for ante, the same wish brings it in" (printingsIn Zone.Hand S.alice (resolveWish exercising wishId forAnte)) [contract]
  -- CR 407.3 / 729.4: only the nine ante cards remove a card from the ante, and
  -- a subgame wish is not one of them. alice's main-game ante holds a Sign in
  -- Blood; inside a Shahrazad subgame, Burning Wish asks for it in vain. The
  -- game is played for ante, so the subgame antes one card from each library
  -- too: each library holds one Mountain more than the CR 729.4a case's, and
  -- the subgame's random ante is pinned away from the Burning Wish.
  Spec.it s "CR 407.3/729.4 gameplay: a subgame's Burning Wish cannot take a main-game ante card" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    shahrazad <- S.printingOf s registry "Shahrazad"
    burningWish <- S.printingOf s registry "Burning Wish"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let g0 = Setup.gameWith GameSettings.plain {GameSettings.ante = True} S.bothPlayers
        (signId, g1) = S.addObjectIn Zone.Ante signInBlood S.alice g0
        g2 = S.landsFor plains S.alice 2 g1
        (wishLibraryId, g3) = S.addLibraryCard burningWish S.alice g2
        g4 = stockLibrary mountain 10 S.bob (stockLibrary mountain 9 S.alice g3)
        (_shahrazadId, g5) = S.addHandCard shahrazad S.alice g4
        before = g5 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
        wanted candidate = case candidate of
          OutsideCard.InAnotherGame oid -> oid == signId
          OutsideCard.InPool _ -> False
        -- Records whether the Sign in Blood was ever on offer, and asks for it.
        recordOffers :: Prompt.Prompt r -> State.State Bool r
        recordOffers p = case p of
          Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> do
            State.modify' (\seen -> seen || any wanted (NonEmpty.toList offered))
            pure [Maybe.fromMaybe (NonEmpty.head offered) (List.find wanted (NonEmpty.toList offered))]
          Prompt.RandomFirstPlayer _ -> pure S.alice
          -- CR 407.2 in the subgame: never ante the wish.
          Prompt.RandomObject offered -> pure (Maybe.fromMaybe (NonEmpty.head offered) (List.find (/= wishLibraryId) (NonEmpty.toList offered)))
          _ -> pure (S.castAnswer p)
        ((_, after), offeredSign) = State.runState (Engine.runGame recordOffers before Engine.priorityLoop) False
    Spec.assertEqWith s "CR 407.3 the main-game ante card was never offered" offeredSign False
    Spec.assertEqWith s "CR 407.3 and it is still in alice's main-game ante" (Set.member signId (GameState.ante after), fmap Object.zone (Game.lookupObject signId after)) (True, Just Zone.Ante)
    Spec.assertEqWith s "CR 729.1a the subgame did not decide the main game, so nobody decked out" (GameState.result after) Nothing
  Spec.it s "CR 407.3/729.4 a main-game ante card is not outside the subgame; the same card in exile is" $ do
    sign <- S.printingOf s registry "Sign in Blood"
    let parent = Setup.gameWith GameSettings.plain {GameSettings.ante = True} S.bothPlayers
        outsideOf zone = let (oid, placed) = S.addObjectIn zone sign S.alice parent in Map.member oid (GameState.outsideObjects (Setup.subgameStateFrom S.alice placed))
    Spec.assertEqWith s "CR 407.3 the ante card is not outside the subgame" (outsideOf Zone.Ante) False
    Spec.assertEqWith s "CR 729.4 the same card in exile is" (outsideOf Zone.Exile) True
  -- CR 400.11b: a destination that is not the hand. The Raven's Warning
  -- ({1}{W}{U} Enchantment -- Saga; name, cost, type line and Oracle text checked
  -- against api.scryfall.com 2026-09-16, paper printing `khm`) is the only
  -- printing that says one -- Scryfall o:"outside the game" -is:digital
  -- -is:funny, 2026-09-16, twenty-one hits, of which the other non-hand
  -- destination is Research's "shuffle up to four cards you own from outside the
  -- game into your library", which the Research group below drives.
  --
  -- Chapter III alone is what this case drives: the Saga is placed with two lore
  -- counters, so CR 714.3c's turn-based action puts the third on and the final
  -- chapter is the one trigger on the stack.
  --
  -- ONE card already in her library, so the top is a different place from the
  -- bottom; TWO cards in the pool, so which one arrives is a choice she is asked
  -- and this answerer pins by naming the last offered. Every printing distinct.
  Spec.it s "CR 400.11b The Raven's Warning puts the card it found on top of her library and not into her hand" $ do
    ravens <- S.printingOf s registry "The Raven's Warning"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    psychicMiasma <- S.printingOf s registry "Psychic Miasma"
    let (sagaId, placed) = S.addPermanent ravens S.alice (Setup.emptyGame S.bothPlayers)
        stocked = stockLibrary dragon 1 S.alice placed
        -- Interned in LIST ORDER, wishBoard's reason: the ids ascend with the
        -- list, so Psychic Miasma is what the prompt offers last.
        intern printing (acc, gs) = let (printingId, gs2) = Game.intern printing gs in (acc <> [(printingId, 1)], gs2)
        (entries, interned) = List.foldl' (flip intern) ([], stocked) [signInBlood, psychicMiasma]
        stock p = p {Player.outsideTheGame = Map.fromList entries}
        withPool = interned {GameState.players = Map.adjust stock S.alice (GameState.players interned)}
        ready =
          (S.addCounter CounterKind.Lore 2 sagaId withPool)
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        advanced = S.runPure S.identityAnswer ready (Engine.runTurnBasedActions Phase.PrecombatMain)
        answeringLast :: Prompt.Prompt r -> r
        answeringLast p = case p of
          Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> [List.last (foldr (:) [] offered)]
          _ -> exercising p
        settled = S.runPure answeringLast advanced Engine.settleForPriority
        resolved = S.runPure answeringLast settled Stack.resolveTop
    -- THE BEHAVIOUR, ahead of every proxy, and one assertion for both readings the
    -- destination carries: the ZONE, which the wish cycle's hand would fail, and
    -- the END, which LibraryPosition.defaultValue's bottom would fail.
    Spec.assertEqWith s "CR 400.11b/401.2 the card she named is on TOP of her library, above the card already there" (printingsIn Zone.Library S.alice resolved) [psychicMiasma, dragon]
    Spec.assertEqWith s "CR 400.11b and nothing reached her hand" (printingsIn Zone.Hand S.alice resolved) []
    Spec.assertEqWith s "CR 400.11b and only the copy she named left the pool" (Map.size (poolOf S.alice resolved)) 1
    -- The fixture's own preconditions, AFTER the behaviour so neither can absorb a
    -- mutation the library is what observes.
    Spec.assertEqWith s "setup: CR 714.3c's turn-based action put the third lore counter on" (S.counterOf CounterKind.Lore sagaId advanced) 3
    Spec.assertEqWith s "setup: her library held the one card and no more before chapter III resolved" (printingsIn Zone.Library S.alice settled) [dragon]
  -- A count other than one. Research ({G}{U} Instant, the left half of Research
  -- // Development: "Shuffle up to four cards you own from outside the game into
  -- your library."; read off card_faces at api.scryfall.com 2026-09-27) on
  -- researchBoard, answered by position in the offer. Two copies of Sign in
  -- Blood are named, so one answer spends a printing twice (CR 100.4a); the
  -- shuffle REVERSES what it is offered, so the order the library comes back in
  -- says the shuffle happened after the cards arrived.
  Spec.it s "CR 400.11c Research shuffles the cards she chose, two copies of one printing among them, into her library" $ do
    (gs, researchId, (signInBlood, _, dragon, _), before) <- researchBoard s registry
    let resolved = S.runPure (researching [0, 0, 2]) gs (castHalf "Research" researchId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 701.24a the three cards she named are in her library, which was shuffled after they arrived" (printingsIn Zone.Library S.alice resolved) (reverse before <> [signInBlood, signInBlood, dragon])
    Spec.assertEqWith s "CR 400.11b both copies and the Dragon left the pool, and the other two cards did not" (Map.elems (poolOf S.alice resolved)) [1, 1]
    Spec.assertEqWith s "and nothing reached her hand" (printingsIn Zone.Hand S.alice resolved) []
  -- "Up to" reaches none, and CR 701.24d shuffles the library all the same: the
  -- same board, answered with nothing.
  Spec.it s "CR 701.24d Research bringing in nothing still shuffles her library" $ do
    (gs, researchId, _, before) <- researchBoard s registry
    let resolved = S.runPure (researching []) gs (castHalf "Research" researchId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 701.24d her library holds what it did, shuffled" (printingsIn Zone.Library S.alice resolved) (reverse before)
    Spec.assertEqWith s "CR 400.11b and the pool is untouched" (Map.elems (poolOf S.alice resolved)) [2, 1, 1, 1]
  -- The count is a ceiling on the answer: five named, the first four arrive and
  -- the fifth card stays out (Event.settleOutside).
  Spec.it s "CR 400.11c Research brings in no more than four however many she names" $ do
    (gs, researchId, (signInBlood, miasma, dragon, _), before) <- researchBoard s registry
    let resolved = S.runPure (researching [0, 0, 1, 2, 3]) gs (castHalf "Research" researchId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 400.11c four cards arrived, in the order named, and the shuffle followed" (printingsIn Zone.Library S.alice resolved) (reverse before <> [signInBlood, signInBlood, miasma, dragon])
    Spec.assertEqWith s "and the fifth is still outside the game" (Map.elems (poolOf S.alice resolved)) [1]
  -- CR 701.23j: a search that reaches outside the game beside its zones.
  -- Invasion of Arcavios ({3}{U}{U} Battle -- Siege, "When this Siege enters,
  -- search your library, graveyard, and/or outside the game for an instant or
  -- sorcery card you own, reveal it, and put it into your hand. If you search
  -- your library this way, shuffle."; api.scryfall.com 2026-09-27) on
  -- arcaviosBoard, the cases differing in the answers alone: which places she
  -- takes, which library card she finds, and whether she takes what she is
  -- offered from outside the game.
  Spec.it s "CR 701.23j Invasion of Arcavios brings an instant in from outside the game, and a search that skipped her library shuffles nothing" $ do
    board <- arcaviosBoard s registry
    let resolved = resolveArcavios (arcaviosAnswer [SearchPlace.OutsideTheGame] [] True) board
    -- THE BEHAVIOUR first. An engine that did not offer outside the game as a
    -- place falls back to every zone, and CR 400.2 then forces the graveyard's
    -- Sign in Blood into her hand instead.
    Spec.assertEqWith s "CR 701.23j the Fog from outside the game is in her hand" (printingsIn Zone.Hand S.alice resolved) [arcaviosFog board]
    Spec.assertEqWith s "CR 400.11b and it left the pool, the Island the filter rejects staying" (Map.elems (poolOf S.alice resolved)) [1]
    Spec.assertEqWith s "the graveyard's sorcery was not found" (printingsIn Zone.Graveyard S.alice resolved) [arcaviosSign board]
    Spec.assertEqWith s "and the library she did not search was not shuffled" (printingsIn Zone.Library S.alice resolved) (arcaviosLibrary board)
  -- CR 701.23j and CR 400.2 together: the graveyard is public, so a search of it
  -- must find its match -- unless the one count is filled from outside the game,
  -- which the searcher "may" take. Paired with the case after it, which differs
  -- only in declining the Fog.
  Spec.it s "CR 701.23j a card taken from outside the game fills the count, sparing the graveyard's match" $ do
    board <- arcaviosBoard s registry
    let resolved = resolveArcavios (arcaviosAnswer [SearchPlace.InZone Zone.Graveyard, SearchPlace.OutsideTheGame] [] True) board
    Spec.assertEqWith s "CR 701.23j the Fog is in her hand" (printingsIn Zone.Hand S.alice resolved) [arcaviosFog board]
    Spec.assertEqWith s "CR 701.23a one count over every place, so the graveyard's sorcery stayed" (printingsIn Zone.Graveyard S.alice resolved) [arcaviosSign board]
  Spec.it s "CR 400.2 declining outside the game too, the graveyard's match is found" $ do
    board <- arcaviosBoard s registry
    let resolved = resolveArcavios (arcaviosAnswer [SearchPlace.InZone Zone.Graveyard, SearchPlace.OutsideTheGame] [] False) board
    Spec.assertEqWith s "CR 400.2 the graveyard's Sign in Blood is in her hand" (printingsIn Zone.Hand S.alice resolved) [arcaviosSign board]
    Spec.assertEqWith s "CR 400.11b and the pool is untouched" (Map.elems (poolOf S.alice resolved)) [1, 1]
  -- CR 701.23j's "may" on its own: outside the game alone, declined, finds
  -- nothing although the graveyard holds a match -- which it was not searched.
  Spec.it s "CR 701.23j outside the game alone may find nothing" $ do
    board <- arcaviosBoard s registry
    let resolved = resolveArcavios (arcaviosAnswer [SearchPlace.OutsideTheGame] [] False) board
    Spec.assertEqWith s "CR 701.23j her hand is empty" (printingsIn Zone.Hand S.alice resolved) []
    Spec.assertEqWith s "and the graveyard's sorcery stayed" (printingsIn Zone.Graveyard S.alice resolved) [arcaviosSign board]
  -- Every place, and the library's instant named: the one count is filled from
  -- the zones, so nothing out there is taken whatever she would have answered,
  -- and the card's own "if you search your library this way, shuffle" follows.
  Spec.it s "CR 701.23a/701.23j a library find fills the one count, and the library is shuffled" $ do
    board <- arcaviosBoard s registry
    let everywhere = [SearchPlace.InZone Zone.Library, SearchPlace.InZone Zone.Graveyard, SearchPlace.OutsideTheGame]
        resolved = resolveArcavios (arcaviosAnswer everywhere [arcaviosBoltId board] True) board
    Spec.assertEqWith s "CR 701.23a the library's Lightning Bolt is in her hand, and nothing else" (printingsIn Zone.Hand S.alice resolved) [arcaviosBolt board]
    Spec.assertEqWith s "CR 701.23a the pool is untouched" (Map.elems (poolOf S.alice resolved)) [1, 1]
    Spec.assertEqWith s "the library was shuffled" (printingsIn Zone.Library S.alice resolved) (reverse (filter (/= arcaviosBolt board) (arcaviosLibrary board)))

-- alice with a Forest and an Island, Research // Development in hand -- {G}{U}
-- pays Research and nothing pays Development --
-- Goblin Piker and Cancel in her library, and five cards over four printings
-- outside the game: one more card than "up to four" takes, and two copies of
-- Sign in Blood. The pool printings are interned in list order, so the offer
-- lists them Sign in Blood, Psychic Miasma, Hoarding Dragon, Lightning Bolt.
-- Returns the pool's four printings and the library before the cast, top first.
researchBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId, (Printing.Printing, Printing.Printing, Printing.Printing, Printing.Printing), [Printing.Printing])
researchBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  research <- S.printingOf s registry "Research"
  piker <- S.printingOf s registry "Goblin Piker"
  cancel <- S.printingOf s registry "Cancel"
  signInBlood <- S.printingOf s registry "Sign in Blood"
  miasma <- S.printingOf s registry "Psychic Miasma"
  dragon <- S.printingOf s registry "Hoarding Dragon"
  bolt <- S.printingOf s registry "Lightning Bolt"
  let lands = S.landsFor island S.alice 1 (S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers))
      (board, researchId) = S.handOne research lands
      (_, withPiker) = S.addLibraryCard piker S.alice board
      (_, stocked) = S.addLibraryCard cancel S.alice withPiker
      intern (printing, n) (acc, gs) = let (printingId, gs2) = Game.intern printing gs in (acc <> [(printingId, n)], gs2)
      (entries, interned) = List.foldl' (flip intern) ([], stocked) [(signInBlood, 2), (miasma, 1), (dragon, 1), (bolt, 1)]
      stock p = p {Player.outsideTheGame = Map.fromList entries}
      ready = interned {GameState.players = Map.adjust stock S.alice (GameState.players interned)}
  pure (ready, researchId, (signInBlood, miasma, dragon, bolt), printingsIn Zone.Library S.alice ready)

-- alice casts one half of a split card by name (CR 709.3), paying from her lands.
castHalf :: String -> ObjectId.ObjectId -> Game ()
castHalf half oid = Cast.castSpell S.manaPerformer S.alice oid (CardName.MkCardName (Text.pack half)) Facing.FaceUp

-- Research's two answers: the cards it brings in, named by POSITION in the offer
-- so one position may be named twice, and a shuffle that reverses whatever it
-- is offered, which is what makes one visible.
researching :: [Int] -> Prompt.Prompt r -> r
researching picks p = case p of
  Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> Maybe.mapMaybe (\i -> Maybe.listToMaybe (drop i (NonEmpty.toList offered))) picks
  Prompt.Shuffle ids -> reverse ids
  _ -> S.identityAnswer p

-- Invasion of Arcavios's board: alice with five Islands and the Siege in hand;
-- a Lightning Bolt above two Islands in her library, so the filter has a card to
-- reject there and a shuffle of what is left is visible; a Sign in Blood in her
-- graveyard; and an Island and a Fog outside the game, the Island interned first
-- so an offer the filter did not narrow would lead with it.
data ArcaviosBoard = MkArcaviosBoard
  { arcaviosState :: GameState.GameState,
    arcaviosSpell :: ObjectId.ObjectId,
    arcaviosBoltId :: ObjectId.ObjectId,
    arcaviosBolt :: Printing.Printing,
    arcaviosSign :: Printing.Printing,
    arcaviosFog :: Printing.Printing,
    -- | Her library before the cast, top first.
    arcaviosLibrary :: [Printing.Printing]
  }

arcaviosBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m ArcaviosBoard
arcaviosBoard s registry = do
  island <- S.printingOf s registry "Island"
  invasion <- S.printingOf s registry "Invasion of Arcavios"
  bolt <- S.printingOf s registry "Lightning Bolt"
  signInBlood <- S.printingOf s registry "Sign in Blood"
  fog <- S.printingOf s registry "Fog"
  let (board, spellId) = S.handOne invasion (S.landsInPlay island 5)
      withIslands = stockLibrary island 2 S.alice board
      (boltId, stocked) = S.addLibraryCard bolt S.alice withIslands
      buried = snd (S.addGraveyardCard signInBlood S.alice stocked)
      intern printing (acc, gs) = let (printingId, gs2) = Game.intern printing gs in (acc <> [(printingId, 1)], gs2)
      (entries, interned) = List.foldl' (flip intern) ([], buried) [island, fog]
      stock p = p {Player.outsideTheGame = Map.fromList entries}
      ready = interned {GameState.players = Map.adjust stock S.alice (GameState.players interned)}
  pure (MkArcaviosBoard ready spellId boltId bolt signInBlood fog (printingsIn Zone.Library S.alice ready))

-- Cast the Siege, resolve it, and resolve its enters trigger.
resolveArcavios :: (forall r. Prompt.Prompt r -> r) -> ArcaviosBoard -> GameState.GameState
resolveArcavios answer board =
  let entered = S.runPure answer (arcaviosState board) (S.cast S.alice (arcaviosSpell board) >> Stack.resolveTop)
      settled = S.runPure answer entered Engine.settleForPriority
   in S.runPure answer settled Stack.resolveTop

-- The places she takes, the zone cards she finds, whether she takes everything
-- she is offered from outside the game, and a shuffle that reverses what it is
-- offered, so one is visible.
arcaviosAnswer :: [SearchPlace.SearchPlace] -> [ObjectId.ObjectId] -> Bool -> Prompt.Prompt r -> r
arcaviosAnswer places found takeOutside p = case p of
  Prompt.ChooseSearchZones {} -> Set.fromList places
  Prompt.Search {} -> found
  Prompt.ChooseFromOutsideTheGame _ _ offered _ _ -> if takeOutside then NonEmpty.toList offered else []
  Prompt.Shuffle ids -> reverse ids
  _ -> S.identityAnswer p

-- alice with five untapped lands, a Ring of Ma'rûf on the battlefield, two
-- library cards of one printing and one card of another outside the game, with
-- the Ring's ability activated and resolved when `arm` is True and untouched when
-- it is False. One builder for both legs, so the two boards differ in that alone.
--
-- Five lands for the {5}, with the {T} and the exile beside it; CR 113.6m is what
-- keeps that ability working on the battlefield alone. The ObjectId returned is
-- the Ring's, so a case can read what the cost did to it.
ringBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (GameState.GameState, ObjectId.ObjectId)
ringBoard plains ring stock outside arm =
  let (ringId, g1) = S.addPermanent ring S.alice (S.landsInPlay plains 5)
      g2 = stockLibrary stock 2 S.alice g1
      (outsideId, g3) = Game.intern outside g2
      pool p = p {Player.outsideTheGame = Map.singleton outsideId 1}
      g4 = g3 {GameState.players = Map.adjust pool S.alice (GameState.players g3)}
      final = case Face.activatedAbilities (S.combinedFace ring) of
        ability : _ | arm -> S.runPure S.identityAnswer g4 (Activate.activateAbility S.alice ringId ability >> Stack.resolveTop)
        _ -> g4
   in (final, ringId)
