{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers rule 702.139 end to end: Pawl.Types.Player's startingDeck, companion
-- and companionTaken with Pawl.Engine.Setup.createDeck that writes the first of
-- them, Pawl.Engine.Companion's condition, reveal round and special action, the
-- Pawl.Types.Keyword.Companion arm that carries the condition, and
-- Pawl.Types.Action's PutCompanionIntoHand with Pawl.Engine.Action's offer and
-- Pawl.Engine.Engine's arm for it.
--
-- Zirda, the Dawnwaker (M20 companion cycle) is the fixture: its condition is
-- "each permanent card in your starting deck has an activated ability", written
-- in card data as @Or [Not permanentCard, HasActivatedAbility]@.
--
-- THE BOARD SHAPE that makes the condition case discriminating. Both decks hold
-- Mountains, Birds of Paradise and Lightning Bolts; bob's holds one Doomed
-- Traveler and alice's does not, and that is the whole difference between them.
-- Each card is a control of its own:
--
-- \* the Mountain is CR 305.6's intrinsic ability, printed on no card, so an
--   implementation reading only Face.activatedAbilities rejects every real deck;
-- \* Birds of Paradise's only activated ability is a MANA ability, so an
--   implementation reading Filter.HasNonManaActivatedAbility instead rejects
--   alice's deck too -- which is rule 605.1a's exclusion, and Zirda does not
--   write it;
-- \* the Lightning Bolt is not a permanent card, so an implementation dropping
--   the condition's @Not@ disjunct rejects both decks;
-- \* the Doomed Traveler has no activated ability at all, which is the one card
--   that is supposed to fail bob's deck.
module Pawl.CompanionSpec where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Companion as Companion
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.OutsideCard as OutsideCard
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Companion" $ do
  startingDeck s registry
  revealing s registry
  newGames s registry
  mainGameCompanion s registry
  specialAction s registry

-- CR 103.2b: reveal the first companion offered rather than declining, which is
-- what Pawl.Engine.Script.declining -- and so S.identityAnswer -- answers.
--
-- Answers only that prompt: every mulligan and every other pre-game choice falls
-- through to the declining base, so the reveal is the one thing this answerer
-- changes about a setup.
revealingAnswer :: Prompt.Prompt r -> r
revealingAnswer p = case p of
  Prompt.ChooseCompanion _ _ candidates -> Just (NonEmpty.head candidates)
  _ -> S.identityAnswer p

-- A deck of `cards` with Zirda set aside in the sideboard, mirrored to nobody --
-- the two seats' decks differ, which is what the condition cases turn on.
deckOf :: Printing.Printing -> [(Printing.Printing, Natural.Natural)] -> Deck.Deck
deckOf zirda cards =
  (Deck.fromCards (Map.fromListWith (+) cards))
    { Deck.sideboard = Map.singleton zirda 1
    }

-- The whole of CR 103, run for real: two decks, the sideboard set aside, the
-- reveal round, and CR 103.5's opening hands.
setup :: (forall r. Prompt.Prompt r -> r) -> Deck.Deck -> Deck.Deck -> GameState.GameState
setup answer aliceDeck bobDeck =
  snd
    ( Engine.runGamePure
        answer
        (Setup.emptyGame S.bothPlayers)
        (Setup.newGame S.performer ((S.alice, aliceDeck) NonEmpty.:| [(S.bob, bobDeck)]))
    )

-- The two decks the condition cases compare, differing in the Doomed Traveler
-- and in nothing else.
decks :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Printing.Printing, Deck.Deck, Deck.Deck)
decks s registry = do
  zirda <- S.printingOf s registry "Zirda, the Dawnwaker"
  mountain <- S.printingOf s registry "Mountain"
  birds <- S.printingOf s registry "Birds of Paradise"
  bolt <- S.printingOf s registry "Lightning Bolt"
  traveler <- S.printingOf s registry "Doomed Traveler"
  let shared = [(mountain, 20), (birds, 4), (bolt, 4)]
  pure (zirda, deckOf zirda shared, deckOf zirda ((traveler, 4) : shared))

companionOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe OutsideCard.OutsideCard
companionOf pid gs = Map.lookup pid (GameState.players gs) >>= Player.companion

startingDeckOf :: PlayerId.PlayerId -> GameState.GameState -> Map.Map PrintingId.PrintingId Natural.Natural
startingDeckOf pid gs =
  maybe Map.empty Player.startingDeck (Map.lookup pid (GameState.players gs))

startingDeck :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
startingDeck s registry = Spec.describe s "CR 103.2a the starting deck" $ do
  -- CR 103.2a: "those cards are set aside. After this happens, each player's deck
  -- is considered their starting deck." The sideboard is the discriminating half
  -- -- a reader that recorded Deck.cards <> Deck.sideboard, or that read the
  -- library after CR 103.5's draws, disagrees with both assertions below.
  Spec.it s "is the deck without the sideboard, and survives the opening draws" $ do
    (zirda, aliceDeck, _) <- decks s registry
    mountain <- S.printingOf s registry "Mountain"
    let gs = setup S.identityAnswer aliceDeck aliceDeck
        recorded = startingDeckOf S.alice gs
        idOf printing = Map.lookup printing (GameState.printingIds gs)
    Spec.assertEqWith s "CR 103.2a: the twenty Mountains are in it" (idOf mountain >>= \i -> Map.lookup i recorded) (Just 20)
    Spec.assertEqWith s "CR 103.2a: and the sideboard's Zirda is not" (idOf zirda >>= \i -> Map.lookup i recorded) Nothing
    Spec.assertEqWith s "CR 400.11a: Zirda is outside the game instead" (fmap (\i -> Map.findWithDefault 0 i (maybe Map.empty Player.outsideTheGame (Map.lookup S.alice (GameState.players gs)))) (idOf zirda)) (Just 1)
    Spec.assertEqWith s "CR 103.5's seven draws did not shrink it" (sum (Map.elems recorded)) 28

  -- CR 702.139b's second sentence: "in a Commander game, this is also before
  -- you've set aside your commander". So the commander is IN the starting deck
  -- even though CR 903.6 starts it in the command zone -- the one place this
  -- field parts from what createDeck dealt into the library.
  Spec.it s "CR 702.139b counts the commander, which never reached the library" $ do
    (zirda, aliceDeck, _) <- decks s registry
    birds <- S.printingOf s registry "Birds of Paradise"
    let commanderDeck = aliceDeck {Deck.commander = Set.singleton birds}
        gs = setup S.identityAnswer commanderDeck aliceDeck
        idOf printing = Map.lookup printing (GameState.printingIds gs)
        recorded = startingDeckOf S.alice gs
    Spec.assertEqWith s "CR 702.139b: the fifth Birds is the commander" (idOf birds >>= \i -> Map.lookup i recorded) (Just 5)
    Spec.assertEqWith s "CR 903.6: and bob, who designated none, has four" (idOf birds >>= \i -> Map.lookup i (startingDeckOf S.bob gs)) (Just 4)
    Spec.assertEqWith s "the sideboard is still out of it" (idOf zirda >>= \i -> Map.lookup i recorded) Nothing

revealing :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
revealing s registry = Spec.describe s "CR 103.2b the reveal" $ do
  -- CR 103.2b: "they may do so only if their deck fulfills the condition of that
  -- card's companion ability." The two seats hold the same sideboard and the same
  -- answerer, so the only thing that can decide the pair is CR 702.139a's
  -- condition read against CR 103.2a's starting deck.
  Spec.it s "CR 702.139a is offered to the deck that fulfills the condition and to no other" $ do
    (zirda, aliceDeck, bobDeck) <- decks s registry
    let gs = setup revealingAnswer aliceDeck bobDeck
        idOf printing = Map.lookup printing (GameState.printingIds gs)
    Spec.assertEqWith s "CR 702.139a: alice's every permanent card has an activated ability" (companionOf S.alice gs) (fmap OutsideCard.InPool (idOf zirda))
    Spec.assertEqWith s "CR 702.139a: bob's Doomed Traveler has none, so he may not reveal" (companionOf S.bob gs) Nothing
    Spec.assertEqWith s "CR 103.2b: the revealed card stays outside the game" (fmap (\i -> Map.findWithDefault 0 i (maybe Map.empty Player.outsideTheGame (Map.lookup S.alice (GameState.players gs)))) (idOf zirda)) (Just 1)

  -- CR 702.122a: crew is an activated ability of a Vehicle card, and CR 113.6
  -- limits only where it FUNCTIONS, so the card HAS it in a library, as a card
  -- has cycling (CR 702.29b) on the battlefield. Consulate Dreadnought's only text is
  -- "Crew 6" (data/cards/consulate-dreadnought.json; Oracle text checked against
  -- api.scryfall.com, 2026-10-09), so a reader of a printed card that minted
  -- rule 702's battlefield abilities nowhere rejects alice's deck. bob's holds the
  -- same cards with Doomed Travelers in the Dreadnoughts' place.
  Spec.it s "CR 702.122a a Vehicle whose one activated ability is crew fulfills it" $ do
    (zirda, aliceDeck, bobDeck) <- decks s registry
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    let vehicles = aliceDeck {Deck.cards = Map.insert dreadnought 4 (Deck.cards aliceDeck)}
        gs = setup revealingAnswer vehicles bobDeck
        idOf printing = Map.lookup printing (GameState.printingIds gs)
    Spec.assertEqWith s "CR 702.139a: alice's four Dreadnoughts have crew, so she reveals Zirda" (companionOf S.alice gs) (fmap OutsideCard.InPool (idOf zirda))
    Spec.assertEqWith s "CR 702.139a: bob's Travelers in their place have nothing" (companionOf S.bob gs) Nothing

  -- CR 103.2b's "if any players WISH to reveal": declining is an answer, and the
  -- default one. The board is alice's from the case above, so the only difference
  -- is what the answerer said.
  Spec.it s "CR 103.2b a player who declines reveals nothing" $ do
    (_, aliceDeck, _) <- decks s registry
    let revealed = setup revealingAnswer aliceDeck aliceDeck
        declined = setup S.identityAnswer aliceDeck aliceDeck
    Spec.assertBool s (Maybe.isJust (companionOf S.alice revealed)) "the same deck can reveal"
    Spec.assertEqWith s "CR 103.2b: and declining is an answer" (companionOf S.alice declined) Nothing

-- CR 727.1 / 729.2: a restart and a subgame each start a new game following rule
-- 103, so CR 103.2a's starting deck is taken again from the cards the new game is
-- built from and CR 103.2b's reveal is put again.
--
-- Every board starts from `setup`'s real CR 103 over alice's Zirda-fulfilling
-- deck, and the pairs differ in ONE Doomed Traveler: where it sits, or whether it
-- is there at all.
newGames :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
newGames s registry = Spec.describe s "CR 727.1 / 729.2 a new game" $ do
  -- CR 727.2 involves every card alice owns that was in the game, a card a wish
  -- brought in among them; the Traveler on the battlefield stands for that card.
  -- So the restarted game's deck holds a permanent card with no activated
  -- ability, and CR 103.2b no longer lets her reveal Zirda.
  Spec.it s "CR 727.2 a restart reads its starting deck again and re-asks the reveal" $ do
    (zirda, aliceDeck, _) <- decks s registry
    traveler <- S.printingOf s registry "Doomed Traveler"
    let revealed = setup revealingAnswer aliceDeck aliceDeck
        restart gs = S.runPure revealingAnswer gs (Setup.restartGame S.performer Set.empty S.alice)
        changed = restart (snd (S.addPermanent traveler S.alice revealed))
        unchanged = restart revealed
        idOf gs printing = Map.lookup printing (GameState.printingIds gs)
    Spec.assertEqWith s "CR 103.2b: the new deck holds the Traveler, so Zirda is not her companion" (companionOf S.alice changed) Nothing
    Spec.assertEqWith s "CR 103.2b: and without it she reveals Zirda again" (companionOf S.alice unchanged) (fmap OutsideCard.InPool (idOf unchanged zirda))
    Spec.assertEqWith s "CR 103.2a: the Traveler is in the new starting deck" (idOf changed traveler >>= \i -> Map.lookup i (startingDeckOf S.alice changed)) (Just 1)
    Spec.assertEqWith s "CR 103.2a: which is all twenty-nine of her cards" (sum (Map.elems (startingDeckOf S.alice changed))) 29

  -- CR 702.139b counts the commander, which CR 903.6 holds back from the new
  -- library exactly as it did from the first.
  Spec.it s "CR 702.139b a restart's starting deck counts the commander" $ do
    (_, aliceDeck, _) <- decks s registry
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let built = setup S.identityAnswer (aliceDeck {Deck.commander = Set.singleton shimatsu}) aliceDeck
        restarted = S.runPure S.identityAnswer built (Setup.restartGame S.performer Set.empty S.alice)
        idOf printing = Map.lookup printing (GameState.printingIds restarted)
    Spec.assertEqWith s "CR 702.139b: Shimatsu is in the new starting deck" (idOf shimatsu >>= \i -> Map.lookup i (startingDeckOf S.alice restarted)) (Just 1)
    Spec.assertEqWith s "CR 903.6: having begun the new game in the command zone" (length (Game.zoneMembers Zone.Command S.alice restarted)) 1

  -- CR 103.2b's "if any players WISH to reveal" is asked anew: a player who
  -- declined in the restarted game may reveal in the new one.
  Spec.it s "CR 727.1 a player who declined may reveal after a restart" $ do
    (zirda, aliceDeck, _) <- decks s registry
    let declined = setup S.identityAnswer aliceDeck aliceDeck
        restarted = S.runPure revealingAnswer declined (Setup.restartGame S.performer Set.empty S.alice)
    Spec.assertEqWith s "CR 103.2b: she reveals Zirda in the new game" (companionOf S.alice restarted) (fmap OutsideCard.InPool (Map.lookup zirda (GameState.printingIds restarted)))
    Spec.assertEqWith s "having revealed nothing in the first" (companionOf S.alice declined) Nothing

  -- CR 729.2: the subgame's deck is the main-game LIBRARY and nothing else, so a
  -- Traveler in alice's main-game hand stays out of it while one in her library
  -- is in it. CR 729.1b: the main game's companion is untouched by the subgame.
  Spec.it s "CR 729.2 a subgame's starting deck is the main-game library" $ do
    (zirda, aliceDeck, _) <- decks s registry
    traveler <- S.printingOf s registry "Doomed Traveler"
    let parentOf place = snd (place traveler S.alice (setup revealingAnswer aliceDeck aliceDeck))
        subOf parent = S.runPure revealingAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty)
        inLibrary = parentOf S.addLibraryCard
        inHand = parentOf S.addHandCard
        idOf gs printing = Map.lookup printing (GameState.printingIds gs)
        hidden = subOf inLibrary
    Spec.assertEqWith s "CR 103.2b: a Traveler in her main-game library bars Zirda in the subgame" (companionOf S.alice hidden) Nothing
    Spec.assertEqWith s "CR 729.2: one in her main-game hand does not" (companionOf S.alice (subOf inHand)) (fmap OutsideCard.InPool (idOf inHand zirda))
    Spec.assertEqWith s "CR 729.1b: the main game still has Zirda as her companion afterwards" (companionOf S.alice (Setup.funnelBack hidden inLibrary)) (fmap OutsideCard.InPool (idOf inLibrary zirda))

-- CR 729.4: inside a subgame every main-game object is outside the game, so a
-- companion card alice owns in her main-game HAND is one CR 103.2b lets her
-- reveal. Her sideboard is empty, so that object is the only candidate there is.
-- CR 116.2g's {3} then brings that very object in, which CR 729.4a takes out of
-- the main game once the subgame ends.
--
-- A FACE-DOWN main-game Zirda beside it is the control: CR 708.2 leaves it no
-- companion ability, so it is never offered.
mainGameCompanion :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
mainGameCompanion s registry = Spec.describe s "CR 729.4 a subgame's companion" $ do
  Spec.it s "CR 729.4 a main-game companion card may be revealed and taken in a subgame" $ do
    zirda <- S.printingOf s registry "Zirda, the Dawnwaker"
    mountain <- S.printingOf s registry "Mountain"
    birds <- S.printingOf s registry "Birds of Paradise"
    let built = setup S.identityAnswer (Deck.fromCards (Map.fromList [(mountain, 20), (birds, 4)])) (Deck.fromCards (Map.fromList [(mountain, 24)]))
        -- The face-down one FIRST, so it has the lower id and would head the
        -- offer if it were offered at all.
        (faceDown, onField) = S.addPermanent zirda S.alice built
        (zirdaInHand, parent) = S.addHandCard zirda S.alice onField
        hidden = parent {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.faceDown FaceDownReason.Manifested}) faceDown (GameState.objects parent)}
        sub = S.runPure revealingAnswer (Setup.subgameStateFrom S.alice hidden) (Setup.startGameFromCards S.performer Set.empty)
        withLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) sub [1 :: Int, 2, 3]
        ready = withLands {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
        taken = S.runPure S.identityAnswer ready (Companion.take S.manaPerformer S.alice)
        zirdasInHand gs = filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (CardName.MkCardName (Text.pack "Zirda, the Dawnwaker"))) (Game.zoneMembers Zone.Hand S.alice gs)
    Spec.assertEqWith s "CR 103.2b / 729.4: she reveals the Zirda in her main-game hand, not the face-down one" (companionOf S.alice sub) (Just (OutsideCard.InAnotherGame zirdaInHand))
    Spec.assertEqWith s "CR 116.2g: paying {3} puts it into her subgame hand" (length (zirdasInHand taken)) 1
    Spec.assertBool s (List.notElem zirdaInHand (Game.zoneMembers Zone.Hand S.alice (Setup.applyCrossings taken hidden))) "CR 729.4a: and the main game has lost it"

-- alice with `lands` Mountains untapped in her precombat main phase holding
-- priority, TWO Zirdas outside the game, and `chosen` saying whether CR 103.2b's
-- reveal named it. bob is seated and has neither.
--
-- TWO copies, which CR 100.4a allows and which is what makes CR 116.2g's
-- once-per-game clause observable at all: with one copy the pool guard in
-- Companion.canTake refuses the second action whatever the flag says, so a board
-- holding one cannot tell the two conjuncts apart.
--
-- Hand-built rather than played on from `setup` above, because CR 116.2g's window
-- is a main phase with an empty stack and a real setup ends before the first
-- turn (CR 103.8).
actionBoard :: Printing.Printing -> Printing.Printing -> Bool -> Int -> (PrintingId.PrintingId, GameState.GameState)
actionBoard mountain zirda chosen lands =
  let gs0 = S.landsInPlay mountain lands
      (zirdaId, gs1) = Game.intern zirda gs0
      stock p =
        p
          { Player.outsideTheGame = Map.singleton zirdaId 2,
            Player.companion = if chosen then Just (OutsideCard.InPool zirdaId) else Nothing
          }
   in ( zirdaId,
        gs1
          { GameState.players = Map.adjust stock S.alice (GameState.players gs1),
            GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

specialAction :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
specialAction s registry = Spec.describe s "CR 116.2g the special action" $ do
  -- CR 116.2g's four clauses, one board apiece and each differing from the
  -- offered board in exactly one thing: no companion chosen, the wrong phase, one
  -- Mountain short of {3}.
  Spec.it s "is offered only to a player who chose one, at sorcery speed, who can pay {3}" $ do
    mountain <- S.printingOf s registry "Mountain"
    zirda <- S.printingOf s registry "Zirda, the Dawnwaker"
    let (_, offered) = actionBoard mountain zirda True 3
        (_, unchosen) = actionBoard mountain zirda False 3
        (_, poor) = actionBoard mountain zirda True 2
        upkeep = offered {GameState.phase = Phase.Beginning BeginningStep.Upkeep}
        opponents = offered {GameState.activePlayer = S.bob}
    Spec.assertBool s (List.elem Action.Type.PutCompanionIntoHand (Action.legalActions S.alice offered)) "CR 116.2g: three Mountains and a chosen companion"
    Spec.assertBool s (List.notElem Action.Type.PutCompanionIntoHand (Action.legalActions S.alice unchosen)) "CR 103.2b: nobody who revealed nothing may take it"
    Spec.assertBool s (List.notElem Action.Type.PutCompanionIntoHand (Action.legalActions S.alice poor)) "CR 116.2g: two Mountains do not pay {3}"
    Spec.assertBool s (List.notElem Action.Type.PutCompanionIntoHand (Action.legalActions S.alice upkeep)) "CR 116.2g: nor in an upkeep step"
    Spec.assertBool s (List.notElem Action.Type.PutCompanionIntoHand (Action.legalActions S.alice opponents)) "CR 116.2g: nor on an opponent's turn"

  -- CR 116.2g / 702.139a: "put that card from outside the game into their hand".
  -- The HAND, which is the assertion this unit exists to prove -- not the
  -- battlefield, not the library, not exile.
  --
  -- CR 702.139c is the second half: the pool is spent, so the card is in the game
  -- for good, and CR 116.2g's once-per-game clause is what the third assertion
  -- reads.
  Spec.it s "CR 702.139a paying {3} puts the companion into its owner's hand, once" $ do
    mountain <- S.printingOf s registry "Mountain"
    zirda <- S.printingOf s registry "Zirda, the Dawnwaker"
    -- SIX Mountains, where the offer case above needs three: the second attempt
    -- has to be able to pay {3} again, or the cost gate refuses it and CR 116.2g's
    -- once-per-game clause is the conjunct no assertion reaches.
    let (zirdaId, offered) = actionBoard mountain zirda True 6
        after = S.runPure S.identityAnswer offered (Companion.take S.manaPerformer S.alice)
        again = S.runPure S.identityAnswer after (Companion.take S.manaPerformer S.alice)
        named oid gs = fmap S.nameOf (Game.cardOf oid gs) == Just (CardName.MkCardName (Text.pack "Zirda, the Dawnwaker"))
        inHand gs = filter (`named` gs) (Game.zoneMembers Zone.Hand S.alice gs)
        poolOf gs = Map.findWithDefault 0 zirdaId (maybe Map.empty Player.outsideTheGame (Map.lookup S.alice (GameState.players gs)))
    Spec.assertEqWith s "CR 116.2g: the companion is in alice's HAND" (length (inHand after)) 1
    Spec.assertEqWith s "CR 702.139c: and one copy is out of the pool, so it is in the game for good" (poolOf after) 1
    Spec.assertEqWith s "CR 116.2g: a second attempt puts nothing else in the hand" (length (inHand again)) 1
    Spec.assertBool s (List.notElem Action.Type.PutCompanionIntoHand (Action.legalActions S.alice after)) "CR 116.2g: and the action is not offered a second time"
    Spec.assertEqWith s "CR 116.2g: which is the flag, read directly" (fmap Player.companionTaken (Map.lookup S.alice (GameState.players after))) (Just True)
