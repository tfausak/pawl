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
import qualified Pawl.Types.GameState as GameState
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

companionOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe PrintingId.PrintingId
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
    let commanderDeck = aliceDeck {Deck.commander = Just birds}
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
    Spec.assertEqWith s "CR 702.139a: alice's every permanent card has an activated ability" (companionOf S.alice gs) (idOf zirda)
    Spec.assertEqWith s "CR 702.139a: bob's Doomed Traveler has none, so he may not reveal" (companionOf S.bob gs) Nothing
    Spec.assertEqWith s "CR 103.2b: the revealed card stays outside the game" (fmap (\i -> Map.findWithDefault 0 i (maybe Map.empty Player.outsideTheGame (Map.lookup S.alice (GameState.players gs)))) (idOf zirda)) (Just 1)

  -- CR 103.2b's "if any players WISH to reveal": declining is an answer, and the
  -- default one. The board is alice's from the case above, so the only difference
  -- is what the answerer said.
  Spec.it s "CR 103.2b a player who declines reveals nothing" $ do
    (_, aliceDeck, _) <- decks s registry
    let revealed = setup revealingAnswer aliceDeck aliceDeck
        declined = setup S.identityAnswer aliceDeck aliceDeck
    Spec.assertBool s (Maybe.isJust (companionOf S.alice revealed)) "the same deck can reveal"
    Spec.assertEqWith s "CR 103.2b: and declining is an answer" (companionOf S.alice declined) Nothing

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
            Player.companion = if chosen then Just zirdaId else Nothing
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
