-- Rule 702.139 in the one voice the rest of the engine cannot supply for itself:
-- CR 103.2b's pre-game reveal of a card whose condition this player's starting
-- deck fulfills, and CR 116.2g's special action that pays {3} to put that card
-- into their hand from outside the game.
--
-- Pawl.Engine.Foretell is the same module for rule 702.143 and this one is
-- written to its shape -- a gate, an offer, and a performer that pays before it
-- moves anything. What is different is the ZONE the card comes from: outside the
-- game is not one (CR 400.11), so no object stands for the companion and
-- Pawl.Engine.Event.bringIn is the door rather than a zone change.
--
-- THE INVARIANT: rule 702 is part of the rulebook, so reading Keyword.Companion
-- here is the same closed-half act as reading a Phase. This module never asks
-- which CARD is being revealed -- the condition is a Filter the card carries, and
-- `fulfilled` evaluates it through the same generic matcher a wish's filter goes
-- through.
module Pawl.Engine.Companion where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Turn as Turn
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.PrintingId (PrintingId)
import qualified Pawl.Types.Prompt as Prompt

-- CR 116.2g: what the special action costs -- "may pay {3} to put that card from
-- outside the game into their hand".
--
-- Minted here rather than read off Keyword.Companion, Pawl.Engine.Foretell's
-- `actionCost` reason: rule 116.2g fixes this amount for every printing, and the
-- keyword's own payload is the CONDITION rather than a price.
actionCost :: Cost Keyword
actionCost =
  Cost.Type.MkCost
    { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]),
      Cost.Type.components = []
    }

-- CR 702.139a's condition, read off a printing's printed face, or Nothing for a
-- printing with no companion ability.
--
-- The PRINTED face, Pawl.Engine.Event.eligible's reason: a card outside the game
-- has no object to project (CR 400.11), and rule 702.139a says the ability
-- "functions outside the game", so there is nothing else to read it from.
conditionOf :: PrintingId -> GameState -> Maybe (Filter.Type.Filter Keyword)
conditionOf printingId gs = do
  card <- Game.cardOfPrinting printingId gs
  let face = Game.resolveFaceFor Nothing card
  Maybe.listToMaybe [predicate | Keyword.Companion predicate <- Foldable.toList (Face.keywords face)]

-- CR 702.139a: does this player's STARTING DECK fulfill the condition? Every card
-- in it must match, which is what makes one Filter enough to state a condition
-- written "each [subject] card ... [requirement]" -- the implication is an @Or@.
--
-- Player.startingDeck and never the library, which is rule 702.139b's whole
-- point: the deck as it stood once the sideboard was set aside and before the
-- commander was, not what is left after CR 103.5's opening hands.
--
-- Matched through a BARE Filter.contextFor with no source and no slot bindings,
-- Pawl.Engine.Event.eligible's posture: nothing is resolving before the game
-- begins, so no slot could be bound and no object could be the source.
--
-- Against the PRINTED face, though a starting-deck card does have a library
-- object by the time this runs: CR 103.2b's moment is before the first turn, so
-- no CR 611 continuous effect exists to have changed anything, and the printed
-- face is the whole of what a projection could answer. It is also the only
-- reading available for a Commander game's commander, which CR 702.139b counts
-- and CR 903.6 has already put in the command zone.
fulfilled :: Filter.Type.Filter Keyword -> PlayerId -> GameState -> Bool
fulfilled predicate pid gs =
  let context = Filter.contextFor (Game.teams gs) (Just pid) Nothing
      deck = maybe Map.empty Player.startingDeck (Map.lookup pid (GameState.players gs))
      admits printingId = case Game.cardOfPrinting printingId gs of
        Nothing -> False
        Just card -> Filter.matches context (Projection.viewOfCard (Game.resolveFaceFor Nothing card)) predicate
   in all admits [printingId | (printingId, n) <- Map.toAscList deck, n > 0]

-- CR 103.2b: the cards this player may reveal as a companion -- the ones they own
-- outside the game that have a companion ability whose condition their starting
-- deck fulfills, in interning order.
--
-- Player.outsideTheGame and not the sideboard field on the deck, because rule
-- 103.2b runs AFTER rule 103.2a has set the sideboard aside; Pawl.Engine.Setup.createDeck
-- is what interns it into the pool this reads.
revealable :: PlayerId -> GameState -> [PrintingId]
revealable pid gs =
  let pool = maybe Map.empty Player.outsideTheGame (Map.lookup pid (GameState.players gs))
      admits printingId = case conditionOf printingId gs of
        Nothing -> False
        Just predicate -> fulfilled predicate pid gs
   in [printingId | (printingId, n) <- Map.toAscList pool, n > 0, admits printingId]

-- CR 103.2b: put the reveal to this player, and record what they revealed.
--
-- OPTIONAL, which is the rule's own "if any players WISH to reveal": the prompt
-- is raised whenever there is anything to reveal, since declining is a real
-- choice even where only one card is offered -- unlike CR 309.2a's dungeon, where
-- a lone candidate leaves nothing to decide.
--
-- FILTERED, NOT TRUSTED, Pawl.Engine.Dungeon.enter's posture: an answer naming a
-- printing this player may not reveal is read as declining.
--
-- The card is NOT taken out of Player.outsideTheGame, which is rule 103.2b's last
-- sentence: "the revealed card remains outside the game". CR 116.2g is what
-- spends it.
reveal :: PlayerId -> Game ()
reveal pid = do
  gs0 <- State.get
  case NonEmpty.nonEmpty (revealable pid gs0) of
    Nothing -> pure ()
    Just offered -> do
      answer <- Game.choose (Prompt.ChooseCompanion (Decide.deciderFor pid gs0) pid offered)
      let chosen = case answer of
            Nothing -> Nothing
            Just printingId ->
              if List.elem printingId (NonEmpty.toList offered) then Just printingId else Nothing
      State.modify' $ \gs ->
        gs
          { GameState.players =
              Map.adjust (\p -> p {Player.companion = chosen}) pid (GameState.players gs)
          }

-- CR 116.2g: may this player take the companion special action right now?
--
-- Four conjuncts, one per clause of the rule: they chose a companion, they have
-- not taken the action yet this game, the window is a main phase of their own
-- turn with the stack empty (Turn.sorcerySpeedWindow, which is CR 116.2k's
-- window too), and the {3} is payable.
--
-- A FIFTH that rule 116.2g leaves implicit: the card is still outside the game.
-- It can only fail where Pawl.Engine.Setup.resetPlayers has given the action back
-- -- CR 727.1's restart and CR 729.2's subgame, each a new game -- while the pool
-- it would spend was already spent in the game before.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f
-- adjustments, Pawl.Engine.Foretell.canForetell's reason: that rule totals the
-- cost of a spell being cast or an ability being activated, and a special action
-- is neither (#90).
--
-- The PRIORITY clause has no conjunct, for the reason CR 116.2a's land play has
-- none: legalActions is asked only of the priority holder.
canTake :: PlayerId -> GameState -> Bool
canTake pid gs = case Map.lookup pid (GameState.players gs) of
  Nothing -> False
  Just player -> case Player.companion player of
    Nothing -> False
    Just printingId ->
      not (Player.companionTaken player)
        && Map.findWithDefault 0 printingId (Player.outsideTheGame player) > 0
        && Turn.sorcerySpeedWindow pid gs
        && Cost.canPay pid (GameState.nextObjectId gs) actionCost gs

-- CR 116.2g / 702.139a: pay {3} and put the companion into this player's hand.
--
-- REJECT-NOT-REPAIR, and the payment first, Pawl.Engine.Foretell.foretell's
-- reasons: a payment that fails restores the state from before it was attempted
-- and the card stays outside the game.
--
-- The cost is priced against an id taken off the supply and given to no object.
-- There is no source object to name, outside the game being no zone (CR 400.11),
-- and rule 116.2g fixes the amount, so nothing a source could contribute is read;
-- the id is a stand-in that makes every object lookup answer Nothing. TAKEN off
-- the supply rather than peeked at, unlike canTake's, because a mana ability
-- activated to pay this cost may mint an object, and the peeked id is exactly the
-- one such an object would be given.
--
-- Event.bringIn and not a zone change, its own haddock's reason: no object stood
-- for the card, so the card is MINTED into the hand. That call spends the pool
-- entry, which is CR 702.139c -- the card "remains in the game until the game
-- ends", so a second action could not find it again even if the flag below did
-- not stop one.
take :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> Game ()
take perform pid = do
  before <- State.get
  if not (canTake pid before)
    then pure ()
    else case Map.lookup pid (GameState.players before) >>= Player.companion of
      Nothing -> pure ()
      Just printingId -> do
        -- CR 118.13c, Pawl.Engine.Foretell.foretell's announcement and for its
        -- reasons. CR 116.2g fixes this cost at {3}, so no symbol here is ever
        -- payable in multiple ways and no prompt is ever raised.
        noSource <- State.state Game.freshObjectId
        (announced, _) <- Cost.announce PaymentSubject.ForNeither ManaSpending.AsProduced pid noSource pure actionCost
        payment <- Cost.pay perform PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced pid noSource announced
        case payment of
          -- CR 733.1's last sentence, Pawl.Engine.Foretell.foretell's reason: a
          -- mana ability tapped in the window this payment opened may have
          -- shuffled or revealed, and this reject-not-repair restore must not
          -- undo that too.
          Payment.Unpaid -> Cost.restoreKeepingLibraryActions before
          Payment.Paid _ -> do
            State.modify' (snd . Event.bringIn pid printingId)
            State.modify' $ \gs ->
              gs
                { GameState.players =
                    Map.adjust (\p -> p {Player.companionTaken = True}) pid (GameState.players gs)
                }
