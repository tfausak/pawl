-- | CR 701.48, learn: "you may discard a card. If you do, draw a card. If you
-- didn't discard a card, you may reveal a Lesson card you own from outside the
-- game and put it into your hand", and the whole of the keyword action.
--
-- Pawl.Engine.Forage's sibling, and standing on the same ground: rule 701 is a
-- keyword-action rule exactly as rule 702 is a keyword rule, so the procedure
-- lives in the engine rather than in card data. The closed\/open invariant
-- forbids the rules core casing on an EFFECT's identity, and nothing here does
-- -- Pawl.Engine.Resolve.Effect's Effect.Learn arm calls in without saying which
-- effect it is.
--
-- Rule 701.48 has no "whenever a player learns", so there is no GameEvent here.
-- Scryfall @o:"learns"@, 2026-09-18, returns no card that watches for one; a
-- printing worded that way is what would need it.
module Pawl.Engine.Learn where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import Pawl.Types.Game (Game)
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LearnMode as LearnMode
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.OutsideDestination as OutsideDestination
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

-- | CR 701.48a's third sentence, as the instruction Pawl.Engine.Event.bringInto
-- already performs: a Lesson card this player owns from outside the game,
-- revealed and put into their hand.
--
-- CR 205.3: Lesson is a subtype, so the quality is HasSubtype and not a card
-- type -- an Instant -- Lesson (Airbending Lesson) and a Sorcery -- Lesson are
-- each as much a Lesson as the other.
--
-- The reveal rides along because rule 701.48a prints it, which is the axis
-- Pawl.Types.FromOutsideTheGame carries for Burning Wish's sentence against
-- Death Wish's.
lessonFromOutside :: FromOutsideTheGame.FromOutsideTheGame
lessonFromOutside =
  FromOutsideTheGame.MkFromOutsideTheGame
    { FromOutsideTheGame.destination = OutsideDestination.Hand,
      FromOutsideTheGame.filter = Filter.HasSubtype Subtype.Lesson :: Filter.Filter Keyword.Keyword,
      FromOutsideTheGame.reveal = True
    }

-- | CR 701.48a: the learner takes one of the rule's two branches, or neither.
--
-- The first ObjectId is the object the prompts name -- the spell or ability
-- resolving. The second is what the Filter is matched against as
-- Pawl.Engine.Event.bringInto scans the pool.
--
-- THREE PROMPTS at most, each raised only where the rules leave something to
-- ask. Which branch, wherever either can be carried out -- declining is an
-- outcome rule 701.48a states, so a lone branch is still a question. Which card
-- to discard, where the hand holds more than one. Which Lesson, which
-- Pawl.Engine.Event.bringInto asks for itself.
--
-- A branch that cannot be carried out is not offered: a player holding no card
-- cannot discard one, and a player owning no Lesson out there has nothing to
-- reveal. That is CR 608.2d's "the player can't choose an option that's illegal
-- or impossible" rather than anything rule 701.48 states.
--
-- FILTERED, NOT TRUSTED, Pawl.Engine.Forage's posture -- but falling back to
-- DECLINING rather than to the offered set's front, because rule 701.48a's "you
-- may" makes declining a legal answer on every board this reaches, where forage
-- is mandatory once canForage holds.
--
-- CR 701.48a's "if you do, draw a card" is sequenced rather than conditional
-- here: the discard below cannot fail once the card was chosen off the hand, so
-- the draw always follows it.
learn :: PlayerId -> ObjectId -> ObjectId -> Game ()
learn pid resolving source = do
  gs <- State.get
  let hand = Game.zoneMembers Zone.Hand pid gs
      lessons = Event.eligible (FromOutsideTheGame.filter lessonFromOutside) source pid gs
      offered =
        [LearnMode.DiscardAndDraw | not (null hand)]
          <> [LearnMode.TakeLesson | not (null lessons)]
  mode <- case NonEmpty.nonEmpty offered of
    Nothing -> pure Nothing
    Just modes -> do
      answer <- Game.choose (Prompt.ChooseLearn (Decide.deciderFor pid gs) pid resolving modes)
      pure (if all (`List.elem` NonEmpty.toList modes) answer then answer else Nothing)
  case mode of
    Nothing -> pure ()
    Just LearnMode.TakeLesson -> Event.bringInto lessonFromOutside source pid
    Just LearnMode.DiscardAndDraw -> case hand of
      [] -> pure ()
      first : rest -> do
        discarded <- case rest of
          [] -> pure first
          second : more -> do
            let cards = first NonEmpty.:| (second : more)
            answer <- Game.choose (Prompt.ChooseCardInHand (Decide.deciderFor pid gs) pid resolving cards)
            pure (if List.elem answer (NonEmpty.toList cards) then answer else first)
        -- CR 701.9a, through the one funnel a discard goes through, so anything
        -- watching for a discard sees it.
        Event.discard DiscardCause.Ordinary pid discarded
        -- CR 121.1, through the draw funnel, so CR 121.3's empty library and CR
        -- 614's draw replacements both get their opportunity.
        Event.drawCard pid
