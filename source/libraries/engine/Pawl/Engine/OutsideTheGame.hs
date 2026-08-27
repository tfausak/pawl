-- | CR 400.11: the cards a player owns that are not in any of the game's zones,
-- and the one road that brings one in.
--
-- Outside the game is NOT a zone (CR 400.11), so this module holds no zone and
-- mints no object until something takes a card out of the pool.
-- Pawl.Types.Player's outsideTheGame is the pool; CR 103.2a's sideboard is what
-- fills it, through Pawl.Engine.Setup.createDeck.
--
-- Pawl.Engine.Dungeon is the sibling that does the same for CR 309.2's dungeon
-- cards, which are outside the game too and are deliberately NOT in this pool:
-- rule 309.2 keeps them out of deck and sideboard both, CR 701.49a chooses among
-- them by a rule of its own rather than by a card's filter, and CR 309.2d forbids
-- anything else from bringing one in. Pawl.Types.Player says so at the field.
--
-- What this module does NOT decide is which cards are eligible: the Filter comes
-- from the card, and Pawl.Engine.Resolve's arm passes it through without asking
-- what effect produced it.
module Pawl.Engine.OutsideTheGame where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.OutsideCard as OutsideCard
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 400.11c: which of a player's cards outside the game an effect's Filter
-- admits, in interning order.
--
-- The Filter is evaluated against the PRINTED FACE, which is the whole of what a
-- card outside the game has: CR 604.3 makes a characteristic-defining ability the
-- one thing that functions out there, and Projection.viewOfCard reads it off the
-- face. No object exists to project (CR 400.11), so there is nothing else to ask.
--
-- A printing whose count has fallen to zero is not offered. `take` below deletes
-- such an entry rather than leaving it at zero, so the guard is a belt on top of
-- braces -- and cheap enough to keep, since a caller assembling this map by hand
-- would otherwise offer a card that is no longer out there.
--
-- Every candidate here is an OutsideCard.InPool: this only reaches CR 103.2a's
-- sideboard pool. CR 729.4's main-game objects (GameState.outsideObjects) are
-- not read yet (gap #152) -- wiring that in is a later unit's job, not this one's.
eligible :: Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> PlayerId -> GameState.GameState -> [OutsideCard.OutsideCard]
eligible predicate source pid gs =
  let pool = maybe Map.empty Player.outsideTheGame (Map.lookup pid (GameState.players gs))
      context = Filter.contextFor (Just pid) (Just source)
      admits printingId = case Game.cardOfPrinting printingId gs of
        Nothing -> False
        Just card -> Filter.matches context (Projection.viewOfCard (Game.resolveFaceFor Nothing card)) predicate
   in [OutsideCard.InPool printingId | (printingId, n) <- Map.toAscList pool, n > 0, admits printingId]

-- CR 400.11c \/ 701.20a: reveal a card this player owns from outside the game
-- matching the Filter and put it into their hand -- Burning Wish's sentence.
--
-- The card is MINTED here, Pawl.Engine.Dungeon.enter's road: outside the game is
-- not a zone (CR 400.11), so no object stood for the card and the move into the
-- hand is not a zone change. It is inserted into the hand directly rather than
-- through Pawl.Engine.Event.changeZone for that reason -- a zone change would
-- announce a departure from a zone the card was never in.
--
-- The pool is SPENT, unlike Pawl.Engine.Dungeon.enter's supply: CR 400.11b keeps
-- a card brought in "in the game until the game ends", so a second Burning Wish
-- cannot find the same copy. A second COPY of the same printing survives the
-- decrement, which is why Player.outsideTheGame counts rather than remembering a
-- set.
--
-- CHOSEN, not targeted (CR 115.10a): CR 601.2c would have announced a target as
-- the spell was cast, and CR 400.11c lets nothing target a card out there.
-- FILTERED, NOT TRUSTED, Pawl.Engine.Dungeon.enter's posture: an answer naming a
-- printing this player does not own out there, or one the Filter does not admit,
-- falls back to the first offered.
--
-- A player with no eligible card reveals nothing and puts nothing into their
-- hand, which is CR 609.3's "if an effect attempts to do something impossible,
-- it does only as much as possible" -- and is why
-- this returns unit rather than the id: nothing about Burning Wish's sentence
-- reads the card back.
--
-- Not implemented: the reveal happens as the card ARRIVES in the hand rather than
-- before the move as the card prints it (#2450).
reveal :: Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> PlayerId -> Game ()
reveal predicate source pid = do
  gs0 <- State.get
  case NonEmpty.nonEmpty (eligible predicate source pid gs0) of
    Nothing -> pure ()
    Just offered -> do
      chosen <- case offered of
        only NonEmpty.:| [] -> pure only
        first NonEmpty.:| _ -> do
          answer <- Game.choose (Prompt.ChooseFromOutsideTheGame (Decide.deciderFor pid gs0) pid offered)
          pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
      -- Against the LIVE state and not gs0, Pawl.Engine.Dungeon.enter's care:
      -- Game.choose above wrote the answer into the transcript, and minting off
      -- the state from before the prompt would drop that.
      case chosen of
        OutsideCard.InPool printingId -> do
          oid <- State.state (bringIn pid printingId)
          Event.reveal RevealCause.Ordinary pid oid
        -- `eligible` above offers only OutsideCard.InPool this unit -- CR 729.4's
        -- main-game half is not wired in yet (gap #152), so this arm is unreached
        -- until that lands.
        OutsideCard.InAnotherGame _ -> pure ()

-- CR 400.11b: take one copy of this printing out of the player's pool and mint
-- the card into their hand. Split out from `reveal` above because it is the half
-- every other road into the game will want -- CR 727.2's restart (#135), CR
-- 707.13's copy created outside the game (#888) and a subgame reaching into the
-- supergame (#152) -- and none of those reveals anything.
bringIn :: PlayerId -> PrintingId.PrintingId -> GameState.GameState -> (ObjectId, GameState.GameState)
bringIn pid printingId gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printingId,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.bestowed = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
      -- One copy, not the entry: CR 100.2a's four-card limit is applied to the
      -- combined deck and sideboard (CR 100.4a), so copies of a card are COUNTED
      -- and a player who set aside two can be brought the second one later.
      spend n = if n <= 1 then Nothing else Just (n - 1)
      spent p = p {Player.outsideTheGame = Map.update spend printingId (Player.outsideTheGame p)}
      gs3 =
        Game.insertIntoZone
          Zone.Hand
          LibraryPosition.defaultValue
          pid
          oid
          gs2 {GameState.objects = Map.insert oid obj (GameState.objects gs2)}
   in (oid, gs3 {GameState.players = Map.adjust spent pid (GameState.players gs3)})
