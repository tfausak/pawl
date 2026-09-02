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

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.FaceDownState as FaceDownState
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.OutsideCard as OutsideCard
import qualified Pawl.Types.OutsideObject as OutsideObject
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RevealCause as RevealCause
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
-- The Context is a BARE Filter.contextFor, carrying no slot bindings even though
-- a resolution is in flight. Honest here rather than #2141's silence: CR 400.11c
-- keeps a spell or ability from affecting a card outside the game, so no slot of
-- the resolution can name one, and the candidate view below is a printed FACE
-- with no `identity` for Filter.IsBound to compare in any case. Pawl.CardSpec's
-- "CR 400.11c no card asks IsBound in a wish's filter" is what keeps a card out
-- of the position; Pawl.OutsideTheGameSpec's "CR 400.11c a wish's filter cannot
-- see what the resolution bound" proves the atom answers nothing here.
--
-- Two sources feed this: CR 103.2a's sideboard pool (OutsideCard.InPool) and,
-- when this game is a subgame, CR 729.4's main-game objects held in
-- GameState.outsideObjects (OutsideCard.InAnotherGame). A face-up entry of
-- either kind is read through the PRINTED FACE, since CR 729.1b gives a
-- main-game effect no meaning inside the subgame -- a main-game object is read
-- as its card, not as its (subgame-invisible) projected characteristics, which
-- is why OutsideObject carries no characteristics of its own.
--
-- A FACE-DOWN main-game object is the one exception, and CR 708.2 is why: "face-
-- down spells and face-down permanents have no characteristics other than those
-- listed by the ability or rules that allowed the spell or permanent to be face
-- down", and those listed values are the object's own COPIABLE values. Copiable
-- values are not an effect or a definition created in the main game, so CR
-- 729.1b does not reach them -- the same reason the entry's owner survives the
-- crossing -- and the object is offered as CR 708.2a's 2/2 creature with no
-- name. So Living Wish reaches a manifested sorcery and Burning Wish does not.
--
-- The alternative rejected: reading every entry through the printed face, on the
-- ground that a face-down permanent's characteristics come from a main-game
-- ability. That collapses CR 729.1b's bar on EFFECTS into a bar on what the
-- object IS -- an object's copiable values would then have to be recomputed from
-- its printing at every frame boundary, which no rule asks for -- and it makes
-- the wish able to name a card by a face the rules say the object does not have.
--
-- Reading it does not LEAK it (CR 708.5): the entries scanned are the acting
-- player's own, by CR 108.3b's guard below.
eligible :: Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> PlayerId -> GameState.GameState -> [OutsideCard.OutsideCard]
eligible predicate source pid gs =
  let pool = maybe Map.empty Player.outsideTheGame (Map.lookup pid (GameState.players gs))
      context = Filter.contextFor (Game.teams gs) (Just pid) (Just source)
      matchesFace face = Filter.matches context (Projection.viewOfCard face) predicate
      admits printingId = case Game.cardOfPrinting printingId gs of
        Nothing -> False
        Just card -> matchesFace (Game.resolveFaceFor Nothing card)
      -- Pawl.Engine.Card.faceDownFace is the same substitution
      -- Pawl.Engine.Game.faceOfObject performs for an object in this game, so
      -- the two frames cannot disagree about what a face-down object is.
      admitsOutside entry = case OutsideObject.facing entry of
        Facing.FaceDown state -> matchesFace (Card.faceDownFace (FaceDownState.listed state))
        Facing.FaceUp -> admits (OutsideObject.printing entry)
      fromPool = [OutsideCard.InPool printingId | (printingId, n) <- Map.toAscList pool, n > 0, admits printingId]
      -- CR 108.3b scopes the reach to the acting player's OWN cards outside the
      -- game -- the owner guard below is that scope, not an ownership check on
      -- the pool (which is already per-player).
      fromOuter =
        [ OutsideCard.InAnotherGame oid
        | (oid, entry) <- Map.toAscList (GameState.outsideObjects gs),
          OutsideObject.owner entry == pid,
          admitsOutside entry
        ]
   in fromPool <> fromOuter

-- CR 400.11c: put a card this player owns from outside the game matching the
-- Filter into their hand, showing it first (CR 701.20a) where the payload's
-- reveal says the card prints one -- Burning Wish's sentence, and Death Wish's
-- without the reveal.
--
-- The card is MINTED, Pawl.Engine.Dungeon.enter's road: outside the game is not
-- a zone (CR 400.11), so no object stood for the card and the move into the hand
-- is not a zone change. Pawl.Engine.Event.mintCard is where that happens, and
-- its haddock says why the insertion does not go through
-- Pawl.Engine.Event.changeZone.
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
-- Not implemented: where the reveal is printed it happens as the card ARRIVES in
-- the hand rather than before the move as the card prints it (#2450).
bringInto :: FromOutsideTheGame.FromOutsideTheGame -> ObjectId -> PlayerId -> Game ()
bringInto payload source pid = do
  gs0 <- State.get
  let predicate = FromOutsideTheGame.filter payload
      -- CR 701.20a is a keyword action of its own, so a card that does not print
      -- it moves the card and shows nobody anything.
      showIt oid = Monad.when (FromOutsideTheGame.reveal payload) (Event.reveal RevealCause.Ordinary pid oid)
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
          showIt oid
        OutsideCard.InAnotherGame outerId -> do
          gs1 <- State.get
          case bringInFrom pid outerId gs1 of
            (Nothing, _) -> pure ()
            (Just oid, gs2) -> do
              State.put gs2
              showIt oid

-- CR 400.11b: take one copy of this printing out of the player's pool and mint
-- the card into their hand. Split out from `bringInto` above because it is the half
-- every other road into the game will want -- CR 727.2's restart (#135) and CR
-- 707.13's copy created outside the game (#888) -- and none of those reveals
-- anything. The SPEND is the whole of what it adds over
-- Pawl.Engine.Event.mintCard, which Alchemy's conjure reaches with nothing to
-- spend.
bringIn :: PlayerId -> PrintingId.PrintingId -> GameState.GameState -> (ObjectId, GameState.GameState)
bringIn pid printingId gs =
  let (oid, gs1) = Event.mintCard pid Nothing printingId Zone.Hand LibraryPosition.defaultValue gs
      -- One copy, not the entry: CR 100.2a's four-card limit is applied to the
      -- combined deck and sideboard (CR 100.4a), so copies of a card are COUNTED
      -- and a player who set aside two can be brought the second one later.
      spend n = if n <= 1 then Nothing else Just (n - 1)
      spent p = p {Player.outsideTheGame = Map.update spend printingId (Player.outsideTheGame p)}
   in (oid, gs1 {GameState.players = Map.adjust spent pid (GameState.players gs1)})

-- CR 729.4a: bring in a card from a game that is on hold. The entry is dropped
-- and the OUTER id is appended to GameState.broughtIn, which is the whole record
-- the outer frame needs: this game cannot reach that game's state, and must
-- not (CR 729.1a keeps the two apart while the subgame runs). `Nothing` when
-- the id no longer names an outside entry -- a second wish reaching for a card
-- something already brought in -- mirrors `bringIn`'s own belt-on-braces guard
-- for a spent InPool entry.
--
-- The card arrives FACE UP whatever the entry's facing was, and the entry's
-- printing is what is minted. CR 708.9 has the owner reveal a face-down
-- permanent as it leaves the battlefield, and CR 400.7 makes what arrives here a
-- new object; a wish that reaches a manifested card gets the card, not the 2/2
-- `eligible` offered it as.
bringInFrom :: PlayerId -> ObjectId -> GameState.GameState -> (Maybe ObjectId, GameState.GameState)
bringInFrom pid outerId gs = case Map.lookup outerId (GameState.outsideObjects gs) of
  Nothing -> (Nothing, gs)
  Just entry ->
    let (oid, gs1) = Event.mintCard pid Nothing (OutsideObject.printing entry) Zone.Hand LibraryPosition.defaultValue gs
     in ( Just oid,
          gs1
            { GameState.outsideObjects = Map.delete outerId (GameState.outsideObjects gs1),
              GameState.broughtIn = GameState.broughtIn gs1 Seq.|> outerId
            }
        )
