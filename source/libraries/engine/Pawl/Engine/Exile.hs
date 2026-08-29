-- | Who may LOOK at a card in exile, and which exiled cards a player may
-- therefore be offered.
--
-- CR 406.3 makes exile a public zone with one exception -- a card "exiled face
-- down" -- and CR 406.4 turns that exception into a rule about CHOOSING: "the
-- player may choose a specific face-down card only if the player is allowed to
-- look at that card". Both questions are per-player, so neither can be answered
-- by a flag on the object alone; this module is where the two are asked.
--
-- THE INVARIANT: this is the closed half. CR 406.3's default and CR 702.143a's
-- grant are both rulebook, so reading Object.foretold here is the same act as
-- reading a Phase. Nothing here asks which CARD is in exile.
module Pawl.Engine.Exile where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Pile as Pile
import Pawl.Types.PlayerId (PlayerId)

-- | CR 406.3: may this player look at this exiled card?
--
-- The rule's default is YES for every player -- "exiled cards are, by default,
-- kept face up and may be examined by any player at any time" -- so the whole
-- question is Object.exiledFaceDown, which is the rider that overrides it.
--
-- CR 702.143a is the one grant in the pool: "that player may look at that card
-- as long as it remains in exile", said of the player who took the foretell
-- special action, who is the card's OWNER (CR 400.1 makes a hand a per-player
-- zone and the action exiles from the actor's own hand). CR 702.143d says the
-- same of a card an effect makes foretold, and names the owner outright. So the
-- Object.foretold stamp Pawl.Engine.Foretell writes IS the permission, and the
-- owner is the one player it names.
--
-- DERIVED rather than stored, and CR 406.3's tail is what makes that exact: the
-- permission runs until the card leaves the exile zone, and the stamp has the
-- same lifetime -- it is per-incarnation state, so CR 400.7's fresh incarnation
-- on the way out clears it at the same moment the rule ends the permission. That
-- rule's OTHER ending -- the card becoming part of a pile of cards that are
-- shuffled -- has nothing to end: pileOf below builds the piles, and no card in
-- `data/cards/` shuffles one. A stored per-player relation would answer
-- identically for every board pawl can build, since foretell is the only grant
-- that exists: no card in the pool has hideaway (CR 702.75a's granted "may look
-- at this card in the exile zone"), and Effect.GrantPlayFromExile carries no
-- look permission of its own.
--
-- Not implemented: a grant that names a player who is not the card's owner.
-- CR 702.75a's hideaway is the printed shape -- "the player who controls the
-- permanent that exiled this card may look at this card in the exile zone" --
-- and no card in `data/cards/` has hideaway, which is the same card the gate one
-- rule over wants (gap #2504).
mayLookAt :: PlayerId -> ObjectId -> GameState.GameState -> Bool
mayLookAt pid oid gs = Maybe.fromMaybe False $ do
  obj <- Game.lookupObject oid gs
  pure (not (Object.exiledFaceDown obj) || (Maybe.isJust (Object.foretold obj) && Object.owner obj == pid))

-- | CR 406.4's first half: may this player choose this exiled card SPECIFICALLY?
--
-- A face-up card is choosable by anybody, and a face-down one only by a player
-- allowed to look at it. The perspective is CR 109.5's "you" -- the player CR
-- 601.2c has choosing targets -- and Nothing is a genuinely absent one, which
-- takes the vacuous posture every player-referencing question in
-- Pawl.Engine.Target takes: no player is allowed to look, so no face-down card
-- is choosable.
--
-- A card this answers False for is not thereby unreachable: CR 406.4's second
-- half offers the chooser the PILE it sits in instead, which
-- Pawl.Engine.Target.piledOffer substitutes and Pawl.Engine.Target.drawFromPiles
-- draws out of.
mayChoose :: Maybe PlayerId -> ObjectId -> GameState.GameState -> Bool
mayChoose perspective oid gs = case perspective of
  Just pid -> mayLookAt pid oid gs
  Nothing -> not (maybe False Object.exiledFaceDown (Game.lookupObject oid gs))

-- | CR 406.4: which pile this card is in, or Nothing for one that is in none --
-- every card in exile face up, and every object outside exile.
--
-- CR 406.4's criteria are when and how the card was exiled. A FORETOLD card is
-- its own pile under CR 702.143e, which requires a player's foretold cards to
-- stay differentiable from each other and from the rest of their face-down
-- exiled cards, and Object.timestamp is CR 613.7d's record of when this
-- incarnation was exiled -- unique, so the pile is a singleton and the rule's
-- random draw over it names the one card in it.
--
-- Not implemented: the same separation for the rest, which are pooled per owner.
-- Two effects that each exile cards face down make one pile here where the rule
-- makes two, so a chooser is offered one candidate where they should have had
-- the choice of which (#2566).
pileOf :: ObjectId -> GameState.GameState -> Maybe Pile.Pile
pileOf oid gs = do
  obj <- Game.lookupObject oid gs
  Monad.guard (Set.member oid (GameState.exile gs) && Object.exiledFaceDown obj)
  pure $ case Object.foretold obj of
    Just _ -> Pile.OfForetold (Object.timestamp obj)
    Nothing -> Pile.OfFaceDown (Object.owner obj)
