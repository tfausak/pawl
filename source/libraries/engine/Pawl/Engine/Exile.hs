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

import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
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
-- shuffled -- has nothing to end, since pawl builds no piles (gap #1480). A stored
-- per-player relation would answer identically for every board pawl can build,
-- since foretell is the only grant that exists: no card in the pool has hideaway
-- (CR 702.75a's granted "may look at this card in the exile zone"), and
-- Effect.GrantPlayFromExile carries no look permission of its own.
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
-- Not implemented: the rule's second half, where a player who may NOT look
-- chooses a PILE of face-down exiled cards and a card is taken at random out of
-- it. A pile is a candidate that is not an object, which no Prompt can carry, so
-- the face-down cards this drops are offered as nothing at all rather than as a
-- pile (#1480).
mayChoose :: Maybe PlayerId -> ObjectId -> GameState.GameState -> Bool
mayChoose perspective oid gs = case perspective of
  Just pid -> mayLookAt pid oid gs
  Nothing -> not (maybe False Object.exiledFaceDown (Game.lookupObject oid gs))
