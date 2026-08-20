-- Rule 709.5 in the one voice the rest of the engine cannot supply for itself:
-- CR 116.2m's special action that pays a locked door's mana cost to open it.
--
-- Everything ELSE rule 709.5 says is arranged so that no module has to know this
-- one exists. CR 709.5's subtraction lives at Pawl.Engine.Game.resolveFaceFor,
-- which every characteristic read reaches through Game.faceOf, so they all get it
-- for free; CR 709.5d's entering designation lives
-- at Pawl.Engine.Event.changeZoneAttaching, inside the move; CR 709.5h's trigger
-- is an ordinary Pawl.Types.TriggerCondition matched against a
-- Pawl.Types.GameEvent. What is left is an action a player takes, and an action
-- needs a place to be offered from and performed in. Pawl.Engine.FaceDown is the
-- same module for rule 708, and this one is written to its shape.
--
-- THE INVARIANT: rule 709 is part of the rulebook, so reading
-- Pawl.Types.Layout's Room here is the same closed-half act as reading a Phase.
-- This module never asks which CARD a door belongs to; a half is a Face with a
-- name and a mana cost, and that is all of it that is read.
module Pawl.Engine.Room where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Types.Card as Card.Type
import Pawl.Types.CardName (CardName)
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Zone as Zone

-- CR 709.5c: the halves of this permanent that are LOCKED -- "a particular half
-- of a permanent is said to be 'unlocked' if it has the appropriate unlocked
-- designation. Otherwise, that half is said to be locked."
--
-- Empty for everything that is not a Room permanent, which is CR 709.5c's own
-- scope read twice over: only a card with a shared type line has halves for a
-- designation to name (Card.hasSharedTypeLine), and only a permanent on the
-- battlefield can have one.
--
-- Answers the FACES rather than their names, because two of its three callers
-- need the mana cost off each (CR 709.5e's unlock cost): canUnlock and unlock
-- below, both through unlockCostOf. The third, unlockable, takes only the name.
--
-- Read off the object's own STORED card, never a projected view: CR 709.5 makes
-- which half a characteristic is in a copiable value, so the doors of a Room
-- that became a copy of another Room are the copy's -- which falls out of
-- Game.cardOf answering with the card underneath (#925).
lockedHalves :: ObjectId -> GameState -> [Face.Face Card.Type.Card]
lockedHalves oid gs = Maybe.fromMaybe [] $ do
  obj <- Game.lookupObject oid gs
  card <- Game.cardOf oid gs
  if Object.zone obj /= Zone.Battlefield || not (Card.hasSharedTypeLine card)
    then Nothing
    else
      Just
        ( filter
            (\face -> not (Set.member (Face.name face) (Object.unlockedHalves obj)))
            (NonEmpty.toList (Card.Type.faces card))
        )

-- CR 709.5e: the unlock cost of one half -- "a player who controls a permanent
-- that has one or more locked halves may pay THE MANA COST of a locked half of
-- that permanent to give that permanent the appropriate unlocked designation.
-- This cost is referred to as an 'unlock cost.'"
--
-- The half's PRINTED mana cost and nothing else: the rule names the mana cost of
-- the half, and CR 709.5's subtraction has already taken that cost away from the
-- permanent, so a projected read would find the wrong object's. A face with no
-- mana cost at all yields an unpayable one, which CR 118.6 is already the rule
-- for -- no printed Room half is such a face.
unlockCostOf :: Face.Face Card.Type.Card -> Cost Keyword
unlockCostOf face = Cost.Type.MkCost (Face.manaCost face) []

-- CR 709.5e / 116.2m: may this player unlock this half of this permanent right
-- now? Four conjuncts, each a clause of the rule:
--
--   * the permanent has this half among its LOCKED ones (lockedHalves above),
--     which also settles that it is a Room permanent at all;
--   * this player CONTROLS it ("a player who controls a permanent");
--   * the window is a main phase of their own turn with the stack empty ("any
--     time they have priority and the stack is empty during a main phase of
--     their turn"), which is CR 307.5's sorcery-speed window conjunct for
--     conjunct -- so it is asked through Turn.sorcerySpeedWindow rather than a
--     near-copy that can drift;
--   * the unlock cost is payable. An action the player cannot take is not
--     offered, which is Pawl.Engine.Action.legalActions' posture throughout.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f
-- adjustments, for the reason FaceDown.canTurnFaceUp gives: that rule totals the
-- cost of a spell being cast or an ability being activated, and a special action
-- is neither (#90).
--
-- The PRIORITY clause has no conjunct, for the reason CR 116.2a's land play has
-- none: legalActions is asked only of the priority holder.
canUnlock :: PlayerId -> ObjectId -> CardName -> GameState -> Bool
canUnlock pid oid half gs =
  case filter ((== half) . Face.name) (lockedHalves oid gs) of
    [] -> False
    face : _ ->
      Projection.controllerOf oid gs == Just pid
        && Turn.sorcerySpeedWindow pid gs
        && Cost.canPay pid oid (unlockCostOf face) gs

-- Every door this player may unlock right now, in battlefield order and then
-- printed order -- what Action.Unlock is built from, and the shape
-- FaceDown.turnableFaceUp has for the same reason.
--
-- ONE ENTRY PER DOOR, never one per permanent: which half to unlock is the
-- player's choice (CR 709.5e says "a locked half"), and offering each as its own
-- legal action is how the engine avoids making it. A Room with both doors shut
-- and the mana for both offers two.
unlockable :: PlayerId -> GameState -> [(ObjectId, CardName)]
unlockable pid gs =
  let forOne oid = fmap ((,) oid . Face.name) (filter (\face -> canUnlock pid oid (Face.name face) gs) (lockedHalves oid gs))
   in concatMap forOne (Set.toAscList (GameState.battlefield gs))

-- CR 709.5e, in the rule's own order: pay the unlock cost, then give the
-- permanent the appropriate unlocked designation.
--
-- REJECT-NOT-REPAIR, the posture FaceDown.turnFaceUp, Cast.castSpell and
-- Activate.activateAbility all take: a payment that fails restores the state
-- from before it was attempted and the door stays shut. The rule's order is what
-- makes that correct rather than merely tidy -- the cost is paid before the
-- designation is given, so a failed payment has opened nothing to undo.
--
-- No CR 400.7 incarnation is minted and nothing enters: unlocking is not a zone
-- change, so damage, counters, attachments, the CR 613.7d timestamp and every
-- continuous effect naming the permanent ride through untouched, and no
-- enters-the-battlefield ability is offered the newly opened door's text.
unlock :: PlayerId -> ObjectId -> CardName -> Game ()
unlock pid oid half = do
  before <- State.get
  if not (canUnlock pid oid half before)
    then pure ()
    else case filter ((== half) . Face.name) (lockedHalves oid before) of
      [] -> pure ()
      face : _ -> do
        payment <- Cost.pay Nothing ManaSpending.AsProduced pid oid (unlockCostOf face)
        case payment of
          Payment.Unpaid -> State.put before
          -- Dropped, Pawl.Engine.Ignore's reason: CR 116.2m's special action
          -- resolves nothing whose effects could read a slot.
          Payment.Paid _ -> Event.unlockHalf oid half
