-- Rule 702.170 in the one voice the rest of the engine cannot supply for itself:
-- CR 116.2k's special action that pays a card's plot cost to exile it from a
-- hand, and the stamp that makes the exiled card a PLOTTED one. CR 702.170c's
-- other route into that stamp -- a spell or ability that makes an exiled card
-- plotted -- is an Effect opcode and belongs to the open half, so its arm sits in
-- Pawl.Engine.Resolve and calls becomePlotted here.
--
-- The rule's other half lives where every other casting question does. CR
-- 702.170d's permission -- "a plotted card's owner may cast it from exile without
-- paying its mana cost ... during any turn after the turn in which it became
-- plotted" -- is read by Pawl.Engine.Cast.permitsCastFromExile and priced by
-- Pawl.Engine.Cost.costsFor, off the Object.plotted stamp this module writes.
-- What is left is an action a player takes, and an action needs a place to be
-- offered from and performed in. Pawl.Engine.Room is the same module for rule
-- 709.5, and this one is written to its shape.
--
-- THE INVARIANT: rule 702 is part of the rulebook, so reading Keyword.Plot here
-- is the same closed-half act as reading a Phase. This module never asks which
-- CARD is being plotted.
module Pawl.Engine.Plot where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Turn as Turn
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Zone as Zone

-- CR 702.170a: what this object's plot ability costs, or Nothing when it has
-- none.
--
-- Read off the CARD (Card.combined) and never a projection, the reading
-- Pawl.Engine.Action.discardableCards gives for CR 116.2e one rule over: the
-- ability functions in the hand, where this reader takes the printed card (#1859). A
-- hand member with no card behind it -- a token, an ability -- has no plot cost.
plotCostOf :: ObjectId -> GameState -> Maybe (Cost Keyword)
plotCostOf oid gs = do
  card <- Game.cardOfHandMember oid gs
  Keyword.plotCost (Face.keywords (Card.combined card))

-- CR 702.170a / 116.2k: may this player plot this card right now? Three
-- conjuncts, each a clause of the rule:
--
--   * the card is in THIS PLAYER'S HAND with a plot cost to pay ("you may exile
--     this card FROM YOUR HAND"), which plotCostOf and the zone test settle
--     together;
--   * the window is a main phase of their own turn with the stack empty ("any
--     time you have priority during your main phase while the stack is empty"),
--     which is CR 307.5's sorcery-speed window conjunct for conjunct -- so it is
--     asked through Turn.sorcerySpeedWindow rather than a near-copy that can
--     drift. CR 116.2k's own wording drops "main phase" and rule 702.170a keeps
--     it; the keyword is what a card grants, so the narrower one governs.
--   * the plot cost is payable. An action the player cannot take is not offered,
--     which is Pawl.Engine.Action.legalActions' posture throughout.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f adjustments,
-- for the reason Room.canUnlock gives: that rule totals the cost of a spell being
-- cast or an ability being activated, and a special action is neither (#90).
--
-- The PRIORITY clause has no conjunct, for the reason CR 116.2a's land play has
-- none: legalActions is asked only of the priority holder.
canPlot :: PlayerId -> ObjectId -> GameState -> Bool
canPlot pid oid gs = case plotCostOf oid gs of
  Nothing -> False
  Just cost ->
    elem oid (Game.zoneMembers Zone.Hand pid gs)
      && Turn.sorcerySpeedWindow pid gs
      && Cost.canPay pid oid cost gs

-- Every card this player may plot right now -- what Action.Plot is built from,
-- and the shape Room.unlockable and FaceDown.turnableFaceUp have.
plottable :: PlayerId -> GameState -> [ObjectId]
plottable pid gs = filter (\oid -> canPlot pid oid gs) (Game.zoneMembers Zone.Hand pid gs)

-- CR 702.170a, in the rule's own order: exile the card from hand and pay the
-- cost, leaving it a plotted card.
--
-- REJECT-NOT-REPAIR, the posture Room.unlock, FaceDown.turnFaceUp and
-- Cast.castSpell all take: a payment that fails restores the state from before it
-- was attempted and the card stays in hand. The payment runs FIRST for that
-- reason -- a failed one has moved nothing to put back -- where rule 702.170a's
-- own sentence names the exile first ("exile this card from your hand and pay
-- [cost]"). Nothing in the pool observes the order: no player has priority inside
-- a special action (CR 116.1), every printed plot cost is mana, and the two
-- halves either both happen or neither does.
--
-- The stamp is written onto the id the move RETURNS and never onto `oid`, the
-- reading Resolve.finishSpell gives CR 715.3d's permission: CR 400.7 mints a
-- fresh incarnation in exile and deletes the one that was in hand, so the plotted
-- designation belongs to the new object. Nothing comes back when the move was
-- cancelled, and then there is no exiled card to be plotted.
--
-- FACE UP, which is the whole difference from CR 702.143a's foretell: rule
-- 702.170a says only "exile this card", so the default facing stands and every
-- player can see what was plotted.
--
-- The GameEvent.Plotted entry rides the same `newId` and the same branch as the
-- stamp, for that reason and one more: a move that was cancelled plotted
-- nothing, so there is no event to record. It is what a "when this card becomes
-- plotted" trigger (CR 702.170a, CR 702.170c) reads, the exile's own zone change
-- saying only that a card left a hand.
plot :: PlayerId -> ObjectId -> Game ()
plot pid oid = do
  before <- State.get
  if not (canPlot pid oid before)
    then pure ()
    else do
      payment <- Cost.pay Nothing ManaSpending.AsProduced pid oid (Maybe.fromMaybe Cost.unpayable (plotCostOf oid before))
      case payment of
        Payment.Unpaid -> State.put before
        -- Dropped, Pawl.Engine.Foretell's reason exactly: the card is exiled and
        -- the later cast pays its own cost.
        Payment.Paid _ -> do
          exiled <- Event.changeZoneReturning oid Zone.Exile
          case exiled of
            Nothing -> pure ()
            Just newId -> State.modify' (becomePlotted newId)

-- "It becomes a plotted card" -- the stamp and the event together, which is the
-- WHOLE of what becoming plotted is.
--
-- Two routes reach it and the rulebook gives them one meaning: CR 702.170a's
-- special action above, and CR 702.170c's "some spells and abilities cause a card
-- in exile to become plotted" (Pawl.Engine.Resolve's Effect.MakePlotted arm).
-- Both land here so neither can drift from the other -- a route that stamped
-- without recording would leave a "when this card becomes plotted" trigger (Aloe
-- Alchemist) silent on a card that had, by the rules, become plotted.
--
-- Takes an ObjectId and nothing else, which is this module's invariant: it never
-- asks which CARD is being plotted, so the opcode's arm hands it CR 400.7's
-- exiled incarnation exactly as the special action does.
becomePlotted :: ObjectId -> GameState -> GameState
becomePlotted newId = Event.recordEvent (GameEvent.Plotted newId) . stamp newId

-- CR 702.170a's "it becomes a plotted card", stamped with the turn the action was
-- taken on -- which is what CR 702.170d's "any turn after the turn in which it
-- became plotted" is compared against.
--
-- Read AFTER the move rather than from `before`: nothing in a zone change ends a
-- turn, so the two numbers agree, and reading the board the stamp is written to
-- is what keeps them from drifting apart if one ever could.
stamp :: ObjectId -> GameState -> GameState
stamp newId gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.plotted = Just (GameState.turnNumber gs)}) newId (GameState.objects gs)
    }
