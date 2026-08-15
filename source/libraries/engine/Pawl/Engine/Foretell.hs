-- Rule 702.143 in the one voice the rest of the engine cannot supply for itself:
-- CR 116.2h's special action that pays {2} to exile a card with foretell from a
-- hand FACE DOWN, and the stamp that makes the exiled card a FORETOLD one.
--
-- The rule's other half lives where every other casting question does. CR
-- 702.143a's permission -- "they may cast that card after the current turn has
-- ended by paying any foretell cost it has" -- is read by
-- Pawl.Engine.Cast.permitsCastFromExile and priced by Pawl.Engine.Cost.costsFor,
-- off the Object.foretold stamp this module writes. What is left is an action a
-- player takes, and an action needs a place to be offered from and performed in.
-- Pawl.Engine.Plot is the same module for rule 702.170, and this one is written
-- to its shape.
--
-- THE INVARIANT: rule 702 is part of the rulebook, so reading Keyword.Foretell
-- here is the same closed-half act as reading a Phase. This module never asks
-- which CARD is being foretold.
module Pawl.Engine.Foretell where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 116.2h: what the special action costs -- "may pay {2} and exile that card
-- face down".
--
-- Minted here rather than read off Keyword.Foretell, which is the whole
-- difference from Pawl.Engine.Plot.plotCostOf: rule 116.2h fixes this amount for
-- every printing, and the keyword's own payload is the later CAST's cost
-- (Pawl.Engine.Keyword.foretellCost). Pawl.Engine.Cost.faceDownCost is the same
-- shape one rule over -- an amount rule 702.37a states rather than any card.
actionCost :: Cost Keyword
actionCost =
  Cost.Type.MkCost
    { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
      Cost.Type.components = []
    }

-- Does this object have foretell at all? Nothing for a hand member with no card
-- behind it -- a token, an ability.
--
-- Read off the CARD (Card.combined) and never a projection, the reading
-- Pawl.Engine.Plot.plotCostOf gives one rule over: the ability functions in the
-- hand, where no pool effect changes a card's abilities (#160).
--
-- Returns the CAST cost the keyword carries, though this module never spends it:
-- what it answers here is only "is the keyword there", and returning the payload
-- keeps one reader of the keyword rather than two.
foretellCostOf :: ObjectId -> GameState -> Maybe (Cost Keyword)
foretellCostOf oid gs = do
  card <- Game.cardOfHandMember oid gs
  Keyword.foretellCost (Face.keywords (Card.combined card))

-- CR 702.143a / 116.2h: may this player foretell this card right now? Three
-- conjuncts, each a clause of the rule:
--
--   * the card is in THIS PLAYER'S HAND with foretell on it ("exile a card with
--     foretell from their hand"), which foretellCostOf and the zone test settle
--     together;
--   * the window is THIS PLAYER'S OWN TURN -- "any time a player has priority
--     during their turn". Nothing more: rule 702.143a asks for neither a main
--     phase nor an empty stack, so this is deliberately NOT
--     Pawl.Engine.Turn.sorcerySpeedWindow, which CR 116.2k's plot and CR
--     116.2m's unlock both are. Pawl.SpecialActionSpec pairs the two boards that
--     prove the difference.
--   * the {2} is payable. An action the player cannot take is not offered, which
--     is Pawl.Engine.Action.legalActions' posture throughout.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f
-- adjustments, for the reason Plot.canPlot gives: that rule totals the cost of a
-- spell being cast or an ability being activated, and a special action is
-- neither (#90).
--
-- The PRIORITY clause has no conjunct, for the reason CR 116.2a's land play has
-- none: legalActions is asked only of the priority holder.
canForetell :: PlayerId -> ObjectId -> GameState -> Bool
canForetell pid oid gs =
  Maybe.isJust (foretellCostOf oid gs)
    && elem oid (Game.zoneMembers Zone.Hand pid gs)
    && GameState.activePlayer gs == pid
    && Cost.canPay pid oid actionCost gs

-- Every card this player may foretell right now -- what Action.Foretell is built
-- from, and the shape Plot.plottable has.
foretellable :: PlayerId -> GameState -> [ObjectId]
foretellable pid gs = filter (\oid -> canForetell pid oid gs) (Game.zoneMembers Zone.Hand pid gs)

-- CR 702.143a: pay {2} and exile the card from hand face down, leaving it a
-- foretold card.
--
-- REJECT-NOT-REPAIR, and the payment first, for the reasons Pawl.Engine.Plot.plot
-- states: a payment that fails restores the state from before it was attempted
-- and the card stays in hand.
--
-- FACE DOWN, which is the whole difference from CR 702.170a's plot: rule 702.143a
-- says "exile a card with foretell from their hand FACE DOWN", so the move
-- carries CR 406.3's rider rather than that rule's face-up default. It goes
-- through Event.changeZoneEntering because that is the door the rider is read
-- from, so the card is never face up in exile for an instant.
--
-- The stamp is written onto the id the move RETURNS and never onto `oid`, for
-- Plot.plot's reason: CR 400.7 mints a fresh incarnation in exile.
--
-- Not implemented: CR 702.143a's "that player may look at that card as long as
-- it remains in exile", and CR 702.143e's ordering of one player's several
-- foretold cards. Neither hides anything, because pawl conceals nothing from an
-- answerer -- Pawl.Types.Asked hands over the whole GameState (#682). What the
-- missing permission does cost is CR 406.4's other side: Target.exileRecipients
-- drops every face-down exiled card, so a foretold card cannot be CHOSEN even by
-- the player rule 702.143a lets look at it. Stricter than printed, and
-- unobservable today -- the only card in the pool that names an exiled one asks
-- for a face-up one (#1480).
foretell :: PlayerId -> ObjectId -> Game ()
foretell pid oid = do
  before <- State.get
  if not (canForetell pid oid before)
    then pure ()
    else do
      payment <- Cost.pay ManaSpending.AsProduced pid oid actionCost
      case payment of
        Payment.Unpaid -> State.put before
        Payment.Paid -> do
          exiled <- Event.changeZoneEntering oid Zone.Exile LibraryPosition.defaultValue riders Nothing
          case exiled of
            Nothing -> pure ()
            Just newId -> State.modify' (stamp newId)

-- CR 406.3's rider and nothing else: every other rider is battlefield-only, and
-- this move names exile.
riders :: EntryRiders.EntryRiders
riders =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = TapState.Untapped,
      EntryRiders.attacking = False,
      EntryRiders.transformed = False,
      EntryRiders.counters = Map.empty,
      EntryRiders.underOwner = False,
      EntryRiders.exiledFaceDown = True,
      EntryRiders.faceDown = False
    }

-- CR 702.143a's foretold card, stamped with the turn the action was taken on --
-- which is what "after the current turn has ended" is compared against.
--
-- Read AFTER the move, for Plot.stamp's reason: nothing in a zone change ends a
-- turn, so the two numbers agree.
stamp :: ObjectId -> GameState -> GameState
stamp newId gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.foretold = Just (GameState.turnNumber gs)}) newId (GameState.objects gs)
    }
