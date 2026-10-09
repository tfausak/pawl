-- Rule 702.170 in the one voice the rest of the engine cannot supply for itself:
-- CR 116.2k's special action that pays a card's plot cost to exile it from a
-- hand or, under CR 702.170f, the zone an effect names, and the stamp that makes
-- the exiled card a PLOTTED one. CR 702.170c's other route into that stamp -- a
-- spell or ability that makes an exiled card plotted -- is an Effect opcode and
-- belongs to the open half, so its arm sits in Pawl.Engine.Resolve and calls
-- becomePlotted here.
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

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Turn as Turn
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Zone as Zone

-- CR 702.170a / 702.170f: every plot cost `pid` may plot this object for from
-- where it is, or none.
--
-- From `pid`'s HAND, the card's own plot ability. From a pile a PlayerEffect
-- PlotFrom grant opens (Fblthp, Lost on the Range), that ability AND the one the
-- grant gives, whose cost is the card's mana cost -- Room.unlockCostOf's
-- reading, so a card with no mana cost has an unpayable one (CR 118.6). Two
-- equal costs are one: nothing tells the actions apart.
--
-- The card's own plot ability is read through the projection (CR 613.1f),
-- Pawl.Engine.Suspend.suspendOf's reading: it functions in a hand or a library,
-- and an effect may take it away there (Patriar's Humiliation).
-- Pawl.SpecialActionSpec's "CR 613.1f a Djinn of Fool's Fall that perpetually
-- lost all abilities cannot be plotted" proves it. A member with no card behind
-- it has no plot cost.
plotCostsOf :: PlayerId -> ObjectId -> GameState -> [Cost Keyword]
plotCostsOf pid oid gs = case Game.cardOfHandMember oid gs of
  Nothing -> []
  Just card ->
    let face = Card.combined card
        own = Keyword.plotCosts (Map.keysSet (Projection.keywordsOf oid gs))
        granted = Cost.Type.MkCost (Face.manaCost face) []
        inHand = elem oid (Game.zoneMembers Zone.Hand pid gs)
        fromPile =
          or
            [ PlayerEffect.mayPlotFrom pid zone oid gs
            | (zone, owner) <- PlayerEffect.plotPiles pid gs,
              elem oid (Cast.pileCandidates zone owner gs)
            ]
     in ListUtils.nubOrd ((if inHand then own else []) <> (if fromPile then own <> [granted] else []))

-- CR 702.170a / 116.2k: may this player plot this card for this cost right now?
-- Three conjuncts, each a clause of the rule:
--
--   * the cost is one of the card's plot costs from where it is ("you may exile
--     this card FROM YOUR HAND", or CR 702.170f's other zone), which
--     plotCostsOf settles;
--   * the window is a main phase of their own turn with the stack empty ("any
--     time you have priority during your main phase while the stack is empty"),
--     which is CR 307.5's sorcery-speed window conjunct for conjunct -- so it is
--     asked through Turn.sorcerySpeedWindow rather than a near-copy that can
--     drift. CR 116.2k's own wording drops "main phase" and rule 702.170a keeps
--     it; the keyword is what a card grants, so the narrower one governs.
--   * the plot cost is payable at X = 0 (CR 107.3d's X is named later, in
--     `plot`). An action the player cannot take is not offered, which is
--     Pawl.Engine.Action.legalActions' posture throughout.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f adjustments,
-- for the reason Room.canUnlock gives: that rule totals the cost of a spell being
-- cast or an ability being activated, and a special action is neither (#90).
--
-- The PRIORITY clause has no conjunct, for the reason CR 116.2a's land play has
-- none: legalActions is asked only of the priority holder.
canPlot :: PlayerId -> ObjectId -> Cost Keyword -> GameState -> Bool
canPlot pid oid cost gs =
  elem cost (plotCostsOf pid oid gs)
    && Turn.sorcerySpeedWindow pid gs
    && Cost.actionPayableAt PaymentSubject.ForNeither 0 pid oid cost gs

-- Every (card, cost) this player may plot right now -- what Action.Plot is built
-- from, and the shape Room.unlockable and FaceDown.turnableFaceUp have. The
-- candidates are the player's hand and the cards Cast.pileCandidates offers of
-- each pile a PlotFrom grant opens.
plottable :: PlayerId -> GameState -> [(ObjectId, Cost Keyword)]
plottable pid gs =
  let candidates =
        ListUtils.nubOrd
          ( Game.zoneMembers Zone.Hand pid gs
              <> concat [Cast.pileCandidates zone owner gs | (zone, owner) <- PlayerEffect.plotPiles pid gs]
          )
   in [(oid, cost) | oid <- candidates, cost <- plotCostsOf pid oid gs, canPlot pid oid cost gs]

-- CR 702.170a, in the rule's own order: exile the card from where it is (the hand,
-- or CR 702.170f's other zone) and pay the cost, leaving it a plotted card.
--
-- REJECT-NOT-REPAIR, the posture Room.unlock, FaceDown.turnFaceUp and
-- Cast.castSpell all take: a payment that fails restores the state from before it
-- was attempted and the card stays where it was. The payment runs FIRST for that
-- reason -- a failed one has moved nothing to put back -- where rule 702.170a's
-- own sentence names the exile first ("exile this card from your hand and pay
-- [cost]"). Nothing observes the order: no player has priority inside a special
-- action (CR 116.1), and the exile names the card by its id. A mana ability can
-- still shuffle a library mid-payment through a replacement effect, which CR
-- 605.1a's last sentence leaves out of the classification (Ashnod's Altar
-- sacrificing Progenitus), but Event.shuffleLibrary reorders the ids it holds and
-- mints none, so the card plotted from the top is exiled wherever it went.
--
-- CR 107.3d's X, which a mana cost granted as a plot cost can hold, is named
-- in Cost.payAction, with CR 118.13c's announcement and CR 733.1's reversal.
--
-- The stamp is written onto the id the move RETURNS and never onto `oid`, the
-- reading Resolve.finishSpell gives CR 715.3d's permission: CR 400.7 mints a
-- fresh incarnation in exile and deletes the one it left, so the plotted
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
-- saying only that a card left a hand or a library.
plot :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> ObjectId -> Cost Keyword -> Game ()
plot perform pid oid printed = do
  before <- State.get
  Monad.when (canPlot pid oid printed before) $ do
    paid <- Cost.payAction perform before PaymentSubject.ForNeither 0 pid oid printed
    -- One stamp per arrival: the funnel answers with more than one only for a
    -- melded permanent leaving the battlefield (CR 712.21), and this special
    -- action exiles a card from a hand or a library.
    Monad.forM_ paid $ \_ -> do
      exiled <- Event.changeZoneReturning oid Zone.Exile
      Monad.forM_ exiled (State.modify' . becomePlotted)

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
