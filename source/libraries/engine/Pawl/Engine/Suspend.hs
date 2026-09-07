-- Rule 702.62's FIRST ability in the one voice the rest of the engine cannot
-- supply for itself: CR 116.2f's special action that pays a card's suspend cost
-- to exile it from a hand with time counters on it.
--
-- The rule's other two abilities are triggered ones that function in exile (CR
-- 702.62a), so they are minted rather than performed --
-- Pawl.Engine.Keyword.exileTriggeredAbilitiesOf writes both, and
-- Pawl.Engine.Event.Trigger's exile scan is what offers them. What is left is an
-- action a player takes, and an action needs a place to be offered from and
-- performed in. Pawl.Engine.Plot is the same module for rule 702.170, and this
-- one is written to its shape.
--
-- THE INVARIANT: rule 702 is part of the rulebook, so reading Keyword.Suspend
-- here is the same closed-half act as reading a Phase. This module never asks
-- which CARD is being suspended.
module Pawl.Engine.Suspend where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Pawl.Types.ManaSpending as ManaSpending
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Suspend as Suspend
import qualified Pawl.Types.Zone as Zone

-- CR 702.62a: the suspend ability this object prints -- the counters and the
-- cost together -- or Nothing when it has none.
--
-- Read off the CARD (Card.combined) and never a projection, the reading
-- Pawl.Engine.Plot.plotCostOf gives one rule over: the ability functions in the
-- hand, where this reader takes the printed card (#1859). A hand member with no
-- card behind it -- a token, an ability -- has no suspend ability.
suspendOf :: ObjectId -> GameState -> Maybe (Suspend.Suspend Keyword)
suspendOf oid gs = do
  card <- Game.cardOfHandMember oid gs
  Keyword.suspend (Face.keywords (Card.combined card))

-- CR 702.62a / 116.2f: may this player suspend this card right now? Three
-- conjuncts, each a clause of the rule:
--
--   * the card is in THIS PLAYER'S HAND with a suspend ability on it ("a player
--     who has a card with suspend IN THEIR HAND"), which suspendOf and the zone
--     test settle together;
--   * they could BEGIN TO CAST it by putting it onto the stack -- rule 116.2f's
--     own window, and the whole of it: that rule states no phase and no empty
--     stack, so this is deliberately neither Pawl.Engine.Turn.sorcerySpeedWindow
--     (CR 116.2k's plot, CR 116.2m's unlock) nor Foretell's own-turn test. A
--     sorcery still only suspends at sorcery speed, because that is what
--     beginning to cast a sorcery asks for -- through Cast.couldBeginToCast, the
--     same conjuncts the cast offer is gated on, so the two cannot drift. CR
--     702.62c's prohibitions are folded in there.
--   * the suspend cost is payable. An action the player cannot take is not
--     offered, which is Pawl.Engine.Action.legalActions' posture throughout.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f adjustments,
-- for the reason Plot.canPlot gives: that rule totals the cost of a spell being
-- cast or an ability being activated, and a special action is neither (#90).
--
-- The PRIORITY clause has no conjunct, for the reason CR 116.2a's land play has
-- none: legalActions is asked only of the priority holder.
--
-- ONE HALF, the combined card's own name: rule 116.2f's subject is a card rather
-- than a half, and no printing has suspend on a multi-faced card.
canSuspend :: PlayerId -> ObjectId -> GameState -> Bool
canSuspend pid oid gs = case suspendOf oid gs of
  Nothing -> False
  Just ability ->
    elem oid (Game.zoneMembers Zone.Hand pid gs)
      && maybe False (\card -> Cast.couldBeginToCast pid oid (Face.name (Card.combined card)) gs) (Game.cardOfHandMember oid gs)
      && Cost.canPay pid oid (Suspend.cost ability) gs

-- Every card this player may suspend right now -- what Action.Suspend is built
-- from, and the shape Plot.plottable and Foretell.foretellable have.
suspendable :: PlayerId -> GameState -> [ObjectId]
suspendable pid gs = filter (\oid -> canSuspend pid oid gs) (Game.zoneMembers Zone.Hand pid gs)

-- CR 702.62a: pay the suspend cost and exile the card from hand with N time
-- counters on it, leaving it a suspended card (CR 702.62b).
--
-- REJECT-NOT-REPAIR, and the payment first, for the reasons Pawl.Engine.Plot.plot
-- states: a payment that fails restores the state from before it was attempted
-- and the card stays in hand.
--
-- THE COUNTERS go on THROUGH THE FUNNEL after the move, not as CR 614's entry
-- riders: that door's `counters` field is CR 122.6's "as it enters THE
-- BATTLEFIELD", and this move names exile. Event.putCounters is the single
-- counter-placement seam, so a "whenever a counter is put on" watcher sees these
-- as it sees any other, and CR 614.16's multipliers are asked once.
--
-- Unobservable as two steps rather than one, which is what rule 702.62a's "exile
-- it WITH N time counters on it" asks for: CR 116.1 gives no player priority
-- inside a special action, so nothing can look at the card in exile between the
-- two.
--
-- CounterCause.ByRule, not ByEffect: rule 702.62a is a keyword ability's own
-- instruction and no effect is resolving.
--
-- FACE UP, which is the difference from CR 702.143a's foretell: rule 702.62a says
-- only "exile it", so the default facing stands and every player can see what was
-- suspended.
--
-- NO STAMP, which is the difference from plot and foretell both: CR 702.62b
-- defines "suspended" out of state the game already holds -- exiled, has
-- suspend, has a time counter -- so there is nothing to write down, and no
-- reader can disagree with the board.
suspend :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> ObjectId -> Game ()
suspend perform pid oid = do
  before <- State.get
  if not (canSuspend pid oid before)
    then pure ()
    else do
      let ability = Maybe.fromMaybe (Suspend.MkSuspend 0 Cost.unpayable) (suspendOf oid before)
          counters = Suspend.counters ability
      -- CR 118.13c, Pawl.Engine.FaceDown.turnFaceUp's announcement and for its
      -- reasons. No printed suspend cost holds such a symbol -- Scryfall
      -- `keyword:suspend`, 2026-09-07, every suspend cost generic or
      -- monocoloured -- so no prompt is raised today.
      (announced, _) <- Cost.announce PaymentSubject.ForNeither ManaSpending.AsProduced pid oid pure (Suspend.cost ability)
      payment <- Cost.pay perform PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced pid oid announced
      case payment of
        -- CR 733.1's last sentence, Cost.keepingLibraryActions' reason: a mana
        -- ability tapped in the window this payment opened may have shuffled
        -- or revealed, and this reject-not-repair restore must not undo that
        -- too.
        Payment.Unpaid -> Cost.restoreKeepingLibraryActions before
        -- Dropped, Pawl.Engine.Plot's reason: the card is exiled and the later
        -- cast is free (CR 702.62a).
        Payment.Paid _ -> do
          -- The counters go on the id the move RETURNS and never onto `oid`, the
          -- reading Plot.plot gives its stamp: CR 400.7 mints a fresh
          -- incarnation in exile and deletes the one that was in hand. Nothing
          -- comes back when the move was cancelled, and then there is no exiled
          -- card to suspend.
          --
          -- One arrival, Plot.plot's reason: the funnel answers with more than
          -- one only for a melded permanent leaving the battlefield (CR 712.21),
          -- and this special action exiles a card from a hand.
          exiled <- Event.changeZoneReturning oid Zone.Exile
          Monad.forM_ exiled (\newId -> Event.putCounters (CounterCause.ByRule pid) newId CounterKind.Time counters)
