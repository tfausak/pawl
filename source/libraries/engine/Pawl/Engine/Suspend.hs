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
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
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
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Suspend as Suspend
import qualified Pawl.Types.SuspendCounters as SuspendCounters
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
  Keyword.suspend (Face.keywordSet (Card.combined card))

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
-- The payability check is asked at the FLOOR of CR 107.3d's announcement -- 0 for
-- a printed numeral, which declares no X, and the card's own least value for
-- "Suspend X" (CR 101.1's "X can't be 0"). Sound because the demand an X makes is
-- monotone in it, Cost.greatestPayableX's own premise: a cost payable at no legal
-- value is payable at none.
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
      && payableAtX (SuspendCounters.leastX (Suspend.counters ability)) pid oid ability gs

-- CR 107.3d's cost at one value of X: what the gate above measures at the floor
-- and what the announcement below is judged against, so a gate and an
-- announcement cannot disagree about what this cost is. Cost.substituteX is the
-- identity on a cost with no X, which is every suspend cost printing a numeral
-- for N.
payableAtX :: Natural -> PlayerId -> ObjectId -> Suspend.Suspend Keyword -> GameState -> Bool
payableAtX x pid oid ability = Cost.canPay PaymentSubject.ForNeither pid oid (Cost.substituteX x (Suspend.cost ability))

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
-- counter-placement seam, so these counters go on the one road every other
-- placement takes rather than a second one written here.
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
-- CR 107.3d's X is announced HERE, "immediately before they pay that cost", and
-- the one value answers both halves of the printed line: rule 107.3i makes the N
-- and the {X} in the cost the same number, so the card is exiled with as many
-- time counters as the mana it cost. Reject-not-repair, Cast.castProposed's
-- posture: the answer is honoured and then measured, against CR 101.1's floor
-- ("X can't be 0") and against the board, and a value that fails either leaves
-- the card in hand with nothing paid.
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
      let ability = Maybe.fromMaybe (Suspend.MkSuspend (SuspendCounters.Literal 0) Cost.unpayable) (suspendOf oid before)
          printed = Suspend.cost ability
      -- CR 107.3d: the taker of the special action names X. The bound is
      -- advisory (see Prompt.ChooseX) and TERMINATES for the reason
      -- Cost.greatestPayableX needs one -- a suspend cost's X is a generic mana
      -- symbol, so its demand grows without bound; Pawl.CardSpec's "CR 107.3d
      -- every suspend cost's X is one the board can refuse" is what keeps a
      -- printing whose X the board could pay forever out of the pool. No ceiling
      -- is passed because no printed suspend line states one -- Scryfall
      -- `keyword:suspend o:"suspend x"`, 2026-09-17, five cards, each stating a
      -- floor and no maximum.
      mAmount <-
        if Cost.hasVariable printed
          then fmap Just (Game.choose (Prompt.ChooseX (Decide.deciderFor pid before) pid oid (SuspendCounters.leastX (Suspend.counters ability)) (Cost.greatestPayableX Nothing (\x -> payableAtX x pid oid ability before) printed)))
          else pure Nothing
      let announcedX = Maybe.fromMaybe 0 mAmount
          -- Rule 107.3i: one announcement, both halves of "Suspend X--{X}...".
          counters = case Suspend.counters ability of
            SuspendCounters.Literal n -> n
            SuspendCounters.Variable _ -> announcedX
      -- CR 101.1 for the floor and CR 101.2 for its direction, CR 116.2f for the
      -- payment: an announcement the card forbids or the board cannot pay takes
      -- the whole special action away rather than being clamped to something the
      -- player did not choose. Measured with the SAME predicate the gate above
      -- asked at the floor.
      if announcedX < SuspendCounters.leastX (Suspend.counters ability) || not (payableAtX announcedX pid oid ability before)
        then pure ()
        else do
          -- CR 118.13c, Pawl.Engine.FaceDown.turnFaceUp's announcement and for
          -- its reasons. No printed suspend cost holds a symbol payable in more
          -- than one way -- Scryfall `keyword:suspend`, 2026-09-17, every suspend
          -- cost generic, monocoloured or CR 107.4b's {X} -- so no prompt is
          -- raised today.
          (announced, _) <- Cost.announce PaymentSubject.ForNeither ManaSpending.AsProduced pid oid pure (Cost.substituteX announcedX printed)
          payment <- Cost.pay perform (Just before) PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced pid oid announced
          case payment of
            -- CR 733.1's reversal, Pawl.Engine.Foretell.foretell's reason: this
            -- special action IS the whole of what failed, so `before` goes to
            -- Cost.pay and the reversal -- the payer's choice about the CR 605.3a
            -- window included -- happens there.
            Payment.Unpaid -> pure ()
            -- Dropped, Pawl.Engine.Plot's reason: the card is exiled and the later
            -- cast is free (CR 702.62a).
            Payment.Paid _ -> do
              -- The counters go on the id the move RETURNS and never onto `oid`,
              -- the reading Plot.plot gives its stamp: CR 400.7 mints a fresh
              -- incarnation in exile and deletes the one that was in hand.
              -- Nothing comes back when the move was cancelled, and then there is
              -- no exiled card to suspend.
              --
              -- One arrival, Plot.plot's reason: the funnel answers with more
              -- than one only for a melded permanent leaving the battlefield (CR
              -- 712.21), and this special action exiles a card from a hand.
              exiled <- Event.changeZoneReturning oid Zone.Exile
              Monad.forM_ exiled (\newId -> Event.putCounters (CounterCause.ByRule pid) newId CounterKind.Time counters)
