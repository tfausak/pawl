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
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaAbilityPerformer as ManaAbilityPerformer
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Suspend as Suspend
import qualified Pawl.Types.SuspendCounters as SuspendCounters
import qualified Pawl.Types.Zone as Zone

-- CR 702.62a: the suspend ability this object has -- the counters and the
-- cost together -- or Nothing when it has none. A hand member with no card
-- behind it -- a token, an ability -- has no suspend ability.
--
-- Read through the projection (CR 613.1f), since the ability functions in the
-- hand and an effect may take it away there: Patriar's Humiliation's perpetual
-- "loses all abilities" follows the card back to its owner's hand.
-- Pawl.SpecialActionSpec's "CR 613.1f a Durkwood Baloth that perpetually lost
-- all abilities cannot be suspended" proves it.
suspendOf :: ObjectId -> GameState -> Maybe (Suspend.Suspend Keyword)
suspendOf oid gs = do
  _ <- Game.cardOfHandMember oid gs
  Keyword.suspend (Map.keysSet (Projection.keywordsOf oid gs))

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
      && Cost.actionPayableAt PaymentSubject.ForNeither (SuspendCounters.leastX (Suspend.counters ability)) pid oid (Suspend.cost ability) gs

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
-- CR 107.3d's X is named in Cost.payAction, and the one value answers both
-- halves of the printed line: rule 107.3i makes the N and the {X} in the cost
-- the same number, so the card is exiled with as many time counters as the mana
-- it cost. An X below CR 101.1's floor ("X can't be 0") or one the board cannot
-- pay leaves the card in hand with nothing paid.
--
-- NO STAMP, which is the difference from plot and foretell both: CR 702.62b
-- defines "suspended" out of state the game already holds -- exiled, has
-- suspend, has a time counter -- so there is nothing to write down, and no
-- reader can disagree with the board.
suspend :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> ObjectId -> Game ()
suspend perform pid oid = do
  before <- State.get
  Monad.when (canSuspend pid oid before) $ do
    let ability = Maybe.fromMaybe (Suspend.MkSuspend (SuspendCounters.Literal 0) Cost.unpayable) (suspendOf oid before)
    -- CR 107.3d: the taker of the special action names X, at least CR 101.1's
    -- floor. The X prompt TERMINATES for the reason Cost.greatestPayableX needs
    -- one -- a suspend cost's X is a generic mana symbol, so its demand grows
    -- without bound; Pawl.CardSpec's "CR 107.3d every suspend cost's X is one
    -- the board can refuse" is what keeps a printing whose X the board could pay
    -- forever out of the pool. No printed suspend line states a ceiling --
    -- Scryfall `keyword:suspend o:"suspend x"`, 2026-09-17, five cards, each
    -- stating a floor and no maximum.
    paid <- Cost.payAction perform before PaymentSubject.ForNeither (SuspendCounters.leastX (Suspend.counters ability)) pid oid (Suspend.cost ability)
    Monad.forM_ paid $ \announcedX -> do
      -- Rule 107.3i: one announcement, both halves of "Suspend X--{X}...".
      let counters = case Suspend.counters ability of
            SuspendCounters.Literal n -> n
            SuspendCounters.Variable _ -> announcedX
      -- The counters go on the id the move RETURNS and never onto `oid`, the
      -- reading Plot.plot gives its stamp: CR 400.7 mints a fresh incarnation in
      -- exile and deletes the one that was in hand. Nothing comes back when the
      -- move was cancelled, and then there is no exiled card to suspend.
      --
      -- One arrival, Plot.plot's reason: the funnel answers with more than one
      -- only for a melded permanent leaving the battlefield (CR 712.21), and this
      -- special action exiles a card from a hand.
      exiled <- Event.changeZoneReturning oid Zone.Exile
      Monad.forM_ exiled (\newId -> Event.putCounters (CounterCause.ByRule pid) newId CounterKind.Time counters)
