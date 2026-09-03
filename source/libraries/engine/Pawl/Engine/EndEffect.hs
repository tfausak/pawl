-- CR 116.2c: the special action that lets a player pay a cost an effect named to
-- end that effect. Gliding Licid's "You may pay {U} to end this effect" is the
-- clause; all twelve Licids print it. CR 116.2c names a second use -- "to stop a
-- delayed triggered ability from triggering" -- which no printing reaches, so
-- Synthetic Standing Bounty is what drives it.
--
-- The offer side and the payment side, exactly as Pawl.Engine.Ignore is for CR
-- 116.2d. Which stored effects a payment reaches is Pawl.Engine.Expiry's
-- question -- that module owns Pawl.Types.Expiry, and this one never sees a
-- constructor of it.
--
-- The permission is created by a RESOLUTION and not printed, which is why there
-- is no Pawl.Types.SpecialAction arm beside CR 116.2d's: the price rides the
-- stored effect (Expiry.WhenPaid), so a card that has not resolved its ability
-- offers nothing.
module Pawl.Engine.EndEffect where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Resolve as Resolve
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.ManaSpending as ManaSpending
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PaidExpiry as PaidExpiry
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PlayerId (PlayerId)

-- The offer ending this object's effect, if it stored one that a payment can end
-- at all: CR 116.2c's price, and CR 109.5's "you" as baked when the effect was
-- stored.
--
-- FIRST offer wins where an object somehow stored two. Nothing in `data/cards/`
-- does: a Licid that has activated has lost the ability, so it cannot animate
-- itself twice, and the synthetic's offer belongs to an instant that has left
-- the stack, so it cannot resolve a second time under the same id.
offerToEnd :: ObjectId -> GameState -> Maybe PaidExpiry.PaidExpiry
offerToEnd oid gs = List.lookup oid (Expiry.paidExpiries gs)

-- CR 116.2c: may this player end this object's effect right now? Three
-- conjuncts:
--
--   * an effect of that object states a price at all, which is the whole of the
--     permission -- "for as long as the effect allows it";
--   * this player is the one the effect allows it to. CR 109.5's "you" is the
--     player who activated the ability, or controlled the spell, that stored the
--     effect -- not the current controller of the object it is on. That
--     seat rides the stored effect (PaidExpiry.player), baked by
--     Pawl.Engine.Expiry.arm, so a control change afterwards leaves the offer
--     where it was -- proved by Pawl.AuraSpec's "CR 109.5 the activator keeps the
--     pay-to-end offer after Confiscate steals the animated Licid";
--   * the cost is payable. An action the player cannot take is not offered,
--     Pawl.Engine.Action.legalActions' posture throughout.
--
-- No conjunct for the PRIORITY clause, for the reason CR 116.2d's has none:
-- legalActions is asked only of the priority holder. And none for a timing
-- restriction, which CR 116.2c allows an effect to state and no producer states.
--
-- Cost.canPay and not Cost.total's CR 601.2f adjustments, Ignore.canIgnore's
-- reason: that rule totals the cost of a spell being cast or an ability being
-- activated, and a special action is neither (#90).
canEnd :: PlayerId -> ObjectId -> GameState -> Bool
canEnd pid oid gs = case offerToEnd oid gs of
  Nothing -> False
  Just offer -> PaidExpiry.player offer == pid && Cost.canPay pid oid (PaidExpiry.cost offer) gs

-- Every object whose effect this player may pay to end right now, in object
-- order -- what Action.EndEffect is built from, Ignore.ignorable's shape.
--
-- Read off the STORED EFFECTS rather than off the battlefield, which is where
-- this differs from every other special action: CR 116.2c's permission belongs
-- to the effect, not to a permanent's printed text, and a stored effect outlives
-- its source leaving the battlefield (CR 611.2's duration is what ends it). The
-- baked seat is then what settles who is offered one whose source has left, and
-- it needs no board to read.
endable :: PlayerId -> GameState -> [ObjectId]
endable pid gs = filter (\oid -> canEnd pid oid gs) (List.nub (fmap fst (Expiry.paidExpiries gs)))

-- CR 116.2c, in order: pay the cost, then end the effect.
--
-- REJECT-NOT-REPAIR, Ignore.ignore's posture: a payment that fails restores the
-- state from before it was attempted and nothing is ended.
--
-- The payment's bound slots are dropped: a special action puts nothing on the
-- stack (CR 116.1), so there is no resolving object whose effects could read one.
endEffect :: PlayerId -> ObjectId -> Game ()
endEffect pid oid = do
  before <- State.get
  case offerToEnd oid before of
    Nothing -> pure ()
    Just offer -> do
      -- CR 118.13c, Pawl.Engine.FaceDown.turnFaceUp's announcement and for its
      -- reasons. CR 116.2c's cost comes off the stored effect, and nothing in
      -- `data/cards/` writes a hybrid or Phyrexian symbol into one, so no
      -- prompt is raised today. A printing that did would be the one to
      -- refute that.
      (announced, _) <- Cost.announce PaymentSubject.ForNeither ManaSpending.AsProduced pid oid pure (PaidExpiry.cost offer)
      payment <- Cost.pay Resolve.performManaAbility PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced pid oid announced
      case payment of
        Payment.Unpaid -> State.put before
        Payment.Paid _ -> State.modify' (Expiry.dropWhenPaidBy oid)
