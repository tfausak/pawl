-- CR 116.2c: the special action that lets a player pay a cost an effect named to
-- end that effect. Gliding Licid's "You may pay {U} to end this effect" is the
-- clause; all twelve Licids print it, and nothing else in the pool does.
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
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Cost as Cost.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaSpending as ManaSpending
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)

-- What ending this object's effect costs, if it stored one that a payment can
-- end at all.
--
-- FIRST offer wins where an object somehow stored two. No printing does: a Licid
-- that has activated has lost the ability, so it cannot animate itself twice,
-- and no other card prints the clause.
costToEnd :: ObjectId -> GameState -> Maybe (Cost.Type.Cost Keyword)
costToEnd oid gs = List.lookup oid (Expiry.paidExpiries gs)

-- CR 116.2c: may this player end this object's effect right now? Three
-- conjuncts:
--
--   * an effect of that object states a price at all, which is the whole of the
--     permission -- "for as long as the effect allows it";
--   * this player is the one the effect allows it to. CR 109.5 makes the "you"
--     of "you may pay" the effect's controller, and a stored effect remembers
--     its source rather than a controller, so the source's CURRENT controller is
--     read (Projection.controllerOf). That is the same object whose ability
--     created the effect and, for every producer, the same seat -- a Licid
--     animating itself is its own source, so a control-change moves the offer
--     with the permanent. Not implemented: an effect whose controller is not its
--     source's current controller, which needs a controller on
--     Pawl.Types.ContinuousEffect and has no producer while the Licids are the
--     only ones (#2137);
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
canEnd pid oid gs = case costToEnd oid gs of
  Nothing -> False
  Just cost -> Projection.controllerOf oid gs == Just pid && Cost.canPay pid oid cost gs

-- Every object whose effect this player may pay to end right now, in object
-- order -- what Action.EndEffect is built from, Ignore.ignorable's shape.
--
-- Read off the STORED EFFECTS rather than off the battlefield, which is where
-- this differs from every other special action: CR 116.2c's permission belongs
-- to the effect, not to a permanent's printed text, and a stored effect outlives
-- its source leaving the battlefield (CR 611.2's duration is what ends it).
-- canEnd's controller conjunct is then what declines to offer one whose source
-- has no controller to read.
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
  case costToEnd oid before of
    Nothing -> pure ()
    Just cost -> do
      payment <- Cost.pay Nothing ManaSpending.AsProduced pid oid cost
      case payment of
        Payment.Unpaid -> State.put before
        Payment.Paid _ -> State.modify' (Expiry.dropWhenPaidBy oid)
