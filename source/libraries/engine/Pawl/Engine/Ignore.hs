-- CR 116.2d: the special action that lets a player pay to ignore the effect of a
-- permanent's static ability for a duration. Leonin Arbiter's "any player may
-- pay {2} for that player to ignore this effect until end of turn" is one
-- producer; Damping Engine, whose sentence narrows both the effect's scope and
-- therefore the offer, is the other in the pool.
--
-- The offer side and the payment side, exactly as Pawl.Engine.Room is for CR
-- 116.2m: what the ignore then SUPPRESSES is Pawl.Engine.PlayerEffect.applying's
-- question, which this module never asks. It asks that module only WHO the
-- ability is affecting, which is the rule's own gate on the offer -- a typed
-- question, so no PlayerEffect or PlayerScope constructor is visible here.
module Pawl.Engine.Ignore where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Set as Set
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaSpending as ManaSpending
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.SpecialAction as SpecialAction

-- What this permanent charges to be ignored, if its current face grants the
-- permission at all.
--
-- Read off Game.faceOf and never Card.combined: CR 604.1 has a static ability
-- function from the face the permanent actually shows, which is the same
-- direction Pawl.Engine.PlayerEffect.applying reads Face.playerAbilities from.
--
-- FIRST grant wins where a face somehow printed two. No printing does -- the
-- four producers print one sentence each.
ignoreCostOf :: ObjectId -> GameState -> Maybe (Cost.Type.Cost Keyword)
ignoreCostOf oid gs = case Game.faceOf oid gs of
  Nothing -> Nothing
  Just face -> case [c | SpecialAction.IgnoreThisUntilEndOfTurn c <- Face.specialActions face] of
    [] -> Nothing
    c : _ -> Just c

-- CR 116.2d: may this player ignore this permanent right now? Three conjuncts:
--
--   * the permanent grants the permission, which also settles that it is on the
--     battlefield with a face to read it from;
--   * the rule's own WHO -- this player is one the permanent's static abilities
--     are actually affecting, which is what every printed producer's sentence
--     says. Leonin Arbiter's "any player" is the EachPlayer scope its own
--     prohibition carries, and Damping Engine's "that player" is the one player
--     its scope reaches; a seat the ability is not changing the game for is
--     offered nothing to ignore. Asked as
--     Pawl.Engine.PlayerEffect.affectedBy, which is the typed question -- this
--     module sees no PlayerEffect and no PlayerScope constructor.
--   * the cost is payable. An action the player cannot take is not offered,
--     which is Pawl.Engine.Action.legalActions' posture throughout.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f
-- adjustments, for the reason Room.canUnlock gives: that rule totals the cost of
-- a spell being cast or an ability being activated, and a special action is
-- neither (#90).
--
-- No conjunct for the PRIORITY clause, for the reason CR 116.2a's land play has
-- none: legalActions is asked only of the priority holder. And none for having
-- already ignored it, which CR 116.2d does not forbid: paying again is legal,
-- spends the cost again and changes nothing else -- which is why affectedBy is
-- asked over the UNFILTERED gather rather than over
-- Pawl.Engine.PlayerEffect.applying.
canIgnore :: PlayerId -> ObjectId -> GameState -> Bool
canIgnore pid oid gs = case ignoreCostOf oid gs of
  Nothing -> False
  Just cost -> PlayerEffect.affectedBy pid oid gs && Cost.canPay pid oid cost gs

-- Every permanent this player may pay to ignore right now, in battlefield order
-- -- what Action.Ignore is built from, the shape Room.unlockable has.
ignorable :: PlayerId -> GameState -> [ObjectId]
ignorable pid gs = filter (\oid -> canIgnore pid oid gs) (Set.toAscList (GameState.battlefield gs))

-- CR 116.2d, in the rule's own order: pay the cost, then start ignoring.
--
-- REJECT-NOT-REPAIR, the posture Room.unlock and FaceDown.turnFaceUp take: a
-- payment that fails restores the state from before it was attempted and nothing
-- is ignored. The rule's order is what makes that correct rather than merely
-- tidy -- the cost is paid first, so a failed payment has suppressed nothing to
-- undo.
--
-- Expiry.AtCleanup and not Expiry.arm: arming reads a printed
-- Pawl.Types.Duration, and CR 116.2d's is not printed as one -- see
-- Pawl.Types.SpecialAction on why the duration is not carried. CR 514.2 is what
-- AtCleanup means, and "until end of turn" is what every producer says.
ignore :: PlayerId -> ObjectId -> Game ()
ignore pid oid = do
  before <- State.get
  case ignoreCostOf oid before of
    Nothing -> pure ()
    Just cost -> do
      payment <- Cost.pay ManaSpending.AsProduced pid oid cost
      case payment of
        Payment.Unpaid -> State.put before
        Payment.Paid ->
          State.modify'
            ( \gs ->
                gs
                  { GameState.ignoredAbilities =
                      IgnoredAbility.MkIgnoredAbility
                        { IgnoredAbility.player = pid,
                          IgnoredAbility.source = oid,
                          IgnoredAbility.expiry = Expiry.AtCleanup
                        }
                        : GameState.ignoredAbilities gs
                  }
            )
