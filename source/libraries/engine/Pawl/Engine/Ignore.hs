-- CR 116.2d: the special action that lets a player pay to ignore the effect of a
-- permanent's static ability for a duration. Three producers are in the pool,
-- across both axes an ability can be aimed on: Leonin Arbiter's "any player may
-- pay {2} for that player to ignore this effect until end of turn" and Damping
-- Engine, whose sentence narrows both the effect's scope and therefore the
-- offer, aim at PLAYERS; Volrath's Curse aims at the enchanted creature.
--
-- The offer side and the payment side, exactly as Pawl.Engine.Room is for CR
-- 116.2m: what the ignore then SUPPRESSES is each carrier's own question, asked
-- through Pawl.Engine.IgnoredAbility, and this module asks none of them. It asks
-- only WHO the ability is affecting, which is the rule's own gate on the offer --
-- a typed question, so no PlayerEffect, PlayerScope, CombatRestriction or
-- ActivationProhibition constructor is visible here.
module Pawl.Engine.Ignore where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Set as Set
import qualified Pawl.Engine.ActivationProhibition as ActivationProhibition
import qualified Pawl.Engine.CombatRestriction as CombatRestriction
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Expiry as Expiry
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaSpending as ManaSpending
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.SpecialAction as SpecialAction

-- Every ignore this permanent's copiable rules text grants: which ability each
-- names (CR 116.2d's "that ability") and what it charges.
--
-- Read off Projection.specialActionsOf and never Card.combined: CR 604.1 has a
-- static ability function from the face the permanent actually shows, and CR
-- 707.2a has a copy derive its abilities from the rules text it copied -- which
-- is the same accessor, and so the same answer, as
-- Pawl.Engine.PlayerEffect.playerAbilitiesOf gives for the prohibition this
-- permission accompanies.
--
-- A LIST rather than the first grant, now that each names an ability: a face may
-- grant the permission on one of several, and each grant is its own offer. No
-- printing grants two -- the four producers print one sentence each -- so the
-- list is a singleton for every card in the pool.
ignoreGrants :: ObjectId -> GameState -> [(AbilityName.AbilityName, Cost.Type.Cost Keyword)]
ignoreGrants oid gs = [(n, c) | SpecialAction.IgnoreThisUntilEndOfTurn n c <- Projection.specialActionsOf oid gs]

-- What this permanent charges to be ignored under this ability's name, if its
-- rules text grants that at all. FIRST grant wins where a face somehow named
-- one ability twice; no printing does.
ignoreCostOf :: ObjectId -> AbilityName.AbilityName -> GameState -> Maybe (Cost.Type.Cost Keyword)
ignoreCostOf oid name gs = case [c | (n, c) <- ignoreGrants oid gs, n == name] of
  [] -> Nothing
  c : _ -> Just c

-- CR 116.2d: may this player ignore this permanent's NAMED ability right now?
-- Three conjuncts:
--
--   * the permanent grants the permission under that name, which also settles
--     that it is on the battlefield with a face to read it from;
--   * the rule's own WHO -- this player is one the NAMED ability is actually
--     affecting, which is what every printed producer's sentence says. Leonin
--     Arbiter's "any player" is the EachPlayer scope its own prohibition carries,
--     and Damping Engine's "that player" is the one player its scope reaches; a
--     seat that ability is not changing the game for is offered nothing to
--     ignore, however much the rest of the permanent is doing to them. Asked as
--     Pawl.Engine.PlayerEffect.affectedBy, which is the typed question -- this
--     module sees no PlayerEffect and no PlayerScope constructor.
--
--     An ability aimed at an OBJECT names no player at all, so the seat is
--     derived from what it restricts: Volrath's Curse and Lost in Thought both
--     say "that creature's controller", so a player controlling any permanent
--     the named ability restricts is offered the action (`affectedThrough`). A
--     DISJUNCTION rather than a second question, because one ability is on one
--     axis or the other and no printing states both -- and neither carrier is
--     cased on here either.
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
canIgnore :: PlayerId -> ObjectId -> AbilityName.AbilityName -> GameState -> Bool
canIgnore pid oid name gs = case ignoreCostOf oid name gs of
  Nothing -> False
  Just cost -> (PlayerEffect.affectedBy pid oid name gs || affectedThrough pid oid name gs) && Cost.canPay pid oid cost gs

-- CR 116.2d's WHO for an ability aimed at an OBJECT: does this player CONTROL a
-- permanent the named ability restricts? That is "that creature's controller",
-- read live rather than off the Aura -- the enchanted creature's controller is
-- offered the action, and the Aura's controller is not.
--
-- Both object-axis carriers, since Volrath's Curse's one sentence states its
-- combat restrictions and its activation prohibition under the same name; each
-- answers a list of ids and neither hands over a constructor.
affectedThrough :: PlayerId -> ObjectId -> AbilityName.AbilityName -> GameState -> Bool
affectedThrough pid oid name gs =
  any
    (\subject -> Projection.controllerOf subject gs == Just pid)
    (CombatRestriction.namedSubjects oid name gs <> ActivationProhibition.namedSubjects oid name gs)

-- Every (permanent, ability name) this player may pay to ignore right now, in
-- battlefield order -- what Action.Ignore is built from, the shape
-- Room.unlockable has.
ignorable :: PlayerId -> GameState -> [(ObjectId, AbilityName.AbilityName)]
ignorable pid gs =
  [ (oid, name)
  | oid <- Set.toAscList (GameState.battlefield gs),
    (name, _) <- ignoreGrants oid gs,
    canIgnore pid oid name gs
  ]

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
ignore :: PlayerId -> ObjectId -> AbilityName.AbilityName -> Game ()
ignore pid oid name = do
  before <- State.get
  case ignoreCostOf oid name before of
    Nothing -> pure ()
    Just cost -> do
      -- CR 118.13c, Pawl.Engine.FaceDown.turnFaceUp's announcement and for its
      -- reasons. CR 116.2d's cost is the one the permission's own sentence
      -- names, and none of the four producers writes a hybrid or Phyrexian
      -- symbol into it, so no prompt is raised today. A printing that did would
      -- be the one to refute that.
      (announced, _) <- Cost.announce PaymentSubject.ForNeither ManaSpending.AsProduced pid oid pure cost
      payment <- Cost.pay Resolve.performManaAbility PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced pid oid announced
      case payment of
        -- CR 733.1's last sentence, Cost.keepingLibraryActions' reason: a mana
        -- ability tapped in the window this payment opened may have shuffled
        -- or revealed, and this reject-not-repair restore must not undo that
        -- too.
        Payment.Unpaid -> Cost.restoreKeepingLibraryActions before
        -- The payment's bound slots are dropped: CR 116.2d's special action puts
        -- nothing on the stack, so there is no resolving object whose effects
        -- could read one.
        Payment.Paid _ ->
          State.modify'
            ( \gs ->
                gs
                  { GameState.ignoredAbilities =
                      IgnoredAbility.MkIgnoredAbility
                        { IgnoredAbility.player = pid,
                          IgnoredAbility.source = oid,
                          IgnoredAbility.ability = name,
                          IgnoredAbility.expiry = Expiry.AtCleanup
                        }
                        : GameState.ignoredAbilities gs
                  }
            )
