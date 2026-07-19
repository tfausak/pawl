module Pawl.Cast where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Target as Target
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CastingPermission as CastingPermission
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.ManaCost (ManaCost)
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone

-- Nothing when the object has no mana cost at all (CR 202.1: a land).
costOf :: ObjectId -> GameState -> Maybe ManaCost
costOf oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> case Object.source obj of
    Source.OfCard printing -> Card.Type.manaCost (Printing.card printing)
    -- An ability on the stack is not a spell; it has no mana cost to cast.
    Source.OfAbility _ _ -> Nothing
    Source.OfTrigger _ _ -> Nothing

-- CR 601.3a / 302.1: a creature spell may be cast only when its controller could
-- cast a sorcery -- a main phase of their own turn, with an empty stack. (The
-- priority requirement is implicit: the engine only offers actions to the player
-- who holds priority.)
--
-- M1a has nothing castable at instant speed, so this gate is the whole timing
-- story. It grows a per-card timing classification when instants arrive.
sorcerySpeed :: PlayerId -> GameState -> Bool
sorcerySpeed pid gs =
  Turn.isMainPhase (GameState.phase gs)
    && GameState.activePlayer gs == pid
    && null (GameState.stack gs)

-- CR 117.1a / 304.1: an instant is castable whenever its controller has
-- priority; anything else needs sorcery speed (CR 601.3a). Priority is
-- implicit: the engine only offers actions to the priority holder.
timingOk :: PlayerId -> ObjectId -> GameState -> Bool
timingOk pid oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> Card.isInstant card || sorcerySpeed pid gs

-- CR 601.2c: every target slot must have at least one legal recipient, or the
-- spell cannot be cast. Unobservable for Bolt (AnyTarget always holds a living
-- player); first falsified by Giant Growth at M3b.
targetable :: ObjectId -> GameState -> Bool
targetable oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> not (any Set.null (Map.elems (Target.legalSets (Card.Type.targetSpecs card) gs)))

-- Affordable and correctly timed, actually in this player's hand, and fillable.
castable :: PlayerId -> ObjectId -> GameState -> Bool
castable pid oid gs = case costOf oid gs of
  Nothing -> False
  Just cost ->
    timingOk pid oid gs
      && elem oid (Game.zoneMembers Zone.Hand pid gs)
      && Mana.canPay pid cost gs
      && targetable oid gs

castableSpells :: PlayerId -> GameState -> [ObjectId]
castableSpells pid gs = filter (\oid -> castable pid oid gs) (Game.zoneMembers Zone.Hand pid gs)

-- CR 601.3 (Panglacial): may this card be cast from the library while its
-- controller searches their own library? A membership test on the card's casting
-- permissions -- Cast is the sole reader of CastingPermission, and this is a
-- classification, never card identity.
permitsCastWhileSearching :: Card.Type.Card -> Bool
permitsCastWhileSearching card =
  elem CastingPermission.CastFromLibraryWhileSearching (Card.Type.castingPermissions card)

-- The library cards this player may cast while searching their own library:
-- permitted, affordable (Mana.canPay), and with a fillable target set (Cast
-- .targetable). Deliberately omits timingOk -- the permission IS the CR 601.3
-- timing exception (the ruling: "follows all normal rules ... except for timing").
castableWhileSearching :: PlayerId -> GameState -> [ObjectId]
castableWhileSearching pid gs =
  let permitted oid = maybe False permitsCastWhileSearching (Game.cardOf oid gs)
      affordable oid = case costOf oid gs of
        Nothing -> False
        Just cost -> Mana.canPay pid cost gs
   in filter (\oid -> permitted oid && affordable oid && targetable oid gs) (Game.zoneMembers Zone.Library pid gs)

-- CR 601.3 (Panglacial): while a player searches their own library, offer them
-- the chance to cast a castable-while-searching card from it, before any card is
-- found (per the ruling). Loops so multiple copies may be cast (also per the
-- ruling); each cast removes a card from the library, so castableWhileSearching
-- shrinks and the loop terminates. castSpell is the re-entrant call -- casting
-- mid-resolution, the whole point.
castWhileSearching :: PlayerId -> Game ()
castWhileSearching pid = do
  gs <- State.get
  case castableWhileSearching pid gs of
    [] -> pure ()
    options -> do
      let decider = Decide.deciderFor pid gs
      choice <- Trans.lift (Program.prompt (Prompt.CastWhileSearching decider pid options))
      case choice of
        Nothing -> pure ()
        Just oid ->
          -- Reject-not-repair: an option not in the offered set is a no-op that
          -- ends the loop, never a repair.
          Monad.when (elem oid options) $ do
            castSpell pid oid
            castWhileSearching pid

-- CR 601: choose targets (601.2c), pay (601.2f-h), move to the stack (601.2a),
-- stamp the choices on the NEW stack incarnation (CR 400.7). Prompting before
-- payment is 601.2's own order; what stays elided is the rewind -- legalActions
-- only offers affordable, fully-fillable casts, so a legal answer cannot fail
-- after the prompt (expiry: mid-announcement failure, e.g. cast-during-search).
-- An illegal answer makes the whole cast a no-op: reject-not-repair, the
-- AssignCombatDamage posture. A spell with no slots asks nothing.
castSpell :: PlayerId -> ObjectId -> Game ()
castSpell pid oid = do
  gs <- State.get
  case costOf oid gs of
    Nothing -> pure ()
    Just cost -> case Game.cardOf oid gs of
      Nothing -> pure ()
      Just card -> do
        let decider = Decide.deciderFor pid gs
            sets = Target.legalSets (Card.Type.targetSpecs card) gs
        chosen <-
          if Map.null sets
            then pure Map.empty
            else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid oid sets))
        let keysAgree = Map.keysSet chosen == Map.keysSet sets
            eachLegal = and (Map.intersectionWith Set.member chosen sets)
        if not (keysAgree && eachLegal)
          then pure ()
          else do
            -- CR 612 binding: choose the basic land types for each text-change
            -- slot. Always answerable (the five basics), so no castability gate.
            let textSlots = Resolve.textChangeSlots card
                ask slot = do
                  pair <- Trans.lift (Program.prompt (Prompt.ChooseBasicLandTypes decider pid oid slot))
                  pure (slot, pair)
            bound <- fmap Map.fromList (traverse ask textSlots)
            case Mana.payCost pid cost gs of
              Nothing -> pure ()
              Just paid -> do
                let moved = Event.changeZone oid Zone.Stack paid
                case GameState.stack moved of
                  [] -> State.put moved
                  top : _ ->
                    State.put
                      moved
                        { GameState.objects =
                            Map.adjust
                              (\o -> o {Object.bindings = Binding.fromChoices chosen bound Nothing})
                              top
                              (GameState.objects moved)
                        }
