module Pawl.Cast where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Cost as Cost
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Modal as Modal
import qualified Pawl.PlayerEffect as PlayerEffect
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Target as Target
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CastingPermission as CastingPermission
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.ManaCost (ManaCost)
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
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
    -- A token is never cast: it is created onto the battlefield, never on the stack.
    Source.OfToken _ -> Nothing
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

-- CR 601.2c / 700.2a: castable when at least as many modes are fillable as the
-- selection demands ("choose one" / ChooseExactly 1, the only selection so
-- far). Unobservable for Bolt (AnyTarget always holds a living player); first
-- falsified for a single-mode card by Giant Growth at M3b, and for a modal
-- card by Chaos Charm at M4g (castable via its damage/haste mode with no Wall
-- in play). For a non-modal card (one mode, count 1) this is identical to
-- "every slot fillable": the single mode fillable = all its slots fillable =
-- the whole card's slots.
targetable :: ObjectId -> GameState -> Bool
targetable oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card ->
    let count = Modal.selectionCount (Card.Type.spell card)
     in Set.size (Target.fillableModes oid (Card.Type.spell card) gs) >= fromIntegral count

-- Affordable and correctly timed, actually in this player's hand, fillable, and
-- not prohibited.
castable :: PlayerId -> ObjectId -> GameState -> Bool
castable pid oid gs = case costOf oid gs of
  Nothing -> False
  Just cost ->
    timingOk pid oid gs
      && elem oid (Game.zoneMembers Zone.Hand pid gs)
      -- CR 601.3: no rule or effect prohibits this player from casting a spell
      -- (Rule of Law, Silence). Gated HERE, upstream of Action.legalActions,
      -- because the engine never offers an illegal action and then rejects it.
      && not (PlayerEffect.prohibitsCasting pid gs)
      -- CR 601.2b: a {X} cost is affordable when payable at X=0 (the caster may
      -- always choose X=0); substituteX 0 is the identity on any Variable-free
      -- cost, so every existing card is unaffected.
      --
      -- CR 601.2f: affordability is measured against the TOTAL cost, not the
      -- printed one. Taxing castability without taxing payment lets the player
      -- underpay; taxing payment without taxing castability offers a cast that
      -- cannot be afforded, and there is no mid-announcement rewind (#56).
      && Mana.canPay pid (Cost.total pid oid (Mana.substituteX 0 cost) gs) gs
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
-- permitted, not prohibited, affordable (Mana.canPay), and with a fillable
-- target set (Cast.targetable). Deliberately omits timingOk -- the permission IS
-- the CR 601.3 timing exception (the ruling: "follows all normal rules ...
-- except for timing").
--
-- The prohibition is NOT omitted, and that is the point: CR 601.3 is one
-- sentence with two halves, and the Panglacial permission excepts only the
-- timing one. A Rule of Law still stops a cast from the library.
castableWhileSearching :: PlayerId -> GameState -> [ObjectId]
castableWhileSearching pid gs =
  let permitted oid = maybe False permitsCastWhileSearching (Game.cardOf oid gs)
      affordable oid = case costOf oid gs of
        Nothing -> False
        -- CR 601.2b castability floor at the CR 601.2f total (see Cast.castable).
        Just cost -> Mana.canPay pid (Cost.total pid oid (Mana.substituteX 0 cost) gs) gs
      allowed oid = permitted oid && affordable oid && targetable oid gs
   in if PlayerEffect.prohibitsCasting pid gs
        then []
        else filter allowed (Game.zoneMembers Zone.Library pid gs)

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

-- CR 601.2b then 601.2c: choose modes, THEN targets (601.2c), pay (601.2f-h),
-- move to the stack (601.2a), stamp the choices on the NEW stack incarnation
-- (CR 400.7). Prompting before payment is 601.2's own order; there is no rewind
-- for mid-announcement failure (#56) -- legalActions only offers affordable,
-- fully-fillable casts, so a legal answer cannot fail after the prompt.
-- An illegal answer at ANY step makes the
-- whole cast a no-op: reject-not-repair, the AssignCombatDamage posture. A
-- spell with no slots (in its chosen modes) asks nothing.
castSpell :: PlayerId -> ObjectId -> Game ()
castSpell pid oid = do
  gs <- State.get
  case costOf oid gs of
    Nothing -> pure ()
    Just cost -> case Game.cardOf oid gs of
      Nothing -> pure ()
      Just card -> do
        let decider = Decide.deciderFor pid gs
            legal = Target.fillableModes oid (Card.Type.spell card) gs
            count = Modal.selectionCount (Card.Type.spell card)
        -- CR 601.2b: modes are chosen BEFORE X and targets. Elided (forced,
        -- unprompted) exactly when there is nothing to choose -- as many legal
        -- modes as the selection demands or fewer (a non-modal card's one mode,
        -- or a modal card whose only-just-fillable modes leave no real
        -- choice), #50.
        chosenModes <-
          if Set.size legal <= fromIntegral count
            then pure legal
            else Trans.lift (Program.prompt (Prompt.ChooseModes decider pid oid legal count))
        -- Reject-not-repair: an answer that is not a size-`count` subset of the
        -- legal modes makes the whole cast a no-op, guarding every step below.
        Monad.when (Set.isSubsetOf chosenModes legal && Set.size chosenModes == fromIntegral count) $ do
          let sets = Target.legalSetsExcluding oid (Card.modesTargetSpecs chosenModes card) gs
              -- CR 601.2b precedes 601.2c: choose X before targets, and only when
              -- the cost carries a Variable (a spell with no {X} is not asked).
              hasVariable = case cost of
                ManaCost.MkManaCost symbols -> elem ManaSymbol.Variable symbols
          mAmount <-
            if hasVariable
              then fmap Just (Trans.lift (Program.prompt (Prompt.ChooseX decider pid oid)))
              else pure Nothing
          chosen <-
            if Map.null sets
              then pure Map.empty
              else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid oid sets))
          let keysAgree = Map.keysSet chosen == Map.keysSet sets
              eachLegal = and (Map.intersectionWith Set.member chosen sets)
          Monad.when (keysAgree && eachLegal) $ do
            -- CR 612 binding: choose the basic land types for each text-change
            -- slot. Always answerable (the five basics), so no castability gate.
            let textSlots = Resolve.textChangeSlots card
                ask slot = do
                  pair <- Trans.lift (Program.prompt (Prompt.ChooseBasicLandTypes decider pid oid slot))
                  pure (slot, pair)
            bound <- fmap Map.fromList (traverse ask textSlots)
            -- CR 601.2b then 601.2f: substitute X, then compute the total cost.
            -- The object is still in HAND here, one step before 601.2a moves it
            -- to the stack, so the criterion is read against its hand
            -- projection (#89).
            let paidCost = Cost.total pid oid (maybe cost (\x -> Mana.substituteX x cost) mAmount) gs
            case Mana.payCost pid paidCost gs of
              Nothing -> pure ()
              Just paid -> do
                State.put paid
                Event.changeZone oid Zone.Stack
                -- CR 601.2i: the spell has been cast. Emitted here, AFTER the
                -- last step that can fail, so a rejected announcement records
                -- nothing. Rule of Law counts this event and not the
                -- resolution, so a countered spell still counted (its second
                -- Gatherer ruling).
                State.modify' (Event.recordEvent (GameEvent.SpellCast pid))
                moved <- State.get
                case GameState.stack moved of
                  [] -> pure ()
                  top : _ ->
                    State.put
                      moved
                        { GameState.objects =
                            Map.adjust
                              (\o -> o {Object.bindings = Binding.fromChoices chosen bound mAmount chosenModes})
                              top
                              (GameState.objects moved)
                        }
