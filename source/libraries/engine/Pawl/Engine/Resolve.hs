-- Resolving a spell or ability (CR 608): modes, targets re-checked, the
-- clause-by-clause drive of Pawl.Engine.Resolve.Effect, and the static slot
-- reading in Pawl.Engine.Resolve.Slots. Both siblings are imported by callers
-- under the same Resolve alias.
module Pawl.Engine.Resolve where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword.Engine
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.PlayerDesignation as PlayerDesignation
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replacement as Replacement
import Pawl.Engine.Resolve.Effect (apnapPlayersOf, applyClauseEffects, applyEffect, applyEffectWith, clauseIsImpossible, noSubgame, performManaAbility, targetSlotsOf)
import Pawl.Engine.Resolve.Slots (boundSlots, conditionSlots, effectContext, effectViewOf, joinSlots, oneSlot, playerRefSlots, quantitySlots, slotBindings, slotsAreExhaustive, slotsOf)
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Integer as Integer
import Pawl.Types.AbilityName (AbilityName)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.Binding as Binding.Type
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.Clause as Clause
import Pawl.Types.ClauseIndex (ClauseIndex)
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Crewing as Crewing
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Mode as Mode
import Pawl.Types.ModeIndex (ModeIndex)
import Pawl.Types.ModeInstance (ModeInstance)
import qualified Pawl.Types.ModeInstance as ModeInstance
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.OrElse as OrElse
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PayObligation as PayObligation
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import qualified Pawl.Types.PendingEntryEffect as PendingEntryEffect
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.Result (Result)
import Pawl.Types.SlotArity (SlotArity)
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotCount as SlotCount
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneScope as ZoneScope

-- Every slot ONE target slot reads: its pool's, its filter's, and its CR 202.3
-- computed bound's. Its own name is not among them -- this is what the slot
-- READS, and CR 601.2c binds it only once it has been answered.
--
-- Its own function because a card declares a target slot in THREE places, and
-- each needs its own reader. Enumerated off the three types that hold one:
--
--   * Mode.targetSlots -- CR 601.2c's ordinary target, declared inside a mode.
--     modeSlots below folds this one, and the corpus lint that pairs a mode's
--     reads with its declarations is what consumes it.
--   * Face.enchant -- CR 303.4a's enchant slot, declared on the face BESIDE the
--     modes (Card.enchantSlotMap), so it is in no mode's declared set.
--   * Modification.GainEnchant -- the same slot GRANTED by a CR 613.1f layer 6
--     effect (Cloudform, the Licids, CR 702.103b's bestow). What answers it is
--     never the granting mode: the grant is a CR 611.2 continuous effect that
--     outlives the resolution that made it, so by the time CR 601.2c chooses for
--     a bestowed spell (Card.modesTargetSlotsGiven) or CR 303.4c's state-based
--     action re-reads CR 702.5a against a permanent, the announcement the grant
--     was written in is gone. Declaring the name would not rescue it.
--
-- The last two are one claim, and Pawl.CardSpec's "an enchant slot reads no slot,
-- printed or granted" sweep is what states it: neither may read anything.
targetSlotSlots :: TargetSlot.TargetSlot -> Map.Map SlotName SlotArity
targetSlotSlots slot =
  joinSlots
    [ poolSlot (TargetSlot.pool slot),
      -- Every slot the slot's own FILTER names -- CR 603.2's "target artifact or
      -- enchantment that player controls".
      maybe Map.empty (Map.fromSet (const SlotArity.One) . Filter.boundSlots) (TargetSlot.filter slot),
      -- And every slot its CR 202.3 computed bound names -- Venerable Warsinger's
      -- "mana value X or less ... where X is the amount of damage this creature
      -- dealt to that player", whose X is the trigger's own event amount
      -- (Pawl.Engine.Binding.eventAmount). Target.slotContext is what answers it,
      -- off the announcement the caller hands over.
      --
      -- What it buys is the pairing -- a card whose bound names an amount its
      -- CONDITION does not supply (Pawl.Engine.Event.Binding.eventBindingSlots) is caught
      -- only because the read is reported here. No card in data/cards/ misauthors
      -- that pairing, so the proof is a planted one.
      --
      -- quantitySlots' WHOLE answer, which is what makes a bound naming a slot
      -- only through a PlayerRef buried inside the number ("mana value X or less,
      -- where X is the amount of life THAT PLAYER gained this turn") or through CR
      -- 400.7j's Scope.OverBound visible to the equality above.
      -- Pawl.AbilitySlotLintSpec's "the lint itself catches a computed bound
      -- naming a slot through a player" is the case that proves it.
      maybe Map.empty quantitySlots (TargetSlot.amount slot),
      -- CR 601.2c's computed COUNT is a Quantity too, and names slots the same
      -- way its bound does, so it is reported beside it or a slot named only
      -- there would dangle.
      maybe Map.empty quantitySlots (SlotCount.quantity (TargetSlot.count slot))
    ]

-- Every slot a whole MODE reads: its effects', every payer CR 118.12a's "unless
-- [a player] pays" names, every slot that gate's own "for each" counts over,
-- every slot a CR 701.46a "if" tests, and every slot a target slot's own pool,
-- filter or bound names. A payer, multiplier, gate or pool slot no effect also
-- reads would otherwise dangle.
modeSlots :: Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Map.Map SlotName SlotArity
modeSlots mode =
  let -- Every clause's payer: CR 118.12 scopes a resolution cost to its clause.
      payerSlot = maybe Map.empty (playerRefSlots . PayGate.payer) . Clause.payGate
      -- And the gate's OTHER slot-reading position, its "for each" multiplier
      -- (Pawl.Types.PayGate.perEach): a cost scaled by what a bound object names is
      -- a read the payer field need not repeat, and payGatePaidBy evaluates it
      -- against this resolution's own context, so the slot really is asked for.
      -- quantitySlots' WHOLE answer, targetSlotSlots' computed bound's reason.
      multiplierSlot = maybe Map.empty quantitySlots . (Clause.payGate Monad.>=> PayGate.perEach)
      -- And every clause's ASKER, for its reason: CR 603.5's "may" is scoped to a
      -- clause too, and Jungle Wayfinder's names the table rather than a slot --
      -- but a card may name one, and an asker slot no effect also reads would
      -- otherwise dangle.
      askerSlot clause = case Clause.optionality clause of
        Optionality.Mandatory -> Map.empty
        Optionality.Optional ref -> playerRefSlots ref
      -- And every clause's branch CHOOSER, for the same reason one rider over: CR
      -- 608.2d's announcement is scoped to a clause pair and its reference may
      -- name a slot.
      chooserSlot = maybe Map.empty (playerRefSlots . OrElse.chooser) . Clause.orElse
      -- And every clause's CR 701.46a "if", which CR 608.2c lets read what an
      -- earlier clause of the same resolution bound: Psychic Miasma's "if a land
      -- card is discarded this way" counts over CR 400.7j's fold of the slot its
      -- first clause binds. A gate is the ONLY place a card may read a slot and
      -- perform nothing, so a read reported nowhere else dangles here.
      conditionSlot = maybe Map.empty conditionSlots . Clause.condition
   in joinSlots
        [ joinSlots (fmap slotsOf (Foldable.toList (Mode.allEffects mode))),
          joinSlots (fmap payerSlot (Foldable.toList (Mode.clauses mode))),
          joinSlots (fmap multiplierSlot (Foldable.toList (Mode.clauses mode))),
          joinSlots (fmap askerSlot (Foldable.toList (Mode.clauses mode))),
          joinSlots (fmap chooserSlot (Foldable.toList (Mode.clauses mode))),
          joinSlots (fmap conditionSlot (Foldable.toList (Mode.clauses mode))),
          joinSlots (fmap targetSlotSlots (Map.elems (Mode.targetSlots mode)))
        ]

-- The slot a target pool draws its candidates from, if it draws them from one
-- (CR 400.1's per-player graveyard), read singly.
poolSlot :: Pool.Pool -> Map.Map SlotName SlotArity
poolSlot pool = case pool of
  Pool.Creatures -> Map.empty
  Pool.Players -> Map.empty
  Pool.AnyTarget -> Map.empty
  Pool.Permanents -> Map.empty
  Pool.Spells -> Map.empty
  Pool.Abilities -> Map.empty
  Pool.SpellsAndPermanents -> Map.empty
  Pool.PlayersAndPlaneswalkers -> Map.empty
  Pool.CardsInGraveyard scope -> case scope of
    ZoneScope.Scoped _ -> Map.empty
    ZoneScope.InSlot slot -> oneSlot slot
    ZoneScope.ControllerOfBound slot -> oneSlot slot
  Pool.CardsInExile -> Map.empty
  -- The graveyard half's scope; the battlefield half names no slot.
  Pool.CreaturesAndCardsInGraveyard scope -> case scope of
    ZoneScope.Scoped _ -> Map.empty
    ZoneScope.InSlot slot -> oneSlot slot
    ZoneScope.ControllerOfBound slot -> oneSlot slot

-- CR 603.7: the delayed abilities an effect list ARMS, by name.
armedAbilities :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Set AbilityName
armedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _) -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- CR 603.7: armedAbilities narrowed to the arms whose firing is gated past the
-- turn that armed them, i.e. not Onset.Immediately.
onsetGatedAbilities :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Set AbilityName
onsetGatedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger _ Onset.Immediately _) -> Nothing
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _) -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- boundSlots over a whole effect list: the write half of the dataflow lint.
definedSlots :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Set SlotName
definedSlots = foldMap boundSlots

-- definedSlots' other half, one MODE at a time: the slot a CR 118.12 gate binds
-- as it is answered (Binding.gatePlayers, stamped by payGateAdmits). A mode
-- stating no gate binds nothing, so a card reading that name without offering a
-- resolution cost is still caught by the dataflow lint.
gateDefinedSlots :: Mode.Mode card ability -> Set SlotName
gateDefinedSlots mode
  | any (Maybe.isJust . Clause.payGate) (Mode.clauses mode) = Set.singleton Binding.gatePlayers
  | otherwise = Set.empty

-- gateDefinedSlots' twin for CR 603.5's "may": the seats that took it
-- (Binding.mayPlayers, stamped by exercises). A mode printing no "may" binds
-- nothing, so a card reading that name without an optional clause is still
-- caught by the dataflow lint.
mayDefinedSlots :: Mode.Mode card ability -> Set SlotName
mayDefinedSlots mode
  | any (isOptional . Clause.optionality) (Mode.clauses mode) = Set.singleton Binding.mayPlayers
  | otherwise = Set.empty
  where
    isOptional o = case o of
      Optionality.Mandatory -> False
      Optionality.Optional _ -> True

-- The same for CR 701.55d's villainous pass: the seat whose option is being
-- performed (Binding.facingPlayers, stamped by villainousPass). A mode printing
-- no villainous either-or binds nothing, so a card reading that name outside one
-- is still caught by the dataflow lint.
orElseDefinedSlots :: Mode.Mode card ability -> Set SlotName
orElseDefinedSlots mode
  | any (maybe False OrElse.villainous . Clause.orElse) (Mode.clauses mode) = Set.singleton Binding.facingPlayers
  | otherwise = Set.empty

-- CR 700.2d: run ONE chosen instance's clauses with its own namespace for the
-- slots its mode DEFINES mid-resolution -- Effect.MoveToZone's CR 400.7
-- incarnation, Effect.Create's minted tokens, Effect.Destroy's count, Effect
-- .PlaySubgame's loser. Modal.instanceSlot keeps the slots a mode DECLARES
-- apart; those are written under the name the card PRINTS, so a mode chosen
-- twice would have its second write land on the first's key and "different
-- targets may be chosen" would be unobservable for them.
--
-- A SWAP around the instance rather than a rename at each write, because only
-- some readers of a defined slot come through Modal.instanceView: Resolve
-- .Effect's slotOne and slotGroup, Filter.IsBound and the ObjectRef readers go
-- to the live bindings by the printed name, and ArmDelayedTrigger captures them
-- raw. Emptying the printed name first leaves every one of those reading this
-- instance's own definition, and filing what it wrote under Modal.instanceSlot
-- afterwards leaves the earlier occurrence's standing where it was.
--
-- Observable exactly where an instance's define is SKIPPED and its read still
-- runs -- CR 400.7 having deleted the object that instance names, most simply
-- because an earlier occurrence moved it -- since two instances that both
-- define write in sequence and each then reads its own. Pawl.ModalSpec's "CR
-- 700.2d the second copy of a mode whose target is gone defines no object of
-- its own" is the proof.
--
-- The instance's own writes win over what was stashed (Map.union's left bias),
-- which is what keeps occurrence 0 -- whose instanceSlot name IS the printed one
-- -- writing where it always did.
withDefinedSlots :: ObjectId -> ModeInstance -> Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game a -> Game a
withDefinedSlots holder mi mode action =
  let defined = definedSlots (Foldable.toList (Mode.allEffects mode))
   in if Set.null defined
        then action
        else do
          outer <- State.state (takeBindings holder defined)
          result <- action
          inner <- State.state (takeBindings holder defined)
          State.modify' (putBindings holder (Map.union (Map.mapKeys (Modal.instanceSlot mi) inner) outer))
          pure result

-- withDefinedSlots' first half: lift `names` off `holder`'s bindings, answering
-- what was under them. A holder that has ceased carries nothing to lift and
-- takes nothing back (Map.adjust is silent on a missing key), which is the same
-- answer CR 729.5's detached bindings give the rest of this module.
takeBindings :: ObjectId -> Set SlotName -> GameState -> (Map SlotName Binding.Type.Binding, GameState)
takeBindings holder names gs =
  let taken = maybe Map.empty (flip Map.restrictKeys names . Object.bindings) (Game.lookupObject holder gs)
      put obj = obj {Object.bindings = Map.withoutKeys (Object.bindings obj) names}
   in (taken, gs {GameState.objects = Map.adjust put holder (GameState.objects gs)})

-- takeBindings' inverse: put `bindings` back onto `holder`, preferring them to
-- whatever shares a key.
putBindings :: ObjectId -> Map SlotName Binding.Type.Binding -> GameState -> GameState
putBindings holder bindings gs =
  let put obj = obj {Object.bindings = Map.union bindings (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- A resolving spell's PROJECTED modes: only its chosen ones (CR 608.2c/700.2),
-- with every text change affecting it applied (CR 612). Modes rather than a flat
-- effect list because CR 603.5's "may" belongs to a clause within a mode.
--
-- The mode's TARGET SLOTS are rewritten by targetSlotsOf instead: CR 608.2b
-- re-reads them off the printed face, which unions in CR 303.4a's enchant slot
-- and, for CR 702.96b's overloaded spell, drops them all.
modesOf :: ObjectId -> GameState -> [(ModeInstance, Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))]
modesOf oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Game.faceOf oid gs of
    Nothing -> []
    Just face ->
      let chosen = Binding.modesOf (Object.bindings obj)
          changes = Projection.textChangesAffecting oid gs
          rewrite = Projection.rewriteEffect changes
          rewriteClause c =
            c
              { Clause.effects = fmap rewrite (Clause.effects c),
                -- A clause gate's Filters are printed words CR 612.1 changes.
                Clause.condition = fmap (Projection.rewriteCondition changes) (Clause.condition c)
              }
          rewriteMode m = m {Mode.clauses = fmap rewriteClause (Mode.clauses m)}
       in fmap (fmap rewriteMode) (Card.chosenModes chosen face)

-- CR 405.4: who controls a SPELL on the stack -- both CR 608.2b's legality
-- perspective and the effects' execution, which must name the same player. The
-- player who CAST it, stamped at CR 601.2a's move, but read THROUGH the
-- projection because CR 613.1b's layer 2 can override it (CR 109.4).
spellController :: Object.Object -> ObjectId -> GameState -> PlayerId
spellController obj oid gs = Maybe.fromMaybe (Projection.defaultControllerOf obj) (Projection.controllerOf oid gs)

-- CR 608.2b: are ALL of this spell's targets illegal? A spell with no target slot
-- never fizzles, and one with several survives if any one is still legal.
-- Reserved slots are not targets and are vacuously legal. Shared with the Aura
-- path in Pawl.Engine.Stack, so the two cannot drift.
targetsAllIllegal :: ObjectId -> GameState -> Bool
targetsAllIllegal oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Game.faceOf oid gs of
    Nothing -> False
    Just face ->
      let slots = targetSlotsOf obj oid gs face
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipients = case Map.lookup slot slots of
            Nothing -> recipients
            -- CR 608.2b's perspective is the SPELL's controller (CR 405.4).
            Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just (spellController obj oid gs)) (Object.bindings obj) oid recipient targetSlot gs) recipients
          legal = Map.mapWithKey legalSlot chosen
          targeted = Map.restrictKeys legal (Map.keysSet slots)
       in -- Measured on the TARGETS chosen, not the slots declared: CR 115.6
          -- makes a spell that chose zero targets untargeted.
          not (Map.null targeted) && all Set.null (Map.elems targeted)

-- CR 608.2b then CR 608.2: re-validate every filled slot; if the spell has slots
-- and ALL are now illegal it fizzles to the graveyard with no effect applied.
-- Otherwise the effects run in order (CR 608.2c), each skipping a slot whose
-- target is illegal, and the spell goes to its owner's graveyard (CR 608.2n).
--
-- Per CR 608.2c the bindings are re-read before EACH effect, so a slot DEFINED
-- mid-resolution is visible to a later one; target-slot legality stays fixed at
-- the start. `runSubgame` is the injected nested-game runner.
resolveSpellWith :: Game Result -> ObjectId -> Game ()
resolveSpellWith runSubgame oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Game.faceOf oid gs of
      Nothing -> pure ()
      Just face ->
        -- CR 608.2b/700.2c: re-validate only the CHOSEN modes' slots.
        let chosenSelection = Binding.modesOf (Object.bindings obj)
            slots = targetSlotsOf obj oid gs face
            -- CR 700.2d: the slots the MODES own -- `slots` minus CR 303.4a's
            -- enchant slot.
            modeOwnedSlots = Modal.modesTargetSlots chosenSelection (Face.spell face)
            legalSlot slot recipients = case Map.lookup slot slots of
              -- CR 608.2b is about TARGETS. A slot declaring none is a RESERVED
              -- binding and was never targeted.
              Nothing -> recipients
              -- Per RECIPIENT and not per slot (CR 608.2b): the slot's surviving
              -- targets are still affected.
              Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just (spellController obj oid gs)) (Object.bindings obj) oid recipient targetSlot gs) recipients
         in if targetsAllIllegal oid gs
              then Event.changeZone oid Zone.Graveyard
              else do
                let effectController = spellController obj oid gs
                -- CR 702.174j's spell ability, ahead of everything else the card
                -- says: "the effect of a gift ability always happens before any
                -- other spell abilities of the card". Ahead of ascend's line
                -- below is a REGRESSION FENCE -- no printing carries both
                -- keywords.
                giftOnSpellResolution runSubgame oid effectController
                -- CR 702.131a's spell ability, ahead of the modes: ascend is
                -- printed above the card's other text, and CR 608.2c follows a
                -- spell's instructions in printed order -- Secrets of the Golden
                -- City's "if you have the city's blessing, draw three cards
                -- instead" reads the mark this line may just have granted.
                PlayerDesignation.ascendOnSpellResolution oid effectController
                Monad.forM_ (modesOf oid gs) $ \(mi, mode) -> withDefinedSlots oid mi mode $ do
                  let idx = ModeInstance.index mi
                      -- CR 608.2c's printed order, and the lookup CR 608.2d's
                      -- either-or reads its SIBLING back out of.
                      indexedClauses = zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses mode))
                      applyOne eff = do
                        -- Re-read the live bindings for THIS effect: a prior
                        -- PlaySubgame may have bound its winner slot.
                        bindingsNow <- State.gets (liveBindings obj oid)
                        let chosenNow = Binding.targetsOf bindingsNow
                            legalNow = Map.mapWithKey legalSlot chosenNow
                        applyEffectWith
                          runSubgame
                          oid
                          oid
                          effectController
                          (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) legalNow)
                          (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) chosenNow)
                          eff
                      -- CR 701.55d's per-player limb, the callback villainousPass
                      -- drives. Everything it reads is re-read HERE rather than
                      -- taken from the fold's own snapshot, which is the whole
                      -- point of rule 701.55d: a later chooser's limb runs against
                      -- the board an earlier chooser's limb left.
                      performLimb limbIdx facing (answers, ran) = case lookup limbIdx indexedClauses of
                        Nothing -> pure (answers, ran)
                        Just limb -> do
                          gateBindings <- State.gets (liveBindings obj oid)
                          let instanceView = Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode)
                              legalHere = instanceView (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                              boundHere = Map.keysSet (instanceView gateBindings)
                          gated <- gateHolds effectController oid (instanceView (Binding.targetsOf gateBindings)) gateBindings limb
                          taken <- if gated then exercises oid oid effectController idx limbIdx boundHere legalHere (Just facing) limb else pure False
                          (admitted, answers2) <-
                            if taken
                              then payGateAdmits oid oid effectController idx limbIdx (instanceView (Map.mapWithKey legalSlot (Binding.targetsOf (Object.bindings obj)))) (Just facing) answers limb
                              else pure (False, answers)
                          Monad.when admitted (applyClauseEffects oid applyOne (Foldable.toList (Clause.effects limb)))
                          pure (answers2, recordTaken admitted limbIdx ran)
                  -- CR 608.2e's clause is the unit all four gates cover, so each
                  -- is asked once per clause. The fold carries this mode
                  -- INSTANCE's CR 118.12 answers and the clauses whose
                  -- instructions ran, per instance because CR 700.2d makes a mode
                  -- chosen twice make its offer twice.
                  Monad.foldM_
                    ( \(answers, picked, ran) (cIdx, clause) -> do
                        -- CR 608.2c's "If you do" first: a clause hanging off one
                        -- the fold has not recorded is skipped entirely, so no
                        -- later gate raises a prompt whose answer cannot matter.
                        let hangs = ifTakenHolds ran clause
                        -- CR 701.46a's printed "if" next, against the LIVE
                        -- bindings (CR 608.2c): a slot an earlier clause DEFINED
                        -- is part of the state this one is read against, and the
                        -- re-read adds only defined slots. A REGRESSION FENCE --
                        -- mutating this half back leaves the suite green.
                        gateBindings <- State.gets (liveBindings obj oid)
                        gated <- if hangs then gateHolds effectController oid (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Binding.targetsOf gateBindings)) gateBindings clause else pure False
                        -- CR 603.5 / 608.2d: then the printed "may", against the
                        -- SAME live bindings CR 608.2b's filter is applied to, so
                        -- a clause whose every read is dead is not asked about.
                        let legalNowForMay = Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                            boundNowForMay = Map.keysSet (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) gateBindings)
                        -- CR 608.2d's "or" next, and BEFORE the "may": Twiddle
                        -- prints one "may" over the pair, so a branch a player
                        -- did not announce has no "may" left to offer THEM.
                        --
                        -- A branch is on offer only if its own printed "if"
                        -- holds (CR 701.46a, off the same live bindings this
                        -- clause's gate read) and its instruction can be carried
                        -- out at all (CR 608.2d). The condition half is a
                        -- REGRESSION FENCE: no either-or in data/cards prints one.
                        let eligible i = case lookup i indexedClauses of
                              Nothing -> pure False
                              Just sibling -> do
                                held <- gateHolds effectController oid (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Binding.targetsOf gateBindings)) gateBindings sibling
                                State.gets (\gsNow -> held && not (clauseIsImpossible oid oid effectController legalNowForMay gsNow sibling))
                        -- CR 701.55d's exception to rule 608.2e, ahead of the
                        -- ordinary either-or: a villainous pair is chosen AND
                        -- performed for one player before the next player is
                        -- asked, so the whole pair happens here and the sibling's
                        -- own arrival finds it already answered.
                        case facedVillainously picked cIdx clause of
                          Just (orElse, limbs) | gated -> do
                            (answers2, ran2) <- villainousPass oid effectController idx legalNowForMay orElse limbs performLimb (answers, ran)
                            pure (answers2, Map.insert (NonEmpty.head limbs) Map.empty picked, ran2)
                          _ -> do
                            (announced, picked2) <- if gated then chosenBranch oid effectController idx cIdx legalNowForMay eligible picked clause else pure (Just Set.empty, picked)
                            let branch = maybe True (not . Set.null) announced
                            taken <- if branch then exercises oid oid effectController idx cIdx boundNowForMay legalNowForMay announced clause else pure False
                            -- CR 118.12: then the cost paid on resolution, against the
                            -- START-of-resolution targets to match CR 608.2b's single
                            -- re-validation. Both maps are projected into THIS
                            -- instance's view (CR 700.2d) after legality is decided,
                            -- since deciding it after the rename would miss in `slots`.
                            (admitted, answers2) <-
                              if taken
                                then
                                  let chosenAtStart = Binding.targetsOf (Object.bindings obj)
                                   in payGateAdmits
                                        oid
                                        oid
                                        effectController
                                        idx
                                        cIdx
                                        (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Map.mapWithKey legalSlot chosenAtStart))
                                        announced
                                        answers
                                        clause
                                else pure (False, answers)
                            Monad.when admitted (applyClauseEffects oid applyOne (Foldable.toList (Clause.effects clause)))
                            pure (answers2, picked2, recordTaken admitted cIdx ran)
                    )
                    (Map.empty, Map.empty, Set.empty)
                    indexedClauses
                applyEpic oid effectController
                finishSpell oid face effectController

-- CR 702.174b's instant-and-sorcery half: "If this spell's gift cost was paid,
-- [effect]". A SPELL ability, where rule 702.174b's permanent half is a triggered
-- one, so it is performed here as the spell resolves rather than minted by
-- Pawl.Engine.Keyword.abilitiesFor -- nothing mints a spell ability from a
-- keyword, and CR 702.131a's ascend is the sibling that answers the same shape
-- the same way.
--
-- The keyword AND the card types come off the PROJECTION, ascendOnSpellResolution's
-- reading of CR 613.1f and CR 613.1d: a spell granted gift by a text-changing
-- effect gives one, and one whose text was blanked does not. The type test is what
-- rule 702.174b's two halves are told apart by.
--
-- That type test is a REGRESSION FENCE rather than proved behaviour: Stack's
-- resolve dispatches on the PRINTED face, sending every permanent spell down the
-- entry branch instead, so nothing on any board reaches this line carrying a
-- permanent's types and dropping the test leaves the suite green. Rule 702.174b's
-- own wording is what it rests on -- and what the projection adds over the printed
-- face is CR 613.1d's type change.
--
-- "If this spell's gift cost was paid" is Object.paidCosts, the record CR 601.2b's
-- payment writes and the one Quantity.TimesPaid reads for the permanent half's
-- intervening "if" (Pawl.Engine.Keyword.gift). Read off the spell's own object,
-- which for a COPY of the spell is the copy's own -- and CR 707.2 carries a stack
-- object's cast-time choices to its copy ("whether it was kicked"), so a copy of a
-- promised gift spell gives the gift again. That is the opposite of the permanent
-- half's answer one rule over, where CR 707.2 copies no such record onto a Clone.
--
-- That guard is a REGRESSION FENCE too, and for the reason
-- Pawl.Engine.Keyword.gift's intervening "if" is: widening it to admit an unpaid
-- gift leaves Pawl.CastSpec's Gift group green, because the effect's only read is
-- the chosen player and Object.chosenPlayer is empty in exactly the case rule
-- 702.174k excludes. What would tell them apart is CR 702.174c's "whenever a
-- player gives a gift" (#3945).
--
-- Rule 702.174j's second sentence -- "if the spell is countered or otherwise
-- leaves the stack before resolving, the gift effect doesn't happen" -- needs no
-- code: a countered spell never reaches resolveSpellWith at all, and CR 608.2b's
-- fizzle takes the branch above this one.
--
-- CR 702.174j's position -- ahead of the modes -- is a fence on the same footing:
-- no gift printing's own text can observe the gift's product, since CR 601.2c
-- fixed its targets before the token or card existed. Moving this call past the
-- mode loop leaves the suite green.
--
-- Both slot maps are the SPELL's own bindings and carry no mode's target slots:
-- rule 702.174b's effect targets nothing, and the one slot it reads is CR 113.7a's
-- `self`, which Pawl.Engine.Cast.castSpell stamps on the spell alongside the
-- chosen seat -- Resolve.Slots' ChosenPlayerOfBound arm reads Object.chosenPlayer
-- off whatever that slot names.
--
-- Not implemented: CR 702.174c's "whenever a player gives a gift", which watches
-- both this and the permanent half resolve (#3945).
giftOnSpellResolution :: Game Result -> ObjectId -> PlayerId -> Game ()
giftOnSpellResolution runSubgame oid controller = do
  gs <- State.get
  let object = Game.lookupObject oid gs
      promised = maybe Map.empty Object.paidCosts object
      slots = maybe Map.empty (Binding.targetsOf . Object.bindings) object
      gifts = [something | Keyword.Gift something <- Map.keys (Projection.keywordsOf oid gs), Map.findWithDefault 0 (Keyword.Gift something) promised > 0]
      give something = applyEffectWith runSubgame oid oid controller slots slots (Keyword.Engine.giftEffect something)
  Monad.when (Keyword.Engine.isSpellCard (Projection.cardTypesOf oid gs)) (Monad.forM_ gifts give)

-- CR 702.50a's two SPELL abilities, performed as the last part of the spell's
-- resolution and ahead of finishSpell's move: "for the rest of the game, you
-- can't cast spells", and "at the beginning of each of your upkeeps for the rest
-- of the game, copy this spell except for its epic ability".
--
-- Here rather than in finishSpell, whose riders are all REPLACEMENTS over the
-- move (CR 614.1a): rule 702.50a's are abilities of the SPELL, so they are part
-- of CR 608.2's resolution. A countered or fizzled spell never reaches this line,
-- which is rule 702.50b's "once a spell with epic they control resolves".
--
-- The keyword is read off the PROJECTION, reboundApplies' reading of CR 613.1f.
--
-- CR 702.50b's second sentence needs no code: "effects can still put copies of
-- spells onto the stack" is what Effect.CopyStackObject does, a copy not being
-- cast (CR 707.10).
applyEpic :: ObjectId -> PlayerId -> Game ()
applyEpic oid controller = do
  gs <- State.get
  Monad.when (Keyword.Engine.hasEpic (Map.keysSet (Projection.keywordsOf oid gs))) $ do
    -- Rule 702.50a's FIRST ability, as the stored player effect Silence's opcode
    -- stores (CR 611.1 / 613.11), with Expiry.Never for "for the rest of the
    -- game" and CR 109.5's "you" -- the spell's controller as it resolved (CR
    -- 603.7d) -- baked in as the seat. Scoped rather than Named: no slot was
    -- targeted, so there is nothing of CR 601.2c's to bake.
    State.modify' $ \g ->
      let (ts, g1) = Game.freshTimestamp g
          active =
            ActivePlayerEffect.MkActivePlayerEffect
              { ActivePlayerEffect.source = oid,
                ActivePlayerEffect.controller = controller,
                ActivePlayerEffect.timestamp = ts,
                ActivePlayerEffect.expiry = Expiry.Type.Never,
                ActivePlayerEffect.scope = AffectedPlayers.Scoped PlayerScope.You,
                ActivePlayerEffect.effect = PlayerEffect.CantCastSpells
              }
       in g1 {GameState.playerEffects = active : GameState.playerEffects g1}
    -- Rule 702.50a's SECOND ability. The spell is still on the stack here, so the
    -- object is filed in GameState.stackArchive for Effect.CopyStackObject to
    -- find once CR 400.7 has deleted this id -- carrying rule 702.50a's "except
    -- for its epic ability" in its copiable snapshot, so the copy is a spell
    -- without epic and arms no second copier.
    --
    -- Expiry.Never is what makes the delayed entry repeat: one carrying an expiry
    -- is never spent (Pawl.Engine.Event.Trigger), which is "each of your upkeeps
    -- for the rest of the game".
    Monad.forM_ (Game.lookupObject oid gs) $ \obj ->
      let archived = obj {Object.bindings = Binding.setCopy (Keyword.Engine.withoutEpic (Event.copiedSnapshot oid gs)) (Object.bindings obj)}
       in State.modify' $ \g ->
            Event.armDelayed
              Keyword.Engine.epicCopy
              oid
              controller
              (Map.singleton Keyword.Engine.epicSlot (Binding.toObject oid))
              Onset.Immediately
              (Just Expiry.Type.Never)
              g {GameState.stackArchive = Map.insert oid archived (GameState.stackArchive g)}

-- CR 608.2n / 702.27a / 702.88a / 715.3d / 720.3d: where the spell goes as the
-- last part of its resolution -- its owner's graveyard, unless one of four riders
-- replaces that move: buyback's owner's hand, rebound's exile, the Adventure's
-- exile, or the Omen's shuffle into its owner's library.
--
-- Reached only from the RESOLVING path: a fizzled spell does not resolve (CR
-- 608.2b), so CR 715.3d's "as it resolves" never applies to it. What a rider
-- leaves BEHIND is written onto the id the move RETURNS, since CR 400.7 mints a
-- fresh incarnation wherever the card lands.
--
-- The Adventure and Omen riders are keyed on the CHOSEN FACE's spell type (CR
-- 205.3k) rather than on the card's layout, because the question is which set of
-- characteristics is resolving rather than which card printed them -- a
-- classification either way, never an effect's identity. Buyback's is keyed on the
-- record CR 601.2b's announcement wrote (Pawl.Engine.Cast.stampBoughtBack), which
-- is rule 702.27a's own "if the buyback cost was paid", and rebound's on rule
-- 702.88a's own two conditions, `reboundApplies`.
--
-- All four rewrites are replacement effects (CR 614.1a) and are INSTALLED as rows
-- rather than performed (Replacement.installSpellMoveRow), so CR 616.1's loop
-- orders them against every other row watching the same move AND against each
-- other. What no ZoneChangeR can carry -- rule 702.88a's delayed ability and rule
-- 715.3d's play permission -- is armed after the move and only where
-- Replacement.rowApplied says that row is what moved the card: a row the
-- controller took ahead of it sends the spell somewhere else and CR 614.6 leaves
-- the rider nothing to hang on.
finishSpell :: ObjectId -> Face.Face Card.Type.Card -> PlayerId -> Game ()
finishSpell oid face controller = do
  bought <- State.gets (maybe False Object.boughtBack . Game.lookupObject oid)
  Monad.when bought (State.modify' (snd . Replacement.installBuybackReturn oid controller))
  -- CR 701.24a's shuffle is the Omen row's own rider, so rule 720.3d leaves
  -- nothing to do after the move and its timestamp is not kept.
  Monad.when (Card.isOmen face) (State.modify' (snd . Replacement.installOmenShuffle oid controller))
  rebounds <- State.gets reboundApplies
  reboundRow <- rowWhen rebounds Replacement.installReboundExile
  adventureRow <- rowWhen (Card.isAdventure face) Replacement.installAdventureExile
  landed <- Event.changeZoneResolvingReturning oid Zone.Graveyard
  moved <- State.get
  let took = maybe False (\ts -> Replacement.rowApplied oid ts moved)
  Monad.forM_ landed $ \newId -> do
    -- Rule 702.88a makes the delayed ability part of the SAME rewrite as the
    -- exile, so it is armed only where that rewrite is what moved the card.
    Monad.when (took reboundRow) $
      State.modify' (Event.armDelayed Keyword.Engine.reboundUpkeep newId controller (Map.singleton Keyword.Engine.reboundSlot (Binding.toObject newId)) Onset.Immediately Nothing)
    -- CR 715.3d's "for as long as that card remains exiled, that player may play
    -- it", which is the same sentence as the exile it hangs off.
    Monad.when (took adventureRow) . State.modify' $ \gs ->
      gs
        { GameState.objects =
            Map.adjust (\o -> o {Object.playableFromExile = Just (permission newId)}) newId (GameState.objects gs)
        }
  where
    -- Mint one of CR 608.2n's riders when its rule's own condition holds, keeping
    -- the timestamp that identifies the row to Replacement.rowApplied.
    rowWhen holds install = if holds then fmap Just (State.state (install oid controller)) else pure Nothing
    -- CR 702.88a's two conditions. The keyword is read off the PROJECTION and
    -- not off `face`, so a spell granted rebound by a text-changing effect
    -- rebounds and one whose text was blanked does not (CR 613.1f); rule 702.88c
    -- makes a second instance redundant, which membership is. The zone is CR
    -- 601.2a's, which Object.castFrom stored as the cast began -- Nothing for a
    -- copy put on the stack rather than cast (CR 707.10), which rule 702.88a's
    -- "was cast" excludes.
    --
    -- "YOUR hand" is the owner test beside it: CR 400.3 makes every hand its
    -- owner's, so the spell came out of its controller's own hand exactly when the
    -- card's owner is the player resolving it (CR 109.5). Sen Triplets' "you may
    -- play lands and cast spells from that player's hand" is the board that tells
    -- the two apart, and Pawl.CastRestrictionSpec's "CR 702.88a a rebound spell
    -- alice casts out of bob's hand is not exiled" is what proves it.
    reboundApplies gs =
      Keyword.Engine.hasRebound (Map.keysSet (Projection.keywordsOf oid gs))
        && maybe False fromOwnHand (Game.lookupObject oid gs)
    fromOwnHand obj = Object.castFrom obj == Just Zone.Hand && Object.owner obj == controller
    -- Never per CR 611.2a: CR 715.3d states no duration. What ends it is CR
    -- 400.7 -- leaving exile mints a new incarnation, and newIncarnation clears
    -- the field.
    permission newId =
      ExilePlayPermission.MkExilePlayPermission
        { ExilePlayPermission.player = controller,
          ExilePlayPermission.source = newId,
          ExilePlayPermission.expiry = Expiry.Type.Never,
          -- CR 715.3d says nothing about mana, in either sense: neither
          -- CR 118.14's rider nor CR 118.9a's alternative cost, so the Adventure
          -- half is cast for its printed cost.
          ExilePlayPermission.spending = ManaSpending.AsProduced,
          ExilePlayPermission.alternativeManaCost = Nothing,
          -- This IS rule 715.3d's permission, and so the one its next sentence
          -- excludes the Adventure half from.
          ExilePlayPermission.origin = PlayPermissionOrigin.Adventure
        }

-- The no-subgame spell resolver (Stack's default path and every direct caller).
resolveSpell :: ObjectId -> Game ()
resolveSpell = resolveSpellWith noSubgame

-- CR 608.2: the executor shared by an activated and a triggered ability on the
-- stack. Re-validates filled slots (CR 608.2b), walks the CHOSEN modes in order
-- (CR 608.2c/700.2c) applying each one's effects with `srcId` as the effect source
-- (CR 113.7) and asking about any printed "may" (CR 603.5), then the ability
-- ceases (CR 608.2n). `stackId` is the ability object's own id, and the slots ARE
-- the union of the chosen modes' own (CR 700.2c).
--
-- The bindings are re-read before EACH effect (CR 608.2c), but CR 608.2b's
-- question is asked ONCE off the pre-fold snapshot: re-deriving the fizzle
-- mid-fold would let a token a Create just minted rescue an ability whose every
-- target is gone.
--
-- `runSubgame` is the injected nested-game runner, the same one resolveSpellWith
-- takes: CR 729.1a says "the spell or ability that created the subgame", so an
-- ability's PlaySubgame plays one exactly as a spell's does (see #137).
resolveModesWith :: Game Result -> ObjectId -> ObjectId -> [(ModeInstance, Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))] -> Game ()
resolveModesWith runSubgame stackId srcId modes = do
  gs <- State.get
  case Game.lookupObject stackId gs of
    Nothing -> pure ()
    Just obj ->
      -- CR 700.2d: instance-named, not printed-named -- two instances of one
      -- repeated mode fill two slots this union would otherwise collapse.
      -- Modal.modeInstanceTargetSlots rather than a rename of its own: the slot
      -- names a pool and a filter carry are rewritten along with the key, which
      -- is what makes this the same map CR 601.2c was answered against. CR
      -- 608.2b re-judges each against the SAME declaration CR 603.3d offered, so
      -- the "that player controls" atoms are baked here too; an ability whose
      -- environment binds no player leaves them standing, admitting nothing.
      --
      -- The ORDER is deliberately not load-bearing: Engine.placeBorne bakes the
      -- whole modal and then renames, this renames and then bakes, and CR 700.2d's
      -- rename touches only the names a mode DECLARES (Modal.ownSlot), which the
      -- bake never reads. Pawl.ModalSpec's "CR 700.2d a repeated mode on a trigger
      -- keeps reading the trigger's own bound player" is what proves the two paths
      -- agree.
      let slots = Target.bakeSlots (Binding.playerSlots (Object.bindings obj)) (Map.unions (fmap (uncurry Modal.modeInstanceTargetSlots) modes))
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipients = case Map.lookup slot slots of
            -- CR 608.2b is about TARGETS. A slot declaring none is a RESERVED
            -- binding and can never have become an illegal target.
            Nothing -> recipients
            -- CR 608.2b: the perspective is the ABILITY's controller. `srcId`
            -- stays the source (CR 113.7) and may well be gone -- exactly the
            -- case this rule is about. Judged per RECIPIENT.
            Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just effectController) (Object.bindings obj) srcId recipient targetSlot gs) recipients
          legal = Map.mapWithKey legalSlot chosen
          -- CR 608.2b's fizzle asks about the TARGETED slots only, measured on
          -- the slots FILLED rather than declared (CR 115.6).
          targeted = Map.restrictKeys legal (Map.keysSet slots)
          fizzles = not (Map.null targeted) && all Set.null (Map.elems targeted)
          -- CR 113.8 / 603.3a: stamped as Object.owner at the ability's creation
          -- and never revisited, so a stolen permanent's later controller must
          -- not override it.
          effectController = Object.owner obj
          resolveOne (mi, mode) =
            withDefinedSlots stackId mi mode $
              let idx = ModeInstance.index mi
                  -- CR 700.2d: this instance's slots under the names its mode
                  -- prints, applied to both maps so they cannot disagree.
                  instanceView = Modal.instanceView slots mi (Mode.targetSlots mode)
                  -- CR 608.2c's printed order, and the lookup CR 608.2d's
                  -- either-or reads its SIBLING back out of.
                  indexedClauses = zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses mode))
                  applyOne eff = do
                    -- Re-read the LIVE bindings for THIS effect (CR 608.2c). Both
                    -- maps come from the SAME bindings: `legalNow` is `chosenNow`
                    -- with CR 608.2b's illegal recipients dropped, so re-reading one
                    -- without the other would lose the bindings it just gained.
                    bindingsNow <- State.gets (liveBindings obj stackId)
                    let chosenNow = Binding.targetsOf bindingsNow
                        legalNow = Map.mapWithKey legalSlot chosenNow
                    applyEffectWith runSubgame stackId srcId effectController (instanceView legalNow) (instanceView chosenNow) eff
                  -- CR 701.55d's per-player limb, the spell loop's twin: the
                  -- callback villainousPass drives, re-reading the live bindings
                  -- so a later chooser's limb runs against the board an earlier
                  -- chooser's limb left.
                  performLimb limbIdx facing (answers, ran) = case lookup limbIdx indexedClauses of
                    Nothing -> pure (answers, ran)
                    Just limb -> do
                      gateBindings <- State.gets (liveBindings obj stackId)
                      let legalHere = instanceView (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                          boundHere = Map.keysSet (instanceView gateBindings)
                      gated <- gateHolds effectController srcId (instanceView (Binding.targetsOf gateBindings)) gateBindings limb
                      taken <- if gated then exercises stackId srcId effectController idx limbIdx boundHere legalHere (Just facing) limb else pure False
                      (admitted, answers2) <- if taken then payGateAdmits stackId srcId effectController idx limbIdx (instanceView legal) (Just facing) answers limb else pure (False, answers)
                      Monad.when admitted (applyClauseEffects srcId applyOne (Foldable.toList (Clause.effects limb)))
                      pure (answers2, recordTaken admitted limbIdx ran)
               in -- CR 608.2e's clause is what each gate covers. Run only when
                  -- `fizzles` is False.
                  Monad.foldM_
                    ( \(answers, picked, ran) (cIdx, clause) -> do
                        -- CR 608.2c's "If you do" first, off the same fold the
                        -- spell path keeps. Proved on this path, not merely
                        -- fenced: Aetherplasm's second clause hangs on its first,
                        -- and Pawl.CombatEffectSpec's "declining to return
                        -- Aetherplasm skips the clause its 'If you do' hangs on"
                        -- reddens when this conjunct is defeated. What #1887 still
                        -- covers on this loop is the OTHER gate -- an observable
                        -- MANDATORY clause standing before a printed "may".
                        let hangs = ifTakenHolds ran clause
                        -- CR 701.46a's printed "if" next, read against `srcId` --
                        -- the rule says "this permanent", which is also why
                        -- `payGatePaid` is given `srcId`. Off the LIVE bindings of
                        -- the STACK object (CR 608.2c), where this resolution's
                        -- slots are bound (see bindSlot).
                        gateBindings <- State.gets (liveBindings obj stackId)
                        gated <- if hangs then gateHolds effectController srcId (instanceView (Binding.targetsOf gateBindings)) gateBindings clause else pure False
                        -- CR 603.5 / 608.2d: then the printed "may", against the
                        -- SAME live bindings CR 608.2b's filter is applied to, so a
                        -- clause whose every read is dead is not asked about.
                        let legalNowForMay = instanceView (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                            boundNowForMay = Map.keysSet (instanceView gateBindings)
                        -- CR 608.2d's "or" next, and BEFORE the "may", off the same
                        -- helper the spell path uses. Proved on THIS loop and not
                        -- merely on the spell's twin: Teardrop Kami's "sacrifice
                        -- this creature: you may tap or untap target creature" is
                        -- Pawl.ResolveSpec's "CR 608.2d an untapped Piker leaves
                        -- Teardrop Kami only its tap", which reddens when this
                        -- conjunct is defeated.
                        --
                        -- The branches are FILTERED to the ones CR 608.2d leaves
                        -- to choose, the spell loop's derivation and its fence.
                        let eligible i = case lookup i indexedClauses of
                              Nothing -> pure False
                              Just sibling -> do
                                held <- gateHolds effectController srcId (instanceView (Binding.targetsOf gateBindings)) gateBindings sibling
                                State.gets (\gsNow -> held && not (clauseIsImpossible stackId srcId effectController legalNowForMay gsNow sibling))
                        -- CR 701.55d's exception to rule 608.2e, ahead of the
                        -- ordinary either-or and off the same helper the spell
                        -- loop uses: the pair is chosen AND performed one player
                        -- at a time, so it happens here and the sibling's own
                        -- arrival finds it already answered.
                        case facedVillainously picked cIdx clause of
                          Just (orElse, limbs) | gated -> do
                            (answers2, ran2) <- villainousPass stackId effectController idx legalNowForMay orElse limbs performLimb (answers, ran)
                            pure (answers2, Map.insert (NonEmpty.head limbs) Map.empty picked, ran2)
                          _ -> do
                            (announced, picked2) <- if gated then chosenBranch stackId effectController idx cIdx legalNowForMay eligible picked clause else pure (Just Set.empty, picked)
                            let branch = maybe True (not . Set.null) announced
                            taken <- if branch then exercises stackId srcId effectController idx cIdx boundNowForMay legalNowForMay announced clause else pure False
                            -- CR 118.12: then the cost paid on resolution, against the
                            -- START-of-resolution slots.
                            (admitted, answers2) <- if taken then payGateAdmits stackId srcId effectController idx cIdx (instanceView legal) announced answers clause else pure (False, answers)
                            Monad.when admitted (applyClauseEffects srcId applyOne (Foldable.toList (Clause.effects clause)))
                            pure (answers2, picked2, recordTaken admitted cIdx ran)
                    )
                    (Map.empty, Map.empty, Set.empty)
                    indexedClauses
       in do
            Monad.unless fizzles $ do
              recordAbilityResolution obj
              Monad.forM_ modes resolveOne
            State.modify' (Game.cease stackId)

-- CR 608.2n / 608.2i: file this resolution against the ability that is
-- resolving, so a clause of that very ability can ask how many times it has
-- resolved this turn (Quantity.TimesResolvedThisTurn). Ashling the Pilgrim's
-- "if this is the third time this ability has resolved this turn" is the read.
--
-- BEFORE the clauses run and AFTER CR 608.2b's fizzle, which is what makes the
-- count include the resolution asking: rule 608.2n removes the ability at the
-- END of its resolution, and the clause asking is part of the same resolution.
-- An ability CR 608.2b removed never resolved and is not filed.
--
-- A CLASSIFICATION off the object's Source and never which ability it is: an
-- activated ability is filed, and rule 707.10b's copy of one carries the same
-- ActivatedAbilitySource (Resolve.Effect.copyOnStackOf), so the copy and the
-- original are one key.
--
-- Not implemented: a TRIGGERED ability's resolutions, which this arm leaves
-- unrecorded because nothing can read them back -- CR 602.2a's
-- Binding.thisAbility is the only slot naming an ability's own object and no
-- borne trigger carries it (#3815).
recordAbilityResolution :: Object.Object -> Game ()
recordAbilityResolution obj = case Object.source obj of
  Source.OfAbility activated -> State.modify' (Event.recordEvent (GameEvent.ActivatedAbilityResolved activated))
  _ -> pure ()

-- CR 608.2c: does this clause's printed "If you do" hold? A clause naming no
-- earlier one always happens; one that names an earlier clause of this mode
-- instance happens only if that clause's instructions ran -- Tweeze's "you may
-- discard a card. If you do, draw a card".
--
-- Off the fold's record of what ran, so the answer is the one the named clause's
-- own riders gave (Clause.ifTaken says why that rather than the board), and a
-- name the fold has not reached -- a later clause, or one that does not exist --
-- is False. ANY of the names is enough, which is what Worms of the Earth's "if a
-- player does either" prints over the two halves of an either-or pair. Asked
-- BEFORE the other three, so a skipped clause raises no prompt.
--
-- A pure function rather than a Game action: it reads nothing but the fold.
ifTakenHolds :: Set ClauseIndex -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
ifTakenHolds ran clause = maybe True (any (`Set.member` ran)) (Clause.ifTaken clause)

-- The other end of the same fold: a clause's ordinal is recorded exactly when
-- its instructions ran, which is what CR 608.2c's "If you do" asks about. One
-- writer for both resolution paths, so the spell loop and the ability loop
-- cannot disagree about what "you did" means.
recordTaken :: Bool -> ClauseIndex -> Set ClauseIndex -> Set ClauseIndex
recordTaken admitted cIdx ran = if admitted then Set.insert cIdx ran else ran

-- CR 701.46a: does this clause's printed "if" hold? CR 701.37a prints the same
-- gate on a proper prefix of a longer ability, which is why the rider is on CR
-- 608.2e's clause rather than on the mode. A clause stating no condition always
-- happens. Asked as the clause is REACHED (CR 608.2c) and BEFORE `exercises`, so
-- no CR 603.5 prompt is raised whose answer cannot matter.
--
-- `controller` is CR 109.5's "you"; `source` is the source PERMANENT rather than
-- the ability on the stack (CR 701.46a's "this permanent", CR 113.7a).
--
-- CR 608.2h's view, not the live one: a gate asked BETWEEN clauses may read an
-- object an earlier clause already moved. The CHOSEN slots rather than CR
-- 608.2b's surviving ones -- a target THIS resolution moved is not one that
-- became illegal before it.
--
-- The resolving object's WHOLE binding map comes in beside them, from the same
-- live read the caller takes the chosen slots off, so a gate can ask after a
-- batch an earlier clause named (CR 115.10a) and after an amount an earlier
-- clause stamped. Under the printed slot names, which is how every other live
-- read is written (slotBindings) and unlike the chosen map, which the caller has
-- projected into CR 700.2d's mode instance.
gateHolds :: PlayerId -> ObjectId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName Binding.Type.Binding -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game Bool
gateHolds controller source chosen bindings clause = case Clause.condition clause of
  Nothing -> pure True
  Just condition -> do
    gs <- State.get
    pure (Condition.holds (Projection.viewWithLastKnownAnywhere gs) (effectContext gs controller source chosen bindings) gs source condition)

-- CR 608.2d: which branch of an either-or clause pair happens -- Twiddle's "you
-- may tap or untap target artifact, creature, or land". A clause naming no
-- sibling always happens; one that names an earlier or later clause of this mode
-- instance happens only if the controller announced IT.
--
-- Asked ONCE per pair, at whichever branch the fold reaches first, and the
-- answer carried in `picked` under the pair's LOWEST ordinal -- the key both
-- branches compute, so the loser's arrival raises no second prompt. A separate
-- fold component and not `ifTakenHolds`' `ran`: that set records which clauses'
-- instructions RAN, which `condition` and `payGate` can pull apart from which
-- branch was CHOSEN (Clause.ifTaken says why it is keyed that way), and an
-- either-or must exclude its sibling even when the winner then does nothing.
--
-- PER PLAYER, the way CR 118.12's own offer is: OrElse.chooser is a reference
-- and a card may name the table, so the answers are a map and CR 101.4's order
-- runs over them. Worms of the Earth's "any player may sacrifice two lands of
-- their choice or have this enchantment deal 5 damage to that player" is the
-- card; Twiddle's chooser is the resolving controller, one seat and one answer.
--
-- What comes back is the set of players who announced THIS branch, or Nothing
-- for a clause naming no sibling -- the caller hands it to `exercises` and
-- `payGateAdmits`, which offer their own questions to nobody else. NOT a bound
-- slot: both of those read bindings captured before this question was asked, so
-- a slot bound here would be invisible to them.
--
-- The branches are offered in CR 608.2c's printed order and the answer is
-- FILTERED back through them rather than trusted, the posture every choose-don't-
-- target prompt takes.
--
-- CR 608.2d's other half is `eligible`: "the player can't choose an option
-- that's illegal or impossible", so the pair is FILTERED before it is offered
-- and only the branches the rules leave to choose are put. One survivor is
-- forced -- taken with no prompt raised, since nothing is left to ask, which is
-- Twiddle on every board (a permanent is tapped or untapped, never both) and
-- Keys to the House over a Room with both doors already open. None surviving
-- announces nothing, recorded as an empty answer map so the loser's arrival
-- raises no prompt either.
--
-- A VILLAINOUS pair never reaches the unanswered half of this function: CR
-- 701.55d takes it out of rule 608.2e altogether, so both callers route it to
-- `villainousPass` instead and file an empty answer map here, which is how the
-- sibling's own arrival is turned into the no-op above. Rule 608.2d's filter is
-- therefore unconditional here, rule 701.55b's exemption from it living at that
-- pass.
chosenBranch :: ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> (ClauseIndex -> Game Bool) -> Map.Map ClauseIndex (Map.Map PlayerId ClauseIndex) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game (Maybe (Set PlayerId), Map.Map ClauseIndex (Map.Map PlayerId ClauseIndex))
chosenBranch resolving controller idx cIdx legal eligible picked clause = case Clause.orElse clause of
  Nothing -> pure (Nothing, picked)
  Just orElse ->
    let branches = orElseLimbs cIdx orElse
        key = NonEmpty.head branches
        won answers = Just (Map.keysSet (Map.filter (== cIdx) answers))
     in case Map.lookup key picked of
          Just answers -> pure (won answers, picked)
          Nothing -> do
            gs <- State.get
            offered <- Monad.filterM eligible (NonEmpty.toList branches)
            answers <- case offered of
              [] -> pure Map.empty
              [forced] -> pure (Map.fromList (fmap (\chooser -> (chooser, forced)) (apnapPlayersOf (OrElse.chooser orElse) legal controller gs)))
              first : rest ->
                let live = first NonEmpty.:| rest
                 in Monad.foldM
                      ( \acc chooser -> do
                          gs1 <- State.get
                          answered <- Game.choose (Prompt.ChooseClause (Decide.deciderFor chooser gs1) chooser resolving idx live)
                          pure (Map.insert chooser (if elem answered live then answered else first) acc)
                      )
                      Map.empty
                      (apnapPlayersOf (OrElse.chooser orElse) legal controller gs)
            pure (won answers, Map.insert key answers picked)

-- CR 608.2d's pair, in CR 608.2c's printed order. A clause naming ITSELF is one
-- limb rather than two -- Pawl.CardSpec's cardBranchesAreAsymmetric is what
-- keeps the corpus from writing it -- and the head is the ordinal the pair's
-- answer is filed under, which both limbs compute alike.
orElseLimbs :: ClauseIndex -> OrElse.OrElse -> NonEmpty.NonEmpty ClauseIndex
orElseLimbs cIdx orElse =
  let other = OrElse.sibling orElse
   in if cIdx == other then NonEmpty.singleton cIdx else NonEmpty.sort (cIdx NonEmpty.:| [other])

-- CR 701.55a: is this clause half of a villainous pair whose process has not yet
-- been performed? An empty answer map filed under the pair's ordinal is what
-- `villainousPass` leaves behind, so the sibling's arrival answers Nothing here
-- and falls through to chosenBranch's memo, which skips it.
facedVillainously :: Map.Map ClauseIndex (Map.Map PlayerId ClauseIndex) -> ClauseIndex -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Maybe (OrElse.OrElse, NonEmpty.NonEmpty ClauseIndex)
facedVillainously picked cIdx clause = case Clause.orElse clause of
  Just orElse
    | OrElse.villainous orElse,
      let limbs = orElseLimbs cIdx orElse,
      Map.notMember (NonEmpty.head limbs) picked ->
        Just (orElse, limbs)
  _ -> Nothing

-- CR 701.55d, an exception to rule 608.2e: "if more than one player is
-- instructed to face a villainous choice, the entire process described in rule
-- 701.55a is performed for each of those players one at a time in APNAP order".
-- So this is NOT chosenBranch's shape -- ask the table, then run each limb once
-- for the seats that announced it -- but a loop that asks ONE seat and performs
-- that seat's whole option before the next seat is asked. The Dalek Emperor's
-- "each opponent faces a villainous choice -- that player sacrifices a creature
-- of their choice, or you create a 3/3 black Dalek artifact creature token with
-- menace" is the producer: two opponents taking the token limb make TWO tokens,
-- where one run of the limb for both of them would make one.
--
-- CR 101.4's order over the seats OrElse.chooser names, read off the board as
-- the process begins: the instruction names its players once, and a seat that
-- leaves mid-pass is dropped by its own limb's reads rather than by re-asking
-- who was instructed.
--
-- CR 701.55b is why both limbs are always offered and rule 608.2d's filter never
-- runs: the chooser "may choose an option that is illegal or impossible" and
-- then performs as much of it as is possible. Great Intelligence's Plan is the
-- producer, and Pawl.ResolveSpec's "CR 701.55b Great Intelligence's Plan still
-- offers the discard to an empty-handed opponent" is what proves it.
--
-- The answer is FILTERED back through the limbs rather than trusted, the posture
-- every choose-don't-target prompt takes.
--
-- Rule 701.55b exempts a villainous choice from rule 608.2d's impossibility and
-- from nothing else, so a limb printing CR 701.46a's "if" is still OFFERED here
-- although its condition has already ruled it out; the callback checks that
-- condition again before performing the limb, so choosing it does nothing. No
-- card states the shape -- the callers' own fence says no either-or in
-- data/cards prints a condition at all -- and telling the two conjuncts apart in
-- the offer is what it would take. CR 608.2c's "If you do" is the same: a limb
-- hanging off an earlier clause is not a shape any either-or prints.
--
-- The seat is bound under Binding.facingPlayers before its limb runs, which is
-- how a limb says "that player". A slot and not chosenBranch's plain set,
-- because rule 701.55d's per-seat pass is the one shape where the binding IS
-- readable: the limb's reads are re-taken after the bind, where chosenBranch's
-- callers had captured theirs before the question was put.
--
-- Not implemented: rule 701.55c's replacement of one facing by several, which
-- would run this body's inner step more than once for the same seat (#3898).
villainousPass :: ObjectId -> PlayerId -> ModeIndex -> Map.Map SlotName (Set Recipient) -> OrElse.OrElse -> NonEmpty.NonEmpty ClauseIndex -> (ClauseIndex -> Set PlayerId -> acc -> Game acc) -> acc -> Game acc
villainousPass resolving controller idx legal orElse limbs performLimb acc0 = do
  gs <- State.get
  Monad.foldM
    ( \acc chooser -> do
        gs1 <- State.get
        answered <- Game.choose (Prompt.ChooseClause (Decide.deciderFor chooser gs1) chooser resolving idx limbs)
        let chosen = if elem answered limbs then answered else NonEmpty.head limbs
        State.modify' (bindPlayersSlot resolving Binding.facingPlayers (Set.singleton chooser))
        performLimb chosen (Set.singleton chooser) acc
    )
    acc0
    (apnapPlayersOf (OrElse.chooser orElse) legal controller gs)

-- CR 603.5 / 608.2d: does this clause's instruction list happen at all? A
-- mandatory clause always does; an optional one is its controller's call, made
-- HERE as the effect is applied. The unit is CR 608.2e's clause and not the whole
-- mode, so a "may" printed on one sentence leaves its neighbours alone.
--
-- WHO is asked is the Optionality's own PlayerRef, resolved like every other
-- (playerRefPlayers for the membership) and ordered by CR 101.4 through
-- apnapPlayersOf. Every printed "you may" names the resolving controller -- CR
-- 405.4 for a spell, CR 113.8 for an ability -- and Jungle Wayfinder's "each
-- player may" names the whole table. Each of them is asked through
-- Decide.deciderFor, so a player controlled under CR 723.1 has their controller
-- answer.
--
-- ALL the asks BEFORE any effect runs, which is CR 608.2e: the choices for an
-- action are made in APNAP order and then the action is taken. That is what
-- forbids the ask-and-act-per-seat shape, and rule 101.4b is why each seat is
-- asked against the live board rather than a snapshot.
--
-- The seats that ACCEPTED are bound under Binding.mayPlayers, which is how the
-- clause's own instructions say "they" (PlayerRef.EachInSlot), and the clause
-- happens when anybody accepted -- payGateAdmits' shape one question over. A
-- reference naming nobody therefore accepts nobody and the clause does nothing.
--
-- `announced` narrows the asked seats to the ones that announced THIS branch of
-- a CR 608.2d pair (chosenBranch), Nothing for a clause naming no sibling: the
-- "may" over a branch is offered to the players who took it and to nobody else,
-- which is what stops a player from taking both halves of Worms of the Earth's
-- "sacrifice two lands of their choice or have this enchantment deal 5 damage to
-- that player".
--
-- `bound` is every slot the live bindings hold and `legal` is CR 608.2b's
-- surviving recipients, both under the names this mode instance prints (CR
-- 700.2d); an inert clause is not asked about at all -- see clauseIsInert.
-- Binding.mayPlayers is added to `bound` for that test alone: the slot this very
-- "may" is about to define is not dead, and without that a clause whose only
-- read is its own accepters would be judged inert and decline with no prompt
-- raised.
--
-- CR 608.2d's other half is the second gate, and a different question from the
-- first: an inert clause is about SLOTS, where clauseIsImpossible is about what
-- the instruction would do to what the slots name. Tweeze's "you may discard a
-- card" on an empty hand binds its `you` slot and is not inert, and offering it
-- handed the controller the free card its "If you do" hangs on -- Pawl.ResolveSpec's
-- "CR 608.2d Tweeze's discard is not offered with an empty hand" proves it.
-- Declined silently, so recordTaken stays False and the draw is skipped.
exercises :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Set SlotName -> Map.Map SlotName (Set Recipient) -> Maybe (Set PlayerId) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game Bool
exercises resolving source controller idx cIdx bound legal announced clause = do
  impossible <- State.gets (\gs -> clauseIsImpossible resolving source controller legal gs clause)
  case Clause.optionality clause of
    Optionality.Mandatory -> pure True
    Optionality.Optional asker
      | impossible || clauseIsInert (Set.insert Binding.mayPlayers bound) legal clause -> pure False
      | otherwise -> do
          gs <- State.get
          accepted <-
            Monad.foldM
              ( \acc pid -> do
                  gs1 <- State.get
                  decision <- Game.choose (Prompt.ChooseOptional (Decide.deciderFor pid gs1) pid resolving idx cIdx)
                  pure $ case decision of
                    OptionalDecision.Exercises -> Set.insert pid acc
                    OptionalDecision.Declines -> acc
              )
              Set.empty
              (announcedOnly announced (apnapPlayersOf asker legal controller gs))
          State.modify' (bindPlayersSlot resolving Binding.mayPlayers accepted)
          pure (not (Set.null accepted))

-- CR 608.2d: the seats a branch's own questions are offered to. A clause naming
-- no sibling keeps every seat its reference named; one that names a sibling
-- keeps only the seats that announced it, in the APNAP order the caller already
-- imposed.
announcedOnly :: Maybe (Set PlayerId) -> [PlayerId] -> [PlayerId]
announcedOnly = maybe id (\winners -> filter (`Set.member` winners))

-- CR 608.2b / 603.5: can this clause's answer not matter? Only when every one of
-- its effects reads a slot and every slot it reads is illegal or unfilled, since
-- each opcode's slot reads then name nothing and the clause does nothing either
-- way. Deliberately conservative: an effect reading NO slot, or reading one
-- surviving recipient among several, keeps the prompt. The other elision the
-- prompt admits is CR 608.2d's, which asks what the instruction would DO to what
-- the slots name -- see Pawl.Engine.Resolve.Effect.clauseIsImpossible.
--
-- A CLASSIFICATION and never an identity check: what an effect reads comes from
-- slotsOf, and slotsAreExhaustive is what says slotsOf is the WHOLE of it -- an
-- opcode that reads more than its slots (ArmDelayedTrigger, CR 725.2's
-- ControllerOfSource) answers False there and so is never called inert. That
-- conjunct is a REGRESSION FENCE rather than a proven behaviour: no optional
-- clause in data/cards/ holds such an opcode, so dropping it leaves the suite
-- green.
--
-- "Dead" is per SLOT and takes both maps, because a slot's binding need not be a
-- target at all: a TARGET slot is dead once CR 608.2b has emptied it, and any
-- other slot -- a group an earlier clause revealed, X, a reserved binding -- is
-- dead only when nothing has bound it. Reading `legal` alone would call a
-- revealed-cards slot dead and silently decline Midnight Tilling's return.
--
-- An EMPTY clause is not inert: it has no effect to read a slot, so `all` would
-- hold vacuously. Nothing in data/cards/ prints one, and reaching this ahead of
-- CR 608.2b's fizzle needs a modal payload mixing a live mode with a dead one
-- (Deadly Complication).
clauseIsInert :: Set SlotName -> Map.Map SlotName (Set Recipient) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
clauseIsInert bound legal clause =
  let effects = Foldable.toList (Clause.effects clause)
      dead name = case Map.lookup name legal of
        Just recipients -> Set.null recipients
        Nothing -> not (Set.member name bound)
      inert effect =
        let names = Map.keysSet (slotsOf effect)
         in slotsAreExhaustive effect && not (Set.null names) && all dead names
   in not (null effects) && all inert effects

-- CR 118.12: does this clause's instruction list happen, given the cost paid on
-- resolution it may state? A clause stating none always does; one that states one
-- offers it to the players its `payer` reference names, and the instructions are
-- whichever branch PayGate.branch says.
--
-- The branch is keyed on the ANSWER and never on the board afterwards, which is
-- CR 118.12 in as many words: it checks whether the player chose to pay
-- "regardless of what events actually occurred".
--
-- PER PLAYER, because CR 118.12a's rewriting is: "[Do something] unless [a
-- player does something else]" means "[A player may do something else]. If
-- [that player doesn't], [do something]", so Rishadan Cutpurse's "each opponent
-- sacrifices a permanent of their choice unless they pay {1}" is one offer per
-- opponent gating that opponent's own edict. The seats the branch SELECTS are
-- bound under Binding.gatePlayers, which is how the clause's own instructions
-- say "they", and the clause happens when the branch selected anybody.
--
-- A gate whose reference names NOBODY therefore selects nobody and its clause is
-- skipped, where a single-payer gate used to take the IfNotPaid branch and run
-- its instructions against an unfilled slot. Unobservable across the pool as it
-- stands: only an IfNotPaid clause diverges (an IfPaid one was skipped either
-- way), only the slot-reading references can name nobody, and every IfNotPaid
-- clause in the pool whose payer is one of those aims its own instructions at
-- that same slot -- Mana Leak's Counter, Amulet of Safekeeping's. The rest read
-- `you`, which is stamped for every carrier (Binding.you).
--
-- FIVE ways one player's answer comes out, of which exactly one is "paid": the
-- reference names them but they have LEFT the game (CR 800.4f) or they CANNOT
-- pay (CR 118.3), asked on neither limb;
-- they decline, which only an OPTIONAL cost reaches; they chose to pay -- the
-- one place the answer is not the raw choice, since Pawl.Engine.Cost.pay
-- restores the payments an incomplete attempt made and an Unpaid result buys
-- nothing, though it is not a no-op on the BOARD: Cost.reverseIllegal asks
-- before reversing a mana ability, so a payer who declines keeps CR 605.3a's
-- window -- the mana floating and the sources tapped; or the reference never
-- named them at all, which is not an answer and leaves them out of both
-- branches.
--
-- The cost is paid AGAINST `source` rather than the resolving stack object (CR
-- 113.7a); the two are the same object for a spell.
--
-- ONE offer per payment (CR 118.12): a second clause hanging off the same cost
-- names the first (PayGate.offeredAt) and reuses the recorded answers, `answers`
-- being keyed on the offering clause's ordinal. A clause naming an offer never
-- made falls through and makes it, the named clause having failed its own CR
-- 701.46a "if" or CR 603.5 "may".
payGateAdmits :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> Maybe (Set PlayerId) -> Map.Map ClauseIndex (Map.Map PlayerId Bool) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game (Bool, Map.Map ClauseIndex (Map.Map PlayerId Bool))
payGateAdmits resolving source controller idx cIdx legal announced answers clause = case Clause.payGate clause of
  Nothing -> pure (True, answers)
  Just gate -> do
    let offerAt = Maybe.fromMaybe cIdx (PayGate.offeredAt gate)
    (asked, answers2) <- case Map.lookup offerAt answers of
      Just recorded -> pure (recorded, answers)
      Nothing -> do
        recorded <- payGatePaid resolving source controller idx cIdx legal announced gate
        pure (recorded, Map.insert offerAt recorded answers)
    let selected = Map.keysSet (Map.filter (branchTaken (PayGate.branch gate)) asked)
    State.modify' (bindPlayersSlot resolving Binding.gatePlayers selected)
    pure (not (Set.null selected), answers2)

-- Which branch of CR 118.12 a payment outcome selects, off the classification a
-- card states -- never off what the payment DID.
branchTaken :: PayBranch.PayBranch -> Bool -> Bool
branchTaken branch wasPaid = case branch of
  PayBranch.IfPaid -> wasPaid
  PayBranch.IfNotPaid -> not wasPaid

-- The offer itself: who was offered this gate's cost, and which of them paid?
-- CR 118.12's MANDATORY limb is not offered, and that is the rule rather than an
-- elision -- it asks whether the player "started to pay", so a mandatory cost the
-- payer can afford leaves nothing to choose, and CR 118.3 is asked first so an
-- unpayable one takes the "can't" branch with no prompt either.
--
-- Narrowed by `announcedOnly` to the seats that announced this branch of a CR
-- 608.2d pair, where the clause is one: the offer belongs to the players who
-- took it, so Worms of the Earth's sacrifice is offered to nobody who announced
-- its damage instead.
--
-- CR 101.4's APNAP order over the players the reference names, which is what
-- `apnapPlayersOf` imposes: rule 101.4b lets a later payer answer knowing what an
-- earlier one did. The board is re-read for each of them (payGatePaidBy's own
-- State.get) rather than measured once, so a cost that changes the board -- CR
-- 118.12's own "sacrifice this enchantment" -- is affordable to the next payer
-- against the board it left. Each payer spends only their own resources, so the
-- sequencing is not observable as an ordering of the ACTIONS.
--
-- A player the reference names who has LEFT the game stays in this list and is
-- answered False by payGatePaidBy, CR 800.4f.
payGatePaid :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> Maybe (Set PlayerId) -> PayGate.PayGate -> Game (Map.Map PlayerId Bool)
payGatePaid resolving source controller idx cIdx legal announced gate = do
  gs <- State.get
  Monad.foldM
    ( \acc payer -> do
        paid <- payGatePaidBy resolving source controller idx cIdx legal payer gate
        pure (Map.insert payer paid acc)
    )
    Map.empty
    (announcedOnly announced (apnapPlayersOf (PayGate.payer gate) legal controller gs))

-- One player's answer to one gate. The cost is the PRINTED one with CR 107.3's X
-- resolved (`announcedXOn`) and then multiplied by the gate's "for each"
-- (PayGate.perEach), and that pair of rewrites is what every reader below
-- sees -- CR 118.3's affordability test, the prompt the payer is shown, and the
-- payment itself -- so none of them can disagree about what is owed.
--
-- The multiplier is read HERE rather than once for the whole gate, which is the
-- posture payGatePaid's own comment states: rule 101.4b lets an earlier payer's
-- answer move the board, and CR 118.12's cost is measured against the board each
-- payer faces.
--
-- Measured against the RESOLUTION, not against the payer: the context is the
-- resolving controller's (`effectContext`, never Filter.contextFor), so
-- Rakshasa's Disdain's "for each card in your graveyard" counts the
-- graveyard of the player who cast it while the payer is the targeted spell's
-- controller. The quantity is evaluated against the ability's SOURCE through
-- `effectViewOf`, so CR 113.7a's last known record answers for a source that has
-- already left, with the announcement id CR 601.2b stamped on the RESOLVING
-- object -- Resolve.Slots' TopOfLibrary depth is the same pair. An unevaluable
-- or negative count is zero copies (CR 107.1b). Whether such an ability should
-- be offering anything at all is its own text's business: rule 702.24a's
-- intervening "if" is what stops it (CR 603.4), proved at
-- Pawl.KeywordTriggerSpec's "a Unicorn murdered in response".
--
-- CR 800.4f, asked BEFORE CR 118.3 and before the offer: a payer who has left
-- the game does not pay, and is not asked whether to. That is an answer of False
-- and not a removal from payGatePaid's map, because CR 118.12a's rewriting makes
-- "unless" mean "if they don't, [do something]" and an unpaid cost is exactly
-- what selects that limb -- rule 702.21a's ward counters the spell.
--
-- It cannot ride the CR 118.3 test beneath it. A departed player keeps their
-- CR 102.1 row, so an affordability question asked of their life total or their
-- last known board can still answer yes. Ward is the reachable case: rule
-- 702.21a targets nothing, so CR 608.2b never empties Binding.targetingObject
-- and the trigger resolves after the targeter has gone, with
-- PlayerRef.ControllerOfBound naming them through CR 608.2h.
-- Pawl.DepartureSpec's "CR 800.4f a departed player is not offered a ward cost,
-- and does not pay it" proves this arm.
--
-- CR 800.4g is the same situation for a choice that is NOT a payment, and the
-- opposite answer: the controller of the object picks another player to make it,
-- which Pawl.Engine.Resolve.Effect.askedChooser does. Not here -- a cost is the
-- whole of what this function asks about.
payGatePaidBy :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> PlayerId -> PayGate.PayGate -> Game Bool
payGatePaidBy resolving source controller idx cIdx legal payer gate = do
  gs <- State.get
  let multiplier = case PayGate.perEach gate of
        Nothing -> 1
        Just quantity ->
          let viewOf = effectViewOf source legal gs
              context = effectContext gs controller source legal (slotBindings resolving gs)
           in maybe 0 Integer.toNaturalSaturating (Quantity.evaluateFor viewOf context gs resolving source quantity)
      cost = Cost.repeated multiplier (Cost.substituteX (announcedXOn resolving gs) (PayGate.cost gate))
  if notElem payer (Game.stillPlaying gs) || not (Cost.canPay PaymentSubject.ForNeither payer source cost gs)
    then pure False
    else do
      decision <- case PayGate.obligation gate of
        PayObligation.Mandatory -> pure PaymentDecision.Pays
        PayObligation.Optional -> Game.choose (Prompt.ChooseToPay (Decide.deciderFor payer gs) payer resolving idx cIdx cost)
      case decision of
        PaymentDecision.Declines -> pure False
        PaymentDecision.Pays -> do
          -- CR 118.13b: a symbol payable in multiple ways is announced by the
          -- PAYER "immediately before they pay that cost" -- after CR 118.12's
          -- "may" above, since what is announced is how to pay a cost already
          -- chosen, and before the mana window Cost.pay opens.
          --
          -- CR 601.2f's totalling is `pure`, which is the identity in the list
          -- applicative `Cost.announce` measures through: pawl gathers cost
          -- adjustments for a SPELL (Cast.castSpell) and for an ACTIVATION
          -- (Activate.activateAbility) and nowhere else, and `Cost.pay` below
          -- applies none of its own, so the announced cost IS the cost that will
          -- be paid. A card that reduced a CR 118.12 cost would be the one to
          -- refute that, and `data/cards/` prints none. That also keeps the offer
          -- exactly as permissive as `Cost.canPay` above, which enumerates the
          -- same CR 601.2b nonhybrid equivalents through Mana.resolutions --
          -- so no route Mana.announce offers is one the gate refused, and its
          -- no-payable-route fallback stays unreachable from here.
          -- Discarded, Activate's reason: rule 702.150a asks about a spell's own
          -- cost, not about a cost paid during a resolution (CR 118.13b).
          (announced, _) <- Cost.announce PaymentSubject.ForNeither ManaSpending.AsProduced payer source pure cost
          -- DuringResolution: rule 118.12's cost is paid as the spell or ability
          -- resolves, which is CR 609.1's effect, so a blight paid here is CR
          -- 614.16's subject where Soul Immolation's additional cost is not.
          -- CR 733.1's reversal base: rule 118.12's payment IS the whole of
          -- the action that can fail, so the state it begins in is where a
          -- failure returns to and the CR 605.3a window is the payer's to keep
          -- (Cost.pay). Taken after the announcement above, which writes no
          -- state of its own.
          began <- State.get
          outcome <- Cost.pay performManaAbility (Just began) PaymentMoment.DuringResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced payer source announced
          -- Not implemented: the slots this payment bound are dropped, so a
          -- CR 118.12 cost that sacrifices a permanent cannot be read by a
          -- later clause of the same resolution (#1872).
          pure (case outcome of Payment.Paid _ -> True; Payment.Unpaid -> False)

-- CR 118.4 / CR 107.3a: the value of X in a cost paid during resolution. NOT a
-- choice the payer makes -- CR 107.3a fixes it at the value the object's own
-- controller announced at CR 601.2b, and CR 107.3i gives every instance of X on
-- that object that one value -- so Clash of Wills' "unless its controller pays
-- {X}" charges the X its caster paid for, and nobody is prompted here.
--
-- Read off the object CR 601.2b announced ON: the SPELL (Cast.castSpell stamps
-- the new incarnation) or the ABILITY (Activate.activateAbility stamps the
-- ability object), which is `resolving` in both cases and never `source` --
-- Quantity.evaluateFor's InSlot arm says the same about the same binding.
--
-- Zero when nothing was announced. A face whose gate prints {X} always declares
-- an {X} of its own (Pawl.CardSpec's "every printing that reads X declares X"),
-- so this is a totality guard for a CARD-authored gate. A gate MINTED by a
-- keyword resolves on a triggered ability that announced nothing, and reads 0
-- here.
--
-- CR 702.21b's ward {X} is the value this reader is NOT: determined as the
-- ability resolves rather than announced, it rides PayGate.perEach instead
-- (Pawl.Types.Ward.perEach, Pawl.Engine.Keyword.ward).
announcedXOn :: ObjectId -> GameState -> Natural
announcedXOn oid gs =
  Maybe.fromMaybe
    0
    (Game.lookupObject oid gs >>= Binding.amountOf Binding.variableX . Object.bindings)

-- The no-subgame mode executor: every direct caller, and any path that cannot
-- reach a PlaySubgame.
resolveModes :: ObjectId -> ObjectId -> [(ModeInstance, Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))] -> Game ()
resolveModes = resolveModesWith noSubgame

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (CR 113.7a), not the ability object, and only the CHOSEN modes are read (CR
-- 700.2c). The ability then ceases (CR 608.2n) rather than being buried.
--
-- `runSubgame` rides through to the effects for the reason resolveModesWith
-- gives (CR 729.1a).
resolveAbilityWith :: Game Result -> ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
resolveAbilityWith runSubgame abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj -> do
      let chosen = Binding.modesOf (Object.bindings obj)
      resolveModesWith runSubgame abilId srcId (Modal.chosenModes chosen (ActivatedAbility.modal ability))
      -- CR 702.122e: "becomes crewed" IS "a crew ability of this Vehicle
      -- resolves", so the marker is written here rather than where the cost was
      -- paid -- a crew activation that is countered never makes its Vehicle
      -- become crewed.
      --
      -- Read off ActivatedAbility.keyword, the stamp Keyword.mintedBy writes for
      -- rule 702.122a, so this is a case on a rule-702 KEYWORD and not on an
      -- effect's identity.
      -- The source is the Vehicle -- CR 113.7's "the object whose ability was
      -- activated" -- which is what rule 702.122e's "[this Vehicle]" names.
      --
      -- The creatures rule 702.122a's cost tapped ride along, read off THIS
      -- ability (Binding.tappedForTotalPower, folded on by Activate) so two crew
      -- activations in a turn name two sets. Off the `obj` this resolution
      -- opened with: resolveModesWith ends with CR 608.2n's cease, so a fresh
      -- lookup here answers Nothing. CR 702.122b's crewing itself is
      -- GameEvent.Crewed, which Activate writes at the payment.
      --
      -- Rule 702.122e's rider is what needs them on the event rather than on the
      -- board: Pawl.Engine.Event.Binding stamps this set under
      -- Pawl.Engine.Binding.crewers, so an intervening "if" reads only the
      -- activation that caused the trigger (Pawl.CrewSpec's Mighty Servant of
      -- Leuk-o group).
      case ActivatedAbility.keyword ability of
        Just (Keyword.Crew _) ->
          State.modify'
            ( Event.recordEvent
                ( GameEvent.BecameCrewed
                    Crewing.MkCrewing
                      { Crewing.vehicle = srcId,
                        Crewing.crewedBy = crewersOf obj
                      }
                )
            )
        _ -> pure ()

-- CR 702.122b: the creatures that paid a crew ability's cost, read off the
-- ability object Pawl.Engine.Activate stamped the payment onto. Empty when the
-- slot is absent, which is what a cost with no TapForTotalPower component would
-- leave; every printed crew ability has one (Pawl.Engine.Keyword's `crew`).
crewersOf :: Object.Object -> Set.Set ObjectId
crewersOf obj =
  Set.fromList
    ( Maybe.mapMaybe
        Recipient.objectOf
        (Set.toList (Maybe.fromMaybe Set.empty (Map.lookup Binding.tappedForTotalPower (Binding.targetsOf (Object.bindings obj)))))
    )

-- The no-subgame activated-ability resolver.
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
resolveAbility = resolveAbilityWith noSubgame

-- CR 614.1c: run the effects of every as-enters rewrite that has applied and not
-- run yet. Drains GameState.pendingEntryEffects, which Pawl.Engine.Event filled
-- -- runPreventionRiders above in every structural respect and for the same
-- reason. Emptied before the effects run.
--
-- Its one caller is Pawl.Engine.Engine.performSettle, which runs it before the
-- SBA pass and before the trigger scan. What that ordering does NOT give is CR
-- 614.1c's own placement, inside the entry; see GameState.pendingEntryEffects.
runEntryEffects :: Game ()
runEntryEffects = do
  queued <- State.gets GameState.pendingEntryEffects
  State.modify' (\gs -> gs {GameState.pendingEntryEffects = Seq.empty})
  Foldable.traverse_ runEntryEffect queued

-- One entered permanent's as-enters effects, in printed order.
--
-- `resolving` and `source` are both the permanent (CR 113.7), runPreventionRider's
-- posture. The slot maps are empty because a static ability targets nothing (CR
-- 115.10a).
runEntryEffect :: PendingEntryEffect.PendingEntryEffect -> Game ()
runEntryEffect pending =
  Foldable.traverse_
    ( applyEffect
        (PendingEntryEffect.object pending)
        (PendingEntryEffect.object pending)
        (PendingEntryEffect.controller pending)
        Map.empty
        Map.empty
    )
    (PendingEntryEffect.effects pending)

-- CR 608.2c: the bindings a resolution reads before each of its own effects --
-- the LIVE ones off the stack object, so a slot an earlier effect DEFINED is
-- visible to a later one. `obj` is the object as resolution began, the fallback
-- for the reads below.
--
-- CR 729.5 is why the fallback is not just that snapshot: "the spell or ability
-- that created the subgame finishes resolving, even if it was created by a spell
-- card that's no longer on the stack". A wish cast INSIDE a subgame may name the
-- very spell that is resolving -- CR 729.4 puts the main game's stack outside the
-- subgame -- and Pawl.Engine.Setup.applyCrossings then deletes that object before
-- the resolution resumes. What it bound meanwhile is in
-- GameState.detachedBindings, and takes precedence over the announced bindings
-- the snapshot carries.
--
-- Not implemented: the readers that look the resolving object up by id rather
-- than coming through here -- Pawl.Engine.Count.playersFor's EachPlayerExcept
-- arm and Pawl.Engine.Quantity's InSlot arm -- stay unanswered on that path
-- (#2493).
liveBindings :: Object.Object -> ObjectId -> GameState -> Map SlotName Binding.Type.Binding
liveBindings obj oid gs = case Game.lookupObject oid gs of
  Just live -> Object.bindings live
  Nothing -> Map.union (Map.findWithDefault Map.empty oid (GameState.detachedBindings gs)) departed
  where
    -- GameState.stackArchive ahead of the pre-fold snapshot, and only for the
    -- object's OWN departure: a spell that moved ITSELF (CR 201.5, Chronomantic
    -- Escape) went through the CR 400.7 funnel, which filed its bindings as of the
    -- move, while `obj` is the reading resolution BEGAN with and cannot hold a
    -- slot a clause defined since. Pawl.Engine.Resolve.Slots.resolvingBindings is
    -- the same preference for the readers that take no snapshot.
    departed = maybe (Object.bindings obj) Object.bindings (Map.lookup oid (GameState.stackArchive gs))

-- bindPlayerSlot's plural: bind SEVERAL players a resolution named into `slot` on
-- `holder` -- CR 118.12a's per-player gate, CR 603.5's printed "may", and CR
-- 701.55d's per-seat villainous pass are the callers. The set is
-- written even when it is EMPTY -- Binding.toRecipients turns that into an
-- unbound slot, so a branch nobody took leaves the previous clause's answer
-- unreadable rather than standing.
bindPlayersSlot :: ObjectId -> SlotName -> Set PlayerId -> GameState -> GameState
bindPlayersSlot holder slot players gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toPlayers players) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}
