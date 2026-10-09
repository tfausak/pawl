-- Resolving a spell or ability (CR 608): modes, targets re-checked, the
-- clause-by-clause drive of Pawl.Engine.Resolve.Effect, and the static slot
-- reading in Pawl.Engine.Resolve.Slots. Both siblings are imported by callers
-- under the same Resolve alias.
module Pawl.Engine.Resolve where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword.Engine
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.PlayerDesignation as PlayerDesignation
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replacement as Replacement
import Pawl.Engine.Resolve.Effect (announcedOnly, apnapPlayersOf, applyClauseEffects, applyEffectWith, branchSelects, clauseIsImpossible, gateAffordable, happenedBetween, noSubgame, payGatePaid, targetSlotsOf)
import Pawl.Engine.Resolve.Slots (boundSlots, effectContext, slotsAreExhaustive, slotsOf)
import qualified Pawl.Engine.SourceContext as SourceContext
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.AbilityName (AbilityName)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.Binding as Binding.Type
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
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
import qualified Pawl.Types.IfTaken as IfTaken
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaAddition as ManaAddition
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
import qualified Pawl.Types.PermissionVerb as PermissionVerb
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.Result (Result)
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.WhenSpent as WhenSpent
import qualified Pawl.Types.Zone as Zone

-- CR 603.7: the delayed abilities an effect list ARMS, by name -- an AddMana's
-- spend trigger among them, which its payment arms (CR 106.6). An arm carrying
-- its own ability (ArmDelayedTrigger.ability) names nothing the card declares.
armedAbilities :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Set AbilityName
armedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _ Nothing) -> Just name
        Effect.AddMana addition -> fmap WhenSpent.ability (ManaAddition.whenSpent addition)
        Effect.Firebend addition -> fmap WhenSpent.ability (ManaAddition.whenSpent addition)
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- CR 603.7: armedAbilities narrowed to the arms whose firing is gated past the
-- turn that armed them, i.e. not Onset.Immediately.
onsetGatedAbilities :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Set AbilityName
onsetGatedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger _ Onset.Immediately _ _) -> Nothing
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _ Nothing) -> Just name
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
-- re-reads them off the spell's face (Projection.spellFaceOf), which unions in
-- CR 303.4a's enchant slot and, for CR 702.96b's overloaded spell, drops them
-- all.
modesOf :: ObjectId -> GameState -> [(ModeInstance, Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))]
modesOf oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Projection.spellFaceOf oid gs of
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
       in -- CR 702.47b: the spliced text after the spell's own, and CR 702.47c
          -- makes it the spell's text, so CR 612's changes reach it too.
          fmap (fmap rewriteMode) (Card.chosenModes chosen face <> Game.splicedModes obj gs)

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
--
-- A spell that became a copy of a card announced nothing (CR 707.2,
-- Object.unannounced), so each slot whose text fixes a count and that CR
-- 707.10c's re-choice left empty is an instance of "target" with no legal
-- target -- Transcantation's ruling, "the spell won't resolve". Only then: a
-- slot CR 601.2c let the caster skip (awaken's, cast without awaken) is no
-- target at all. The scenario "CR 608.2b a Transcantated spell left with no
-- target doesn't resolve" proves it.
targetsAllIllegal :: ObjectId -> GameState -> Bool
targetsAllIllegal oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Projection.spellFaceOf oid gs of
    Nothing -> False
    Just face ->
      let slots = targetSlotsOf obj oid gs face
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipients = case Map.lookup slot slots of
            Nothing -> recipients
            -- CR 608.2b's perspective is the SPELL's controller (CR 405.4).
            Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just (spellController obj oid gs)) (Object.bindings obj) oid recipient targetSlot gs) recipients
          legal = Map.mapWithKey legalSlot chosen
          unaimed = if Object.unannounced obj then Map.withoutKeys (Set.empty <$ Map.mapMaybe Target.fixedCount slots) (Map.keysSet chosen) else Map.empty
          targeted = Map.union (Map.restrictKeys legal (Map.keysSet slots)) unaimed
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
    Just obj -> case Projection.spellFaceOf oid gs of
      Nothing -> pure ()
      Just face ->
        -- CR 608.2b/700.2c: re-validate only the CHOSEN modes' slots.
        let chosenSelection = Binding.modesOf (Object.bindings obj)
            slots = targetSlotsOf obj oid gs face
            -- CR 700.2d: the slots the MODES own, the spliced text's among them
            -- (CR 702.47d) -- `slots` minus CR 303.4a's enchant slot. The spliced
            -- half is a REGRESSION FENCE: no effect names a slot under its
            -- suffixed name, so leaving it in an instance's view is unobserved.
            modeOwnedSlots = Map.union (Modal.modesTargetSlots chosenSelection (Face.spell face)) (Game.splicedTargetSlots obj gs)
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
                          let instanceView :: Map SlotName (Set Recipient) -> Map SlotName (Set Recipient)
                              instanceView = Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode)
                              legalHere = instanceView (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                              boundHere = Map.keysSet (instanceView (Set.empty <$ gateBindings))
                          gated <- gateHolds effectController oid (instanceView (Binding.targetsOf gateBindings)) gateBindings limb
                          taken <- if gated then exercises oid oid effectController idx limbIdx boundHere legalHere (Just facing) Set.empty limb else pure False
                          before <- State.get
                          (admitted, answers2) <-
                            if taken
                              then payGateAdmits runSubgame oid oid effectController idx limbIdx (instanceView (Map.mapWithKey legalSlot (Binding.targetsOf (Object.bindings obj)))) (Just facing) Set.empty answers limb
                              else pure (False, answers)
                          Monad.when admitted (asCostWhenNamed indexedClauses limbIdx (applyClauseEffects oid applyOne (Foldable.toList (Clause.effects limb))))
                          after <- State.get
                          pure (answers2, recordTaken limb admitted before after limbIdx ran)
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
                            boundNowForMay = Map.keysSet (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Monad.void gateBindings))
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
                            pure (answers2, Map.insert (NonEmpty.head limbs) (False, Map.empty) picked, ran2)
                          _ -> do
                            (announced, committed, picked2) <- if gated then chosenBranch oid oid effectController idx cIdx legalNowForMay eligible (`lookup` indexedClauses) picked clause else pure (Just Set.empty, Set.empty, picked)
                            let branch = maybe True (not . Set.null) announced
                            taken <- if branch then exercises oid oid effectController idx cIdx boundNowForMay legalNowForMay announced committed clause else pure False
                            -- CR 118.12: then the cost paid on resolution, against the
                            -- START-of-resolution targets to match CR 608.2b's single
                            -- re-validation. Both maps are projected into THIS
                            -- instance's view (CR 700.2d) after legality is decided,
                            -- since deciding it after the rename would miss in `slots`.
                            before <- State.get
                            (admitted, answers2) <-
                              if taken
                                then
                                  let chosenAtStart = Binding.targetsOf (Object.bindings obj)
                                   in payGateAdmits
                                        runSubgame
                                        oid
                                        oid
                                        effectController
                                        idx
                                        cIdx
                                        (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Map.mapWithKey legalSlot chosenAtStart))
                                        announced
                                        committed
                                        answers
                                        clause
                                else pure (False, answers)
                            Monad.when admitted (asCostWhenNamed indexedClauses cIdx (applyClauseEffects oid applyOne (Foldable.toList (Clause.effects clause))))
                            after <- State.get
                            pure (answers2, picked2, recordTaken clause admitted before after cIdx ran)
                    )
                    (Map.empty, Map.empty, Set.empty)
                    indexedClauses
                firstOfName <- noteResolved oid effectController
                applyEpic oid effectController
                exiled <- applyParadigm oid effectController firstOfName
                encoded <- if exiled then pure True else applyCipher oid effectController
                Monad.unless encoded (finishSpell oid face effectController)

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
-- Effect.GiveGift follows, once per spell however many gifts it promised: CR
-- 702.174c's "gives a gift" is the promised spell resolving.
--
-- Rule 702.174j's second sentence -- "if the spell is countered or otherwise
-- leaves the stack before resolving, the gift effect doesn't happen" -- needs no
-- code: a countered spell never reaches resolveSpellWith at all, and CR 608.2b's
-- fizzle takes the branch above this one.
--
-- CR 702.174j's position -- ahead of the modes -- is a REGRESSION FENCE too:
-- no gift printing's own text can observe the gift's product, since CR 601.2c
-- fixed its targets before the token or card existed. Moving this call past the
-- mode loop leaves the suite green.
--
-- Both slot maps are the SPELL's own bindings and carry no mode's target slots:
-- rule 702.174b's effect targets nothing, and the one slot it reads is CR 113.7a's
-- `self`, which Pawl.Engine.Cast.castSpell stamps on the spell alongside the
-- chosen seat -- Resolve.Slots' ChosenPlayerOfBound arm reads Object.chosenPlayer
-- off whatever that slot names.
giftOnSpellResolution :: Game Result -> ObjectId -> PlayerId -> Game ()
giftOnSpellResolution runSubgame oid controller = do
  gs <- State.get
  let object = Game.lookupObject oid gs
      promised = maybe Map.empty Object.paidCosts object
      slots = maybe Map.empty (Binding.targetsOf . Object.bindings) object
      gifts = [something | Keyword.Gift something <- Map.keys (Projection.keywordsOf oid gs), Map.findWithDefault 0 (Keyword.Gift something) promised > 0]
      give something = applyEffectWith runSubgame oid oid controller slots slots (Keyword.Engine.giftEffect something)
  Monad.when (Keyword.Engine.isSpellCard (Projection.cardTypesOf oid gs) && not (null gifts)) $ do
    Monad.forM_ gifts give
    applyEffectWith runSubgame oid oid controller slots slots Effect.GiveGift

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
                ActivePlayerEffect.choices = SourceContext.choicesOf oid g,
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
    -- without epic and arms no second copier. The object's own bindings ride
    -- along, and with them CR 707.10's decisions: Pawl.CastSpec's Eternal
    -- Dominion cases prove the copy keeps the spell's target.
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

-- CR 702.192a's "the first time a spell you control with this spell's name has
-- resolved this game": file the resolving spell's names under its controller in
-- GameState.resolvedNames, answering whether none of them was there already.
--
-- Called from both of CR 608.2's resolving roads -- here for an instant or
-- sorcery, and from Pawl.Engine.Stack for a permanent spell -- and from neither
-- fizzle, since a spell CR 608.2b removes did not resolve. The names come off
-- the PROJECTION, so a spell that is a copy of another (CR 707.2) files the
-- copied name. The permanent road is a REGRESSION FENCE: no permanent card in
-- data/cards/ shares a paradigm card's name.
noteResolved :: ObjectId -> PlayerId -> Game Bool
noteResolved oid controller = do
  gs <- State.get
  let names = Projection.namesOf oid gs
      earlier = Map.findWithDefault Set.empty controller (GameState.resolvedNames gs)
  State.put gs {GameState.resolvedNames = Map.insertWith Set.union controller names (GameState.resolvedNames gs)}
  pure (Set.disjoint names earlier)

-- CR 702.192a's two SPELL abilities, performed as the last part of the spell's
-- resolution in applyEpic's place and for its reason: the delayed ability, armed
-- only where noteResolved answered that this is the first resolution of the name
-- (`firstOfName`), and "exile this spell".
--
-- Answers whether the spell left the stack, applyCipher's answer, so
-- finishSpell's move is skipped: the exile IS where the spell goes. A copy (CR
-- 707.12's, the delayed ability's own) is exiled too and CR 704.5e then ends it.
--
-- The keyword is read off the PROJECTION, applyEpic's reading. The ability is
-- armed with Expiry.Never for "for the rest of the game", epic's reason, and
-- binds this id for Keyword.paradigmCopy to find in GameState.stackArchive.
-- Pawl.CastSpec's "Paradigm" group proves both abilities.
applyParadigm :: ObjectId -> PlayerId -> Bool -> Game Bool
applyParadigm oid controller firstOfName = do
  gs <- State.get
  if not (Keyword.Engine.hasParadigm (Map.keysSet (Projection.keywordsOf oid gs)))
    then pure False
    else do
      Monad.when firstOfName . State.modify' $
        Event.armDelayed
          Keyword.Engine.paradigmCopy
          oid
          controller
          (Map.singleton Keyword.Engine.paradigmSlot (Binding.toObject oid))
          Onset.Immediately
          (Just Expiry.Type.Never)
      Event.changeZone oid Zone.Exile
      pure True

-- CR 702.99a's spell ability, "if this spell is represented by a card, you may
-- exile this card encoded on a creature you control", performed as the last
-- part of resolution in applyEpic's place and for its reason. Answers whether
-- the card left the stack, so finishSpell's move is skipped: the exile IS where
-- the card goes, and no buyback or rebound row is installed over it.
--
-- The keyword is read off the PROJECTION, applyEpic's reading. "Represented by a
-- card" is Source.OfCard: CR 707.12's cast copy (Source.OfCardCopy) and CR
-- 707.10's spell copy are not cards, so a copy cast by the granted trigger is
-- never encoded again.
--
-- The creature is CHOSEN, not targeted (CR 115.1), from the permanents the
-- controller controls that are creatures now -- read through the projection, so
-- an animated land qualifies. No creature leaves nothing to ask.
applyCipher :: ObjectId -> PlayerId -> Game Bool
applyCipher oid controller = do
  gs <- State.get
  let represented = case fmap Object.source (Game.lookupObject oid gs) of
        Just (Source.OfCard _) -> True
        _ -> False
      creature c = Projection.controllerOf c gs == Just controller && Set.member CardType.Creature (Projection.cardTypesOf c gs)
      offered = filter creature (Set.toList (GameState.battlefield gs))
  case NonEmpty.nonEmpty offered of
    Just candidates | represented && Map.member Keyword.Cipher (Projection.keywordsOf oid gs) -> do
      answer <- Game.choose (Prompt.ChooseEncode (Decide.deciderFor controller gs) controller oid candidates)
      -- Filtered, not trusted: an answer outside the offer declines.
      case List.find (\c -> Just c == answer) offered of
        Nothing -> pure False
        Just chosen -> do
          landed <- Event.changeZoneReturning oid Zone.Exile
          -- CR 702.99b: the link is filed only where the card reached exile; a
          -- replacement that sent it elsewhere leaves nothing encoded.
          State.modify' $ \g ->
            let arrived = filter (`Set.member` GameState.exile g) (Foldable.toList landed)
             in g {GameState.encoded = foldr (`Map.insert` chosen) (GameState.encoded g) arrived}
          pure (not (null landed))
    _ -> pure False

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
-- characteristics is resolving rather than which card printed them -- read off
-- Projection.spellFaceOf, so a spell that became a copy asks of the copy (CR
-- 707.2, 715.3c, 720.3c) -- a
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
          ExilePlayPermission.alternativeCost = Nothing,
          ExilePlayPermission.condition = Nothing,
          -- This IS rule 715.3d's permission, and so the one its next sentence
          -- excludes the Adventure half from.
          ExilePlayPermission.origin = PlayPermissionOrigin.Adventure,
          -- CR 715.3d: "that player may play it".
          ExilePlayPermission.verb = PermissionVerb.Play,
          ExilePlayPermission.increase = 0,
          ExilePlayPermission.landEnters = TapState.Untapped
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
      let printedSlots = Target.bakeSlots (Binding.playerSlots (Object.bindings obj)) (Map.unions (fmap (uncurry Modal.modeInstanceTargetSlots) modes))
          -- CR 608.2b judges a "for each opponent" slot per copy it bound;
          -- instanceView folds the copies back under the printed name.
          slots = Target.boundCopies (Map.keysSet (Object.bindings obj)) printedSlots
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
                  instanceView :: Map SlotName (Set Recipient) -> Map SlotName (Set Recipient)
                  instanceView = Modal.instanceView printedSlots mi (Mode.targetSlots mode)
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
                          boundHere = Map.keysSet (instanceView (Set.empty <$ gateBindings))
                      gated <- gateHolds effectController srcId (instanceView (Binding.targetsOf gateBindings)) gateBindings limb
                      taken <- if gated then exercises stackId srcId effectController idx limbIdx boundHere legalHere (Just facing) Set.empty limb else pure False
                      before <- State.get
                      (admitted, answers2) <- if taken then payGateAdmits runSubgame stackId srcId effectController idx limbIdx (instanceView legal) (Just facing) Set.empty answers limb else pure (False, answers)
                      Monad.when admitted (asCostWhenNamed indexedClauses limbIdx (applyClauseEffects srcId applyOne (Foldable.toList (Clause.effects limb))))
                      after <- State.get
                      pure (answers2, recordTaken limb admitted before after limbIdx ran)
               in -- CR 608.2e's clause is what each gate covers. Run only when
                  -- `fizzles` is False.
                  Monad.foldM_
                    ( \(answers, picked, ran) (cIdx, clause) -> do
                        -- CR 608.2c's "If you do" first, off the same fold the
                        -- spell path keeps. Proved on this path, not merely
                        -- fenced: Aetherplasm's second clause hangs on its first,
                        -- and Pawl.CombatEffectSpec's "declining to return
                        -- Aetherplasm skips the clause its 'If you do' hangs on"
                        -- reddens when this conjunct is defeated.
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
                        -- Scoped to its own clause on this loop: Eccentric Farmer's
                        -- declined return still mills, Pawl.LibraryOrderSpec's "CR
                        -- 608.2d whole card: Eccentric Farmer's declined return
                        -- leaves the mill done".
                        let legalNowForMay = instanceView (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                            boundNowForMay = Map.keysSet (instanceView (Set.empty <$ gateBindings))
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
                            pure (answers2, Map.insert (NonEmpty.head limbs) (False, Map.empty) picked, ran2)
                          _ -> do
                            (announced, committed, picked2) <- if gated then chosenBranch stackId srcId effectController idx cIdx legalNowForMay eligible (`lookup` indexedClauses) picked clause else pure (Just Set.empty, Set.empty, picked)
                            let branch = maybe True (not . Set.null) announced
                            taken <- if branch then exercises stackId srcId effectController idx cIdx boundNowForMay legalNowForMay announced committed clause else pure False
                            -- CR 118.12: then the cost paid on resolution, against the
                            -- START-of-resolution slots.
                            before <- State.get
                            (admitted, answers2) <- if taken then payGateAdmits runSubgame stackId srcId effectController idx cIdx (instanceView legal) announced committed answers clause else pure (False, answers)
                            Monad.when admitted (asCostWhenNamed indexedClauses cIdx (applyClauseEffects srcId applyOne (Foldable.toList (Clause.effects clause))))
                            after <- State.get
                            pure (answers2, picked2, recordTaken clause admitted before after cIdx ran)
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
-- activated or an object-borne triggered ability is filed, and rule 707.10b's
-- copy of one carries the same Source (Resolve.Effect.copyOnStackOf), so the
-- copy and the original are one key. Rumor Gatherer's and Omnath, Locus of
-- Creation's landfall read the triggered arm (Pawl.ConditionSpec).
recordAbilityResolution :: Object.Object -> Game ()
recordAbilityResolution obj = case Object.source obj of
  Source.OfAbility activated -> State.modify' (Event.recordEvent (GameEvent.ActivatedAbilityResolved activated))
  Source.OfTrigger triggered -> State.modify' (Event.recordEvent (GameEvent.TriggeredAbilityResolved triggered))
  _ -> pure ()

-- CR 608.2c: does this clause's printed "If you do" hold? A clause naming no
-- earlier one always happens; one that names an earlier clause of this mode
-- instance happens only if that clause's instructions ran -- Tweeze's "you may
-- discard a card. If you do, draw a card".
--
-- Off the fold's record of what ran, so the answer is the one the named clause's
-- own riders gave (Clause.ifTaken says why that rather than the board), and a
-- name the fold has not reached -- a later clause, or one that does not exist --
-- has not run. ANY of the names is enough, which is what Worms of the Earth's "if
-- a player does either" prints over the two halves of an either-or pair; the
-- negative, Browbeat's "if no one does", holds when NONE of them ran. Asked
-- BEFORE the other three, so a skipped clause raises no prompt.
--
-- A pure function rather than a Game action: it reads nothing but the fold.
ifTakenHolds :: Set ClauseIndex -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
ifTakenHolds ran clause = case Clause.ifTaken clause of
  Nothing -> True
  Just (IfTaken.AnyTaken names) -> any (`Set.member` ran) names
  Just (IfTaken.NoneTaken names) -> not (any (`Set.member` ran) names)

-- CR 118.12: a clause some clause of its mode names with "If [a player] does"
-- or "doesn't" (Clause.ifTaken) is that sentence's "[do something]" -- "a cost,
-- paid when the spell or ability resolves" -- so its instructions run under
-- Event.payingOnResolution. Library of Leng's ruling is what observes it:
-- Tweeze's "you may discard a card. If you do, draw a card" discards as a cost,
-- and "costs aren't effects".
asCostWhenNamed :: [(ClauseIndex, Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))] -> ClauseIndex -> Game a -> Game a
asCostWhenNamed indexed cIdx body =
  let names clause = case Clause.ifTaken clause of
        Nothing -> []
        Just (IfTaken.AnyTaken named) -> Foldable.toList named
        Just (IfTaken.NoneTaken named) -> Foldable.toList named
   in if any (elem cIdx . names . snd) indexed then Event.payingOnResolution body else body

-- The other end of the same fold: a clause's ordinal is recorded exactly when
-- its instructions ran, which is what CR 608.2c's "If you do" asks about. One
-- writer for both resolution paths, so the spell loop and the ability loop
-- cannot disagree about what "you did" means.
--
-- A MANDATORY clause is admitted whether or not it can be carried out, and CR
-- 118.12's "If you do" asks whether the player "started to pay a mandatory
-- cost": Standstill's sacrifice of a Standstill already gone is no payment. So
-- it is recorded only when the board shows the payment, between `before` (ahead
-- of its CR 118.12 gate) and `after` -- CR 603.12's own test,
-- `happenedBetween`. An optional one needs no such test: CR 608.2d already
-- declines it where it is impossible (`exercises`). Pawl.ResolveSpec's "CR
-- 118.12 Victimize with no creature to sacrifice returns nothing" proves it on
-- the spell loop, and "CR 118.12 Gristle Glutton with an empty hand draws
-- nothing" on the ability loop. The two CR 701.55d limb sites are a REGRESSION
-- FENCE: no "If you do" in data/cards/ hangs off a villainous limb.
recordTaken :: Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool -> GameState -> GameState -> ClauseIndex -> Set ClauseIndex -> Set ClauseIndex
recordTaken clause admitted before after cIdx ran =
  let happened = case Clause.optionality clause of
        Optionality.Mandatory -> happenedBetween before after
        Optionality.Optional _ -> True
   in if admitted && happened then Set.insert cIdx ran else ran

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
-- CR 608.2d / 101.4: where EVERY branch of the pair carries a decline of its
-- own, asked of the chooser (`declinesAlone`), the choice is among three
-- outcomes -- either branch or neither -- and is made once, in APNAP order,
-- before anything happens. So the prompt offers "neither", and the second set
-- coming back is the seats this announcement COMMITTED: their branch's own "may"
-- and cost are taken for them rather than asked again, which is what stops a
-- player on Worms of the Earth announcing the damage and then backing out after
-- seeing the next player sacrifice. Pawl.ResolveSpec's "CR 608.2d a seat that
-- announced the damage is held to it" proves it. A pair answered with no prompt
-- (one or no branch surviving) commits nobody, and the decline is asked where it
-- was printed; the memo carries which.
--
-- The branches are offered in CR 608.2c's printed order and the answer is
-- FILTERED back through them rather than trusted, the posture every choose-don't-
-- target prompt takes: an answer not on offer reads as "neither" where that is
-- offered, and as the first branch otherwise.
--
-- CR 608.2d's impossibility is also asked PER SEAT, where a branch happens only
-- if that seat pays its cost (`gatesOnPayment`): a seat that cannot pay it (CR
-- 118.3) is not offered it, and a seat left one option and no "neither" is
-- forced with no prompt. Pawl.ResolveSpec's "CR 608.2d a seat who cannot pay
-- the sacrifice is offered only the damage" proves it.
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
chosenBranch :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> (ClauseIndex -> Game Bool) -> (ClauseIndex -> Maybe (Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))) -> Map.Map ClauseIndex (Bool, Map.Map PlayerId ClauseIndex) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game (Maybe (Set PlayerId), Set PlayerId, Map.Map ClauseIndex (Bool, Map.Map PlayerId ClauseIndex))
chosenBranch resolving source controller idx cIdx legal eligible limbOf picked clause = case Clause.orElse clause of
  Nothing -> pure (Nothing, Set.empty, picked)
  Just orElse ->
    let branches = orElseLimbs cIdx orElse
        key = NonEmpty.head branches
        won answers = Map.keysSet (Map.filter (== cIdx) answers)
        settled (committing, answers) = (Just (won answers), if committing then won answers else Set.empty)
        neither = all (maybe False (declinesAlone (OrElse.chooser orElse)) . limbOf) branches
        seatCanTake seat branch gsNow = case limbOf branch >>= Clause.payGate of
          Just gate | gatesOnPayment (OrElse.chooser orElse) gate -> gateAffordable resolving source controller legal seat gate gsNow
          _ -> True
     in case Map.lookup key picked of
          Just memo -> let (announced, committed) = settled memo in pure (announced, committed, picked)
          Nothing -> do
            gs <- State.get
            offered <- Monad.filterM eligible (NonEmpty.toList branches)
            memo <- case offered of
              [] -> pure (False, Map.empty)
              [forced] -> pure (False, Map.fromList (fmap (\chooser -> (chooser, forced)) (apnapPlayersOf (OrElse.chooser orElse) legal controller gs)))
              _ ->
                -- CR 101.4b: each chooser is told the branches the choosers
                -- before them announced.
                fmap (\made -> (neither, Map.fromList [(chooser, branch) | (chooser, Just branch) <- Foldable.toList made])) $
                  Monad.foldM
                    ( \made chooser -> do
                        gs1 <- State.get
                        -- CR 608.2d per SEAT: a branch whose cost this seat
                        -- cannot pay (CR 118.3) is not an option for them.
                        mine <- Monad.filterM (State.gets . seatCanTake chooser) offered
                        let fallback opts = if neither then Nothing else Just (NonEmpty.head opts)
                            filtered opts answered = case answered of
                              Just branch | elem branch opts -> Just branch
                              _ -> fallback opts
                        announcedHere <- case NonEmpty.nonEmpty mine of
                          Nothing -> pure Nothing
                          Just (only NonEmpty.:| []) | not neither -> pure (Just only)
                          Just opts -> fmap (filtered opts) (Game.choose (Prompt.ChooseClause (Decide.deciderFor chooser gs1) chooser resolving idx opts neither made))
                        pure (made Seq.|> (chooser, announcedHere))
                    )
                    Seq.empty
                    (apnapPlayersOf (OrElse.chooser orElse) legal controller gs)
            let (announced, committed) = settled memo
            pure (announced, committed, Map.insert key memo picked)

-- CR 603.5 / 118.12: does this branch of a CR 608.2d pair carry a decline of its
-- own, put to the pair's chooser? A "may" asked of that reference, or a cost
-- paid on resolution that the same reference may refuse and that gates the
-- branch on its being paid. A cost another clause offers (PayGate.offeredAt) is
-- not this branch's to decline.
declinesAlone :: PlayerRef.PlayerRef -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
declinesAlone chooser limb = case Clause.optionality limb of
  Optionality.Optional asker | asker == chooser -> True
  _ -> case Clause.payGate limb of
    Just gate -> gatesOnPayment chooser gate && PayGate.obligation gate == PayObligation.Optional
    Nothing -> False

-- CR 118.12: does this branch happen only for a seat that pays its own cost,
-- offered here and to the pair's chooser? Then a seat that cannot pay it (CR
-- 118.3) cannot choose the branch (CR 608.2d).
gatesOnPayment :: PlayerRef.PlayerRef -> PayGate.PayGate -> Bool
gatesOnPayment chooser gate =
  PayGate.payer gate == chooser
    && Maybe.isNothing (PayGate.offeredAt gate)
    && case PayGate.branch gate of
      PayBranch.IfPaid -> True
      _ -> False

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
facedVillainously :: Map.Map ClauseIndex (Bool, Map.Map PlayerId ClauseIndex) -> ClauseIndex -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Maybe (OrElse.OrElse, NonEmpty.NonEmpty ClauseIndex)
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
-- CR 701.55c: each seat's facing is first proposed as WouldFaceVillainousChoice,
-- so a row like The Valeyard's can make it several -- the whole of rule 701.55a
-- performed that many times for that seat, one at a time, each its own choice
-- read off the board the previous one left. Pawl.ResolveSpec's "CR 701.55c The
-- Valeyard: an opponent faces the choice twice" proves it.
villainousPass :: ObjectId -> PlayerId -> ModeIndex -> Map.Map SlotName (Set Recipient) -> OrElse.OrElse -> NonEmpty.NonEmpty ClauseIndex -> (ClauseIndex -> Set PlayerId -> acc -> Game acc) -> acc -> Game acc
villainousPass resolving controller idx legal orElse limbs performLimb acc0 = do
  gs <- State.get
  -- CR 101.4b: each seat is told the choices the seats before it made. No test
  -- drives two seats through this pass, so the payload is unproven here.
  fmap fst $
    Monad.foldM
      ( \seat chooser -> do
          outcome <- Event.applyReplacements (ProposedEvent.WouldFaceVillainousChoice chooser 1)
          let times = maybe 0 (Natural.toIntSaturating . snd) (outcome >>= Replacement.asVillainousChoice)
          Monad.foldM (\faced _ -> face chooser faced) seat (List.replicate times ())
      )
      (acc0, Seq.empty)
      (apnapPlayersOf (OrElse.chooser orElse) legal controller gs)
  where
    face chooser (acc, made) = do
      gs1 <- State.get
      -- No "neither": CR 701.55a's chooser performs one option or the other.
      answered <- Game.choose (Prompt.ChooseClause (Decide.deciderFor chooser gs1) chooser resolving idx limbs False made)
      let chosen = case answered of
            Just branch | elem branch limbs -> branch
            _ -> NonEmpty.head limbs
      State.modify' (bindPlayersSlot resolving Binding.facingPlayers (Set.singleton chooser))
      performed <- performLimb chosen (Set.singleton chooser) acc
      pure (performed, made Seq.|> (chooser, Just chosen))

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
-- `committed` is the seats whose announcement of a three-outcome pair already
-- answered this "may" (chosenBranch); they accept without being asked again.
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
-- Declined silently, so recordTaken stays False and the draw is skipped. The
-- second gate is asked again per seat, with `thoseWhoMay` holding that seat
-- alone, so a seat that cannot carry the clause out is not asked;
-- Pawl.AnteSpec's Rebirth case proves it.
exercises :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Set SlotName -> Map.Map SlotName (Set Recipient) -> Maybe (Set PlayerId) -> Set PlayerId -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game Bool
exercises resolving source controller idx cIdx bound legal announced committed clause = do
  impossible <- State.gets (\gs -> clauseIsImpossible resolving source controller legal gs clause)
  case Clause.optionality clause of
    Optionality.Mandatory -> pure True
    Optionality.Optional asker
      | impossible || clauseIsInert (Set.insert Binding.mayPlayers bound) legal clause -> pure False
      | otherwise -> do
          gs <- State.get
          let -- CR 608.2d, per seat: a seat for whom this clause, read with that
              -- seat alone as its accepter, would do nothing is not asked. A
              -- committed seat has already answered.
              able pid =
                Set.member pid committed
                  || not (clauseIsImpossible resolving source controller (Map.insert Binding.mayPlayers (Set.singleton (Recipient.ToPlayer pid)) legal) gs clause)
          -- CR 101.4b: each seat is told the answers the seats before it gave.
          answers <-
            Monad.foldM
              ( \made pid -> do
                  gs1 <- State.get
                  decision <-
                    if Set.member pid committed
                      then pure OptionalDecision.Exercises
                      else Game.choose (Prompt.ChooseOptional (Decide.deciderFor pid gs1) pid resolving idx cIdx made)
                  pure (made Seq.|> (pid, decision))
              )
              Seq.empty
              (filter able (announcedOnly announced (apnapPlayersOf asker legal controller gs)))
          let accepted = Set.fromList [pid | (pid, OptionalDecision.Exercises) <- Foldable.toList answers]
          State.modify' (bindPlayersSlot resolving Binding.mayPlayers accepted)
          pure (not (Set.null accepted))

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
-- "Unless ANY player pays" (PayBranch.IfNonePaid) is the one branch read over
-- the whole table: Rhystic Tutor's search happens only if every offered player
-- declined. Proved by Pawl.ResolveSpec's "CR 118.12a bob alone pays, so alice
-- does not search".
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
-- `committed` is `exercises`' set: a seat whose announcement already took this
-- branch pays if it can (CR 118.3) and is not asked again.
--
-- ONE offer per payment (CR 118.12): a second clause hanging off the same cost
-- names the first (PayGate.offeredAt) and reuses the recorded answers, `answers`
-- being keyed on the offering clause's ordinal. A clause naming an offer never
-- made falls through and makes it, the named clause having failed its own CR
-- 701.46a "if" or CR 603.5 "may".
payGateAdmits :: Game Result -> ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> Maybe (Set PlayerId) -> Set PlayerId -> Map.Map ClauseIndex (Map.Map PlayerId Bool) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game (Bool, Map.Map ClauseIndex (Map.Map PlayerId Bool))
payGateAdmits runSubgame resolving source controller idx cIdx legal announced committed answers clause = case Clause.payGate clause of
  Nothing -> pure (True, answers)
  Just gate -> do
    let offerAt = Maybe.fromMaybe cIdx (PayGate.offeredAt gate)
    (asked, answers2) <- case Map.lookup offerAt answers of
      Just recorded -> pure (recorded, answers)
      Nothing -> do
        recorded <- payGatePaid runSubgame resolving source controller idx cIdx legal announced committed gate
        pure (recorded, Map.insert offerAt recorded answers)
    let selected = branchSelects (PayGate.branch gate) asked
    State.modify' (bindPlayersSlot resolving Binding.gatePlayers selected)
    pure (not (Set.null selected), answers2)

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
-- the snapshot carries. Pawl.Engine.Quantity's InSlot arm, which looks the
-- holder up by id rather than coming through here, falls back to the same map.
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
