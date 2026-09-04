-- Pawl.Engine.Card's lints over the slots an ability reads and declares (CR
-- 603.7, CR 602.2b): delayed, triggered and activated abilities, and the lints
-- that catch a reserved or shadowed slot. Split out of Pawl.CardSpec, which
-- keeps the machinery and the lints over modes, references and tokens.
module Pawl.AbilitySlotLintSpec where

-- Aliased Condition.Type, matching Pawl.Types.Count below and the project-wide
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- Dotted, because Pawl.Types.Keyword already holds the short alias here (the
-- The json sublibrary's own modules, for the CR 701.3a completeness cross-check
-- The logic module, alongside Pawl.Types.Modal below: unambiguous under one
-- alias because the two modules export disjoint names (TriggerSpec's
-- alone: it counts the atom in a card's ENCODED form, which is a traversal of the
-- convention (FilterSpec/CardSpec's Filter.Type note): Pawl.Engine.Condition may
-- hand-maintained one below.
-- later be imported and must not collide.
-- precedent), and Modal.allEffects is how this lint reaches an activated or
-- reverse of TriggerSpec's split).
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.
-- triggered ability's effects (Card.allEffects only reaches the spell).
-- whole card written by somebody else and so an independent witness to the
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Pawl.CardSpec (anyFace, cardAuthoredEffects, cardCounts, cardResolutionEffects, declaresVariable, effectCounts, lintMode, modalActivated, modalSlotsOffend, oneEffectActivated, oneEffectTrigger, triggerConditionSlots)
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Event.Binding as Event
import qualified Pawl.Engine.Keyword as Keyword.Engine
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Resolve.Slots as Resolve
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Sacrificer as Sacrificer
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotCount as SlotCount
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneScope as ZoneScope

-- CR 400.1: "each player has their own library, hand, and graveyard. The
-- other zones are shared by all players." Battlefield/Stack/Exile/Command are
-- shared; Library/Hand/Graveyard are per-player.
isSharedZone :: Zone.Zone -> Bool
isSharedZone zone = case zone of
  Zone.Library -> False
  Zone.Hand -> False
  Zone.Graveyard -> False
  Zone.Battlefield -> True
  Zone.Stack -> True
  Zone.Exile -> True
  Zone.Command -> True

-- A Count over a shared zone paired with anything but EachPlayer names a
-- per-player fold over a zone no player individually owns -- permitted by the
-- type, not by the rules. Pawl.Codec.InZone.undividedShared is what ENFORCES
-- this, at the decoder, so a card file carrying the pairing never reaches a
-- registry; this restates the rule over the loaded pool rather than calling
-- that predicate, so the two have to agree independently, see #161.
scopeOffends :: Scope.Scope -> Bool
scopeOffends scope = case scope of
  Scope.InZone (InZone.MkInZone zone ref) -> isSharedZone zone && ref /= PlayerRef.EachPlayer
  Scope.InHistory _ -> False
  -- No zone at all, shared or otherwise: this scope folds the players a
  -- PlayerRef names rather than a copy of a zone each of them owns, so the
  -- pairing the lint rejects cannot arise.
  Scope.OverPlayers _ -> False
  -- No zone at all either: this scope folds the objects a binding names,
  -- wherever they are, so there is no per-player copy of a zone for the pairing
  -- the lint rejects to arise in.
  Scope.OverBound _ -> False

cardOffendsSharedZoneScope :: Face.Face Card.Type.Card -> Bool
cardOffendsSharedZoneScope card =
  any (scopeOffends . Count.Type.scope) (cardCounts card)

-- CR 601.2c's OTHER dataflow question, asked of the same modes: a slot whose
-- count may exceed one holds a set of recipients, and only a reader that takes a
-- set can see all of them (Pawl.Types.SlotArity). A card aiming "up to two target
-- creatures" at an opcode that names one object would affect NEITHER of them --
-- Pawl.Engine.Binding.onlyOne declines a slot naming several rather than picking
-- one -- and no compiler catches that, so it is caught here.
--
-- Read off the same Resolve.modeSlots the D4 lint reads, whose join keeps the
-- narrower arity: a slot two effects of one mode read both ways is One. The two
-- lints ask different questions of the one map -- D4 asks its KEYS whether the
-- slot is read at all, this asks its VALUES how many recipients the read sees --
-- which is what SlotArity.Amount parts: a Quantity.InSlot names the slot's
-- amount rather than its objects (Pawl.Engine.Binding.amountOf), so it is a read
-- D4 must count and no arity claim for this lint to reject a plural slot on;
-- see #2774.
modalCountsOffend :: Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
modalCountsOffend modal =
  let modeOffends mode =
        let read_ = Resolve.modeSlots mode
            plural targetSlot = SlotCount.plural (TargetSlot.count targetSlot)
            offends slot targetSlot = plural targetSlot && Map.lookup slot read_ == Just SlotArity.One
         in or (Map.elems (Map.mapWithKey offends (Mode.targetSlots mode)))
   in any modeOffends (Modal.modes modal)

-- The TRIGGERED-ability half of the D4 dataflow lint: every slot one of a
-- triggered ability's effects READS must be a slot something binds for that
-- ability, and every slot it DECLARES must be read. Without the first half, an
-- effect naming CR 400.7e's `became` under a condition that never binds it
-- loads, places its trigger, misses the lookup and silently no-ops (Resolve's
-- MoveToZone arm moves nothing for a slot that names no object); without the
-- second, an ability announces a target it ignores.
--
-- The spell lint's EQUALITY, which modalSlotsOffend now applies to every carrier
-- (#1043). It was a subset check for as long as the bound side was UNIONED into
-- the declared one, which Pawl.Engine.Binding.triggerSource's comment shows is
-- mutually unsatisfiable with the "a reserved slot is never a declared target
-- slot" rule; subtracting the bound names from the READ side instead -- what the
-- spell lint always did with its cast-time pair -- is what makes the equality
-- statable here.
--
-- What answers a read, and why each part of it answers one:
--
--   * Binding.triggerSource (CR 113.7, the object whose ability triggered) and
--     Binding.you (CR 109.5, the ability's controller) are stamped for EVERY
--     triggered ability as it is placed (Engine.placeBorne, Binding.setYou), so
--     they need no agreement with the condition. `you` is stamped for every
--     ACTIVATION and every SPELL too, by rule 109.5's other sentences -- which is
--     why the spell lint subtracts it on the read side rather than listing it
--     here.
--   * Event.eventBindingSlots is the condition-SPECIFIC half -- CR 400.7e's
--     `became`, CR 702.70a's `thatPlayer` -- and is the whole point of this
--     lint.
--   * Resolve.definedSlots covers a slot the ability's own effects MINT rather
--     than read: a Create's token (CR 603.7c's "it"), a PlaySubgame's winner.
--     The same exemption every carrier takes.
--   * the ability's own declared target slots (CR 601.2c / 700.2c) are the
--     ordinary chosen targets -- the side modalSlotsOffend compares AGAINST, one
--     MODE's at a time, so a mode reading a slot only another mode declares is
--     caught and so is a mode declaring a slot only another mode reads.
--
-- The first two are what this passes to modalSlotsOffend as `abilityBound`: they
-- are stamped for the ability, not for a mode, so every mode gets them.
triggeredAbilityOffends :: TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
triggeredAbilityOffends ability =
  modalSlotsOffend
    ( Set.unions
        [ Set.fromList [Binding.triggerSource, Binding.you],
          Event.eventBindingSlots (TriggeredAbility.condition ability)
        ]
    )
    (TriggeredAbility.modal ability)

-- The ACTIVATED-ability half of the same lint: every slot one of an
-- activated ability's effects READS must be a slot the ACTIVATION binds, and
-- every slot it DECLARES must be read. Without the first half, an ability naming
-- CR 109.5's `you` loads, activates, misses the lookup and silently no-ops,
-- exactly as an unbound `became` does above.
--
-- The same EQUALITY as every other carrier (#1043); see modalSlotsOffend and the
-- triggered lint above for why subtracting the bound names from the read side is
-- what makes it statable.
--
-- What answers a read is what Pawl.Engine.Activate.activateAbility stamps on the
-- ability object as it goes on the stack, and nothing else:
--
--   * Binding.triggerSource. CR 113.7: "The source of an activated ability on
--     the stack is the object whose ability was activated" -- stamped for every
--     activation, so Longtusk Cub's "put a +1/+1 counter on Longtusk Cub" is a
--     slot read.
--   * the ability's own declared target slots, one MODE's at a time
--     (modalSlotsOffend). CR 602.2b: "The remainder of the process for
--     activating an ability is identical to the process for casting a spell
--     listed in rules 601.2b-i", which is what routes an activation through CR
--     601.2c's target announcement -- and CR 700.2c scopes it to the chosen
--     mode.
--   * Binding.you. CR 109.5: "For an activated ability, this is the player who
--     activated the ability" -- stamped for every activation alongside the
--     source slot, so Brothers of Fire's "and 1 damage to you" is a slot read.
--     Cast.castSpell stamps it for every SPELL as well (Char), which the spell
--     lint takes on its read side.
--   * Binding.thisAbility. CR 602.2a: "That ability is created on the stack as
--     an object that's not a card" -- stamped for every activation beside the
--     two above, so Forsworn Paladin's "if mana from a Treasure was spent to
--     activate this ability" is a slot read. Not `triggerSource`, which names
--     the ability's SOURCE and carries the record of a different payment.
--   * Binding.variableX, and ONLY when the ability's own cost prints an {X}:
--     CR 601.2b's "the player announces the value of that variable", measured
--     against what CR 602.2b calls "an activated ability's analog to a spell's
--     mana cost ... its activation cost" (Cinder Elemental). A printed X is an
--     ordinary slot read since #14 retired Quantity.X, so it arrives here like
--     any other, and the activation really does bind it -- leaving it out would
--     reject a read that works. The cast side's bullet in cardOffends below
--     states the same thing about a spell's cost.
--   * Resolve.definedSlots, the slot an effect of this ability MINTS rather than
--     reads. The same exemption every sibling carrier takes.
--
-- What is NOT on it is the point:
--
--   * both event slots (CR 400.7e's `became`, CR 702.70a's `thatPlayer`): an
--     activation is not an event, so Pawl.Engine.Event.Binding.eventBindings never runs
--     for one.
--   * Binding.chosenModes (CR 700.2), which IS stamped and is still not an
--     exemption: its binding carries a mode set and nothing else, so no effect
--     read can be answered from it -- Resolve reads a slot as a recipient
--     (Binding.targetsOf) or as an amount (Binding.amountOf), and both are
--     Nothing there. Admitting it would exempt a read that silently no-ops,
--     which is the failure this lint exists to catch.
--
-- SCOPE: the abilities that reach Activate. CR 605.3b's mana abilities take
-- another road -- one "doesn't go on the stack, so it can't be targeted,
-- countered, or otherwise responded to. Rather, it resolves immediately after it
-- is activated" -- so Cost.tapForManaWith pays the route's cost, adds the mana
-- and runs the rest through Resolve.performManaAbility (CR 405.6c). That binds
-- Binding.triggerSource and Binding.you and nothing else: the payment's own
-- bound slots are dropped, there being no ability object to write them onto. The
-- exemptions this lint grants beyond those two are therefore wider than a mana
-- ability gets, and no mana ability in the pool reads one, so applying the same
-- available side to one is uniformity rather than a claim.
activatedAbilityOffends :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
activatedAbilityOffends ability =
  let announcedX =
        if declaresVariable (ActivatedAbility.cost ability)
          then Set.singleton Binding.variableX
          else Set.empty
      sacrificed =
        if sacrificesAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.sacrificedPermanent
          else Set.empty
      tapped =
        if tapsAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.tappedPermanent
          else Set.empty
   in modalSlotsOffend (Set.unions [Set.fromList [Binding.triggerSource, Binding.you, Binding.thisAbility], announcedX, sacrificed, tapped]) (ActivatedAbility.modal ability)

-- Does this cost sacrifice a permanent the payer CHOOSES? Binding.variableX's
-- shape exactly: CR 601.2h's payment binds the slot (Cost.payComponent's
-- Sacrifice arm, folded on by Activate), so an ability whose cost has such a
-- component may read it and one whose cost has not may not.
--
-- CostComponent.SacrificeThis is deliberately not counted: it sacrifices the
-- source, which CR 113.7's `triggerSource` already names and
-- Projection.viewWithLastKnown already answers off its last known information.
sacrificesAsCost :: Cost.Type.Cost Keyword.Keyword -> Bool
sacrificesAsCost = any isSacrifice . Cost.Type.components
  where
    isSacrifice component = case component of
      CostComponent.Sacrifice {} -> True
      _ -> False

-- Does this cost tap permanents the payer CHOOSES? sacrificesAsCost's shape, and
-- the same reason: CR 601.2h's payment binds Binding.tappedPermanent
-- (Cost.payComponent's TapPermanents arm, folded on by Activate), so Unerring
-- Sling's "the tapped creature's power" is an ordinary slot read.
--
-- CostComponent.TapThis is deliberately not counted: CR 107.5 taps the SOURCE,
-- which CR 113.7's `triggerSource` already names.
--
-- CostComponent.TapForTotalPower is not counted either, and that is not an
-- oversight: its arm binds nothing (#915).
--
-- Not offered on the CAST side (cardOffends below): Cast.castSpell discards the
-- payment map, so a spell reading the slot would silently no-op.
tapsAsCost :: Cost.Type.Cost Keyword.Keyword -> Bool
tapsAsCost = any isTap . Cost.Type.components
  where
    isTap component = case component of
      CostComponent.TapPermanents {} -> True
      _ -> False

-- CR 603.7 / 109.5: does this card arm a delayed ability "on your next turn"
-- whose condition is not scoped to its controller's turn?
--
-- Pawl.Types.Onset.FromYourNextTurn carries BOTH halves of that phrase on its
-- own: Event.armOnset stores TurnWindow.ControllersNextTurn and
-- Event.settleOnsets pins the entry to the one turn that turns out to be, whose
-- active player is the entry's controller. So the ability's own
-- TriggerCondition.StepBegins carrying TurnScope.ControllersTurn is redundant
-- for FIRING.
--
-- It is not redundant in the DATA, which is what this lint is about: a card that
-- arms with the onset but scopes with EachTurn has printed an "each" the window
-- would silently narrow to the controller's turn, so its text would mean
-- something the card does not say. That is what this rejects.
--
-- A dangling name (an onset naming an ability the card does not declare) is
-- ALSO an offence here, and deliberately not silently accepted: the neighbouring
-- "every armed delayed ability is declared" lint is what reports it precisely,
-- and answering False for it here would let a card that offends both pass this
-- one.
onsetOffends :: Face.Face Card.Type.Card -> Bool
onsetOffends card =
  let scoped name = case Map.lookup name (Face.delayedAbilities card) of
        Nothing -> False
        Just ability -> Event.controllerTurnScoped (TriggeredAbility.condition ability)
   in not (all scoped (Set.toList (Resolve.onsetGatedAbilities (cardAuthoredEffects card))))

-- modalActivated's TRIGGERED twin, so the per-mode lint can be shown to hand
-- `abilityBound` -- the condition's event slots and CR 109.5's `you` -- to EVERY
-- mode rather than only the first.
-- Does any of these abilities DECLARE a target slot under a name already in
-- `defined`? Split from the sweep below so the rejecting direction can be put to
-- a hand-built ability, which no committed card supplies.
shadowsSlots :: Set.Set SlotName.SlotName -> [TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Bool
shadowsSlots defined abilities =
  let declaredOf ability =
        foldMap (Map.keysSet . Mode.targetSlots) (Modal.modes (TriggeredAbility.modal ability))
   in not (Set.disjoint defined (foldMap declaredOf abilities))

-- shadowsSlots for one face: the slots its own effects define, against the target
-- slots its delayed abilities declare.
shadowsDefinedSlot :: Face.Face Card.Type.Card -> Bool
shadowsDefinedSlot card =
  shadowsSlots
    (Resolve.definedSlots (cardAuthoredEffects card))
    (Map.elems (Face.delayedAbilities card))

modalTrigger ::
  TriggerCondition.TriggerCondition ->
  [Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] ->
  TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
modalTrigger condition modes =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = condition,
      TriggeredAbility.modal = Modal.MkModal (Seq.fromList modes) (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }

-- The slots an ARMING carrier declares as targets: the three carriers above that
-- can arm a CR 603.7 delayed ability, which is all of them except the delayed
-- abilities themselves. CR 603.7c captures the whole environment of the object
-- that armed, so a target chosen for the arming spell is in the delayed ability's
-- bindings -- Ray of Command's "when you lose control of the creature" reads the
-- creature the spell targeted.
--
-- Its own declared target slots are EXCLUDED on purpose: the delayed-ability read lint
-- compares against those separately, per mode, and folding them in here would make
-- that comparison vacuous.
--
-- LOOSE about WHICH carrier armed, because nothing here tracks that: a delayed
-- ability reading a slot declared by an activated ability that does not arm it
-- would pass. Tightening it means threading the arming site through, which no card
-- in the pool needs -- Ray of Command arms from the spell that declares the slot.
armingTargetSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
armingTargetSlots card =
  Set.unions
    ( Map.keysSet (Card.allTargetSlots card)
        : fmap
          (Map.keysSet . Modal.allTargetSlots)
          ( fmap ActivatedAbility.modal (Face.activatedAbilities card)
              <> fmap TriggeredAbility.modal (Face.triggeredAbilities card)
          )
    )

abilitySlotLintSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
abilitySlotLintSpec s registry = Spec.describe s "Lint" $ do
  -- The AbilityName half of the D4 dataflow lint (CR 603.7): an
  -- ArmDelayedTrigger naming an ability the card does not declare is a FAILING
  -- TEST, never a trigger that silently never fires. Equality, not subset: a
  -- declared ability nothing arms is dead card text.
  --
  -- SCOPE: `cardResolutionEffects`, every effect the card authors -- its
  -- abilities' as well as its spell modes' -- and not `Card.allEffects`, which is
  -- the spell modes alone. Meandering
  -- Towershell is what makes the difference load-bearing: it arms from a
  -- TRIGGERED ability, so the narrower view saw a declared entry that nothing
  -- appeared to arm and failed the equality outright.
  --
  -- The "every slot a delayed ability reads is one its card defines" lint below
  -- takes the same wide view, for the same reason: nothing about where a Create
  -- binds its minted tokens is peculiar to a spell mode.
  --
  -- cardAuthoredEffects and not cardResolutionEffects, for the reason that
  -- function gives: a CR 103.6 opening-hand action can arm one.
  Spec.it s "every armed delayed ability is declared, and every declared one is armed" $ do
    ps <- S.allPrintings s
    let cardOffends card =
          Resolve.armedAbilities (cardAuthoredEffects card) /= Map.keysSet (Face.delayedAbilities card)
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling or unused delayed abilities" (fmap (S.nameOf . Printing.card) offenders) []
  -- The lint above joins names WITHIN a card; this one keeps that namespace clear
  -- of rule 702's. Pawl.Engine.Keyword.mintedDelayedAbilities declares decayed's
  -- "sacrifice it at end of combat" under a name of its own, and
  -- Pawl.Engine.Resolve looks a card's declarations up first -- so a card printing
  -- the same name would shadow the rule for any of its permanents holding the
  -- keyword.
  Spec.it s "CR 603.7 no card declares a delayed ability rule 702 already names" $ do
    ps <- S.allPrintings s
    let cardOffends card = not (Map.null (Map.restrictKeys (Face.delayedAbilities card) (Map.keysSet Keyword.Engine.mintedDelayedAbilities)))
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no card shadows a minted delayed ability" (fmap (S.nameOf . Printing.card) offenders) []
  -- The OTHER AbilityName join (CR 613.1f), the delayed-ability lint's shape one
  -- rule over: a Modification.LoseNamedAbility naming an ability its face does
  -- not declare is a FAILING TEST, never a removal that silently removes nothing.
  -- Equality, not subset: a named ability nothing removes is a name for nobody.
  --
  -- Per FACE, because the name is written on one face's ability and read by that
  -- same face's text -- so two faces may reuse a name without colliding.
  Spec.it s "CR 613.1f every named removal names an ability its face declares, and every named ability is removed" $ do
    ps <- S.allPrintings s
    let faceOffends face = namedRemovals face /= declaredAbilityNames face
        offenders = filter (anyFace faceOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling or unused ability names" (fmap (S.nameOf . Printing.card) offenders) []
  -- Every slot a delayed ability READS must be one the arming card DEFINES:
  -- the reserved trigger-source slot, a token bound by a Create, the
  -- incarnation a MoveToZone bound at its destination (Meandering Towershell's
  -- exiled card), a TARGET the arming carrier declared (armingTargetSlots, which
  -- is CR 603.7c's captured environment -- Ray of Command's third sentence), or a
  -- CR 603.2 event slot the entry's own condition binds as it fires
  -- (Event.eventBindingSlots -- False Cure's "that player ... they gained", which
  -- Event.delayedPending stamps on top of the captured environment exactly as
  -- eventTriggers does for an object's trigger). The
  -- `abilityBound` side is `cardResolutionEffects` for the
  -- reason the lint above takes it: the binding effect can live in the ability
  -- that arms, not only in a spell mode.
  --
  -- Through modalSlotsOffend, so a delayed ability with modes is read PER MODE
  -- (#570) -- and so a mode's own declared target slots count, which this lint
  -- omitted entirely. CR 603.3d puts a delayed ability on the stack "identical
  -- to the process for casting a spell listed in rules 601.2c-d", so a slot it
  -- declares really is announced; declaredTargetSlots already counts delayed
  -- abilities' target slots on the DECLARING side, and this is the matching read
  -- side. Since #1043 that comparison is the spell lint's EQUALITY, so a delayed
  -- ability declaring a slot no effect of its reads fails here too.
  Spec.it s "every slot a delayed ability reads is bound by its card" $ do
    ps <- S.allPrintings s
    let cardBound card = Set.insert Binding.triggerSource (Set.union (armingTargetSlots card) (Resolve.definedSlots (cardResolutionEffects card)))
        abilityOffends card ability =
          modalSlotsOffend
            (Set.union (cardBound card) (Event.eventBindingSlots (TriggeredAbility.condition ability)))
            (TriggeredAbility.modal ability)
        -- The CONDITION's own slot, which modalSlotsOffend never sees: it walks
        -- the ability's modes, and CR 603.7's slot-named condition sits beside
        -- them. A SUBSET rather than the equality above, and against cardBound
        -- alone: the condition is matched before CR 603.3d puts the ability on
        -- the stack, so a slot the ability's own mode DECLARES is not yet
        -- announced, and a slot Event.eventBindingSlots names is bound by the
        -- match rather than available to it.
        conditionOffends bound ability = not (Set.isSubsetOf (Set.fromList (triggerConditionSlots (TriggeredAbility.condition ability))) bound)
        cardOffends card = any (\ability -> abilityOffends card ability || conditionOffends (cardBound card) ability) (Map.elems (Face.delayedAbilities card))
        offenders = filter (anyFace cardOffends . Printing.card) ps
        watching slot = modalTrigger (TriggerCondition.LoseControlOfBound slot) [lintMode [] []]
        armingSlot = SlotName.MkSlotName (Text.pack "target")
        strangerSlot = SlotName.MkSlotName (Text.pack "elsewhere")
    -- The condition half is vacuous as a corpus sweep: every slot-named
    -- condition in the pool names a slot its own spell declares (Ray of
    -- Command's target), so the sweep would pass under a predicate that always
    -- answered False. The REJECTING direction is therefore put to a hand-built
    -- pair differing in the slot name alone, the posture the batch-bound lint
    -- below takes.
    Spec.assertBool s (conditionOffends (Set.singleton armingSlot) (watching strangerSlot)) "a condition naming a slot the card never binds is caught"
    Spec.assertBool s (not (conditionOffends (Set.singleton armingSlot) (watching armingSlot))) "a condition naming the arming spell's target slot is left alone"
    Spec.assertEqWith s "no dangling delayed-ability slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- A delayed ability may not DECLARE a target slot under a name its own card
  -- already DEFINES, because the two would land in one slot and the reader would
  -- have to pick. Pawl.Engine.Engine.placeOne merges the ability's placement-time
  -- choices with the environment captured when it was armed, per FIELD, so a
  -- collision leaves one Binding carrying both a CR 601.2c target and a Create's
  -- minted group -- and Pawl.Engine.Resolve.Slots.slotGroup answers with the group,
  -- silently discarding a target that CR 608.2b was owed a re-validation of.
  --
  -- Rejected rather than resolved by precedence: the card would be saying two
  -- different things under one name, which is a card-data mistake and not a rules
  -- question the engine should have an answer to. The neighbouring "every slot a
  -- delayed ability reads is bound by its card" lint could not catch it while it
  -- was a SUBSET check: both sides were on the available list, so a name
  -- appearing in both passed it twice over. Under #1043's equality it rejects the
  -- same shape as a side effect, whether or not the ability reads the name back
  -- -- a read is answered by the bound side and so cancels, and no read leaves the
  -- read side empty, so either way the declared slot goes unmatched. Kept anyway:
  -- it states the claim that is actually true of the data (a name may not be both
  -- declared and defined) rather than deriving it from a dataflow count, and it
  -- names the offending card outright.
  Spec.it s "no delayed ability declares a target slot under a name its card defines" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace shadowsDefinedSlot . Printing.card) ps
    Spec.assertEqWith s "no delayed ability shadows a defined slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep above is vacuous on its own -- no card offends, and none would
  -- under a predicate that always answered False -- so both directions are put
  -- to a hand-built pair. The accepted one is Thatcher Revolt's exact shape,
  -- which must stay legal: reading a defined slot is the whole point, and only
  -- DECLARING one is the mistake.
  Spec.it s "the shadowing lint accepts a delayed ability that only reads the slot" $ do
    let tokens = SlotName.MkSlotName (Text.pack "tokens")
        reads_ = modalTrigger TriggerCondition.SelfEnters [lintMode [Effect.Sacrifice SacrificeEffect.MkSacrificeEffect {SacrificeEffect.ref = ObjectRef.InSlot tokens, SacrificeEffect.sacrificer = Sacrificer.EffectController}] []]
        declares = modalTrigger TriggerCondition.SelfEnters [lintMode [Effect.Sacrifice SacrificeEffect.MkSacrificeEffect {SacrificeEffect.ref = ObjectRef.InSlot tokens, SacrificeEffect.sacrificer = Sacrificer.EffectController}] [tokens]]
    Spec.assertBool s (not (shadowsSlots (Set.singleton tokens) [reads_])) "reading a Create's slot is legal"
    Spec.assertBool s (shadowsSlots (Set.singleton tokens) [declares]) "declaring a target slot under the same name is not"
  -- The pairing Pawl.Types.Onset.FromYourNextTurn depends on and cannot enforce
  -- alone. See onsetOffends for why the onset and the condition's TurnScope
  -- are two halves of one printed "your next turn", and what goes wrong when a
  -- card supplies only one of them.
  Spec.it s "every delayed ability armed for YOUR next turn is controller-scoped" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace onsetOffends . Printing.card) ps
    Spec.assertEqWith s "no onset over a condition that admits another player's turn" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep above is NOT vacuous -- Meandering Towershell is a real card with
  -- an onset, so the accepting direction is exercised by the pool -- but nothing
  -- committed offends it, so the REJECTING direction is proven here instead,
  -- against that same card misauthored on purpose. Never a card file: a card
  -- that offends a lint must not be loadable.
  Spec.it s "the lint itself catches an onset over an EachTurn condition" $ do
    towershell <- S.printingOf s registry "Meandering Towershell"
    let face = S.combinedFace towershell
        -- The Towershell's own condition with CR 603.2b's OTHER turn scope: "at
        -- the beginning of EACH declare attackers step", which an opponent's
        -- turn satisfies. Built rather than pattern-matched, so this fixture
        -- states the offence outright.
        eachTurn ability =
          ability
            { TriggeredAbility.condition =
                TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Combat CombatStep.DeclareAttackers) TurnScope.EachTurn)
            }
        widened = face {Face.delayedAbilities = fmap eachTurn (Face.delayedAbilities face)}
        -- The other way a card can reach this: an onset naming an ability the
        -- card does not declare at all.
        dangling = face {Face.delayedAbilities = Map.empty}
        -- CR 508.3d, the other condition whose answer turns on a payload rather
        -- than on the constructor: CR 508.1 lets only the active player declare,
        -- so "whenever YOU attack" can only happen on the controller's turn and
        -- "whenever A PLAYER attacks" can happen on anyone's.
        attacksWith relation ability = ability {TriggeredAbility.condition = TriggerCondition.PlayerAttacks relation}
        youAttack = face {Face.delayedAbilities = fmap (attacksWith PlayerRelation.You) (Face.delayedAbilities face)}
        anyoneAttacks = face {Face.delayedAbilities = fmap (attacksWith PlayerRelation.AnyPlayer) (Face.delayedAbilities face)}
    Spec.assertBool s (not (onsetOffends face)) "the real card, ControllersTurn, is accepted"
    Spec.assertBool s (onsetOffends widened) "EachTurn under an onset is rejected"
    Spec.assertBool s (onsetOffends dangling) "and so is an onset naming no declared ability"
    Spec.assertBool s (not (onsetOffends youAttack)) "CR 508.3d \"whenever you attack\" is controller-scoped"
    Spec.assertBool s (onsetOffends anyoneAttacks) "and \"whenever a player attacks\" is not"
    -- Not a check that fires for every card: one with no onset at all has
    -- nothing for this to reject, whatever its delayed abilities are scoped to.
    tidalWave <- S.printingOf s registry "Tidal Wave"
    Spec.assertBool s (not (onsetOffends (S.combinedFace tidalWave))) "a card with no onset is not swept up"
  -- The same equality over a card's TRIGGERED abilities, which is where
  -- the condition-specific reserved slots live -- CR 400.7e's `became` and
  -- CR 702.70a's `thatPlayer`. See triggeredAbilityOffends for what answers a
  -- read there.
  Spec.it s "every slot a triggered ability reads is bound for its condition, and every slot it declares is read" $ do
    ps <- S.allPrintings s
    let cardOffends = any triggeredAbilityOffends . Face.triggeredAbilities
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling triggered-ability slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep above passes VACUOUSLY: no committed card misauthors the
  -- pairing, so the sweep proves nothing about the lint. Both directions are
  -- proven here instead, against a hand-built offender (never a card file --
  -- a misauthored card must not be loadable) and against the real pairing.
  --
  -- Both reserved event slots, because a classification that answered "every
  -- slot, always" would pass the offending half of either one alone.
  Spec.it s "the lint itself catches a reserved event slot the condition never binds" $ do
    roaches <- S.printingOf s registry "Endless Cockroaches"
    let -- Endless Cockroaches' own payload: "return it to its owner's hand".
        returnIt = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.became) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue Nothing)
        -- Rule 702.70a's shape, as a targetless read of "that player".
        thatPlayerDraws = Effect.Draw (Draw.MkDraw (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1) Nothing)
    Spec.assertBool
      s
      (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfEnters returnIt))
      "CR 400.7e became under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDies returnIt)))
      "and under a dies trigger it is accepted"
    Spec.assertBool
      s
      (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDies thatPlayerDraws))
      "CR 702.70a thatPlayer under a dies trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDealsCombatDamageToPlayer thatPlayerDraws)))
      "and under a combat-damage trigger it is accepted"
    Spec.assertBool
      s
      (not (any triggeredAbilityOffends (Face.triggeredAbilities (S.combinedFace roaches))))
      "the real card's dies trigger is accepted"
  -- The same equality, on the read that is NOT an effect's operand: CR 603.2's
  -- "that player" may be named by a target slot's own FILTER (Trygon Predator),
  -- which Resolve.modeSlots sees through Filter.boundSlots. Without that clause
  -- the pairing below would be invisible and a card could narrow a slot by a
  -- player its condition never binds -- a slot that then admits nothing.
  Spec.it s "the lint itself catches a reserved event slot named by a target filter" $ do
    trygon <- S.printingOf s registry "Trygon Predator"
    let target = SlotName.MkSlotName (Text.pack "target")
        narrowed =
          Mode.MkMode
            (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Tap (ObjectRef.InSlot target)))))
            (Map.singleton target (TargetSlot.required Pool.Permanents (Just (Filter.Type.ControlledByBound Binding.triggerPlayer))))
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfEnters [narrowed]))
      "CR 603.2 thatPlayer in a filter under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfDealsCombatDamageToPlayer [narrowed])))
      "and under a combat-damage trigger it is accepted"
    Spec.assertBool
      s
      (not (any triggeredAbilityOffends (Face.triggeredAbilities (S.combinedFace trygon))))
      "the real card's own trigger is accepted"
  -- The same equality on the slot's THIRD read, beside its pool's and its
  -- filter's: CR 202.3's computed bound (Resolve.targetSlotSlots). A bound may
  -- name CR 603.2's "that player" through a PlayerRef buried INSIDE the number --
  -- "with mana value X or less, where X is the amount of life that player gained
  -- this turn" -- and QuantitySlot.slots does not report that read, only
  -- QuantitySlot.nestedRefs does. Without it the pairing is invisible and a card could
  -- bound a slot by a player its condition never binds, which is the dead bound
  -- the fold exists to catch.
  Spec.it s "the lint itself catches a computed bound naming a slot through a player" $ do
    let target = SlotName.MkSlotName (Text.pack "target")
        -- Celestine, the Living Saint's own bound, one reference over: hers reads
        -- the caster's life gain, this reads the trigger's player.
        bounded amount =
          Mode.MkMode
            (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Tap (ObjectRef.InSlot target)))))
            (Map.singleton target (TargetSlot.withAmount amount (TargetSlot.required Pool.Permanents (Just Filter.Type.ManaValueAtMostAmount))))
        thatPlayer = Quantity.Type.LifeGainedThisTurn (PlayerRef.InSlot Binding.triggerPlayer)
        you = Quantity.Type.LifeGainedThisTurn (PlayerRef.Relative PlayerRelation.You)
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfEnters [bounded thatPlayer]))
      "CR 603.2 thatPlayer inside a bound under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfDealsCombatDamageToPlayer [bounded thatPlayer])))
      "and under a combat-damage trigger it is accepted"
    -- The pair that differs in exactly one thing: the same bound on the same
    -- trigger, reading CR 109.5's caster instead of a slot, stays accepted -- so
    -- the lint reads the reference rather than rejecting every computed bound.
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfEnters [bounded you])))
      "where a bound naming no slot at all is accepted"
    -- And the OTHER read QuantitySlot.slots does not report: CR 400.7j's fold,
    -- which names a slot outright rather than through a reference. Asserted on the map
    -- itself, there being no trigger condition that binds an ordinary target slot
    -- for the lint to accept it against.
    let overBound =
          Quantity.Type.Count
            ( Count.Type.MkCount
                (Scope.OverBound target)
                (Filter.Type.And [])
                Aggregation.Members
            )
    Spec.assertEqWith
      s
      "a bound folding over the objects a slot names reports that slot"
      (Map.keysSet (Resolve.targetSlotSlots (TargetSlot.withAmount overBound (TargetSlot.required Pool.Permanents Nothing))))
      (Set.singleton target)
  -- The same equality over a card's ACTIVATED abilities, the one carrier with no
  -- event slot answering a read at all: an activation is not an event. See
  -- activatedAbilityOffends for the whole of it.
  --
  -- Brothers of Fire is what this caught: its "and 1 damage to you" reads CR
  -- 109.5's slot from an ACTIVATED ability, which nothing bound until
  -- Activate.activateAbility started stamping it (#569).
  Spec.it s "every slot an activated ability reads is bound for its activation, and every slot it declares is read" $ do
    ps <- S.allPrintings s
    let abilitiesOf p = fmap ((,) (Face.name (S.combinedFace p))) (Face.activatedAbilities (S.combinedFace p))
        abilities = concatMap abilitiesOf ps
        readsAnySlot ab = not (all (Map.null . Resolve.slotsOf) (Modal.allEffects (ActivatedAbility.modal ab)))
    -- Guards the sweep against passing vacuously, in both directions: an empty
    -- pool of abilities, and a pool in which none reads a slot at all (where
    -- every ability would pass on an empty read side whatever the lint said).
    Spec.assertBool s (not (null abilities)) "the pool has activated abilities"
    Spec.assertBool s (any (readsAnySlot . snd) abilities) "and one of them reads a slot"
    Spec.assertEqWith s "no dangling activated-ability slot" (fmap fst (filter (activatedAbilityOffends . snd) abilities)) []
  -- CR 601.2c's count, over every carrier at once: see modalCountsOffend.
  Spec.it s "every slot that may take more than one target is read where a set fits" $ do
    ps <- S.allPrintings s
    let carriers p =
          let face = S.combinedFace p
           in fmap ((,) (Face.name face)) $
                Face.spell face
                  : fmap ActivatedAbility.modal (Face.activatedAbilities face)
                    <> fmap TriggeredAbility.modal (Face.triggeredAbilities face)
        modals = concatMap carriers ps
        takesSeveral (_, modal) =
          any (any (SlotCount.plural . TargetSlot.count) . Mode.targetSlots) (Modal.modes modal)
    -- The pool must actually contain one, or the sweep says nothing.
    Spec.assertBool s (any takesSeveral modals) "the pool has a slot that takes more than one target"
    Spec.assertEqWith s "no multi-target slot is read one at a time" (fmap fst (filter (modalCountsOffend . snd) modals)) []
  -- The rejecting direction, which the sweep above cannot show: a mode whose slot
  -- takes two targets and whose only reader is Effect.TurnFaceUp, a bare SlotName.
  Spec.it s "the lint itself catches a multi-target slot read one at a time" $ do
    let slot = SlotName.MkSlotName (Text.pack "creature")
        modeReading targetSlot readers =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList readers))) (Map.singleton slot targetSlot)))
            (ModeSelection.ChooseExactly 1)
        modeWith targetSlot reader = modeReading targetSlot [reader]
        two = TargetSlot.upTo 2 Pool.Creatures Nothing
        -- A NUMBER reading the same name: Quantity.InSlot asks for the slot's
        -- amount (Pawl.Engine.Binding.amountOf), never for an object.
        amountReader = Effect.Bolster (Quantity.Type.InSlot slot)
        -- ONE number reading the same name both ways: the sum of that amount and
        -- an inner number aimed at the object the slot names.
        bothReader =
          Effect.Bolster
            ( Quantity.Type.Plus
                ( Plus.MkPlus
                    (Quantity.Type.InSlot slot)
                    (Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot slot (Quantity.Type.Literal 1)))
                )
            )
    Spec.assertBool
      s
      (modalCountsOffend (modeWith two (Effect.TurnFaceUp slot)))
      "a two-target slot read as one object offends"
    Spec.assertBool
      s
      (not (modalCountsOffend (modeWith two (Effect.Tap (ObjectRef.InSlot slot)))))
      "and the same slot read through an ObjectRef does not"
    Spec.assertBool
      s
      (not (modalCountsOffend (modeWith (TargetSlot.required Pool.Creatures Nothing) (Effect.TurnFaceUp slot))))
      "nor does a one-target slot read as one object"
    -- CR 601.2c's "any number of target ...", which states no maximum to compare
    -- against: an unbounded slot is plural, so the same one-object reader offends.
    -- No card in the corpus makes this mistake, so this is the only observer the
    -- OFFENDING direction of TargetCount.plural's unbounded arm has; the
    -- permitting direction is Tinybones Joins Up, whose "any number of target
    -- players" the sweep above walks.
    Spec.assertBool
      s
      (modalCountsOffend (modeWith (TargetSlot.anyNumber Pool.Creatures Nothing) (Effect.TurnFaceUp slot)))
      "an unbounded slot read as one object offends too"
    -- The OTHER slot-naming arm of a number, on the offending board's own slot:
    -- an amount read is no arity claim, so the two-target slot is legal beside
    -- it. The pair differs from the first assertion in the READER alone -- same
    -- slot, same count -- so it proves the lint discriminates rather than
    -- rejecting every number that names a plural slot; see #2774.
    Spec.assertBool
      s
      (not (modalCountsOffend (modeWith two amountReader)))
      "and a two-target slot read as an amount does not offend"
    -- The half neither board above can prove: a classification that dropped the
    -- amount read from the arity map by dropping it from the map ALTOGETHER
    -- would pass both, and silently stop the D4 dataflow lint seeing the read.
    -- Asserted on the map itself, that lint reading exactly these keys.
    Spec.assertEqWith
      s
      "and the D4 lint still sees the amount read"
      (fmap (Map.keysSet . Resolve.modeSlots) (Foldable.toList (Modal.modes (modeWith two amountReader))))
      [Set.singleton slot]
    -- And the two reads of one slot, which the boards above cannot tell apart
    -- either: an amount read must not MASK an object read of the same name.
    -- Twice, because the two reads meet in two different joins -- inside one
    -- number (Resolve.quantitySlots' own left-biased union) and across two
    -- effects of one mode (Resolve.joinTwo's min) -- and each keeps the narrower
    -- answer on its own.
    Spec.assertBool
      s
      (modalCountsOffend (modeWith two bothReader))
      "and one number reading the slot both ways offends"
    Spec.assertBool
      s
      (modalCountsOffend (modeReading two [amountReader, Effect.TurnFaceUp slot]))
      "as does an object read beside an amount read of the same slot"
  -- The sweep above passes VACUOUSLY on the rejecting side: no committed
  -- activated ability reads a slot it is not given, so the REJECTING direction is
  -- proven here instead, against hand-built offenders and against the four real
  -- cards that between them exercise every part of the available side.
  --
  -- Every reserved slot an activation does NOT bind gets its own case, because a
  -- classification answering "every slot, always" would pass any one of them
  -- alone. CR 109.5's `you` is the case that runs the other way, and is asserted
  -- on BOTH lints: the rule defines the word for an activated ability and for a
  -- triggered one, so the same effect is accepted either way (#569). Leaving it
  -- off one lint's available side is what a card would then fail on, so the pair
  -- is what keeps the two halves of the rule from drifting apart.
  Spec.it s "the lint itself catches an activated ability reading a slot activation never binds" $ do
    longtuskCub <- S.printingOf s registry "Longtusk Cub"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    cinderElemental <- S.printingOf s registry "Cinder Elemental"
    brothers <- S.printingOf s registry "Brothers of Fire"
    let free = Just (ManaCost.MkManaCost [])
        variable = Just (ManaCost.MkManaCost [ManaSymbol.Variable])
        -- CR 109.5's "you", in the shape Baral, Chief of Compliance's TRIGGERED
        -- ability uses it: a bare-SlotName opcode naming the controller.
        youDiscards = Effect.Discard (Discard.Counted (CountedDiscard.MkCountedDiscard Binding.you (Quantity.Type.Literal 1) Nothing))
        -- Endless Cockroaches' payload (CR 400.7e) and rule 702.70a's, the two
        -- event slots, neither of which an activation has an event to bind.
        returnIt = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.became) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue Nothing)
        thatPlayerDraws = Effect.Draw (Draw.MkDraw (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1) Nothing)
        -- CR 113.7's source slot, which every activation DOES bind.
        tapSelf = Effect.Tap (ObjectRef.InSlot Binding.triggerSource)
        -- An ordinary slot this ability neither declares nor mints.
        tapGhost = Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "ghost")))
        -- CR 601.2b's announced value, read as a slot rather than as Quantity.X.
        drawX = Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.InSlot Binding.variableX) Nothing)
    Spec.assertBool
      s
      (not (activatedAbilityOffends (oneEffectActivated free youDiscards)))
      "CR 109.5 you is accepted: an activation binds the player who activated it"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDies youDiscards)))
      "and the very same effect is accepted on a triggered ability"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free returnIt))
      "CR 400.7e became is rejected: an activation is not an event"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free thatPlayerDraws))
      "CR 702.70a thatPlayer is rejected for the same reason"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free tapGhost))
      "and so is an ordinary slot the ability never declares"
    Spec.assertBool
      s
      (not (activatedAbilityOffends (oneEffectActivated free tapSelf)))
      "CR 113.7 self is accepted, stamped for every activation"
    Spec.assertBool
      s
      (not (activatedAbilityOffends (oneEffectActivated variable drawX)))
      "CR 601.2b X is accepted when the activation cost prints {X}"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free drawX))
      "and rejected when it does not"
    -- The four real cards between them cover every part of the available side
    -- that a committed card reaches: CR 113.7's self, CR 601.2c's declared
    -- target, an ability whose cost carries CR 601.2b's {X}, and CR 109.5's
    -- `you`. Brothers of Fire is the last one's only producer, so dropping it
    -- from this list would leave that part of the available side asserted by
    -- the hand-built case alone.
    Spec.assertEqWith
      s
      "Longtusk Cub, Prodigal Sorcerer, Cinder Elemental and Brothers of Fire are all accepted"
      (fmap (any activatedAbilityOffends . Face.activatedAbilities . S.combinedFace) [longtuskCub, sorcerer, cinderElemental, brothers])
      [False, False, False, False]
  -- The PER-MODE half of all three read lints (#570), which no sweep above can
  -- reach: a one-mode ability cannot have a mode read another mode's slot at
  -- all, and all three multi-mode abilities in the pool have each mode reading
  -- only what that mode declares, so per-mode and the old union shape agree on
  -- every card committed today. Aether Channeler's is the one that comes closest
  -- to exercising the difference and the only non-synthetic one -- three modes,
  -- of which only the middle declares a slot -- and the other two (Synthetic
  -- Modal Activator's, Synthetic Modal Trigger's) declare one apiece. Proven
  -- here against hand-built offenders instead.
  --
  -- Both halves of the union, because closing one and leaving the other would
  -- pass this: the declared TARGET slots (CR 700.2c) and the slots an effect
  -- MINTS (Resolve.definedSlots).
  Spec.it s "the lint itself catches a mode reading a slot only another mode declares" $ do
    let creature = SlotName.MkSlotName (Text.pack "creature")
        victim = SlotName.MkSlotName (Text.pack "victim")
        exiled = SlotName.MkSlotName (Text.pack "exiled")
        tap slot = Effect.Tap (ObjectRef.InSlot slot)
        -- Mode 0 declares `creature` and reads it; mode 1 reads it and declares
        -- nothing. Under ChooseExactly 1, choosing mode 1 alone stamps mode 1's
        -- target slots -- which is nothing -- so the read is unbound at runtime.
        crossDeclared = modalActivated [lintMode [tap creature] [creature], lintMode [tap creature] []]
        -- The same two reads, each mode declaring the slot it reads.
        ownDeclared = modalActivated [lintMode [tap creature] [creature], lintMode [tap victim] [victim]]
        -- Mode 0 MINTS `exiled` at a MoveToZone's destination; mode 1 reads it.
        -- The two never resolve together, so mode 1's read is dangling.
        exileIt = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot creature) Zone.Exile EntryRiders.defaultValue (Just exiled) Nothing LibraryPlacement.defaultValue Nothing)
        crossMinted = modalActivated [lintMode [exileIt] [creature], lintMode [tap exiled] []]
        ownMinted = modalActivated [lintMode [exileIt, tap exiled] [creature], lintMode [tap victim] [victim]]
    Spec.assertBool s (activatedAbilityOffends crossDeclared) "a mode reading a slot only another mode declares is rejected"
    Spec.assertBool s (not (activatedAbilityOffends ownDeclared)) "and each mode reading only what it declares is accepted"
    Spec.assertBool s (activatedAbilityOffends crossMinted) "a mode reading a slot only another mode mints is rejected"
    Spec.assertBool s (not (activatedAbilityOffends ownMinted)) "and a mode reading what it mints itself is accepted"
    -- The ABILITY-scoped side must still reach every mode, not just the first:
    -- CR 400.7e's `became` is bound by the condition for the whole ability, so a
    -- SECOND mode reading it is accepted, and a mode reading it under a
    -- condition that never binds it is rejected however late the mode sits.
    let returnBecame = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.became) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue Nothing)
        secondModeReads condition = modalTrigger condition [lintMode [] [], lintMode [returnBecame] []]
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (secondModeReads TriggerCondition.SelfDies)))
      "the condition's event slots reach a later mode too"
    Spec.assertBool
      s
      (triggeredAbilityOffends (secondModeReads TriggerCondition.SelfEnters))
      "and a later mode is still rejected when the condition binds nothing"
  -- The DECLARED-BUT-UNREAD half of the ability lints, which the spell lint has
  -- always had and the three ability ones acquired with #1043. Before that they
  -- were subset checks, so an ability announcing a target no effect of its reads
  -- passed -- and so did an Effect whose Resolve.slotsOf arm UNDER-REPORTED,
  -- since a forgotten read only shrinks the read side that the subset check let
  -- be small. Reverting Resolve.slotsOf's BecomeMonarch arm to Set.empty left the
  -- entire suite green when Denethor, Stone Seer landed (#1040); under the
  -- equality it fails, because Denethor's ability declares two slots and would
  -- then read one.
  --
  -- Every carrier gets a case: what makes this an ability-side gap is that the
  -- claim was stated for Face.spell alone, so proving it on one ability would not
  -- show it reaching the others.
  Spec.it s "the lint itself catches an ability declaring a slot no effect reads" $ do
    denethor <- S.printingOf s registry "Denethor, Stone Seer"
    let creature = SlotName.MkSlotName (Text.pack "creature")
        victim = SlotName.MkSlotName (Text.pack "victim")
        tap slot = Effect.Tap (ObjectRef.InSlot slot)
        -- One mode declaring two slots and reading only one of them: Denethor's
        -- exact shape under a slotsOf arm that forgot a read.
        unread = [lintMode [tap creature] [creature, victim]]
        read_ = [lintMode [tap creature] [creature]]
        -- The delayed lint calls modalSlotsOffend itself, with the whole card's
        -- minted slots on the bound side; nothing here mints, so the bound side
        -- is CR 113.7's source alone.
        delayed modes = modalSlotsOffend (Set.singleton Binding.triggerSource) (TriggeredAbility.modal (modalTrigger TriggerCondition.SelfDies modes))
        -- The chosen graveyard card's SCOPE as the SOLE reader of a declared
        -- slot, which is Grasping Tentacles' second clause with its mill set
        -- aside. The pair differs in the scope alone, so it proves
        -- Resolve.objectRefSlots reports that read rather than the chooser's.
        takeFrom scope =
          Effect.MoveToZone
            ( MoveToZone.MkMoveToZone
                (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard Chooser.TheController scope (Filter.Type.HasCardType CardType.Artifact)))
                Zone.Battlefield
                EntryRiders.defaultValue
                Nothing
                Nothing
                LibraryPlacement.defaultValue
                Nothing
            )
        scoped = [lintMode [takeFrom (ZoneScope.Scoped PlayerScope.You)] [victim]]
        inSlot = [lintMode [takeFrom (ZoneScope.InSlot victim)] [victim]]
    Spec.assertBool s (activatedAbilityOffends (modalActivated unread)) "an activated ability declaring an unread slot is rejected"
    Spec.assertBool s (not (activatedAbilityOffends (modalActivated read_))) "and reading everything it declares is accepted"
    Spec.assertBool s (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfDies unread)) "a triggered ability declaring an unread slot is rejected"
    Spec.assertBool s (not (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfDies read_))) "and reading everything it declares is accepted"
    Spec.assertBool s (delayed unread) "a delayed ability declaring an unread slot is rejected"
    Spec.assertBool s (not (delayed read_)) "and reading everything it declares is accepted"
    -- The real card whose landing exposed the gap, accepted: its ability declares
    -- a player slot for the crown and an `any target` slot for the damage, and
    -- Resolve.slotsOf reports both.
    Spec.assertBool
      s
      (not (any activatedAbilityOffends (Face.activatedAbilities (S.combinedFace denethor))))
      "Denethor, Stone Seer's two-slot ability is accepted"
    Spec.assertBool s (not (activatedAbilityOffends (modalActivated inSlot))) "a slot read only by a chosen graveyard card's scope is accepted"
    Spec.assertBool s (activatedAbilityOffends (modalActivated scoped)) "and the same effect over a scope naming no slot leaves that slot unread"
  -- CR 400.1: every InZone Count over a shared zone (battlefield, stack,
  -- exile, command) must pair with PlayerRef.EachPlayer -- the type
  -- permits any PlayerRef there, but only EachPlayer is meaningful for a
  -- zone no player owns individually, see #161. A REGRESSION FENCE rather than
  -- the guard: the decoder refuses such a file, so an offender would abort the
  -- whole corpus load before this sweep ran. Pawl.RegistrySpec's "CR 400.1 a
  -- corpus dividing a shared zone between players" is where that is proved.
  Spec.it s "every InZone Count over a shared zone pairs with EachPlayer" $ do
    ps <- S.allPrintings s
    let offenders =
          filter
            (anyFace cardOffendsSharedZoneScope . Printing.card)
            ps
    Spec.assertEqWith s "no shared-zone scope with a non-EachPlayer ref" (fmap (S.nameOf . Printing.card) offenders) []
    -- The sweep above is vacuous on any Count position the traversal forgets, so
    -- the newest one is asserted positively: Sphere of Safety's CR 508.1h share
    -- counts "enchantments you control", the only Count a cost to attack can
    -- hold, and cardCounts must see it.
    sphere <- S.printingOf s registry "Sphere of Safety"
    Spec.assertEqWith
      s
      "a cost to attack's counted share is in the sweep"
      (fmap Count.Type.scope (cardCounts (S.combinedFace sphere)))
      [Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer)]
    -- And the newest position of all, planted rather than read off a card: CR
    -- 122.5's GIVER became an ObjectRef when the first side was widened to a
    -- group, so a library walk's depth there is a Count position this traversal
    -- has to reach. No printing can exercise it -- rule 122.5 moves counters
    -- between two things on the battlefield, so a giver naming a card in a
    -- library moves nothing -- and the arm reading the moved kinds alone kept
    -- compiling (#2729).
    let counted = Count.Type.MkCount (Scope.InZone (InZone.MkInZone Zone.Graveyard (PlayerRef.Relative PlayerRelation.You))) (Filter.Type.HasCardType CardType.Creature) Aggregation.Members
        moving =
          Effect.MoveCounters
            ( MoveCounters.MkMoveCounters
                (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Count counted)))
                (MovedKinds.EveryOfKind CounterKind.PlusOnePlusOne)
                Nothing
                (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "recipient")))
            )
    Spec.assertEqWith s "a counter move's giver puts its depth's Count in the sweep" (effectCounts moving) [counted]

-- Every AbilityName a face's layer-6 removals refer to. Both carriers of a
-- Modification are swept: the printed ones on Face.staticAbilities, and the
-- stored ones a resolution creates, which reach a Modification only through
-- Effect.ModifyTarget.
--
-- The WILDCARD on Effect is deliberate and is the one hole here: ModifyTarget is
-- the sole Effect arm carrying a Modification, so a second one added later would
-- escape this lint without breaking the build.
namedRemovals :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
namedRemovals face =
  let stored effect = case effect of
        Effect.ModifyTarget modify -> [ModifyTarget.modification modify]
        _ -> []
      printed ability = Foldable.toList (StaticAbility.modifications ability)
      removals modification = case modification of
        Modification.LoseNamedAbility name -> [name]
        _ -> []
   in Set.fromList
        ( concatMap removals (concatMap printed (Face.staticAbilities face))
            <> concatMap removals (concatMap stored (cardAuthoredEffects face))
        )

-- Every AbilityName a face declares -- the other side of the join above. Both
-- carriers of one: an activated ability (Gliding Licid) and a printed
-- replacement (Glittering Lion). A HAND-KEPT union, so a third carrier added to
-- Pawl.Types.AbilityName's readers must be added here too, or its cards' names
-- read as dangling.
declaredAbilityNames :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
declaredAbilityNames face =
  Set.fromList
    ( Maybe.mapMaybe ActivatedAbility.name (Face.activatedAbilities face)
        <> Maybe.mapMaybe PrintedReplacement.name (Face.replacementEffects face)
    )

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Card" $ do
  abilitySlotLintSpec s registry
