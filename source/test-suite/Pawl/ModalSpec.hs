{-# LANGUAGE GADTs #-}

-- Covers modal casting: Pawl.Engine.Cast mode selection, Pawl.Engine.Resolve chosen-mode
-- resolution.
module Pawl.ModalSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.

import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChooseBetween as ChooseBetween
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as ModalT
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Chaos Charm's three modes, in printed order (CR 700.2 / data/cards/chaos-charm.json):
--   0. destroy target Wall
--   1. deal 1 damage to target creature
--   2. target creature gains haste until end of turn
-- An answerer that always chooses `idx` and, when asked for a target, aims
-- every slot at `recipient` -- Chaos Charm's modes each have exactly one slot,
-- so a single fixed recipient is unambiguous. Everything else defers to
-- S.identityAnswer.
chooseModeAt :: ModeIndex.ModeIndex -> Recipient.Recipient -> Prompt.Prompt r -> r
chooseModeAt idx recipient p = case p of
  Prompt.ChooseModes {} -> Seq.singleton idx
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton recipient)) sets
  _ -> S.identityAnswer p

-- Rejects a ChooseModes prompt outright -- used to prove a non-modal cast
-- never issues one (CR 700.2a: a forced selection is not asked). `error` here
-- is a deliberately unreachable branch, not library code.
neverAskModes :: Prompt.Prompt r -> r
neverAskModes p = case p of
  Prompt.ChooseModes {} -> error "ChooseModes prompt issued for a non-modal spell"
  _ -> S.identityAnswer p

gateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gateSpec s registry = Spec.describe s "Gate" $ do
  Spec.it s "CR 608.2c mode 1 (damage) deals 1 to the chosen creature" $ do
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
        (pikerOid, gs1) = S.addCreature piker S.bob gs0
        answer :: Prompt.Prompt r -> r
        answer = chooseModeAt (ModeIndex.MkModeIndex 1) (Recipient.ToCreature pikerOid)
        cast = snd (Engine.runGamePure answer gs1 (S.cast S.alice oid))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "1 damage marked" (S.damageOf pikerOid after) (Just 1)

  Spec.it s "CR 608.2c mode 2 (haste) grants haste to the chosen (summoning-sick) creature" $ do
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
        (creatureId, gs1) = S.addCreature piker S.alice gs0
        sick = gs1 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) creatureId (GameState.objects gs1)}
        answer :: Prompt.Prompt r -> r
        answer = chooseModeAt (ModeIndex.MkModeIndex 2) (Recipient.ToCreature creatureId)
        cast = snd (Engine.runGamePure answer sick (S.cast S.alice oid))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste creatureId after) "projected keywords include Haste"

  Spec.it s "CR 608.2c mode 0 (destroy Wall) destroys the chosen Wall" $ do
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    mountain <- S.printingOf s registry "Mountain"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
        (wallId, gs1) = S.addCreature wallOfStone S.bob gs0
        answer :: Prompt.Prompt r -> r
        answer = chooseModeAt (ModeIndex.MkModeIndex 0) (Recipient.ToCreature wallId)
        cast = snd (Engine.runGamePure answer gs1 (S.cast S.alice oid))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertBool s (not (Set.member wallId (GameState.battlefield after))) "no longer on the battlefield"
    Spec.assertEqWith s "in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1

-- CR 700.2a: an illegal mode can't be chosen, so a mode with even one
-- unfillable slot is excluded wholesale -- the falsifier for the M3a
-- all-slots-fillable engine, which would have called Chaos Charm uncastable
-- the moment ANY mode (here, the Wall mode with no Wall in play) had an
-- unfillable slot.
falsifierSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
falsifierSpec s registry = Spec.describe s "Falsifier" $ do
  Spec.it s "CR 700.2c/601.2c castable via the damage/haste modes with no Wall on the board" $ do
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
        (_, gs1) = S.addCreature piker S.bob gs0
    Spec.assertBool s (S.castable S.alice oid gs1) "castable"
    Spec.assertEqWith
      s
      "the Wall mode (0) is absent from the fillable set"
      (Target.fillableModes Nothing Map.empty oid (Card.enchantSlotMap (S.combinedFace chaosCharm)) (Face.spell (S.combinedFace chaosCharm)) gs1)
      (Set.fromList [ModeIndex.MkModeIndex 1, ModeIndex.MkModeIndex 2])

-- CR 601.2c/700.2c: only the CHOSEN mode's slots are ever prompted or stamped
-- on the stack object -- an unchosen mode's slot name never appears at all.
onlyChosenModeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
onlyChosenModeSpec s registry = Spec.describe s "OnlyChosenModeTargets" $ do
  Spec.it s "casting the damage mode binds the 'damaged' slot, never 'wall'" $ do
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
        (pikerOid, gs1) = S.addCreature piker S.bob gs0
        answer :: Prompt.Prompt r -> r
        answer = chooseModeAt (ModeIndex.MkModeIndex 1) (Recipient.ToCreature pikerOid)
        cast = snd (Engine.runGamePure answer gs1 (S.cast S.alice oid))
    case Game.zoneMembers Zone.Stack S.alice cast of
      [] -> Spec.assertFailure s "Chaos Charm never reached the stack"
      stackId : _ -> case Game.lookupObject stackId cast of
        Nothing -> Spec.assertFailure s "the cast stack object vanished"
        Just obj ->
          let keys = Map.keysSet (Object.bindings obj)
           in do
                Spec.assertBool s (Set.member (SlotName.MkSlotName (Text.pack "damaged")) keys) "has the 'damaged' slot"
                Spec.assertBool s (not (Set.member (SlotName.MkSlotName (Text.pack "wall")) keys)) "does not have the 'wall' slot"

fizzleSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
fizzleSpec s registry = Spec.describe s "Fizzle" $ do
  Spec.it s "CR 608.2b the damage mode fizzles when its only target leaves before resolution" $ do
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
        (pikerOid, gs1) = S.addCreature piker S.bob gs0
        answer :: Prompt.Prompt r -> r
        answer = chooseModeAt (ModeIndex.MkModeIndex 1) (Recipient.ToCreature pikerOid)
        cast = snd (Engine.runGamePure answer gs1 (S.cast S.alice oid))
        -- CR 400.7: leaving the battlefield mints a new incarnation, so
        -- pikerOid's chosen recipient no longer names a legal target.
        gone = S.runPure S.identityAnswer cast (Event.changeZone pikerOid Zone.Graveyard)
        after = snd (Engine.runGamePure answer gone Stack.resolveTop)
    Spec.assertEqWith s "Chaos Charm in alice's graveyard, unresolved" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "no damage was dealt" (S.damageEventsOf after) []
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0

forcedSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
forcedSpec s registry = Spec.describe s "ForcedNoPrompt" $ do
  Spec.it s "CR 700.2a casting a non-modal spell (Lightning Bolt) never issues ChooseModes" $ do
    -- No creature on the battlefield, so neverAskModes's identityAnswer
    -- fallback picks ToPlayer alice via Set.lookupMin (a self-Bolt, the same
    -- shape as ResolveSpec's "CR 120.3a a Bolt at a player drains life
    -- without marking"). The point of this test is that ChooseModes is never
    -- reached at all -- if it were, neverAskModes's error would fire.
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs0, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        cast = snd (Engine.runGamePure neverAskModes gs0 (S.cast S.alice oid))
        after = snd (Engine.runGamePure neverAskModes cast Stack.resolveTop)
    Spec.assertEqWith s "alice at 17 (Bolt resolved, forced/unprompted mode selection)" (S.lifeOf S.alice after) (Just 17)

-- M4h task 1: Aether Channeler's "another nonland permanent" slot as data --
-- Pool.Permanents narrowed by Not (HasCardType Land), with CR 601.2c's "another"
-- as the Not IsSource conjunct (#163). This proves the target slot and the exclusion in
-- isolation; the wiring to a consumer is a later M4h task.
nonlandPermanentTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nonlandPermanentTargetSpec s registry = Spec.describe s "M4h NonlandPermanentTarget" $ do
  Spec.it s "NonlandPermanentTarget excludes lands (CR 109.2/110.4)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mindslaver <- S.printingOf s registry "Mindslaver"
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.boardWithCreatureArtifactLand piker mindslaver mountain
        got = Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Permanents (Just (Filter.Type.Not (Filter.Type.HasCardType CardType.Land)))) gs
    Spec.assertEqWith
      s
      "two nonland permanents, no land"
      got
      (Set.fromList [Recipient.ToObject (S.creatureId gs), Recipient.ToObject (S.artifactId gs)])

  Spec.it s "Not IsSource drops the source (CR \"another\")" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mindslaver <- S.printingOf s registry "Mindslaver"
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.boardWithCreatureArtifactLand piker mindslaver mountain
        nonlandOther = Filter.Type.And [Filter.Type.Not (Filter.Type.HasCardType CardType.Land), Filter.Type.Not Filter.Type.IsSource]
        slots = Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSlot.required Pool.Permanents (Just nonlandOther))
        got = Target.legalSets Nothing Map.empty (S.creatureId gs) slots gs
    Spec.assertEqWith
      s
      "source excluded from its own set"
      got
      (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (Set.singleton (Recipient.ToObject (S.artifactId gs))))

-- M4h task 2: the mode-scoped reader folds, lifted off Pawl.Engine.Card onto
-- Pawl.Engine.Modal (a card-free Modal card -> ... shape shared by the spell and,
-- later, both ability types). Card.Type.Card just fixes the ambiguous `card`
-- type parameter -- these two Modes never mention a card value.
modalReaderSpec :: (Monad m) => Spec.Spec m n -> n ()
modalReaderSpec s = Spec.describe s "M4h Modal reader" $ do
  Spec.it s "modesEffects reads only chosen modes, ModeIndex order" $ do
    let m =
          ModalT.MkModal
            ( Seq.fromList
                [ Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)))))) Map.empty,
                  Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)))))) Map.empty
                ]
            )
            (ModeSelection.ChooseExactly 1) ::
            ModalT.Modal Card.Type.Card
        chosen = Seq.singleton (ModeIndex.MkModeIndex 1)
    Spec.assertEqWith s "only mode 1's effect" (Modal.modesEffects chosen m) [Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))]
    Spec.assertEqWith s "an exact instruction's two bounds are its count" (fmap ($ ModalT.selection m) [Modal.leastOf, Modal.mostOf]) [1, 1]

-- M4h task 4: CR 602.2b -- the activation path prompts ChooseModes exactly like
-- Cast.castSpell does, gated by a synthetic modal activated ability (no real one
-- exists in the pool yet: data/cards/synthetic-modal-activated.json, a {2} 2/2
-- whose lone {1} ability is ChooseExactly 1 over two CreatureTarget modes --
-- deal 1 damage, or put a +1/+1 counter). Both modes' target sets are nonempty
-- with just the activator and one victim on the battlefield (CreatureTarget
-- does not self-exclude), so `legal = {0,1}` and `count = 1`: a real prompt,
-- not the forced/single-mode path.
activationModalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
activationModalSpec s registry = Spec.describe s "M4h activation modal (CR 602.2b)" $ do
  Spec.it s "activating a modal ability prompts the mode; only the chosen mode resolves" $ do
    syntheticModalActivated <- S.printingOf s registry "Synthetic Modal Activator"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    case Face.activatedAbilities (S.combinedFace syntheticModalActivated) of
      [ability] ->
        let gs0 = S.landsInPlay mountain 1
            (srcId, gs1) = S.addCreature syntheticModalActivated S.alice gs0
            (victimId, gs2) = S.addCreature piker S.bob gs1
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 0) (Recipient.ToCreature victimId)
            activated = snd (Engine.runGamePure answer gs2 (Activate.activateAbility S.alice srcId ability))
            resolved = snd (Engine.runGamePure answer activated Stack.resolveTop)
         in case Game.lookupObject victimId resolved of
              Nothing -> Spec.assertFailure s "the victim vanished"
              Just obj -> do
                Spec.assertEqWith s "victim took 1 damage (mode 0 only)" (S.damageOf victimId resolved) (Just 1)
                Spec.assertEqWith s "no +1/+1 counter (mode 1 never resolved)" (Map.lookup CounterKind.PlusOnePlusOne (Object.counters obj)) Nothing
      _ -> Spec.assertFailure s "the fixture must have exactly one activated ability"

  Spec.it s "CR 608.2b the chosen mode fizzles when its only target leaves before resolution" $ do
    syntheticModalActivated <- S.printingOf s registry "Synthetic Modal Activator"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    case Face.activatedAbilities (S.combinedFace syntheticModalActivated) of
      [ability] ->
        let gs0 = S.landsInPlay mountain 1
            (srcId, gs1) = S.addCreature syntheticModalActivated S.alice gs0
            (victimId, gs2) = S.addCreature piker S.bob gs1
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 0) (Recipient.ToCreature victimId)
            activated = snd (Engine.runGamePure answer gs2 (Activate.activateAbility S.alice srcId ability))
            -- CR 400.7: leaving the battlefield mints a new incarnation, so
            -- victimId's chosen recipient no longer names a legal target.
            gone = S.runPure S.identityAnswer activated (Event.changeZone victimId Zone.Graveyard)
            resolved = snd (Engine.runGamePure answer gone Stack.resolveTop)
         in do
              Spec.assertEqWith s "no damage was dealt" (S.damageEventsOf resolved) []
              Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
      _ -> Spec.assertFailure s "the fixture must have exactly one activated ability"

-- M4h task 5: CR 700.2b/603.3d -- the TRIGGER-placement path (Engine.placeOne)
-- prompts the mode and, for the chosen mode(s), the targets, exactly like
-- Cast.castSpell does for a spell. Gated by Aether Channeler (a {2}{U} 3/3 Human
-- Wizard whose ETB is ChooseExactly 1 over: create a 1/1 flying Bird, return
-- ANOTHER nonland permanent to hand, or draw a card). Aether Channeler enters
-- via S.addCreature and the SelfEnters trigger is fed to Engine.placePendingTriggers
-- through S.entersWithTrigger's hand-built enters event (the same shape
-- EventSpec uses), then the placed ability resolves off the stack.
triggerModalOf :: Printing.Printing -> Maybe (ModalT.Modal Card.Type.Card)
triggerModalOf acPrinting = case Face.triggeredAbilities (S.combinedFace acPrinting) of
  [ab] -> Just (TriggeredAbility.modal ab)
  _ -> Nothing

triggerModalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
triggerModalSpec s registry = Spec.describe s "M4h trigger modal (CR 700.2b/603.3d)" $ do
  Spec.it s "create mode ({0}) makes a 1/1 flying Bird; nothing bounced or drawn" $ do
    aetherChanneler <- S.printingOf s registry "Aether Channeler"
    let (acId, gs) = S.entersWithTrigger aetherChanneler S.alice (Setup.emptyGame S.bothPlayers)
        answer :: Prompt.Prompt r -> r
        answer = chooseModeAt (ModeIndex.MkModeIndex 0) (Recipient.ToObject acId)
        placed = snd (Engine.runGamePure answer gs Engine.placePendingTriggers)
        resolved = snd (Engine.runGamePure answer placed Stack.resolveTop)
        newObjs = Set.toList (Set.delete acId (GameState.battlefield resolved))
    Spec.assertEqWith s "alice's hand is still empty (nothing drawn)" (length (Game.zoneMembers Zone.Hand S.alice resolved)) 0
    Spec.assertEqWith s "Aether Channeler still on the battlefield (nothing bounced)" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Aether Channeler") S.alice resolved) 1
    case newObjs of
      [tokId] -> do
        -- CR 111.4: Aether Channeler's first mode does not name the token,
        -- so its name is its subtype plus the word "Token".
        Spec.assertEqWith s "the token is named Bird Token" (fmap Face.name (Game.faceOf tokId resolved)) (Just . CardName.MkCardName $ Text.pack "Bird Token")
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying tokId resolved) "the Bird has flying (projected)"
      _ -> Spec.assertFailure s "expected exactly one new (Bird token) permanent"

  Spec.it s "bounce mode ({1}) returns another nonland permanent to its owner's hand (CR 601.2c)" $ do
    aetherChanneler <- S.printingOf s registry "Aether Channeler"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, gs1) = S.entersWithTrigger aetherChanneler S.alice (Setup.emptyGame S.bothPlayers)
        (victimId, gs2) = S.addCreature piker S.bob gs1
        answer :: Prompt.Prompt r -> r
        answer = chooseModeAt (ModeIndex.MkModeIndex 1) (Recipient.ToObject victimId)
        placed = snd (Engine.runGamePure answer gs2 Engine.placePendingTriggers)
        resolved = snd (Engine.runGamePure answer placed Stack.resolveTop)
        boundSlots = case GameState.stack placed of
          abilId : _ -> case Game.lookupObject abilId placed of
            Just obj -> Just (Map.keysSet (Binding.targetsOf (Object.bindings obj)))
            Nothing -> Nothing
          [] -> Nothing
    Spec.assertEqWith s "victim is in bob's hand" (length (Game.zoneMembers Zone.Hand S.bob resolved)) 1
    Spec.assertBool s (not (Set.member victimId (GameState.battlefield resolved))) "victim no longer on the battlefield"
    -- M4.5 P4 Task 3 (CR 113.7): EVERY placed trigger now also carries
    -- its source under the reserved Binding.triggerSource ("self") slot
    -- -- Engine.placeOne stamps it unconditionally, not only for a
    -- Sacrifice-using card, and Binding.targetsOf projects it right
    -- alongside a real chosen target (the same field). So "permanent"
    -- (the real chosen target) is no longer the only bound slot.
    --
    -- M4.5 P4 Task 8 (CR 109.5): Engine.placeOne now ALSO stamps the
    -- ability's controller under the reserved Binding.you ("you") slot,
    -- unconditionally and for the same reason -- so it joins "self" here
    -- too, whether or not this card's own text ever reads it.
    Spec.assertEqWith
      s
      "the 'permanent' slot and the reserved self/you slots are bound"
      boundSlots
      (Just (Set.fromList [SlotName.MkSlotName (Text.pack "permanent"), Binding.triggerSource, Binding.you]))

  Spec.it s "draw mode ({2}) draws exactly one; no token made" $ do
    aetherChanneler <- S.printingOf s registry "Aether Channeler"
    piker <- S.printingOf s registry "Goblin Piker"
    let (acId, gs1) = S.entersWithTrigger aetherChanneler S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs2) = S.addLibraryCard piker S.alice gs1
        answer :: Prompt.Prompt r -> r
        answer = chooseModeAt (ModeIndex.MkModeIndex 2) (Recipient.ToObject acId)
        placed = snd (Engine.runGamePure answer gs2 Engine.placePendingTriggers)
        resolved = snd (Engine.runGamePure answer placed Stack.resolveTop)
    Spec.assertEqWith s "alice drew one card" (length (Game.zoneMembers Zone.Hand S.alice resolved)) 1
    Spec.assertEqWith s "no new permanent (mode 0 never resolved)" (GameState.battlefield resolved) (Set.singleton acId)

  Spec.it s "bounce mode ({1}) excludes Aether Channeler itself (CR \"another\")" $ do
    aetherChanneler <- S.printingOf s registry "Aether Channeler"
    let (acId, gs) = S.entersWithTrigger aetherChanneler S.alice (Setup.emptyGame S.bothPlayers)
    case triggerModalOf aetherChanneler of
      Nothing -> Spec.assertFailure s "Aether Channeler must have exactly one triggered ability"
      Just modal ->
        Spec.assertEqWith
          s
          "with Aether Channeler the only nonland permanent, only modes 0 and 2 are fillable"
          (Target.fillableModes Nothing Map.empty acId Map.empty modal gs)
          (Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2])

  -- CR 603.3c/700.2b: "If no mode is chosen, the ability is removed from
  -- the stack." This is the trigger-only rule -- a SPELL that can't choose
  -- is simply never offered (CR 601.2c elides the cast entirely, M4g), but
  -- a TRIGGER is already placed on the stack before modes are chosen
  -- (CR 603.3d), so an unfillable trigger must be taken back OFF the
  -- stack. Gated by a new synthetic fixture: Synthetic Modal Trigger's ETB
  -- is ChooseExactly 1 over two NonlandPermanentTarget (self-excluding,
  -- CR "another") modes; with the fixture as the ONLY nonland permanent on
  -- the board, both modes' legal target sets are empty, so NEITHER mode is
  -- fillable and the trigger must be removed rather than forced or
  -- prompted (CreatureTarget would be self-fillable here and could never
  -- exercise this path -- spec sec 9).
  Spec.it s "no legal mode removes the trigger from the stack (CR 603.3c)" $ do
    smtPrinting <- S.printingOf s registry "Synthetic Modal Trigger"
    mountain <- S.printingOf s registry "Mountain"
    let gs0 = S.landsInPlay mountain 2
        (smtId, gs1) = S.addCreature smtPrinting S.alice gs0
        entered = ZoneChange.MkZoneChange smtId smtId Zone.Stack Zone.Battlefield
        gs2 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project smtId gs1))] gs1
        answer :: Prompt.Prompt r -> r
        answer = S.identityAnswer
        placed = snd (Engine.runGamePure answer gs2 Engine.placePendingTriggers)
        stackHasTrigger =
          any
            ( \abilId -> case Game.lookupObject abilId placed of
                Just obj -> case Object.source obj of
                  Source.OfTrigger _ _ -> True
                  _ -> False
                Nothing -> False
            )
            (GameState.stack placed)
    Spec.assertBool s (not stackHasTrigger) "the trigger was removed from the stack, not left lingering"
    Spec.assertEqWith s "Synthetic Modal Trigger still on the battlefield (nothing bounced or exiled)" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Synthetic Modal Trigger") S.alice placed) 1
    Spec.assertEqWith s "alice's hand is still empty (nothing bounced)" (length (Game.zoneMembers Zone.Hand S.alice placed)) 0
    Spec.assertEqWith s "nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice placed)) 0

-- Cryptic Command's four modes, in printed order (CR 700.2 /
-- data/cards/cryptic-command.json), under "Choose two --":
--   0. counter target spell                        -- slot "spell"
--   1. return target permanent to its owner's hand -- slot "permanent"
--   2. tap all creatures your opponents control    -- no slot (EachMatching)
--   3. draw a card                                 -- no slot
crypticModes :: Set.Set ModeIndex.ModeIndex
crypticModes = Set.fromList (fmap ModeIndex.MkModeIndex [0, 1, 2, 3])

spellSlot, permanentSlot :: SlotName.SlotName
spellSlot = SlotName.MkSlotName (Text.pack "spell")
permanentSlot = SlotName.MkSlotName (Text.pack "permanent")

-- Chooses the two modes `idxs` and aims each named slot at the recipient `picks`
-- gives it -- chooseModeAt's shape, except that a "choose two" cast can have TWO
-- slots to fill at once (modes 0 and 1 together), which one fixed recipient
-- cannot answer. A slot `picks` does not name falls back to the least member of
-- its own legal set, so the answer stays total.
chooseTwo :: [ModeIndex.ModeIndex] -> [(SlotName.SlotName, Recipient.Recipient)] -> Prompt.Prompt r -> r
chooseTwo idxs picks p = case p of
  Prompt.ChooseModes {} -> Seq.fromList idxs
  Prompt.ChooseTargets _ _ _ sets -> Map.mapWithKey pickFor sets
  _ -> S.identityAnswer p
  where
    pickFor slot (_, legal) = case lookup slot picks of
      Just recipient -> Set.singleton recipient
      Nothing -> maybe Set.empty Set.singleton (Set.lookupMin legal)

-- alice has four Islands and Cryptic Command in hand ({1}{U}{U}{U}); bob has one
-- Goblin Piker on the battlefield.
crypticBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
crypticBoard island crypticCommand piker =
  let (pikerId, gs1) = S.addCreature piker S.bob (S.landsInPlay island 4)
      (gs, spellId) = S.handOne crypticCommand gs1
   in (gs, spellId, pikerId)

-- CR 601.2b/700.2: "Choose two" of four -- the case the forced-mode shortcut in
-- Cast.castProposed must stay out of the way of. Cryptic Command is the pool's
-- first ChooseExactly n with n > 1, and its last two modes take no targets at
-- all, so the fillable count can never fall below two -- and at cast time it is
-- always more than two, so the prompt is always asked.
chooseTwoSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
chooseTwoSpec s registry = Spec.describe s "ChooseTwo (CR 700.2)" $ do
  Spec.it s "CR 700.2 Cryptic Command demands two of four fillable modes" $ do
    island <- S.printingOf s registry "Island"
    crypticCommand <- S.printingOf s registry "Cryptic Command"
    piker <- S.printingOf s registry "Goblin Piker"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let (board, spellId, _) = crypticBoard island crypticCommand piker
        (_, gs) = S.spellOnStack slaughterDrone S.bob board
        modal = Face.spell (S.combinedFace crypticCommand)
    Spec.assertEqWith s "the selection demands two" (fmap ($ ModalT.selection modal) [Modal.leastOf, Modal.mostOf]) [2, 2]
    Spec.assertEqWith
      s
      "with a spell on the stack and permanents in play, all four modes are fillable"
      (Target.fillableModes (Just S.alice) Map.empty spellId (Card.enchantSlotMap (S.combinedFace crypticCommand)) modal gs)
      crypticModes
    -- The forced case's own boundary, and why casting does not reach it: modes 2
    -- and 3 take no targets, so they are fillable even on an empty board --
    -- exactly the two the selection demands. No cast in this pool arrives here,
    -- since every mana source in it is a permanent, and a permanent on the
    -- battlefield is a legal target for mode 1.
    Spec.assertEqWith
      s
      "on an empty board only the two targetless modes are fillable"
      (Target.fillableModes (Just S.alice) Map.empty S.noSource Map.empty modal (Setup.emptyGame S.bothPlayers))
      (Set.fromList (fmap ModeIndex.MkModeIndex [2, 3]))

  -- The prompt itself, asserted rather than assumed: the answer below refuses to
  -- name any mode unless ChooseModes really offers all four and asks for two, and
  -- an empty answer is not a size-2 subset, so Cast.castProposed's
  -- reject-not-repair rewinds the whole cast and every assertion here fails.
  Spec.it s "CR 601.2b the mode prompt offers all four and asks for exactly two" $ do
    island <- S.printingOf s registry "Island"
    crypticCommand <- S.printingOf s registry "Cryptic Command"
    piker <- S.printingOf s registry "Goblin Piker"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let (board, spellId, pikerId) = crypticBoard island crypticCommand piker
        -- A spell on the stack is what makes mode 0 fillable, so the prompt this
        -- answer insists on -- all four modes -- is the one really issued.
        (_, withSpell) = S.spellOnStack slaughterDrone S.bob board
        (_, gs) = S.addLibraryCard piker S.alice withSpell
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseModes _ _ _ legal selection ->
            if legal == crypticModes && selection == ModeSelection.ChooseExactly 2
              then Seq.fromList (fmap ModeIndex.MkModeIndex [2, 3])
              else Seq.empty
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    -- Cryptic Command in the graveyard is what says the cast was NOT rewound:
    -- a rejected proposal leaves it in hand, where the drawn-card count below
    -- would pass for the wrong reason.
    Spec.assertEqWith s "Cryptic Command resolved into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "alice drew a card (mode 3)" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
    Spec.assertEqWith s "bob's Piker is tapped (mode 2)" (fmap Object.tapped (Game.lookupObject pikerId after)) (Just TapState.Tapped)

  Spec.it s "CR 608.2c both chosen modes resolve: the spell is countered and a card is drawn" $ do
    island <- S.printingOf s registry "Island"
    crypticCommand <- S.printingOf s registry "Cryptic Command"
    piker <- S.printingOf s registry "Goblin Piker"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let (board, spellId, pikerId) = crypticBoard island crypticCommand piker
        (droneId, withSpell) = S.spellOnStack slaughterDrone S.bob board
        (_, gs) = S.addLibraryCard piker S.alice withSpell
        answer :: Prompt.Prompt r -> r
        answer = chooseTwo (fmap ModeIndex.MkModeIndex [0, 3]) [(spellSlot, Recipient.ToObject droneId)]
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertBool s (notElem droneId (GameState.stack after)) "the Drone spell is off the stack"
    Spec.assertEqWith s "countered into bob's graveyard, never onto the battlefield" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "alice drew a card (mode 3)" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
    -- The two UNCHOSEN modes did nothing: no bounce, and nothing tapped.
    Spec.assertBool s (Set.member pikerId (GameState.battlefield after)) "bob's Piker was not bounced (mode 1 unchosen)"
    Spec.assertEqWith s "bob's Piker is untapped (mode 2 unchosen)" (fmap Object.tapped (Game.lookupObject pikerId after)) (Just TapState.Untapped)

  -- Two chosen modes whose effects touch the same board: the bounce (mode 1)
  -- runs first (CR 608.2c), so "tap all creatures your opponents control" sweeps
  -- what is left. Not a test OF that order -- tapping the Piker and then bouncing
  -- it would leave the same board, since CR 400.7 mints a new object in hand
  -- either way -- but of the sweep itself, which is what "all creatures your
  -- OPPONENTS control" makes discriminating: alice's own creature stays untapped.
  Spec.it s "CR 608.2c bounce then tap: only the opponent's remaining creatures are tapped" $ do
    island <- S.printingOf s registry "Island"
    crypticCommand <- S.printingOf s registry "Cryptic Command"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (board, spellId, pikerId) = crypticBoard island crypticCommand piker
        (wallId, withWall) = S.addCreature wallOfStone S.bob board
        (mineId, gs) = S.addCreature piker S.alice withWall
        answer :: Prompt.Prompt r -> r
        answer = chooseTwo (fmap ModeIndex.MkModeIndex [1, 2]) [(permanentSlot, Recipient.ToObject pikerId)]
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertBool s (not (Set.member pikerId (GameState.battlefield after))) "the targeted Piker left the battlefield"
    Spec.assertEqWith s "and is in bob's hand" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
    Spec.assertEqWith s "bob's Wall of Stone is tapped" (fmap Object.tapped (Game.lookupObject wallId after)) (Just TapState.Tapped)
    -- "your opponents control", not "all creatures": alice's own Piker is untouched.
    Spec.assertEqWith s "alice's own Piker is untapped" (fmap Object.tapped (Game.lookupObject mineId after)) (Just TapState.Untapped)

  -- The Gatherer ruling, verbatim: "if you choose the second and fourth modes,
  -- and the permanent is an illegal target when Cryptic Command tries to resolve,
  -- you won't draw a card." CR 608.2b counters the whole SPELL when all its
  -- targets are illegal, so the targetless mode falls with the targeted one.
  Spec.it s "CR 608.2b bounce + draw fizzles entirely when the bounce target is gone" $ do
    island <- S.printingOf s registry "Island"
    crypticCommand <- S.printingOf s registry "Cryptic Command"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, spellId, pikerId) = crypticBoard island crypticCommand piker
        (_, gs) = S.addLibraryCard piker S.alice board
        answer :: Prompt.Prompt r -> r
        answer = chooseTwo (fmap ModeIndex.MkModeIndex [1, 3]) [(permanentSlot, Recipient.ToObject pikerId)]
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        -- CR 400.7: leaving the battlefield mints a new incarnation, so pikerId's
        -- chosen recipient no longer names a legal target.
        gone = S.runPure S.identityAnswer cast (Event.changeZone pikerId Zone.Graveyard)
        after = snd (Engine.runGamePure answer gone Stack.resolveTop)
    Spec.assertEqWith s "alice's hand is empty: no card was drawn" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
    Spec.assertEqWith s "Cryptic Command is in alice's graveyard, unresolved" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0

  -- The other half of CR 608.2b, and the falsifier for an engine that fizzled per
  -- MODE rather than per spell: one of the two chosen modes' targets is gone and
  -- the other is not, so the spell resolves and only the illegal mode is skipped.
  Spec.it s "CR 608.2b counter + bounce still counters when only the bounce target is gone" $ do
    island <- S.printingOf s registry "Island"
    crypticCommand <- S.printingOf s registry "Cryptic Command"
    piker <- S.printingOf s registry "Goblin Piker"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let (board, spellId, pikerId) = crypticBoard island crypticCommand piker
        (droneId, gs) = S.spellOnStack slaughterDrone S.bob board
        answer :: Prompt.Prompt r -> r
        answer =
          chooseTwo
            (fmap ModeIndex.MkModeIndex [0, 1])
            [(spellSlot, Recipient.ToObject droneId), (permanentSlot, Recipient.ToObject pikerId)]
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        gone = S.runPure S.identityAnswer cast (Event.changeZone pikerId Zone.Graveyard)
        after = snd (Engine.runGamePure answer gone Stack.resolveTop)
    Spec.assertBool s (notElem droneId (GameState.stack after)) "the Drone spell was still countered"
    Spec.assertEqWith s "bob's graveyard holds the countered Drone and the Piker that left" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertEqWith s "nothing reached bob's hand: the bounce mode was skipped" (length (Game.zoneMembers Zone.Hand S.bob after)) 0

-- Ojutai's Command's four modes, in printed order (CR 700.2 /
-- data/cards/ojutais-command.json), under "Choose two --":
--   0. return target creature card with mana value 2 or less
--      from your graveyard to the battlefield -- slot "creature"
--   1. you gain 4 life                        -- no slot
--   2. counter target creature spell          -- slot "spell"
--   3. draw a card                            -- no slot
ojutaiModes :: Set.Set ModeIndex.ModeIndex
ojutaiModes = Set.fromList (fmap ModeIndex.MkModeIndex [0, 1, 2, 3])

creatureSlot :: SlotName.SlotName
creatureSlot = SlotName.MkSlotName (Text.pack "creature")

-- alice has three Islands and a Plains ({2}{W}{U}) and Ojutai's Command in hand.
-- Her graveyard and the stack are EMPTY, which is what makes modes 0 and 2
-- unfillable: nothing to reanimate, no creature spell to counter.
ojutaiBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId)
ojutaiBoard island plains ojutaisCommand =
  let (_, gs1) = S.addCreature plains S.alice (S.landsInPlay island 3)
   in S.handOne ojutaisCommand gs1

-- CR 601.2b/700.2: the OTHER side of Cryptic Command's group above -- a "choose
-- two" whose fillable modes really can come down to exactly two, so the forced
-- (unprompted) branch of Cast.castProposed is reachable at cast time and the
-- claim that nothing is hidden from the player can be checked rather than argued.
-- Ojutai's Command's two targeting modes both look somewhere a cast can leave
-- empty: your graveyard, and the stack.
forcedTwoSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
forcedTwoSpec s registry = Spec.describe s "ForcedTwo (CR 700.2a)" $ do
  -- CR 700.2a: "If one of the modes would be illegal … that mode can't be
  -- chosen." Two demanded, two choosable, so there is nothing to ask -- and
  -- neverAskModes turns the prompt into a test failure rather than a comment.
  Spec.it s "CR 700.2a with exactly two fillable modes no mode prompt is issued" $ do
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    ojutaisCommand <- S.printingOf s registry "Ojutai's Command"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, spellId) = ojutaiBoard island plains ojutaisCommand
        (_, gs) = S.addLibraryCard piker S.alice board
        cast = snd (Engine.runGamePure neverAskModes gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure neverAskModes cast Stack.resolveTop)
    Spec.assertEqWith
      s
      "only the two targetless modes are fillable"
      (Target.fillableModes (Just S.alice) Map.empty spellId Map.empty (Face.spell (S.combinedFace ojutaisCommand)) gs)
      (Set.fromList (fmap ModeIndex.MkModeIndex [1, 3]))
    Spec.assertEqWith s "alice gained 4 life (mode 1)" (S.lifeOf S.alice after) (Just 24)
    Spec.assertEqWith s "alice drew a card (mode 3)" (length (Game.zoneMembers Zone.Hand S.alice after)) 1

  -- The falsifier for that pair, and for CR 202.3's bound: the SAME board with a
  -- creature card in the graveyard. Wall of Stone ({1}{R}{R}, mana value 3) is
  -- above "2 or less" and leaves the two modes forced; Goblin Piker ({1}{R}, mana
  -- value 2) is within it and makes a third mode fillable, so the choice is real
  -- and the prompt must be issued.
  Spec.it s "CR 202.3 a mana value 3 creature card in the graveyard is still not choosable" $ do
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    ojutaisCommand <- S.printingOf s registry "Ojutai's Command"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (board, spellId) = ojutaiBoard island plains ojutaisCommand
        (_, gs) = S.addGraveyardCard wallOfStone S.alice board
    Spec.assertEqWith
      s
      "the reanimation mode stays unfillable"
      (Target.fillableModes (Just S.alice) Map.empty spellId Map.empty (Face.spell (S.combinedFace ojutaisCommand)) gs)
      (Set.fromList (fmap ModeIndex.MkModeIndex [1, 3]))

  Spec.it s "CR 202.3 a mana value 2 creature card in the graveyard makes a third mode choosable" $ do
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    ojutaisCommand <- S.printingOf s registry "Ojutai's Command"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, spellId) = ojutaiBoard island plains ojutaisCommand
        (_, gs) = S.addGraveyardCard piker S.alice board
    Spec.assertEqWith
      s
      "the reanimation mode joins the two targetless ones"
      (Target.fillableModes (Just S.alice) Map.empty spellId Map.empty (Face.spell (S.combinedFace ojutaisCommand)) gs)
      (Set.fromList (fmap ModeIndex.MkModeIndex [0, 1, 3]))

  -- CR 115.2's other-zone pool doing real work: the chosen mode reads a card in
  -- alice's graveyard and puts it onto the battlefield (CR 400.7 mints the new
  -- object there).
  Spec.it s "CR 601.2b reanimating and gaining life: both chosen modes resolve" $ do
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    ojutaisCommand <- S.printingOf s registry "Ojutai's Command"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, spellId) = ojutaiBoard island plains ojutaisCommand
        (pikerId, gs) = S.addGraveyardCard piker S.alice board
        answer :: Prompt.Prompt r -> r
        answer = chooseTwo (fmap ModeIndex.MkModeIndex [0, 1]) [(creatureSlot, Recipient.ToObject pikerId)]
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "a Goblin Piker is on the battlefield under alice" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice after) 1
    Spec.assertEqWith s "alice gained 4 life (mode 1)" (S.lifeOf S.alice after) (Just 24)
    Spec.assertEqWith s "alice's hand is empty: mode 3 was not chosen" (length (Game.zoneMembers Zone.Hand S.alice after)) 0

  -- Mode 2's own filter: "target CREATURE spell", so a noncreature spell on the
  -- stack leaves the two modes forced -- the counter mode is not choosable just
  -- because something is on the stack.
  Spec.it s "CR 700.2a a noncreature spell on the stack does not make the counter mode choosable" $ do
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    ojutaisCommand <- S.printingOf s registry "Ojutai's Command"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, spellId) = ojutaiBoard island plains ojutaisCommand
        (_, withBolt) = S.spellOnStack lightningBolt S.bob board
        (_, gs) = S.spellOnStack piker S.bob board
        modal = Face.spell (S.combinedFace ojutaisCommand)
    Spec.assertEqWith
      s
      "an instant on the stack leaves only the targetless modes"
      (Target.fillableModes (Just S.alice) Map.empty spellId Map.empty modal withBolt)
      (Set.fromList (fmap ModeIndex.MkModeIndex [1, 3]))
    Spec.assertEqWith
      s
      "a creature spell on the stack makes the counter mode choosable"
      (Target.fillableModes (Just S.alice) Map.empty spellId Map.empty modal gs)
      (Set.fromList (fmap ModeIndex.MkModeIndex [1, 2, 3]))

  -- All four at once, so the prompt has the widest choice this card can offer.
  Spec.it s "CR 601.2b all four modes fillable: the prompt is issued and the chosen two resolve" $ do
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    ojutaisCommand <- S.printingOf s registry "Ojutai's Command"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, spellId) = ojutaiBoard island plains ojutaisCommand
        (deadPiker, withGrave) = S.addGraveyardCard piker S.alice board
        (castPiker, gs) = S.spellOnStack piker S.bob withGrave
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseModes _ _ _ legal selection ->
            if legal == ojutaiModes && selection == ModeSelection.ChooseExactly 2
              then Seq.fromList (fmap ModeIndex.MkModeIndex [0, 2])
              else Seq.empty
          Prompt.ChooseTargets _ _ _ sets ->
            Map.mapWithKey
              (\slot _ -> Set.singleton (if slot == creatureSlot then Recipient.ToObject deadPiker else Recipient.ToObject castPiker))
              sets
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertBool s (notElem castPiker (GameState.stack after)) "bob's creature spell was countered"
    Spec.assertEqWith s "the graveyard Piker is on the battlefield under alice" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice after) 1
    Spec.assertEqWith s "alice is still at 20: mode 1 was not chosen" (S.lifeOf S.alice after) (Just 20)

-- Mystic Confluence's three modes, in printed order (CR 700.2 /
-- data/cards/mystic-confluence.json), under "Choose three. You may choose the
-- same mode more than once.":
--   0. counter target spell unless its controller pays {3} -- slot "spell"
--   1. return target creature to its owner's hand          -- slot "creature"
--   2. draw a card                                         -- no slot
confluenceModes :: Set.Set ModeIndex.ModeIndex
confluenceModes = Set.fromList (fmap ModeIndex.MkModeIndex [0, 1, 2])

drawMode, bounceMode :: ModeIndex.ModeIndex
drawMode = ModeIndex.MkModeIndex 2
bounceMode = ModeIndex.MkModeIndex 1

-- alice has five Islands ({3}{U}{U}) and Mystic Confluence in hand, over a
-- library of `libraryCards` Goblin Pikers. CR 104.3c is live, so the library is
-- deep enough that drawing three from one resolution -- plus anything else the
-- fixture draws -- cannot deck her; a shallower one would fail these tests by
-- ending the game rather than by counting wrong.
confluenceBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (GameState.GameState, ObjectId.ObjectId)
confluenceBoard island mysticConfluence libraryCard libraryCards =
  let stocked = foldr (\_ gs -> snd (S.addLibraryCard libraryCard S.alice gs)) (S.landsInPlay island 5) [1 .. libraryCards]
   in S.handOne mysticConfluence stocked

-- CR 700.2d's exception, "some modal spells include the instruction 'You may
-- choose the same mode more than once' ... If a particular mode is chosen
-- multiple times, the spell is treated as if that mode appeared that many times
-- in sequence." Mystic Confluence is the pool's one printing of it.
repeatedModeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
repeatedModeSpec s registry = Spec.describe s "RepeatedModes (CR 700.2d)" $ do
  -- The discriminating case: ONE mode, three times, and the count is what
  -- separates a real repetition from a selection that merely tolerated the
  -- answer. The hand is empty when the spell leaves it (handOne puts exactly one
  -- card there and it is the one being cast), so three cards is a delta of three
  -- from zero and cannot coincide with a starting hand.
  Spec.it s "CR 700.2d choosing 'draw a card' three times draws three cards" $ do
    island <- S.printingOf s registry "Island"
    mysticConfluence <- S.printingOf s registry "Mystic Confluence"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId) = confluenceBoard island mysticConfluence piker 10
        -- The answer insists on the prompt really offering all three modes under
        -- the repeating instruction: anything else answers with an empty
        -- selection, which Cast.castProposed rejects and rewinds, and then every
        -- assertion below fails.
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseModes _ _ _ legal selection ->
            if legal == confluenceModes && selection == ModeSelection.ChooseExactlyWithRepeats 3
              then Seq.replicate 3 drawMode
              else Seq.empty
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "Mystic Confluence resolved into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "alice drew three cards, not one" (S.handSize S.alice after) 3
    Spec.assertEqWith s "and they came off the library" (length (Game.zoneMembers Zone.Library S.alice after)) 7

  -- The mixed case: two DIFFERENT modes, one of them repeated, so both the count
  -- and the identity of what happened are exercised at once. The two bounces get
  -- two independent target slots (CR 700.2d's "different targets may be chosen"),
  -- and they are aimed at two different creatures -- an engine that collapsed the
  -- repeated mode's slots into one would bounce one creature and leave the other.
  Spec.it s "CR 700.2d bounce twice plus draw: two different creatures are returned" $ do
    island <- S.printingOf s registry "Island"
    mysticConfluence <- S.printingOf s registry "Mystic Confluence"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (board, spellId) = confluenceBoard island mysticConfluence piker 10
        (pikerId, withPiker) = S.addCreature piker S.bob board
        (wallId, gs) = S.addCreature wallOfStone S.bob withPiker
        -- Mode 1's printed slot is "creature"; its second instance fills
        -- Modal.instanceSlot's derived name. The answer aims the two at two
        -- different creatures by name, and refuses to answer a set that does not
        -- offer exactly those two slots.
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseModes {} -> Seq.fromList [bounceMode, bounceMode, drawMode]
          Prompt.ChooseTargets _ _ _ sets ->
            Map.mapWithKey
              (\slot _ -> Set.singleton (if slot == creatureSlot then Recipient.ToCreature pikerId else Recipient.ToCreature wallId))
              sets
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertBool s (not (Set.member pikerId (GameState.battlefield after))) "bob's Piker left the battlefield"
    Spec.assertBool s (not (Set.member wallId (GameState.battlefield after))) "bob's Wall of Stone left the battlefield too"
    Spec.assertEqWith s "both are in bob's hand" (S.handSize S.bob after) 2
    Spec.assertEqWith s "alice drew exactly one card (the draw mode was chosen once)" (S.handSize S.alice after) 1

  -- The two slots really are two, asserted on the prompt rather than inferred
  -- from the board: a card whose repeated mode's slots collapsed would offer one.
  Spec.it s "CR 601.2c a twice-chosen targeting mode is prompted for two slots" $ do
    island <- S.printingOf s registry "Island"
    mysticConfluence <- S.printingOf s registry "Mystic Confluence"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (board, spellId) = confluenceBoard island mysticConfluence piker 10
        (pikerId, withPiker) = S.addCreature piker S.bob board
        (_, gs) = S.addCreature wallOfStone S.bob withPiker
        modal = Face.spell (S.combinedFace mysticConfluence)
        twiceBounced = Seq.fromList [bounceMode, bounceMode, drawMode]
    Spec.assertEqWith
      s
      "choosing the bounce mode twice declares two target slots"
      (Map.size (Modal.modesTargetSlots twiceBounced modal))
      2
    Spec.assertEqWith
      s
      "choosing it once declares one"
      (Map.size (Modal.modesTargetSlots (Seq.fromList [bounceMode, drawMode, drawMode]) modal))
      1
    let answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseModes {} -> twiceBounced
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature pikerId))) sets
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
    -- CR 700.2d: "the same player or object may be chosen as the target for each
    -- of those modes" -- so aiming both instances at ONE creature is legal, and
    -- the cast goes through rather than being rewound.
    Spec.assertEqWith s "aiming both bounces at one creature is a legal cast" (length (GameState.stack cast)) 1

  -- The negative leg, and the one that proves the flag GATES the behaviour rather
  -- than the engine simply always allowing repeats: Cryptic Command prints the
  -- ordinary instruction, so CR 700.2d's default applies and a repeated choice is
  -- not a legal answer. Cast.castProposed's reject-not-repair rewinds the whole
  -- cast, leaving the card in hand.
  Spec.it s "CR 700.2d a modal card without the permission rejects a repeated mode" $ do
    island <- S.printingOf s registry "Island"
    crypticCommand <- S.printingOf s registry "Cryptic Command"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, spellId, _) = crypticBoard island crypticCommand piker
        (_, gs) = S.addLibraryCard piker S.alice board
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          -- Mode 3 is "draw a card", twice -- a size-2 answer to a "choose two",
          -- so only the repetition can make it illegal.
          Prompt.ChooseModes {} -> Seq.replicate 2 (ModeIndex.MkModeIndex 3)
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "the cast was rewound: Cryptic Command is still in alice's hand" (S.handSize S.alice after) 1
    Spec.assertBool s (null (GameState.stack after)) "nothing reached the stack"
    Spec.assertEqWith s "no card was drawn: alice's library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 1
    Spec.assertEqWith s "and her graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

-- Vandalize's two modes, in printed order (CR 700.2 / data/cards/vandalize.json),
-- under "Choose one or both --":
--   0. destroy target artifact -- slot "artifact"
--   1. destroy target land     -- slot "land"
vandalizeModes :: Set.Set ModeIndex.ModeIndex
vandalizeModes = Set.fromList (fmap ModeIndex.MkModeIndex [0, 1])

artifactSlot, landSlot :: SlotName.SlotName
artifactSlot = SlotName.MkSlotName (Text.pack "artifact")
landSlot = SlotName.MkSlotName (Text.pack "land")

destroyArtifact, destroyLand :: ModeIndex.ModeIndex
destroyArtifact = ModeIndex.MkModeIndex 0
destroyLand = ModeIndex.MkModeIndex 1

-- alice has five Mountains ({4}{R}) and Vandalize in hand; bob has a Forest, and
-- each caller adds the Bonesplitter or not, since an artifact on the battlefield is
-- what makes the artifact mode choosable. Both of bob's permanents are legal targets
-- and neither is alice's, so each mode's effect is visible as bob losing exactly that
-- permanent -- and the names are distinct from alice's Mountains, which the land mode
-- could otherwise be answered with.
vandalizeBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
vandalizeBoard mountain vandalize forest =
  let (forestId, gs1) = S.addCreature forest S.bob (S.landsInPlay mountain 5)
      (gs, spellId) = S.handOne vandalize gs1
   in (gs, spellId, forestId)

bonesplitterCount, forestCount :: GameState.GameState -> Int
bonesplitterCount = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Bonesplitter")) S.bob
forestCount = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Forest")) S.bob

-- Answers the mode prompt with `idxs`, but ONLY if it really offers both modes
-- under the printed range -- anything else answers with the empty selection, which
-- is below the range's floor, so Cast.castProposed rewinds the whole cast and every
-- assertion fails. Each slot is aimed at the permanent that slot names, by name
-- rather than by searching the legal set, so a mutation cannot be repaired here. A
-- slot named neither of the card's two gets the empty answer, which fails the target
-- announcement rather than aiming somewhere plausible.
insistOneOrBoth :: [ModeIndex.ModeIndex] -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
insistOneOrBoth idxs boneId forestId p = case p of
  Prompt.ChooseModes _ _ _ legal selection ->
    if legal == vandalizeModes && selection == ModeSelection.ChooseBetween (ChooseBetween.MkChooseBetween 1 2)
      then Seq.fromList idxs
      else Seq.empty
  Prompt.ChooseTargets _ _ _ sets -> Map.mapWithKey aimAt sets
  _ -> S.identityAnswer p
  where
    aimAt slot _
      | slot == artifactSlot = Set.singleton (Recipient.ToObject boneId)
      | slot == landSlot = Set.singleton (Recipient.ToObject forestId)
      | otherwise = Set.empty

-- CR 700.2's range instruction: "Choose one or both --" (Vandalize), the first
-- selection in the pool whose size is not fixed by the card. One mode, the other,
-- or both are three answers, so the prompt is real; fewer than one and a repeat are
-- not, so the announcement is rejected.
chooseOneOrBothSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
chooseOneOrBothSpec s registry = Spec.describe s "ChooseOneOrBoth (CR 700.2)" $ do
  Spec.it s "CR 700.2 the instruction states a range, not a count" $ do
    vandalize <- S.printingOf s registry "Vandalize"
    let modal = Face.spell (S.combinedFace vandalize)
    Spec.assertEqWith s "one through both" (ModalT.selection modal) (ModeSelection.ChooseBetween (ChooseBetween.MkChooseBetween 1 2))
    Spec.assertEqWith s "the two bounds differ" (fmap ($ ModalT.selection modal) [Modal.leastOf, Modal.mostOf]) [1, 2]

  -- The ceiling, read directly off Modal.selectionSatisfiedBy because no card in the
  -- pool can observe it: this range ends at the printed mode count, so any answer
  -- above it repeats a mode and CR 700.2d's default rejects it for that instead. A
  -- narrower printed range -- "choose one or two" of three modes -- would need this
  -- conjunct, and until one exists it is a fence rather than a proven behaviour.
  Spec.it s "CR 700.2 an answer above the range's ceiling does not satisfy it" $ do
    let both = Seq.fromList [destroyArtifact, destroyLand]
    Spec.assertBool s (Modal.selectionSatisfiedBy vandalizeModes (ModeSelection.ChooseBetween (ChooseBetween.MkChooseBetween 1 2)) both) "both modes satisfy one-or-both"
    Spec.assertBool s (not (Modal.selectionSatisfiedBy vandalizeModes (ModeSelection.ChooseBetween (ChooseBetween.MkChooseBetween 0 1)) both)) "two modes do not satisfy a zero-to-one range"

  -- The choice is really offered and really taken: the answer above refuses to name
  -- a mode unless the prompt carries both modes and the range, and this one names
  -- only the artifact mode. Bob's Forest surviving is what says the unchosen mode
  -- did nothing -- an engine that took every legal mode (the exact instruction's
  -- forced arm) would destroy it too.
  Spec.it s "CR 601.2b choosing one of the two destroys only that mode's target" $ do
    mountain <- S.printingOf s registry "Mountain"
    vandalize <- S.printingOf s registry "Vandalize"
    forest <- S.printingOf s registry "Forest"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let (board, spellId, forestId) = vandalizeBoard mountain vandalize forest
        (boneId, gs) = S.addCreature bonesplitter S.bob board
        answer :: Prompt.Prompt r -> r
        answer = insistOneOrBoth [destroyArtifact] boneId forestId
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "Vandalize resolved into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "bob's Bonesplitter was destroyed" (bonesplitterCount after) 0
    Spec.assertEqWith s "bob's Forest survived: the land mode was not chosen" (forestCount after) 1

  -- The same board and the same mana, differing only in the answer: both modes, which
  -- is the other size the instruction allows. Two separate slots are filled, and the
  -- two destructions are separately visible.
  Spec.it s "CR 700.2 choosing both destroys both targets" $ do
    mountain <- S.printingOf s registry "Mountain"
    vandalize <- S.printingOf s registry "Vandalize"
    forest <- S.printingOf s registry "Forest"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let (board, spellId, forestId) = vandalizeBoard mountain vandalize forest
        (boneId, gs) = S.addCreature bonesplitter S.bob board
        answer :: Prompt.Prompt r -> r
        answer = insistOneOrBoth [destroyArtifact, destroyLand] boneId forestId
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "Vandalize resolved into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "bob's Bonesplitter was destroyed" (bonesplitterCount after) 0
    Spec.assertEqWith s "bob's Forest was destroyed too" (forestCount after) 0

  -- The floor, on that same board: "one or both" is not "up to both", so naming no
  -- mode is not an answer and Cast.castProposed's reject-not-repair rewinds.
  Spec.it s "CR 700.2 naming no mode does not satisfy 'one or both'" $ do
    mountain <- S.printingOf s registry "Mountain"
    vandalize <- S.printingOf s registry "Vandalize"
    forest <- S.printingOf s registry "Forest"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let (board, spellId, _) = vandalizeBoard mountain vandalize forest
        (_, gs) = S.addCreature bonesplitter S.bob board
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseModes {} -> Seq.empty
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "the cast was rewound: Vandalize is still in alice's hand" (S.handSize S.alice after) 1
    Spec.assertBool s (null (GameState.stack after)) "nothing reached the stack"
    Spec.assertEqWith s "bob's Bonesplitter is untouched" (bonesplitterCount after) 1
    Spec.assertEqWith s "and so is his Forest" (forestCount after) 1

  -- The ceiling is a bound on the SIZE and not a licence to repeat: CR 700.2d's
  -- default still applies, so "the artifact mode, twice" is rejected even though two
  -- is a size the range allows.
  Spec.it s "CR 700.2d a range does not permit the same mode twice" $ do
    mountain <- S.printingOf s registry "Mountain"
    vandalize <- S.printingOf s registry "Vandalize"
    forest <- S.printingOf s registry "Forest"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let (board, spellId, _) = vandalizeBoard mountain vandalize forest
        (_, gs) = S.addCreature bonesplitter S.bob board
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseModes {} -> Seq.replicate 2 destroyArtifact
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "the cast was rewound: Vandalize is still in alice's hand" (S.handSize S.alice after) 1
    Spec.assertEqWith s "bob's Bonesplitter is untouched" (bonesplitterCount after) 1

  -- The floor at the CAST GATE (CR 601.2c/700.2a): a range is castable when its
  -- MINIMUM can be met, where an exact "choose two" would need both. Two boards
  -- differing in one thing -- alice pays {4}{R} off five Birds of Paradise either
  -- way, so there is no land and no artifact to target until bob's Forest arrives,
  -- and the mana is identical on both sides.
  Spec.it s "CR 700.2a a range is castable once its minimum can be met" $ do
    birdsOfParadise <- S.printingOf s registry "Birds of Paradise"
    vandalize <- S.printingOf s registry "Vandalize"
    forest <- S.printingOf s registry "Forest"
    let birds = List.foldl' (\gs _ -> snd (S.addCreature birdsOfParadise S.alice gs)) (Setup.emptyGame S.bothPlayers) [1 .. 5 :: Int]
        (noModes, spellId) = S.handOne vandalize birds
        (_, oneMode) = S.addCreature forest S.bob noModes
    Spec.assertBool s (not (S.castable S.alice spellId noModes)) "no artifact and no land: no mode is choosable, so the spell is uncastable"
    Spec.assertBool s (S.castable S.alice spellId oneMode) "a land on the battlefield makes one mode choosable, which 'one or both' accepts"

  -- CR 700.2a on the range: with no artifact anywhere the artifact mode can't be
  -- chosen, which leaves the land mode as the ONE selection satisfying the
  -- instruction -- so the engine must not ask, and must not treat the spell as
  -- uncastable either (an exact "choose two" would be). The answerer fails the test
  -- if a prompt is issued.
  Spec.it s "CR 700.2a one choosable mode leaves nothing to ask" $ do
    mountain <- S.printingOf s registry "Mountain"
    vandalize <- S.printingOf s registry "Vandalize"
    forest <- S.printingOf s registry "Forest"
    let (gs, spellId, forestId) = vandalizeBoard mountain vandalize forest
        modal = Face.spell (S.combinedFace vandalize)
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseModes {} -> error "ChooseModes prompt issued for a forced selection"
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject forestId))) sets
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith
      s
      "with no artifact in play only the land mode is choosable"
      (Target.fillableModes (Just S.alice) Map.empty spellId Map.empty modal gs)
      (Set.singleton destroyLand)
    Spec.assertEqWith s "Vandalize resolved into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "bob's Forest was destroyed" (forestCount after) 0

-- Modal.combinations' two halves, side by side: the same inputs under CR 700.2d's
-- default and under its exception. The enumeration Pawl.Engine.Mana reads when a
-- mana ability has several modes, which no card in the pool prints -- so it is
-- checked directly.
combinationsSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
combinationsSpec s = Spec.describe s "Combinations (CR 700.2d)" $ do
  Spec.it s "without repeats, size-2 sublists of three options" $
    Spec.assertEq s (Modal.combinations 2 "abc") ["ab", "ac", "bc"]

  Spec.it s "with repeats, the multisubsets, printed order kept" $
    Spec.assertEq s (Modal.combinationsWithRepeats 2 "abc") ["aa", "ab", "ac", "bb", "bc", "cc"]

  -- The boundary that matters: fewer options than the count. The default has no
  -- selection at all; the exception has exactly one.
  Spec.it s "one option satisfies any count only under the exception" $ do
    Spec.assertEqWith s "default: none" (Modal.combinations 3 "a") []
    Spec.assertEqWith s "exception: one" (Modal.combinationsWithRepeats 3 "a") ["aaa"]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Modal" $ do
  gateSpec s registry
  falsifierSpec s registry
  onlyChosenModeSpec s registry
  fizzleSpec s registry
  forcedSpec s registry
  nonlandPermanentTargetSpec s registry
  modalReaderSpec s
  activationModalSpec s registry
  triggerModalSpec s registry
  chooseTwoSpec s registry
  forcedTwoSpec s registry
  repeatedModeSpec s registry
  chooseOneOrBothSpec s registry
  combinationsSpec s
