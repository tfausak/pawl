{-# LANGUAGE GADTs #-}

-- Covers modal casting: Pawl.Engine.Cast mode selection, Pawl.Engine.Resolve chosen-mode
-- resolution.
module Pawl.ModalSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
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
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as ModalT
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
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
import qualified Pawl.Types.TargetSpec as TargetSpec
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
  Prompt.ChooseModes {} -> Set.singleton idx
  Prompt.ChooseTargets _ _ _ sets -> fmap (const recipient) sets
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
        cast = snd (Engine.runGamePure answer gs1 (Cast.castSpell S.alice oid))
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
        cast = snd (Engine.runGamePure answer sick (Cast.castSpell S.alice oid))
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
        cast = snd (Engine.runGamePure answer gs1 (Cast.castSpell S.alice oid))
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
    Spec.assertBool s (Cast.castable S.alice oid gs1) "castable"
    Spec.assertEqWith
      s
      "the Wall mode (0) is absent from the fillable set"
      (Target.fillableModes Nothing oid (Card.enchantSpecs (Printing.card chaosCharm)) (Card.Type.spell (Printing.card chaosCharm)) gs1)
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
        cast = snd (Engine.runGamePure answer gs1 (Cast.castSpell S.alice oid))
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
        cast = snd (Engine.runGamePure answer gs1 (Cast.castSpell S.alice oid))
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
        cast = snd (Engine.runGamePure neverAskModes gs0 (Cast.castSpell S.alice oid))
        after = snd (Engine.runGamePure neverAskModes cast Stack.resolveTop)
    Spec.assertEqWith s "alice at 17 (Bolt resolved, forced/unprompted mode selection)" (S.lifeOf S.alice after) (Just 17)

-- M4h task 1: Aether Channeler's "another nonland permanent" slot as data --
-- Pool.Permanents narrowed by Not (HasCardType Land), with CR 601.2c's "another"
-- as the Not IsSource conjunct (#163). This proves the spec and the exclusion in
-- isolation; the wiring to a consumer is a later M4h task.
nonlandPermanentTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nonlandPermanentTargetSpec s registry = Spec.describe s "M4h NonlandPermanentTarget" $ do
  Spec.it s "NonlandPermanentTarget excludes lands (CR 109.2/110.4)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mindslaver <- S.printingOf s registry "Mindslaver"
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.boardWithCreatureArtifactLand piker mindslaver mountain
        got = Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.Not (Filter.Type.HasCardType CardType.Land)))) gs
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
        specs = Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSpec.MkTargetSpec Pool.Permanents (Just nonlandOther))
        got = Target.legalSets Nothing (S.creatureId gs) specs gs
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
                [ Mode.MkMode (Seq.singleton (Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))) Map.empty Optionality.Mandatory,
                  Mode.MkMode (Seq.singleton (Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))) Map.empty Optionality.Mandatory
                ]
            )
            (ModeSelection.ChooseExactly 1) ::
            ModalT.Modal Card.Type.Card
        chosen = Set.singleton (ModeIndex.MkModeIndex 1)
    Spec.assertEqWith s "only mode 1's effect" (Modal.modesEffects chosen m) [Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)]
    Spec.assertEqWith s "selectionCount is the ChooseExactly count" (Modal.selectionCount m) 1

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
    case Card.Type.activatedAbilities (Printing.card syntheticModalActivated) of
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
    case Card.Type.activatedAbilities (Printing.card syntheticModalActivated) of
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
triggerModalOf acPrinting = case Card.Type.triggeredAbilities (Printing.card acPrinting) of
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
        Spec.assertEqWith s "the token is named Bird Token" (fmap Card.Type.name (Game.cardOf tokId resolved)) (Just . CardName.MkCardName $ Text.pack "Bird Token")
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
          (Target.fillableModes Nothing acId Map.empty modal gs)
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
        gs2 = S.withEvents [GameEvent.Moved entered (Projection.project smtId gs1)] gs1
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
