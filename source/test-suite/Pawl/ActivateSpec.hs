{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Activate: activating an ability onto the stack, summoning-sickness
-- gating, and the CR 605 mana-ability exclusion from stack activations.
module Pawl.ActivateSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Finds as many matching library cards as the search allows, off the head of the
-- offered list, else fails to find.
findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches cap -> List.genericTake cap matches
  _ -> S.identityAnswer p

-- Answers ChooseModes with the empty selection, which is an illegal answer to
-- any selection demanding one or more -- the reject-not-repair trigger.
chooseNoModes :: Prompt.Prompt r -> r
chooseNoModes p = case p of
  Prompt.ChooseModes {} -> Seq.empty
  _ -> S.identityAnswer p

-- The single ability of a printing (all M3e gates have exactly one). Total: the
-- empty-ability fallback is unreachable in these fixtures, and honors the
-- no-partial-functions rule (no `error`).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (singleModeAbility [] Map.empty) [] Nothing

-- A single forced mode (ChooseExactly 1, M4g's non-modal shape) -- the fixture
-- shape every pre-M4h single-mode ActivatedAbility now takes.
singleModeAbility :: [Effect.Effect card] -> Map.Map SlotName.SlotName TargetSlot.TargetSlot -> Modal.Modal card
singleModeAbility effects slots =
  Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) slots)) (ModeSelection.ChooseExactly 1)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Activate" $ do
  printedActivationRestrictionSpec s registry
  printedActivationConjunctionSpec s registry
  printedActivationCombatPointSpec s registry
  printedActivationWholePhaseSpec s registry
  printedActivationTurnScopeSpec s registry
  variableActivationCostSpec s registry
  youOnActivatedAbilitySpec s registry
  textChangedAbilitySpec s registry
  textChangedCostSpec s registry
  textChangedTargetSpec s registry
  graveyardEffectZoneSpec s registry
  twoSacrificeComponentSpec s registry
  outlastSpec s registry
  activationCostReductionSpec s registry
  unflooredActivationCostReductionSpec s registry
  activationCostAdditionSpec s registry
  goldenEggSpec s registry
  presenceOfGondSpec s registry

  Spec.it s "CR 602 activating Prodigal Sorcerer's {T} puts an ability on the stack and taps it" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        g1 = g0 {GameState.priority = Just S.alice}
        after = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility prodigalSorcerer)))
    Spec.assertEqWith s "one thing on the stack" (length (GameState.stack after)) 1
    Spec.assertEqWith s "source tapped" (fmap Object.tapped (Game.lookupObject srcId after)) (Just TapState.Tapped)

  Spec.it s "CR 602.5/302.6 a summoning-sick creature's {T} ability is not offered" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) srcId (GameState.objects g0), GameState.priority = Just S.alice}
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice sick))) "no Activate offered"

  -- CR 302.6 keyed to the ACTIVATING player, not to the object: bob's
  -- Sorcerer has settled under bob, and alice's Control Magic does not
  -- inherit that settle along with the creature (#198).
  Spec.it s "CR 302.6 a stolen creature's {T} ability is not offered to the thief" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    controlMagic <- S.printingOf s registry "Control Magic"
    let (srcId, g0) = S.addCreature prodigalSorcerer S.bob (Setup.emptyGame S.bothPlayers)
        settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.bob)
        (aura, withAura) = S.addCreature controlMagic S.alice settled
        stolen = (S.attach aura srcId withAura) {GameState.priority = Just S.alice}
    Spec.assertBool s (any isActivate (Action.legalActions S.bob settled {GameState.priority = Just S.bob})) "bob could have activated it"
    Spec.assertBool s (Projection.controllerOf srcId stolen == Just S.alice) "alice controls it now"
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice stolen))) "but no Activate is offered to her"

  -- CR 702.6a: "Activate only as a sorcery." CR 307.5 says what that means,
  -- and says it narrowly: "the player must have priority, it must be during
  -- the main phase of their turn, and the stack must be empty."
  Spec.it s "CR 307.5 equip is offered in your own main phase with an empty stack" $ do
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    piker <- S.printingOf s registry "Goblin Piker"
    -- Lands, so the {1} equip cost is payable and the ONLY thing under test
    -- is the timing rider.
    let (_, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
        (_, g1) = S.addCreature bonesplitter S.alice g0
        gs = g1 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain}
    Spec.assertBool s (any isActivate (Action.legalActions S.alice gs)) "equip offered"

  Spec.it s "CR 307.5 equip is NOT offered outside a main phase" $ do
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    piker <- S.printingOf s registry "Goblin Piker"
    -- Lands, so the {1} equip cost is payable and the ONLY thing under test
    -- is the timing rider.
    let (_, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
        (_, g1) = S.addCreature bonesplitter S.alice g0
        gs = g1 {GameState.priority = Just S.alice, GameState.phase = Phase.Combat CombatStep.DeclareBlockers}
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "no equip during combat"

  Spec.it s "CR 307.5 equip is NOT offered on an opponent's turn" $ do
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    piker <- S.printingOf s registry "Goblin Piker"
    -- Lands, so the {1} equip cost is payable and the ONLY thing under test
    -- is the timing rider.
    let (_, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
        (_, g1) = S.addCreature bonesplitter S.alice g0
        gs = g1 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.bob}
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "not on bob's turn"

  Spec.it s "CR 307.5 equip is NOT offered while the stack is not empty" $ do
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    piker <- S.printingOf s registry "Goblin Piker"
    -- Lands, so the {1} equip cost is payable and the ONLY thing under test
    -- is the timing rider.
    let (_, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
        (_, g1) = S.addCreature bonesplitter S.alice g0
        (spellId, g2) = S.spellOnStack piker S.alice g1
        gs = g2 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain}
    Spec.assertBool s (elem spellId (GameState.stack gs)) "the stack really is occupied"
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "no equip with a spell on the stack"

  -- CR 702.8a's window is the SPELL's, not an ability's. Rule 307.5's window is
  -- shared between Pawl.Engine.Cast.sorcerySpeed and Activate.restrictionsOk, so
  -- the way to lift it for a flash spell and not for an activated ability is to
  -- lift it in Cast's disjunction and leave Turn.sorcerySpeedWindow alone; this
  -- is the case that says the shared window really did stay put.
  --
  -- ONE state, two questions, and the state is chosen so that only ONE of rule
  -- 307.5's three conjuncts is doing the refusing. alice is the active player,
  -- in her own precombat main phase, with a Pouncing Cheetah on the battlefield
  -- (the equip target), a Bonesplitter, a second Cheetah in hand -- and a spell
  -- on the stack. So the turn and the phase are right for both questions, the
  -- Cheetah in hand is castable because flash lifts the empty-stack requirement,
  -- and the equip is refused because nothing lifted it there.
  --
  -- The four cases above make the same refusal from an EMPTY board, so what this
  -- one adds is a flash permanent in play, one of them the equip's own target:
  -- an implementation that read the keyword off the ability's source, or off the
  -- board at all, would offer the equip here.
  Spec.it s "CR 307.5 flash does not make an activated ability instant-speed" $ do
    forest <- S.printingOf s registry "Forest"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature pouncingCheetah S.alice (S.landsInPlay forest 4)
        (_, g1) = S.addCreature bonesplitter S.alice g0
        (g2, inHand) = S.handOne pouncingCheetah g1
        (spellId, gs) = S.spellOnStack piker S.alice g2
    Spec.assertBool s (elem spellId (GameState.stack gs)) "the stack really is occupied"
    Spec.assertBool s (S.castable S.alice inHand gs) "the flash spell is castable in response"
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "but equip is not offered"

  -- The control: an UNRESTRICTED ability is unaffected by all three, so the
  -- gate cannot pass by refusing everything outside a main phase.
  Spec.it s "CR 602.2 an ability with no timing rider is still offered during combat" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (_, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.alice)
        gs = settled {GameState.priority = Just S.alice, GameState.phase = Phase.Combat CombatStep.DeclareBlockers}
    Spec.assertBool s (any isActivate (Action.legalActions S.alice gs)) "Prodigal Sorcerer still offered"

  Spec.it s "CR 602 a settled Prodigal Sorcerer's ability IS offered" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (_, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        g1 = g0 {GameState.priority = Just S.alice}
    Spec.assertBool s (any isActivate (Action.legalActions S.alice g1)) "Activate offered"

  Spec.it s "CR 602 activating then resolving deals 1 damage and the ability ceases" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        g1 = g0 {GameState.priority = Just S.alice}
        -- identityAnswer's ChooseTargets picks the lowest recipient; with no
        -- creatures but two players, it targets a player. Resolve the stack.
        activated = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility prodigalSorcerer)))
        resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
    Spec.assertEqWith s "stack empty after resolution" (GameState.stack resolved) []

  Spec.it s "CR 605.3b a mana ability is not offered as a stack activation" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (_, g0) = S.addCreature llanowarElves S.alice (Setup.emptyGame S.bothPlayers)
        g1 = g0 {GameState.priority = Just S.alice}
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice g1))) "no Activate for the mana ability"

  Spec.it s "CR 701.21/701.23 Evolving Wilds sacrifices itself and fetches a basic land tapped" $ do
    -- The fetched land gets a NEW id (CR 400.7); assert by count/tapped-count.
    evolvingWilds <- S.printingOf s registry "Evolving Wilds"
    forest <- S.printingOf s registry "Forest"
    let base = Setup.emptyGame S.bothPlayers
        (wildsId, g1) = S.addCreature evolvingWilds S.alice base
        (_, g2) = S.addLibraryCard forest S.alice g1
        g3 = g2 {GameState.priority = Just S.alice}
        ability = theAbility evolvingWilds
        activated = snd (Engine.runGamePure findFirst g3 (Activate.activateAbility S.alice wildsId ability))
        resolved = snd (Engine.runGamePure findFirst activated Stack.resolveTop)
    Spec.assertBool s (not (ManaAbility.isManaAbility ability)) "Evolving Wilds' ability is NOT a mana ability"
    Spec.assertBool s (not (Set.member wildsId (GameState.battlefield resolved))) "Evolving Wilds sacrificed (gone from battlefield)"
    Spec.assertEqWith s "one permanent on the battlefield (the fetched land)" (length (Game.zoneMembers Zone.Battlefield S.alice resolved)) 1
    Spec.assertEqWith s "the fetched land is tapped" (S.tappedCount S.alice resolved) 1

  Spec.it s "CR 302.6 a freshly-added land can tap+sac immediately (no summoning sickness)" $ do
    evolvingWilds <- S.printingOf s registry "Evolving Wilds"
    let base = Setup.emptyGame S.bothPlayers
        (wildsId, g1) = S.addCreature evolvingWilds S.alice base
        -- Force it Sick: a land ignores sickness, so the ability is still offered.
        g2 = g1 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) wildsId (GameState.objects g1), GameState.priority = Just S.alice}
    Spec.assertBool s (any isActivate (Action.legalActions S.alice g2)) "land ability offered despite sickness"

  Spec.it s "CR 613/602 a Humility'd Prodigal Sorcerer's ability is not offered" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    humility <- S.printingOf s registry "Humility"
    let (_, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        gs = (S.withHumility humility g0) {GameState.priority = Just S.alice}
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "no Activate under Humility"

  Spec.it s "CR 602.1b: an activation with a mana cost needs the mana" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs = S.landsInPlay mountain 1
        (srcId, gs1) = S.addCreature piker S.alice gs
        costlyAbility =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost =
                Cost.Type.MkCost
                  { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
                    Cost.Type.components = []
                  },
              ActivatedAbility.modal = singleModeAbility [] Map.empty,
              ActivatedAbility.restrictions = [],
              ActivatedAbility.condition = Nothing
            }
    Spec.assertBool s (not (Activate.activatable S.alice srcId costlyAbility gs1)) "one Mountain cannot pay {2}"

  Spec.it s "CR 701.19a Drudge Skeletons regenerates: activate, survive Murder, die to the next" $ do
    swamp <- S.printingOf s registry "Swamp"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let base = S.landsInPlay swamp 1
        (skel, gs0) = S.addCreature drudgeSkeletons S.alice base
        ability = theAbility drudgeSkeletons -- the local ActivateSpec helper
        activated = snd (Engine.runGamePure S.identityAnswer gs0 (Activate.activateAbility S.alice skel ability))
        resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
        -- First Murder: replaced by the shield.
        firstKill = S.settleSba (S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable [skel]))
        -- Second Murder: no shield -> dies.
        secondKill = S.settleSba (S.runPure S.identityAnswer firstKill (Event.destroy Regenerability.Regenerable [skel]))
    Spec.assertEqWith
      s
      "the shield's source is the skeleton itself"
      (fmap ActiveReplacement.source (GameState.replacements resolved))
      [skel]
    Spec.assertEqWith s "survived the first destruction (regenerated)" (Set.member skel (GameState.battlefield firstKill)) True
    Spec.assertEqWith s "died to the second (one-shot shield consumed)" (Set.member skel (GameState.battlefield secondKill)) False

  -- CR 113.8: the controller of an activated ability on the stack is the
  -- player who activated it; the controller of a triggered ability on the
  -- stack is whoever controlled its source when it triggered. Fixed once,
  -- at activation (Activate.activateAbility sets Object.owner = pid), and
  -- never revisited -- so a control change to the SOURCE PERMANENT after
  -- activation must not move the ability to the new controller.
  --
  -- Prodigal Sorcerer's own ability (a flat DealDamage 1; CR 115.4's "any
  -- target" has no controller-sensitive restriction) has no
  -- controller-sensitive OUTPUT, so it could not make "whose control"
  -- observable. Aladdin's can: "{1}{R}{R}, {T}: Gain control of target
  -- artifact for as long as you control this creature" is the same
  -- GainControl/ForAsLongAs shape Master Thief's ETB uses in
  -- Pawl.ExpirySpec's masterThiefSpec, but on the ACTIVATED path -- which
  -- is what retired the synthetic ability these two tests used to carry.
  Spec.it s "CR 113.8 an activated ability resolves under whoever activated it, not a later controller" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    mountain <- S.printingOf s registry "Mountain"
    aladdin <- S.printingOf s registry "Aladdin"
    let base = S.landsInPlay mountain 3
        -- The Myr is ALICE's own artifact (not bob's): if control ever
        -- moved to bob it would be a genuine change, not a fixture
        -- coincidence, so this assertion actually discriminates the bug.
        (myrId, g0) = S.addCreature darksteelMyr S.alice base
        (srcId, g1) = S.addCreature aladdin S.alice g0
        g2 = g1 {GameState.priority = Just S.alice}
        ability = theAbility aladdin
        activated = snd (Engine.runGamePure S.identityAnswer g2 (Activate.activateAbility S.alice srcId ability))
        -- Control of the SOURCE CREATURE (not the ability object) moves to bob
        -- while the ability sits on the stack.
        taken = S.giveControl srcId S.bob activated
        resolved = snd (Engine.runGamePure S.identityAnswer taken Stack.resolveTop)
    Spec.assertEqWith s "one thing on the stack before resolution" (length (GameState.stack taken)) 1
    Spec.assertEqWith s "stack empty after resolution" (GameState.stack resolved) []
    Spec.assertEqWith s "control of the artifact never changed" (Projection.controllerOf myrId resolved) (Just S.alice)
    -- Filtered to the artifact: S.giveControl's own AtCleanup
    -- SetController effect on srcId itself is already in this list and
    -- is not what CR 611.2b's "never starts" is about.
    Spec.assertEqWith
      s
      "nothing was stored for the artifact -- the duration never starts for alice, the ability's frozen activator"
      (filter (S.continuousEffectAffects myrId) (GameState.continuousEffects resolved))
      []

  -- The stolen-creature case the OLD (deleted) Resolve.hs comment named --
  -- the POSITIVE mirror of the test above, on the same Aladdin: control of
  -- the source moves to bob FIRST, bob then activates, and the effect must
  -- arm and store under bob, the ability's frozen (and only) controller
  -- (CR 113.8). Object.owner is stamped with bob at activation time, so there
  -- is no later re-read to get wrong.
  --
  -- ASYMMETRIC on purpose, and this half is the weaker one: bob is both the
  -- activator and the later controller, so a regression to a live re-read of
  -- the source's controller passes HERE and fails only the test above. The
  -- pair is what covers CR 113.8, not either case alone -- do not "fix" this
  -- one into a second copy of the first.
  Spec.it s "CR 113.8 a stolen creature's ability, activated by the new controller, resolves under them" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    mountain <- S.printingOf s registry "Mountain"
    aladdin <- S.printingOf s registry "Aladdin"
    -- bob pays for it, so the Mountains are HIS. S.giveControl also settles
    -- the stolen Aladdin under bob, which CR 302.6 requires before he can
    -- pay its {T} (#198 -- a thief does not inherit the previous
    -- controller's settle).
    let addMountains g = List.foldl' (\acc _ -> snd (S.addCreature mountain S.bob acc)) g [1 .. (3 :: Int)]
        base = addMountains (Setup.emptyGame S.bothPlayers)
        (myrId, g0) = S.addCreature darksteelMyr S.alice base
        (srcId, g1) = S.addCreature aladdin S.alice g0
        ability = theAbility aladdin
        -- Control of the SOURCE CREATURE moves to bob BEFORE activation.
        taken = S.giveControl srcId S.bob g1
        g2 = taken {GameState.priority = Just S.bob}
        activated = snd (Engine.runGamePure S.identityAnswer g2 (Activate.activateAbility S.bob srcId ability))
        resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
        stored = filter (S.continuousEffectAffects myrId) (GameState.continuousEffects resolved)
    Spec.assertEqWith s "bob controls the stolen Aladdin" (Projection.controllerOf srcId activated) (Just S.bob)
    Spec.assertEqWith s "one thing on the stack" (length (GameState.stack activated)) 1
    Spec.assertEqWith s "stack empty after resolution" (GameState.stack resolved) []
    Spec.assertEqWith
      s
      "control of the artifact changed to bob, the new controller who activated it"
      (Projection.controllerOf myrId resolved)
      (Just S.bob)
    Spec.assertEqWith s "exactly one stored effect names the artifact" (length stored) 1

  lastKnownSpec s registry

-- Answers every target slot with `who`, so a damage test reads a player's life
-- total rather than whichever recipient happens to sort lowest. The Fire-Eater
-- fixtures need it: their source is itself a legal AnyTarget candidate, and a
-- self-targeting answer would put the damage on an object that the cost has
-- already sacrificed -- observable nowhere.
aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimAt who p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
  _ -> S.identityAnswer p

-- CR 113.7a / 608.2h: an ability resolves using its source's LAST KNOWN
-- information once the source is gone. Ghitu Fire-Eater is the card that reaches
-- it -- "{T}, Sacrifice this creature: It deals damage equal to its power to any
-- target" pays a cost that removes the very object the effect then reads.
--
-- CR 113.7a says which moment is read: an ability that "references information
-- about the source for use while announcing" checks it as the ability is put on
-- the stack, "otherwise, it will check that information when it resolves. In both
-- instances, if the source is no longer in the zone it's expected to be in, its
-- last known information is used."
lastKnownSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lastKnownSpec s registry = Spec.describe s "LastKnownInformation" $ do
  Spec.it s "CR 113.7a whole card: a sacrificed Ghitu Fire-Eater still deals damage equal to its power" $ do
    ghituFireEater <- S.printingOf s registry "Ghitu Fire-Eater"
    let (srcId, g0) = S.addCreature ghituFireEater S.alice (Setup.emptyGame S.bothPlayers)
        g1 = g0 {GameState.priority = Just S.alice}
        activated = snd (Engine.runGamePure (aimAt S.bob) g1 (Activate.activateAbility S.alice srcId (theAbility ghituFireEater)))
        resolved = snd (Engine.runGamePure (aimAt S.bob) activated Stack.resolveTop)
    Spec.assertBool s (not (Set.member srcId (GameState.battlefield activated))) "the cost really sacrificed it"
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject srcId activated)) "and the id it left behind names nothing"
    Spec.assertEqWith s "bob took the Fire-Eater's 2" (S.lifeOf S.bob resolved) (Just 18)

  Spec.it s "CR 608.2h the value is LAST KNOWN, not printed: a pumped Fire-Eater deals 5" $ do
    -- The discriminator between reading last known information and reading
    -- the printed card. Both answer 2 for a vanilla Fire-Eater; only last
    -- known information answers 5 for one that was pumped before it left.
    ghituFireEater <- S.printingOf s registry "Ghitu Fire-Eater"
    let (srcId, g0) = S.addCreature ghituFireEater S.alice (Setup.emptyGame S.bothPlayers)
        pumped = S.withEffect srcId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal 3) (Quantity.Type.Literal 3))) g0
        g1 = pumped {GameState.priority = Just S.alice}
        activated = snd (Engine.runGamePure (aimAt S.bob) g1 (Activate.activateAbility S.alice srcId (theAbility ghituFireEater)))
        resolved = snd (Engine.runGamePure (aimAt S.bob) activated Stack.resolveTop)
    Spec.assertEqWith s "it was a 5/5 while it was on the battlefield" (Projection.powerOf srcId g1) (Just 5)
    Spec.assertEqWith s "bob took 5, not the printed 2" (S.lifeOf S.bob resolved) (Just 15)

  Spec.it s "CR 608.2h leaving a zone files last known information under the OLD id" $ do
    -- The substrate, on its own. CR 400.7 mints a fresh id for the graveyard
    -- incarnation, so the id an ability on the stack still holds is the one
    -- this map has to be keyed by.
    ghituFireEater <- S.printingOf s registry "Ghitu Fire-Eater"
    let (srcId, g0) = S.addCreature ghituFireEater S.alice (Setup.emptyGame S.bothPlayers)
        pumped = S.withEffect srcId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal 3) (Quantity.Type.Literal 3))) g0
    Spec.assertEqWith s "nothing filed before it moves" (Map.lookup srcId (GameState.lastKnown pumped)) Nothing
    let moved = S.runPure S.identityAnswer pumped (Event.changeZone srcId Zone.Graveyard)
    Spec.assertEqWith
      s
      "the snapshot is the projected power it had, not the printed one"
      (fmap (PC.power . LastKnown.characteristics) (Map.lookup srcId (GameState.lastKnown moved)))
      (Just (Just 5))
    -- CR 613.1b / 603.3a: the record keeps who controlled it as it left, not
    -- only what it looked like -- the half Event.eventTriggers needs to hand
    -- a dead entrant's trigger to the right player.
    Spec.assertEqWith
      s
      "and who controlled it as it left"
      (fmap LastKnown.controller (Map.lookup srcId (GameState.lastKnown moved)))
      (Just S.alice)

  Spec.it s "CR 608.2h the fallback is only a fallback: a source still there reads LIVE" $ do
    -- Discriminating against a viewWithLastKnown that always consults the
    -- map: a Fire-Eater that has not moved must read its current projection,
    -- and the map has nothing filed for it at all.
    ghituFireEater <- S.printingOf s registry "Ghitu Fire-Eater"
    let (srcId, g0) = S.addCreature ghituFireEater S.alice (Setup.emptyGame S.bothPlayers)
        pumped = S.withEffect srcId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal 3) (Quantity.Type.Literal 3))) g0
    Spec.assertEqWith
      s
      "the live projection is what the source-aware view returns"
      (Projection.viewWithLastKnown srcId pumped srcId >>= Filter.power)
      (Just 5)
    Spec.assertEqWith
      s
      "and every other id is untouched by the substitution"
      (Projection.viewWithLastKnown srcId pumped S.noSource >>= Filter.power)
      (Projection.fullView pumped S.noSource >>= Filter.power)

  cyclingSpec s registry
  reinforceSpec s registry
  authoredHandAbilitySpec s registry

-- CR 702.29: cycling, the first activated ability in the pool that is activated
-- from a zone other than the battlefield. Barkhide Mauler is a {4}{G} 4/4 whose
-- only text is "Cycling {2}", so every test below isolates the keyword: the
-- creature half is never cast.
--
-- Alice holds it, two Forests pay the {2}, and her library has a card to draw --
-- otherwise CR 704.5b would end the game around the assertion rather than the
-- draw being observable.
cyclingBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, GameState.GameState)
cyclingBoard s registry = do
  mauler <- S.printingOf s registry "Barkhide Mauler"
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  let (_, g0) = S.addLibraryCard piker S.alice (S.landsInPlay forest 2)
      (g1, oid) = S.handOne mauler g0
  pure (oid, g1 {GameState.priority = Just S.alice})

cyclingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cyclingSpec s registry = Spec.describe s "Cycling" $ do
  -- CR 702.29a: "Cycling is an activated ability that functions only while
  -- the card with cycling is in a player's hand."
  Spec.it s "CR 702.29a cycling is offered from the hand" $ do
    (_, gs) <- cyclingBoard s registry
    Spec.assertBool s (any isActivate (Action.legalActions S.alice gs)) "an Activate is offered"

  -- The ability rule 702.29a MEANS, rather than any the card prints: Barkhide
  -- Mauler's own activatedAbilities list is empty, and what is offered is
  -- minted from the keyword -- printed cost plus the rule's discard.
  Spec.it s "CR 702.29a the minted ability is '{2}, Discard this card: Draw a card'" $ do
    mauler <- S.printingOf s registry "Barkhide Mauler"
    (oid, gs) <- cyclingBoard s registry
    Spec.assertEqWith s "the card itself prints no activated ability" (Face.activatedAbilities (S.combinedFace mauler)) []
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        Spec.assertEqWith
          s
          "the printed {2} plus rule 702.29a's discard, recorded as CR 702.29c's cycle"
          (ActivatedAbility.cost ability)
          (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) [CostComponent.DiscardThis DiscardCause.ToPayCyclingCost])
        Spec.assertEqWith s "instant speed" (ActivatedAbility.restrictions ability) []
      abilities -> Spec.assertFailure s ("expected exactly one ability, got " <> show (length abilities))

  -- The whole card: pay {2}, discard it, draw. The Mauler is in the graveyard
  -- BEFORE the draw resolves, which is rule 702.29a putting the discard in
  -- the cost rather than in the effect.
  Spec.it s "CR 702.29a whole card: cycling discards the Mauler and draws" $ do
    (oid, gs) <- cyclingBoard s registry
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        let activated = snd (Engine.runGamePure S.identityAnswer gs (Activate.activateAbility S.alice oid ability))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            tapped g = length (filter (\o -> Object.tapped o == TapState.Tapped) (Maybe.mapMaybe (\o -> Game.lookupObject o g) (Set.toList (GameState.battlefield g))))
        Spec.assertEqWith s "the Mauler left the hand as the cost was paid" (length (Game.zoneMembers Zone.Hand S.alice activated)) 0
        Spec.assertEqWith s "and is in the graveyard while the draw is still on the stack" (length (Game.zoneMembers Zone.Graveyard S.alice activated)) 1
        Spec.assertEqWith s "the draw is on the stack" (length (GameState.stack activated)) 1
        Spec.assertEqWith s "both Forests paid" (tapped activated) 2
        Spec.assertEqWith s "then the draw resolves into her hand" (length (Game.zoneMembers Zone.Hand S.alice resolved)) 1
        Spec.assertEqWith s "her library is empty now" (length (Game.zoneMembers Zone.Library S.alice resolved)) 0
        Spec.assertEqWith s "the stack is empty" (GameState.stack resolved) []
      _ -> Spec.assertFailure s "expected exactly one cycling ability"

  -- The zone gate, from the other side. CR 702.29b: the ability still EXISTS
  -- on the battlefield ("it continues to exist ... in all other zones"), so
  -- this is not the ability being absent -- it is not being activatable
  -- there. A Mauler that resolved as a creature cannot be cycled.
  Spec.it s "CR 702.29a cycling is NOT offered from the battlefield" $ do
    mauler <- S.printingOf s registry "Barkhide Mauler"
    forest <- S.printingOf s registry "Forest"
    let (_, g0) = S.addCreature mauler S.alice (S.landsInPlay forest 2)
        gs = g0 {GameState.priority = Just S.alice}
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "no Activate offered"

  -- "In a PLAYER's hand" is that player's, and CR 108.4 is why this needs
  -- saying: a card in a hand has no controller at all, so an activation gate
  -- written against control would have offered it to nobody -- or, read
  -- loosely, to anybody.
  Spec.it s "CR 702.29a the other player cannot cycle a card in your hand" $ do
    (_, gs) <- cyclingBoard s registry
    Spec.assertBool s (not (any isActivate (Action.legalActions S.bob gs {GameState.priority = Just S.bob}))) "not offered to bob"

  -- CR 118.3: the cost still has to be payable. One Forest is not {2}.
  Spec.it s "CR 118.3 cycling is not offered without the mana" $ do
    mauler <- S.printingOf s registry "Barkhide Mauler"
    forest <- S.printingOf s registry "Forest"
    let (g0, _) = S.handOne mauler (S.landsInPlay forest 1)
        gs = g0 {GameState.priority = Just S.alice}
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "one Forest cannot pay {2}"

  -- CR 702.29e: typecycling is the same ability with a search in place of the
  -- draw. Ash Barrens' basic landcycling {1} finds a basic land card and puts
  -- it in HAND -- not onto the battlefield, which is the destination Evolving
  -- Wilds' search has and the one this rule does not.
  Spec.it s "CR 702.29e whole card: basic landcycling Ash Barrens fetches a Forest to hand" $ do
    barrens <- S.printingOf s registry "Ash Barrens"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    let (_, g0) = S.addLibraryCard forest S.alice (S.landsInPlay island 1)
        (g1, oid) = S.handOne barrens g0
        gs = g1 {GameState.priority = Just S.alice}
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        let cycled = S.runPure findFirst gs (Activate.activateAbility S.alice oid ability)
            after = S.runPure findFirst cycled Stack.resolveTop
        Spec.assertEqWith s "Ash Barrens paid its own cost into the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cycled)) 1
        Spec.assertEqWith s "the Forest is in hand" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
        Spec.assertEqWith s "and out of the library" (length (Game.zoneMembers Zone.Library S.alice after)) 0
        Spec.assertEqWith s "nothing was put onto the battlefield: one Island, no Forest" (length (Set.toList (GameState.battlefield after))) 1
      abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))

  -- CR 702.29e's filter is the card's, and it is a real narrowing: a library
  -- with no basic land in it finds nothing, and the cycler is still discarded
  -- (CR 701.23b -- a player "isn't required to find" a card, and here cannot).
  Spec.it s "CR 702.29e basic landcycling finds nothing in a library of nonbasics" $ do
    barrens <- S.printingOf s registry "Ash Barrens"
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    let (_, g0) = S.addLibraryCard piker S.alice (S.landsInPlay island 1)
        (g1, oid) = S.handOne barrens g0
        gs = g1 {GameState.priority = Just S.alice}
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        let cycled = S.runPure findFirst gs (Activate.activateAbility S.alice oid ability)
            after = S.runPure findFirst cycled Stack.resolveTop
        Spec.assertEqWith s "the Piker is not a basic land, so it stays in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
        Spec.assertEqWith s "the hand is empty" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
        Spec.assertEqWith s "and Ash Barrens was still discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
      abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))

  -- CR 701.20a: "To reveal a card, show that card to all players for a brief
  -- time." Rule 702.29e's typecycling says "reveal it", and CR 701.23e is
  -- what makes that the CARD's instruction rather than the search's: "If the
  -- effect that contains the search instruction doesn't also contain
  -- instructions to reveal the found card(s), then they're not revealed."
  --
  -- What the assertion is FOR: a tutored card is otherwise private -- it
  -- goes from one hidden zone to another -- so the log entry is the only
  -- record that a Forest in Alice's hand is a fact Bob gets to play around.
  -- The log, not a per-player view: pawl has no such view yet (#1412), and
  -- this is the record one would read.
  Spec.it s "CR 701.20a basic landcycling reveals the Forest it fetches" $ do
    barrens <- S.printingOf s registry "Ash Barrens"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    let (_, g0) = S.addLibraryCard forest S.alice (S.landsInPlay island 1)
        (g1, oid) = S.handOne barrens g0
        gs = g1 {GameState.priority = Just S.alice}
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        let cycled = S.runPure findFirst gs (Activate.activateAbility S.alice oid ability)
            after = S.runPure findFirst cycled Stack.resolveTop
        -- TWO reveals, from the two different rules that ask for one. CR
        -- 602.2a reveals Ash Barrens itself as the ability is announced,
        -- because a hand is a hidden zone; CR 702.29e's "reveal it" then
        -- reveals the Forest when the ability resolves. In that order.
        Spec.assertEqWith s "only the announcement reveal before the ability resolves" (S.revealsOf cycled) [(S.alice, Set.singleton . CardName.MkCardName $ Text.pack "Ash Barrens")]
        Spec.assertEqWith
          s
          "then Alice revealed the Forest she found"
          (S.revealsOf after)
          [(S.alice, Set.singleton . CardName.MkCardName $ Text.pack "Ash Barrens"), (S.alice, Set.singleton . CardName.MkCardName $ Text.pack "Forest")]
      abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))

  -- CR 701.20a reveals "that card", so a search that finds none reveals
  -- nothing. The negative half of the test above: CR 701.23b lets a player
  -- fail to find, and a failed find must not put an empty or filler reveal
  -- into the log for an opponent to read something into. The CR 602.2a
  -- announcement reveal is unaffected -- it already happened, and whether
  -- the search finds anything cannot reach back and unshow the cycler.
  Spec.it s "CR 701.20a a search that finds nothing reveals nothing" $ do
    barrens <- S.printingOf s registry "Ash Barrens"
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    let (_, g0) = S.addLibraryCard piker S.alice (S.landsInPlay island 1)
        (g1, oid) = S.handOne barrens g0
        gs = g1 {GameState.priority = Just S.alice}
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        let cycled = S.runPure findFirst gs (Activate.activateAbility S.alice oid ability)
            after = S.runPure findFirst cycled Stack.resolveTop
        Spec.assertEqWith s "no basic land found, so only the announcement reveal" (S.revealsOf after) [(S.alice, Set.singleton . CardName.MkCardName $ Text.pack "Ash Barrens")]
      abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))

  -- CR 702.29f: "typecycling abilities are cycling abilities, and typecycling
  -- costs are cycling costs." The engine gets that for free by minting both
  -- from one keyword arm -- the discard is in the cost either way, and the
  -- only difference is what resolves.
  Spec.it s "CR 702.29f a typecycling ability is a cycling ability" $ do
    barrens <- S.printingOf s registry "Ash Barrens"
    mauler <- S.printingOf s registry "Barkhide Mauler"
    island <- S.printingOf s registry "Island"
    let (g0, barrensId) = S.handOne barrens (S.landsInPlay island 2)
        (g1, maulerId) = S.handOne mauler g0
        gs = g1 {GameState.priority = Just S.alice}
    case (Activate.abilitiesFor barrensId gs, Activate.abilitiesFor maulerId gs) of
      ([typecycler], [plain]) -> do
        Spec.assertEqWith
          s
          "both costs end in rule 702.29a's discard"
          (Maybe.listToMaybe (reverse (Cost.Type.components (ActivatedAbility.cost typecycler))), Maybe.listToMaybe (reverse (Cost.Type.components (ActivatedAbility.cost plain))))
          (Just (CostComponent.DiscardThis DiscardCause.ToPayCyclingCost), Just (CostComponent.DiscardThis DiscardCause.ToPayCyclingCost))
        Spec.assertEqWith s "and both are instant speed" (ActivatedAbility.restrictions typecycler, ActivatedAbility.restrictions plain) ([], [])
      _ -> Spec.assertFailure s "expected one cycling ability on each"

  -- CR 701.20a from the battlefield, and by a card that prints the word
  -- itself: "{2}, {T}, Sacrifice this artifact: Search your library for a
  -- basic land card, reveal that card, put it into your hand, then shuffle."
  -- Ash Barrens above reaches the same destination through rule 702.29e, so
  -- this is the half that proves the reveal belongs to the CARD's sentence
  -- (CR 701.23e) rather than to the keyword that happened to arrive first.
  Spec.it s "CR 701.20a whole card: Braidwood Sextant fetches a Forest and reveals it" $ do
    sextant <- S.printingOf s registry "Braidwood Sextant"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    let (sextantId, g0) = S.addCreature sextant S.alice (S.landsInPlay island 2)
        (_, g1) = S.addLibraryCard forest S.alice g0
        gs = g1 {GameState.priority = Just S.alice}
        ability = theAbility sextant
        activated = S.runPure findFirst gs (Activate.activateAbility S.alice sextantId ability)
        after = S.runPure findFirst activated Stack.resolveTop
    Spec.assertBool s (not (Set.member sextantId (GameState.battlefield activated))) "the Sextant sacrificed itself paying the cost"
    -- ONE reveal, where Ash Barrens' typecycling produces two: the Sextant
    -- is announced from the BATTLEFIELD, which CR 400.2 makes a public zone,
    -- so CR 602.2a adds nothing. The contrast between these two tests is the
    -- whole of what that rule's zone condition does.
    Spec.assertEqWith s "nothing revealed while the ability is still on the stack" (S.revealsOf activated) []
    Spec.assertEqWith s "Alice revealed the Forest, and only the Forest" (S.revealsOf after) [(S.alice, Set.singleton . CardName.MkCardName $ Text.pack "Forest")]
    Spec.assertEqWith s "the Forest is in her hand" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
    Spec.assertEqWith s "and out of her library" (length (Game.zoneMembers Zone.Library S.alice after)) 0
    -- CR 701.20b: revealing does not move the card, so the two Islands that
    -- paid for this are the whole battlefield -- the Forest went to hand.
    Spec.assertEqWith s "only the two Islands remain on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 2
    -- "reveal that card, put it into your hand" is an ORDER, not two
    -- independent facts, and CR 701.20b is what gives it teeth: the reveal
    -- happens while the card is still in the library, so it precedes the
    -- move in the log. Swapping the two would leave every other assertion
    -- here passing.
    case (List.findIndex (Maybe.isJust . Event.revealOf) (S.eventsOf after), List.findIndex ((== Just Zone.Hand) . fmap ZoneChange.to . Event.movedOf) (S.eventsOf after)) of
      (Just revealed, Just moved) -> Spec.assertBool s (revealed < moved) "the reveal is logged before the move into the hand"
      indices -> Spec.assertFailure s ("expected both a reveal and a move into the hand, got " <> show indices)

  -- CR 602.2a: "The player announces that they are activating the ability.
  -- If an activated ability is being activated from a hidden zone, the card
  -- that has that ability is revealed." CR 400.2 names the hidden zones:
  -- "Library and hand are hidden zones."
  --
  -- The reveal is at ANNOUNCEMENT, which for cycling is before its own cost
  -- discards the card -- so the log shows the Mauler being revealed and only
  -- then moving to the graveyard. Both halves are asserted: an engine that
  -- revealed at the wrong moment would still show the right card.
  Spec.it s "CR 602.2a cycling from hand reveals the Mauler as the ability is announced" $ do
    (oid, gs) <- cyclingBoard s registry
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability)
            events = S.eventsOf activated
        Spec.assertEqWith s "Alice revealed Barkhide Mauler" (S.revealsOf activated) [(S.alice, Set.singleton . CardName.MkCardName $ Text.pack "Barkhide Mauler")]
        Spec.assertEqWith s "and the draw has not resolved, so this is the announcement" (length (GameState.stack activated)) 1
        case (List.findIndex (Maybe.isJust . Event.revealOf) events, List.findIndex ((== Just Zone.Graveyard) . fmap ZoneChange.to . Event.movedOf) events) of
          (Just revealed, Just discarded) -> Spec.assertBool s (revealed < discarded) "revealed before the cost discarded it"
          indices -> Spec.assertFailure s ("expected both a reveal and a discard, got " <> show indices)
      abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))

  -- The control, and the reason CR 602.2a needs a zone test rather than
  -- revealing unconditionally: CR 400.2 makes the battlefield a PUBLIC zone,
  -- where every player can already see the card. Prodigal Sorcerer's {T} is
  -- announced the same way and shows nobody anything.
  Spec.it s "CR 400.2 activating from the battlefield reveals nothing" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        gs = g0 {GameState.priority = Just S.alice}
        activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice srcId (theAbility prodigalSorcerer))
    Spec.assertEqWith s "the ability is on the stack" (length (GameState.stack activated)) 1
    Spec.assertEqWith s "and the battlefield is public, so nothing was revealed" (S.revealsOf activated) []

  -- CR 602.2a's reveal belongs to an activation that HAPPENED. pawl's
  -- reject-not-repair guard (an interpreter answering with an illegal mode
  -- set) makes the whole activation a no-op, and the reveal has to go back
  -- with it -- otherwise the log claims Alice showed her opponent a card
  -- over an activation the engine refused.
  --
  -- The ability is hand-built because no card in the pool prints a MODAL
  -- ability activatable from a hidden zone; the Mauler is a real card in a
  -- real hand, which is the part under test.
  Spec.it s "CR 602.2a a rejected activation reveals nothing" $ do
    mauler <- S.printingOf s registry "Barkhide Mauler"
    forest <- S.printingOf s registry "Forest"
    let (g0, oid) = S.handOne mauler (S.landsInPlay forest 2)
        gs = g0 {GameState.priority = Just S.alice}
        -- Two fillable modes, choose one: more legal than the count, so
        -- ChooseModes is really asked and a bad answer can really be given.
        -- The modes are empty because what they DO is not under test.
        twoModes =
          ActivatedAbility.MkActivatedAbility
            (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [])
            ( Modal.MkModal
                (Seq.fromList [Mode.MkMode Seq.empty Map.empty, Mode.MkMode Seq.empty Map.empty])
                (ModeSelection.ChooseExactly 1)
            )
            []
            Nothing
        after = S.runPure chooseNoModes gs (Activate.activateAbility S.alice oid twoModes)
    Spec.assertEqWith s "the activation was rejected: nothing on the stack" (GameState.stack after) []
    Spec.assertEqWith s "so no reveal survives it either" (S.revealsOf after) []
    Spec.assertEqWith s "and the Mauler is still in her hand" (length (Game.zoneMembers Zone.Hand S.alice after)) 1

  -- The control: the gate cannot pass by offering every card in hand. A
  -- Piker has no cycling and nothing is minted for it.
  Spec.it s "CR 702.29a a card without cycling offers nothing from the hand" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    forest <- S.printingOf s registry "Forest"
    let (g0, oid) = S.handOne piker (S.landsInPlay forest 2)
        gs = g0 {GameState.priority = Just S.alice}
    Spec.assertEqWith s "no abilities minted" (Activate.abilitiesFor oid gs) []
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "and no Activate offered"

-- CR 702.77: reinforce, cycling's zone with a TARGET. Mosquito Guard is a {W}
-- 1/1 whose only other text is first strike, so nothing but rule 702.77a
-- produces the ability under test.
--
-- Alice holds it and has two Plains for the {1}{W}. The two creatures are the
-- target pool, and they belong to DIFFERENT players with different power boxes:
-- rule 702.77a prints "target creature" with no controller qualifier, so aiming
-- at Bob's is the reading under test, and 4/4 against 2/1 means a counter that
-- landed on the wrong one is visible in the power.
reinforceBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
reinforceBoard s registry = do
  guard <- S.printingOf s registry "Mosquito Guard"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  mauler <- S.printingOf s registry "Barkhide Mauler"
  let (pikerId, g0) = S.addCreature piker S.alice (S.landsInPlay plains 2)
      (maulerId, g1) = S.addCreature mauler S.bob g0
      (g2, guardId) = S.handOne guard g1
  pure (guardId, pikerId, maulerId, g2 {GameState.priority = Just S.alice})

reinforceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
reinforceSpec s registry = Spec.describe s "Reinforce" $ do
  -- CR 702.77a: "Reinforce is an activated ability that functions only while
  -- the card with reinforce is in a player's hand. 'Reinforce N-[cost]' means
  -- '[Cost], Discard this card: Put N +1/+1 counters on target creature.'"
  -- The card prints no activated ability; what is offered is minted.
  Spec.it s "CR 702.77a the minted ability is '{1}{W}, Discard this card: put a counter on target creature'" $ do
    guard <- S.printingOf s registry "Mosquito Guard"
    (guardId, _, _, gs) <- reinforceBoard s registry
    Spec.assertEqWith s "the card itself prints no activated ability" (Face.activatedAbilities (S.combinedFace guard)) []
    Spec.assertBool s (any isActivate (Action.legalActions S.alice gs)) "an Activate is offered from the hand"
    case Activate.abilitiesFor guardId gs of
      [ability] -> do
        Spec.assertEqWith
          s
          "the printed {1}{W} plus rule 702.77a's discard, whose cause is NOT rule 702.29c's cycle"
          (ActivatedAbility.cost ability)
          (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.White)])) [CostComponent.DiscardThis DiscardCause.Ordinary])
        Spec.assertEqWith s "instant speed" (ActivatedAbility.restrictions ability) []
        Spec.assertEqWith
          s
          "one slot, 'target creature' with nothing narrowing it"
          (foldMap (Map.elems . Mode.targetSlots) (Modal.modes (ActivatedAbility.modal ability)))
          [TargetSlot.required Pool.Creatures Nothing]
      abilities -> Spec.assertFailure s ("expected one reinforce ability, got " <> show (length abilities))

  -- The whole card, aimed across the table. The Guard is in the graveyard while
  -- the ability is still on the stack, which is rule 702.77a's discard sitting
  -- before the colon; then N counters land on the chosen creature and on no
  -- other.
  Spec.it s "CR 702.77a whole card: reinforce discards the Guard and puts one counter on Bob's Mauler" $ do
    (guardId, pikerId, maulerId, gs) <- reinforceBoard s registry
    case Activate.abilitiesFor guardId gs of
      [ability] -> do
        let activated = S.runPure (aimAtCreature maulerId) gs (Activate.activateAbility S.alice guardId ability)
            resolved = S.runPure (aimAtCreature maulerId) activated Stack.resolveTop
            countersOn oid g = fmap (Map.lookup CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid g)
            tapped g = length (filter (\o -> Object.tapped o == TapState.Tapped) (Maybe.mapMaybe (\o -> Game.lookupObject o g) (Set.toList (GameState.battlefield g))))
        Spec.assertEqWith s "the Guard left the hand as the cost was paid" (length (Game.zoneMembers Zone.Hand S.alice activated)) 0
        Spec.assertEqWith s "and is in the graveyard while the ability is still on the stack" (length (Game.zoneMembers Zone.Graveyard S.alice activated)) 1
        Spec.assertEqWith s "the ability is on the stack" (length (GameState.stack activated)) 1
        Spec.assertEqWith s "both Plains paid" (tapped activated) 2
        Spec.assertEqWith s "nothing has a counter yet" (countersOn maulerId activated) (Just Nothing)
        Spec.assertEqWith s "reinforce 1 puts exactly one counter on the target" (countersOn maulerId resolved) (Just (Just 1))
        Spec.assertEqWith s "so Bob's 4/4 Mauler is a 5/5" (S.powerToughnessOf maulerId resolved) (Just (5, 5))
        Spec.assertEqWith s "Alice's own Piker was not the target and is untouched" (countersOn pikerId resolved) (Just Nothing)
        Spec.assertEqWith s "still a 2/1" (S.powerToughnessOf pikerId resolved) (Just (2, 1))
        Spec.assertEqWith s "the stack is empty" (GameState.stack resolved) []
      abilities -> Spec.assertFailure s ("expected one reinforce ability, got " <> show (length abilities))

  -- CR 702.77a's discard against CR 702.29c's: reinforce's cost ends in the same
  -- "Discard this card" rule 702.29a's does, and it is NOT cycling -- rule 702.77
  -- never says so, where CR 702.29f says exactly that of typecycling. Prickly
  -- Marmoset ("whenever you cycle a card, this creature gets +2/+0 until end of
  -- turn") is the observer, under the same seat that pays the reinforce cost, so
  -- CR 603.3a's "you" is alice either way and only the CAUSE can tell the two
  -- discards apart.
  --
  -- Distinct numbers throughout: the Marmoset is a 2/3 and a 4/3 pumped, the
  -- Piker a 2/1 and a 3/2 reinforced, so no reading of the rule lands on another's
  -- pair.
  Spec.it s "CR 702.77a a reinforce discard is not a cycle" $ do
    guard <- S.printingOf s registry "Mosquito Guard"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    marmoset <- S.printingOf s registry "Prickly Marmoset"
    let (pikerId, g0) = S.addCreature piker S.alice (S.landsInPlay plains 2)
        (marmosetId, g1) = S.addCreature marmoset S.alice g0
        (g2, guardId) = S.handOne guard g1
        gs = g2 {GameState.priority = Just S.alice}
    Spec.assertEqWith s "the Marmoset starts a 2/3" (S.powerToughnessOf marmosetId gs) (Just (2, 3))
    case Activate.abilitiesFor guardId gs of
      [ability] -> do
        let activated = S.runPure (aimAtCreature pikerId) gs (Activate.activateAbility S.alice guardId ability)
            placed = S.runPure (aimAtCreature pikerId) activated Engine.settleForPriority
            after = S.runPure (aimAtCreature pikerId) placed Stack.resolveTop
        Spec.assertEqWith s "the Marmoset is untouched by a discard that is not a cycle" (S.powerToughnessOf marmosetId after) (Just (2, 3))
        -- Also the proof that the activation really happened: a 2/1 reads 3/2
        -- only once rule 702.77a's counter has landed.
        Spec.assertEqWith s "and reinforce still put its counter on the Piker" (S.powerToughnessOf pikerId after) (Just (3, 2))
        Spec.assertEqWith s "only the reinforce ability was on the stack -- no cycling trigger joined it" (length (GameState.stack placed)) 1
        Spec.assertEqWith s "the Guard was discarded to pay the cost" (length (Game.zoneMembers Zone.Graveyard S.alice activated)) 1
      abilities -> Spec.assertFailure s ("expected one reinforce ability, got " <> show (length abilities))

  -- CR 601.2c through CR 602.2b: an ability with a target it cannot legally
  -- choose cannot be activated. This board is reinforceBoard's with the two
  -- creatures taken away and nothing else changed -- same Guard, same two
  -- Plains -- so the offer flipping is the empty target pool and not the mana.
  Spec.it s "CR 601.2c reinforce is not offered with no creature to target" $ do
    guard <- S.printingOf s registry "Mosquito Guard"
    plains <- S.printingOf s registry "Plains"
    let (g0, guardId) = S.handOne guard (S.landsInPlay plains 2)
        gs = g0 {GameState.priority = Just S.alice}
    Spec.assertEqWith s "the ability is still minted" (length (Activate.abilitiesFor guardId gs)) 1
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "but no Activate is offered"

  -- The zone gate, cycling's test one keyword over: a Mosquito Guard that
  -- resolved as a creature cannot be reinforced. Rule 702.77b keeps the ability
  -- in existence there -- Projection.abilitiesOf reports it, which is what
  -- Pawl.UntapRestrictionSpec proves with a Rustic Clachan -- and what rule
  -- 702.77b does NOT do is make it activatable.
  --
  -- CR 113.6m is the gate that answers here (Activate.functionsIn, over the cost's
  -- DiscardThis), and abilitiesFor is where it is applied, so the empty list below
  -- is the existence/activation split rather than an absence of minting.
  Spec.it s "CR 702.77a reinforce is NOT offered from the battlefield" $ do
    guard <- S.printingOf s registry "Mosquito Guard"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature piker S.alice (S.landsInPlay plains 2)
        (guardId, g1) = S.addCreature guard S.alice g0
        gs = g1 {GameState.priority = Just S.alice}
    Spec.assertEqWith s "nothing minted for it on the battlefield" (Activate.abilitiesFor guardId gs) []
    Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "and no Activate offered"

-- CR 113.6j: an activated ability a card AUTHORS, functioning from the hand
-- because its cost can only be paid there. Faerie Macabre is a {1}{B}{B} 2/2 with
-- flying and "Discard this card: Exile up to two target cards from graveyards" --
-- the first card in the pool to write a DiscardThis cost component itself, where
-- cycling and reinforce have theirs minted from a keyword (CR 702.29a, CR
-- 702.77a). Nothing else on the card can produce the ability under test.
--
-- The board: alice holds the Macabre and nothing else, bob's graveyard holds a
-- Goblin Piker and a Barkhide Mauler, and alice's holds a Forest that nothing
-- aims at. THREE candidates for a two-target announcement, so the choice is a
-- real one, and the Forest is what tells "exiled the targets" from "swept the
-- graveyards". No land is needed: the whole activation cost is the discard.
macabreBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
macabreBoard s registry = do
  macabre <- S.printingOf s registry "Faerie Macabre"
  piker <- S.printingOf s registry "Goblin Piker"
  mauler <- S.printingOf s registry "Barkhide Mauler"
  forest <- S.printingOf s registry "Forest"
  let (pikerId, g0) = S.addGraveyardCard piker S.bob (Setup.emptyGame S.bothPlayers)
      (maulerId, g1) = S.addGraveyardCard mauler S.bob g0
      (forestId, g2) = S.addGraveyardCard forest S.alice g1
      (g3, macabreId) = S.handOne macabre g2
  pure (macabreId, pikerId, maulerId, forestId, g3 {GameState.priority = Just S.alice})

-- Aims at exactly these cards by FILTERING the offered recipients rather than
-- building one: a hand-built Recipient of another tag is a different recipient,
-- and CR 608.2b's re-read drops it with no error.
aimAtCards :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
aimAtCards oids p =
  let isWanted recipient = case recipient of
        Recipient.ToObject oid -> elem oid oids
        _ -> False
   in case p of
        Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, legal) -> Set.filter isWanted legal) asked
        _ -> S.identityAnswer p

-- The card names in one player's copy of a zone, sorted. Written by NAME rather
-- than by object id because CR 400.7 mints a new incarnation on every zone
-- change, so the id a fixture placed a card under answers Nothing the moment the
-- card moves -- which is indistinguishable from "moved somewhere else".
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
namesIn zone pid gs = List.sort (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))

named :: String -> CardName.CardName
named = CardName.MkCardName . Text.pack

authoredHandAbilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
authoredHandAbilitySpec s registry = Spec.describe s "Authored hand ability" $ do
  -- CR 113.6j: "An object's activated ability that has a cost that can't be paid
  -- while the object is on the battlefield functions from any zone in which its
  -- cost can be paid." CR 113.6m names that zone -- the cost discards the object
  -- itself, so Cost.zoneOfComponent answers the hand -- and this is the whole
  -- card, end to end, from a hand.
  --
  -- Driven through the offered list rather than a hand-built ability: an engine
  -- that never offers the ability activates nothing, so the exile below is what
  -- goes red rather than a count ahead of it.
  Spec.it s "CR 113.6j an authored discard-this ability functions from the hand" $ do
    (macabreId, pikerId, maulerId, _, gs) <- macabreBoard s registry
    let abilities = Activate.abilitiesFor macabreId gs
        activated = S.runPure (aimAtCards [pikerId, maulerId]) gs (mapM_ (Activate.activateAbility S.alice macabreId) abilities)
        after = S.runPure (aimAtCards [pikerId, maulerId]) activated Stack.resolveTop
    Spec.assertEqWith s "both targeted graveyard cards are exiled" (namesIn Zone.Exile S.bob after) [named "Barkhide Mauler", named "Goblin Piker"]
    Spec.assertEqWith s "so bob's graveyard is empty" (namesIn Zone.Graveyard S.bob after) []
    -- CR 115.6's "up to two" is a maximum and not a sweep: alice's own Forest was
    -- offered and not chosen, so it stays where it is -- beside the Macabre the
    -- cost discarded (CR 404.1).
    Spec.assertEqWith s "and the third card in a graveyard, which nothing targeted, is still there" (namesIn Zone.Graveyard S.alice after) [named "Faerie Macabre", named "Forest"]
    Spec.assertEqWith s "alice exiled nothing of her own" (namesIn Zone.Exile S.alice after) []
    Spec.assertEqWith s "exactly the one printed ability was offered from the hand" (length abilities) 1
    Spec.assertBool s (not (null (activationsOf macabreId (Action.legalActions S.alice gs)))) "and the enumeration offers it as an action"
    Spec.assertEqWith s "the Macabre paid its own cost out of the hand" (namesIn Zone.Hand S.alice activated) []
    Spec.assertEqWith s "and is in its owner's graveyard (CR 404.1) while its ability is still on the stack" (namesIn Zone.Graveyard S.alice activated) [named "Faerie Macabre", named "Forest"]
    Spec.assertEqWith s "the ability was on the stack" (length (GameState.stack activated)) 1

  -- CR 702.29c: only a CYCLING ability's discard is a cycle. Faerie Macabre's is
  -- authored, so Pawl.Codec.CostComponent decodes it to DiscardCause.Ordinary --
  -- the wire has one spelling for both causes, so nothing but this silence can
  -- tell the two apart. Prickly Marmoset ("Whenever you cycle a card, this
  -- creature gets +2/+0 until end of turn") is the observer, under the same seat
  -- that pays the cost so CR 603.3a's "you" is alice either way, and the
  -- reinforce group one over runs the same shape for the MINTED discard.
  --
  -- Distinct numbers: the Marmoset is a 2/3 and a 4/3 pumped.
  Spec.it s "CR 702.29c an authored discard-this cost is not a cycle" $ do
    marmoset <- S.printingOf s registry "Prickly Marmoset"
    (macabreId, pikerId, maulerId, _, board) <- macabreBoard s registry
    let (marmosetId, gs) = S.addCreature marmoset S.alice board
        abilities = Activate.abilitiesFor macabreId gs
        activated = S.runPure (aimAtCards [pikerId, maulerId]) gs (mapM_ (Activate.activateAbility S.alice macabreId) abilities)
        placed = S.runPure (aimAtCards [pikerId, maulerId]) activated Engine.settleForPriority
        after = S.runPure (aimAtCards [pikerId, maulerId]) placed Stack.resolveTop
    Spec.assertEqWith s "the Marmoset starts a 2/3" (S.powerToughnessOf marmosetId gs) (Just (2, 3))
    Spec.assertEqWith s "and is untouched by a discard that is not a cycle" (S.powerToughnessOf marmosetId after) (Just (2, 3))
    Spec.assertEqWith s "the activation really happened -- both targets are exiled" (namesIn Zone.Exile S.bob after) [named "Barkhide Mauler", named "Goblin Piker"]
    Spec.assertEqWith s "only the Macabre's ability was on the stack -- no cycling trigger joined it" (length (GameState.stack placed)) 1

isActivate :: A.Action -> Bool
isActivate a = case a of
  A.Activate _ _ -> True
  _ -> False

-- The activations offered for ONE source, so a board carrying two activatable
-- permanents can say which of them was offered. `any isActivate` cannot.
activationsOf :: ObjectId.ObjectId -> [A.Action] -> [A.Action]
activationsOf oid = filter (isActivationOf oid)

isActivationOf :: ObjectId.ObjectId -> A.Action -> Bool
isActivationOf oid a = case a of
  A.Activate o _ -> o == oid
  _ -> False

-- CR 307.5's phase-scoped rider, printed on a card about itself: Desert (Arabian
-- Nights) prints "{T}: This land deals 1 damage to target attacking creature.
-- Activate only during the end of combat step."
--
-- The mirror of Pawl.CastSpec's printedCastingRestrictionSpec, and deliberately
-- not the same type -- see Pawl.Types.ActivationRestriction for why the two gates
-- cannot share one, CR 307.5's last two sentences being the load-bearing part.
--
-- The fixture is alice attacking with one Goblin Piker (2/1) into a bob who
-- controls one Desert and nothing else. Every test declares the attack itself,
-- so the step each one then names is the only variable.
desertBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
desertBoard piker desert =
  let (gs0, ours, _) = S.combatBoardOf [piker] []
      (desertId, gs1) = S.addCreature desert S.bob gs0
   in case ours of
        attackerId : _ ->
          (desertId, attackerId, gs1)
        -- combatBoardOf returns one id per printing given, so this is
        -- unreachable; a bogus id fails the assertions rather than the suite.
        [] -> (desertId, S.noSource, gs1)

-- Declares alice's attack and hands priority to bob, the defending player (CR
-- 506.2). The board every timing assertion below starts from, and the ones that
-- name a later step do it by overwriting GameState.phase on this.
desertAttacked :: GameState.GameState -> GameState.GameState
desertAttacked gs =
  (S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.alice)) {GameState.priority = Just S.bob}

-- Activates the first offered activation, else passes -- the interpreter that
-- takes the Desert's ping the moment the engine offers it.
pingAnswer :: Prompt.Prompt r -> r
pingAnswer p = case p of
  Prompt.ChooseAction _ _ options -> case filter isActivate options of
    a : _ -> a
    [] -> A.Pass
  _ -> S.aggressiveAnswer p

printedActivationRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
printedActivationRestrictionSpec s registry = Spec.describe s "PrintedActivationRestriction" $ do
  -- The rider, isolated. bob is in the declare attackers step with an
  -- untapped Desert, a legal target (CR 508.1f has just tapped alice's
  -- attacker, and it is attacking) and no cost to pay beyond {T}. The only
  -- thing withholding the ability is the printed window.
  --
  -- Carries its own control in the same test, on the same board and for the
  -- same player: bob's Prodigal Sorcerer ("{T}: This creature deals 1 damage
  -- to any target", CR 602.2 and no rider) IS offered, so what stops the
  -- Desert is the rider and not the step being closed to bob altogether.
  Spec.it s "CR 307.5 the Desert's ping is NOT offered in the declare attackers step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    desert <- S.printingOf s registry "Desert"
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (gs0, _, theirs) = S.combatBoardOf [piker] [prodigalSorcerer]
        (desertId, gs1) = S.addCreature desert S.bob gs0
        attacked = desertAttacked gs1
        offered = Action.legalActions S.bob attacked
    Spec.assertEqWith s "no activation of the Desert" (activationsOf desertId offered) []
    case theirs of
      sorcererId : _ ->
        Spec.assertBool s (not (null (activationsOf sorcererId offered))) "but bob's unrestricted ability is offered in the same step"
      [] -> Spec.assertFailure s "fixture should have given bob a Prodigal Sorcerer"

  -- CR 511.1: "The end of combat step has no turn-based actions. Once it
  -- begins, the active player gets priority." The printed window, reached.
  --
  -- CR 511.3 is what makes the window useful at all: "As soon as the end of
  -- combat step ENDS, all creatures ... are removed from combat", so a
  -- creature declared as an attacker is still attacking throughout this step
  -- and the ability still has something to target.
  Spec.it s "CR 307.5 the Desert's ping IS offered in the end of combat step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    desert <- S.printingOf s registry "Desert"
    let (desertId, _, board) = desertBoard piker desert
        atEndOfCombat = (desertAttacked board) {GameState.phase = Phase.Combat CombatStep.EndOfCombat}
    Spec.assertEqWith s "exactly one activation, the ping" (length (activationsOf desertId (Action.legalActions S.bob atEndOfCombat))) 1

  -- The same board one step later, differing in nothing but the phase. The
  -- combat record still holds the attack -- asserted, so this cannot pass
  -- because the target pool emptied -- and the ability is gone all the same.
  Spec.it s "CR 307.5 the Desert's ping is NOT offered in the postcombat main phase" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    desert <- S.printingOf s registry "Desert"
    let (desertId, _, board) = desertBoard piker desert
        attacked = desertAttacked board
        later = attacked {GameState.phase = Phase.PostcombatMain}
    Spec.assertBool s (not (Set.null (Combat.Type.attacked (GameState.combat later)))) "the attack is still on the record"
    Spec.assertEqWith s "still offered in the step it names" (length (activationsOf desertId (Action.legalActions S.bob (attacked {GameState.phase = Phase.Combat CombatStep.EndOfCombat})))) 1
    Spec.assertEqWith s "and not one phase later" (activationsOf desertId (Action.legalActions S.bob later)) []

  -- The gameplay-level proof (design.md section 4), driven through
  -- Engine.runStep and the priority loop rather than by calling
  -- Activate.activateAbility: bob takes every activation the engine offers
  -- him, and the whole combat phase runs.
  --
  -- bob's life is the falsifier, and it is why this test is worth more than
  -- the enumeration tests above. An engine that ignored the rider would offer
  -- the ping at bob's FIRST priority -- in the declare attackers step -- the
  -- Piker would die before CR 510.2 dealt its combat damage, and bob would
  -- still be at 20. Losing the 2 life and THEN killing the attacker is the
  -- shape only the printed window produces.
  --
  -- CR 704.5g: "If a creature has toughness greater than 0, it has damage
  -- marked on it, and the total damage marked on it is greater than or equal
  -- to its toughness, that creature has been dealt lethal damage and is
  -- destroyed." One damage on a 2/1.
  Spec.it s "CR 307.5 whole card: Desert pings the attacker dead in the end of combat step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    desert <- S.printingOf s registry "Desert"
    let (desertId, attackerId, board) = desertBoard piker desert
        after = S.runCombat pingAnswer board
    Spec.assertEqWith s "the Piker connected first" (S.lifeOf S.bob after) (Just 18)
    Spec.assertBool s (not (Set.member attackerId (GameState.battlefield after))) "and then died to the ping"
    Spec.assertEqWith s "the Desert paid its {T}" (fmap Object.tapped (Game.lookupObject desertId after)) (Just TapState.Tapped)

  -- The control for the whole-card test: the same board and the same combat,
  -- with an interpreter that never activates. The Piker survives, so what
  -- killed it above was the ability and not combat.
  Spec.it s "CR 307.5 whole card: the attacker survives a combat bob does not ping in" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    desert <- S.printingOf s registry "Desert"
    let (_, attackerId, board) = desertBoard piker desert
        after = S.runCombat S.aggressiveAnswer board
    Spec.assertEqWith s "the Piker still connected" (S.lifeOf S.bob after) (Just 18)
    Spec.assertBool s (Set.member attackerId (GameState.battlefield after)) "and is still on the battlefield"

-- CR 602.5's conjunction, printed on a card about itself: Kongming's
-- Contraptions (Portal Three Kingdoms) prints "{T}: This creature deals 2 damage
-- to target attacking creature. Activate only during the declare attackers step
-- and only if you've been attacked this step."
--
-- TWO clauses on ONE ability, which is what Desert's group above cannot reach:
-- Desert prints the step clause alone, so a reader that stopped after the first
-- clause would satisfy every assertion up there. CR 602.5 -- "A player can't
-- begin to activate an ability that's prohibited from being activated" -- is what
-- makes a printed list of clauses a conjunction, the ability-side counterpart of
-- CR 601.3 for a spell.
--
-- THREE SEATS, and that is the whole fixture. The pair of boards below differs in
-- nothing but WHOM alice attacked. With carol as the defending player (CR 507.1,
-- which is CR 506.2's choice once there are three seats to choose from)
-- bob is still in the declare attackers step, still holds priority, and still has
-- an attacking creature to target -- everything the first clause asks for -- so
-- only the second clause can withhold the ability. On two seats "an attack
-- happened" and "bob was attacked" are one fact, and the second clause would go
-- untested.
--
-- bob also gets a Prodigal Sorcerer, the same control Desert's group uses: an
-- unrestricted {T} of bob's on the same board, so a board that offered him
-- nothing at all cannot be mistaken for the rider working.
kongmingBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
kongmingBoard piker contraptions sorcerer defender =
  let (gs0, _, theirs, _) = S.threePlayerCombat [piker] [contraptions, sorcerer] []
      -- Combat.defender is STATED rather than run: CR 507.1's turn-based action
      -- is what would fill it in, and a direct-call test never reaches it.
      ready =
        gs0
          { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            GameState.combat = (GameState.combat gs0) {Combat.Type.defender = Just defender}
          }
      -- CR 508.1, then priority to bob -- who is the defending player on one of
      -- these two boards and a bystander on the other, which is the variable.
      declared = (S.runPure S.aggressiveAnswer ready (Combat.declareAttackers S.alice)) {GameState.priority = Just S.bob}
   in case theirs of
        contraptionsId : sorcererId : _ -> (contraptionsId, sorcererId, declared)
        -- threePlayerCombat returns one id per printing given, so this is
        -- unreachable; bogus ids fail the assertions rather than the suite.
        _ -> (S.noSource, S.noSource, declared)

-- Chooses `who` as the defending player, declines every block, and takes the
-- first activation the engine offers -- the interpreter the whole-card tests
-- below drive a real combat phase with.
--
-- CR 509.1: no blocks, so the only thing that can remove the attacker from the
-- battlefield is the ping. Without that clause bob's own 2/4 would eat the Piker
-- in the combat damage step and the life totals would stop discriminating.
kongmingAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
kongmingAnswer who p = case p of
  Prompt.ChooseDefender {} -> who
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.ChooseAction _ _ options -> case filter isActivate options of
    a : _ -> a
    [] -> A.Pass
  _ -> S.aggressiveAnswer p

printedActivationConjunctionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
printedActivationConjunctionSpec s registry = Spec.describe s "PrintedActivationConjunction" $ do
  -- The second clause, isolated. Everything the FIRST clause asks for holds --
  -- the game is in the declare attackers step, bob has priority, his Contraptions
  -- is untapped and settled, and alice's Piker is attacking and so a legal target
  -- -- and the ability is withheld anyway, because the attack was on carol.
  --
  -- Both halves of "everything the first clause asks for" are asserted rather
  -- than assumed: without the attack there would be no attacking creature to
  -- target, and the ability would be absent for a reason that has nothing to do
  -- with either clause.
  Spec.it s "CR 602.5 the Contraptions' ping is NOT offered when the attack was on somebody else" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    contraptions <- S.printingOf s registry "Kongming's Contraptions"
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (contraptionsId, sorcererId, board) = kongmingBoard piker contraptions prodigalSorcerer S.carol
        offered = Action.legalActions S.bob board
    Spec.assertBool s (Set.member (AttackTarget.OfPlayer S.carol) (Combat.Type.declaredAttacked (GameState.combat board))) "carol is the player who was attacked"
    Spec.assertBool s (not (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.declaredAttacked (GameState.combat board)))) "and bob is not"
    Spec.assertBool s (not (Set.null (Combat.Type.attacked (GameState.combat board)))) "but there IS an attacking creature to target"
    Spec.assertEqWith s "no activation of the Contraptions" (activationsOf contraptionsId offered) []
    Spec.assertBool s (not (null (activationsOf sorcererId offered))) "and bob's unrestricted ability is offered in the same step"

  -- The discriminating twin: the same seats, the same step, the same permanent,
  -- one fact changed. Without this half the assertion above would pass on an
  -- engine that never offered this ability at all.
  Spec.it s "CR 602.5 the Contraptions' ping IS offered when bob is the player attacked" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    contraptions <- S.printingOf s registry "Kongming's Contraptions"
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (contraptionsId, _, board) = kongmingBoard piker contraptions prodigalSorcerer S.bob
        offered = Action.legalActions S.bob board
    Spec.assertBool s (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.declaredAttacked (GameState.combat board))) "bob is the player who was attacked"
    Spec.assertEqWith s "exactly one activation, the ping" (length (activationsOf contraptionsId offered)) 1

  -- The first clause has not gone soft under the second: the same attack ON BOB,
  -- one step later. CR 511.3 keeps the attacker in combat through the end of
  -- combat step, and CR 508.1's declaration record is not cleared either, so the
  -- second clause still holds -- and the ability is gone all the same.
  Spec.it s "CR 602.5 being attacked is not enough on its own: not offered in the end of combat step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    contraptions <- S.printingOf s registry "Kongming's Contraptions"
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (contraptionsId, _, board) = kongmingBoard piker contraptions prodigalSorcerer S.bob
        later = board {GameState.phase = Phase.Combat CombatStep.EndOfCombat}
    Spec.assertBool s (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.declaredAttacked (GameState.combat later))) "bob is still on the record as attacked"
    Spec.assertBool s (not (Set.null (Combat.Type.attacked (GameState.combat later)))) "and the attacker is still attacking"
    Spec.assertEqWith s "but the step has passed" (activationsOf contraptionsId (Action.legalActions S.bob later)) []

  -- The gameplay-level proof (design.md section 4), driven through Engine.runStep
  -- and the priority loop rather than by calling Activate.activateAbility: bob
  -- takes every activation the engine offers him, and the whole combat phase runs.
  --
  -- bob's life is the falsifier. CR 508.1 declares the attack, bob's first
  -- priority is in that same step, and the ping (2 damage, CR 704.5g against a
  -- 2/1) removes the attacker before CR 510.2 can deal combat damage -- so bob is
  -- still at 20.
  Spec.it s "CR 602.5 whole card: the Contraptions ping the attacker dead before it connects" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    contraptions <- S.printingOf s registry "Kongming's Contraptions"
    let (gs0, ours, theirs, _) = S.threePlayerCombat [piker] [contraptions] []
        after = S.runCombat (kongmingAnswer S.bob) gs0
    case (ours, theirs) of
      (attackerId : _, contraptionsId : _) -> do
        Spec.assertEqWith s "bob took no combat damage" (S.lifeOf S.bob after) (Just 20)
        Spec.assertBool s (not (Set.member attackerId (GameState.battlefield after))) "because the Piker died to the ping"
        Spec.assertEqWith s "which cost the Contraptions its {T}" (fmap Object.tapped (Game.lookupObject contraptionsId after)) (Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should have given alice an attacker and bob a Contraptions"

  -- The same combat with the SAME interpreter, attacking carol instead. This is
  -- the second clause at gameplay level: bob is never offered the ping, so the
  -- Piker lives, carol takes the 2, and the Contraptions is still untapped at the
  -- end of it.
  Spec.it s "CR 602.5 whole card: bob may not ping an attack aimed at carol" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    contraptions <- S.printingOf s registry "Kongming's Contraptions"
    let (gs0, ours, theirs, _) = S.threePlayerCombat [piker] [contraptions] []
        after = S.runCombat (kongmingAnswer S.carol) gs0
    case (ours, theirs) of
      (attackerId : _, contraptionsId : _) -> do
        Spec.assertEqWith s "carol took the hit" (S.lifeOf S.carol after) (Just 18)
        Spec.assertBool s (Set.member attackerId (GameState.battlefield after)) "the Piker survived the combat"
        Spec.assertEqWith s "and the Contraptions never paid its {T}" (fmap Object.tapped (Game.lookupObject contraptionsId after)) (Just TapState.Untapped)
      _ -> Spec.assertFailure s "fixture should have given alice an attacker and bob a Contraptions"

-- alice controls a Jade Statue and two Mountains, and holds priority. The {2} is
-- payable exactly once, which is all any of these tests needs.
statueBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
statueBoard statue mountain =
  let (statueId, gs1) = S.addCreature statue S.alice (Setup.emptyGame S.bothPlayers)
      (_, gs2) = S.addCreature mountain S.alice gs1
      (_, gs3) = S.addCreature mountain S.alice gs2
   in (statueId, gs3 {GameState.activePlayer = S.alice, GameState.priority = Just S.alice})

-- CR 307.5's rider naming a phase that HAS steps, which no Pawl.Types.Phase value
-- can say: Jade Statue (Arabian Nights) prints "{2}: This artifact becomes a 3/6
-- Golem artifact creature until end of combat. Activate only during combat."
--
-- The third axis of the arm, after Desert's step and Llanowar Augur's turn. CR
-- 500.1 -- "The beginning, combat, and ending phases are further broken down
-- into steps, which proceed in order" -- and CR 506.1 lists combat's five, so
-- "during combat" is one window over five schedule entries. Pawl.Types.PhaseSelector
-- is what says it, and Pawl.Engine.Turn.inWindow is the containment test that
-- reads it.
--
-- Desert's group above is the other half of the pair: it names ONE of these five
-- steps, and its tests must go on refusing the other four. Between them they
-- prove the reader did not simply become permissive.
printedActivationWholePhaseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
printedActivationWholePhaseSpec s registry = Spec.describe s "PrintedActivationWholePhase" $ do
  -- Every one of CR 506.1's five steps, in one assertion: a rider naming the
  -- combat phase is live in all of them. An arm that could only name a step
  -- would have to pick one, and would fail four of these five.
  Spec.it s "CR 500.1/506.1 the Statue's animation is offered in EVERY combat step" $ do
    statue <- S.printingOf s registry "Jade Statue"
    mountain <- S.printingOf s registry "Mountain"
    let (statueId, board) = statueBoard statue mountain
        offeredIn step = length (activationsOf statueId (Action.legalActions S.alice (board {GameState.phase = Phase.Combat step})))
    Spec.assertEqWith
      s
      "one activation in each of the five"
      (fmap offeredIn [CombatStep.BeginningOfCombat, CombatStep.DeclareAttackers, CombatStep.DeclareBlockers, CombatStep.CombatDamage, CombatStep.EndOfCombat])
      [1, 1, 1, 1, 1]

  -- The discriminating half. CR 505.1 makes the two main phases their own
  -- phases, so "during combat" excludes both -- and the postcombat main phase is
  -- the sharp one, since it is adjacent to the window and shares its turn.
  --
  -- Carries its own control in the same test, on the same board and for the same
  -- player: alice's Prodigal Sorcerer ("{T}: This creature deals 1 damage to any
  -- target", CR 602.2 and no rider) IS offered, so what withholds the Statue is
  -- the rider and not the phase being closed to alice altogether.
  Spec.it s "CR 505.1 the Statue's animation is NOT offered in either main phase" $ do
    statue <- S.printingOf s registry "Jade Statue"
    mountain <- S.printingOf s registry "Mountain"
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (statueId, board) = statueBoard statue mountain
        (sorcererId, withSorcerer) = S.addCreature prodigalSorcerer S.alice board
        offeredIn phase = activationsOf statueId (Action.legalActions S.alice (withSorcerer {GameState.phase = phase}))
    Spec.assertEqWith s "not before combat" (offeredIn Phase.PrecombatMain) []
    Spec.assertEqWith s "not after it" (offeredIn Phase.PostcombatMain) []
    Spec.assertEqWith s "not in the upkeep either" (offeredIn (Phase.Beginning BeginningStep.Upkeep)) []
    Spec.assertEqWith s "and not in the end step" (offeredIn (Phase.Ending EndingStep.EndStep)) []
    Spec.assertBool
      s
      (not (null (activationsOf sorcererId (Action.legalActions S.alice (withSorcerer {GameState.phase = Phase.PostcombatMain})))))
      "but alice's unrestricted ability is offered in the same phase"

  -- CR 102.1's axis, unchanged by the widening: Jade Statue prints no "your", so
  -- its TurnScope is EachTurn and bob's combat phase is a window for it too.
  -- Asked of alice, who controls it -- CR 602.2 restricts activation to the
  -- controller -- on a turn that is not hers.
  Spec.it s "CR 102.1 the window is EachTurn, so an opponent's combat phase qualifies" $ do
    statue <- S.printingOf s registry "Jade Statue"
    mountain <- S.printingOf s registry "Mountain"
    let (statueId, board) = statueBoard statue mountain
        bobsCombat = board {GameState.activePlayer = S.bob, GameState.phase = Phase.Combat CombatStep.DeclareBlockers}
    Spec.assertEqWith s "still offered on bob's turn" (length (activationsOf statueId (Action.legalActions S.alice bobsCombat))) 1

-- CR 307.5's rider narrowed by TURN as well as by step: Llanowar Augur
-- (Future Sight) prints "Sacrifice this creature: Target creature gets +3/+3 and
-- gains trample until end of turn. Activate only during your upkeep."
--
-- Desert above carries the same arm with no turn clause, so between them the two
-- groups pin both axes: CR 500.1 breaks a turn into phases and steps and says
-- nothing about whose turn it is, and CR 102.1 ("The active player is the player
-- whose turn it is") makes that a second, independent fact.
--
-- alice controls both permanents: the Piker is added FIRST so it holds the lower
-- ObjectId, which is what makes S.aggressiveAnswer's `Set.lookupMin` over the
-- target set pick it rather than the Augur (itself a legal target: CR 602.2b
-- routes an activation through CR 601.2b-i, and CR 601.2c chooses targets before
-- CR 601.2h pays the sacrifice).
augurBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
augurBoard piker augur =
  let (pikerId, gs1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      (augurId, gs2) = S.addCreature augur S.alice gs1
   in (augurId, pikerId, gs2)

-- The upkeep step of `active`'s turn, with alice -- the Augur's controller --
-- holding priority either way. CR 117.3a gives the active player priority first,
-- but a nonactive player reaches an upkeep of someone else's turn all the same,
-- which is exactly the case the turn axis has to refuse.
--
-- The schedule loses its head because Setup.emptyGame's `remaining` still begins
-- with the upkeep step; dropping it leaves the draw step next, so a runStep-driven
-- test advances out of the upkeep instead of back into it.
augurUpkeep :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
augurUpkeep active gs =
  gs
    { GameState.activePlayer = active,
      GameState.phase = Phase.Beginning BeginningStep.Upkeep,
      GameState.priority = Just S.alice,
      GameState.remaining = Seq.drop 1 (GameState.remaining gs)
    }

-- Activates the first offered activation, else passes -- pingAnswer's twin for
-- the Augur, and the interpreter that takes the pump the moment it is offered.
pumpAnswer :: Prompt.Prompt r -> r
pumpAnswer p = case p of
  Prompt.ChooseAction _ _ options -> case filter isActivate options of
    a : _ -> a
    [] -> A.Pass
  _ -> S.aggressiveAnswer p

printedActivationTurnScopeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
printedActivationTurnScopeSpec s registry = Spec.describe s "PrintedActivationTurnScope" $ do
  -- The window the card names, reached: alice's own upkeep.
  Spec.it s "CR 307.5 the Augur's pump IS offered during its controller's upkeep" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    augur <- S.printingOf s registry "Llanowar Augur"
    let (augurId, _, board) = augurBoard piker augur
        mine = augurUpkeep S.alice board
    Spec.assertEqWith s "exactly one activation, the pump" (length (activationsOf augurId (Action.legalActions S.alice mine))) 1

  -- The discriminating half: the SAME step, one turn earlier. CR 109.5 -- "The
  -- words 'you' and 'your' on an object refer to the object's controller ... For
  -- an activated ability, this is the player who activated the ability" -- is
  -- what makes the printed "your upkeep" alice's and not bob's, and CR 102.1 is
  -- what makes the two steps distinguishable at all. An implementation carrying
  -- only CR 500.1's phase passes the test above and fails this one.
  --
  -- Carries its own control on the same board, for the same player, in the same
  -- step: alice's Prodigal Sorcerer ("{T}: This creature deals 1 damage to any
  -- target", CR 602.2 and no rider) IS offered, so what withholds the Augur is
  -- the rider and not alice being shut out of a nonactive player's upkeep.
  Spec.it s "CR 307.5/109.5 the Augur's pump is NOT offered during the opponent's upkeep" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    augur <- S.printingOf s registry "Llanowar Augur"
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (augurId, _, board) = augurBoard piker augur
        (sorcererId, withSorcerer) = S.addCreature prodigalSorcerer S.alice board
        theirs = augurUpkeep S.bob withSorcerer
        offered = Action.legalActions S.alice theirs
    Spec.assertEqWith s "no activation of the Augur" (activationsOf augurId offered) []
    Spec.assertBool s (not (null (activationsOf sorcererId offered))) "but alice's unrestricted ability is offered in the same step"

  -- The phase axis, still enforced: alice's own turn, one step later. CR 501.1 --
  -- "The beginning phase consists of three steps, in this order: untap, upkeep,
  -- and draw" -- and the draw step is not the one the card names.
  Spec.it s "CR 500.1 the Augur's pump is NOT offered during its controller's draw step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    augur <- S.printingOf s registry "Llanowar Augur"
    let (augurId, _, board) = augurBoard piker augur
        mine = augurUpkeep S.alice board
        later = mine {GameState.phase = Phase.Beginning BeginningStep.DrawStep}
    Spec.assertEqWith s "still offered in the step it names" (length (activationsOf augurId (Action.legalActions S.alice mine))) 1
    Spec.assertEqWith s "and not one step later" (activationsOf augurId (Action.legalActions S.alice later)) []

  -- The gameplay-level proof (design.md section 4), driven through
  -- Engine.runStep and the priority loop rather than by calling
  -- Activate.activateAbility: alice takes every activation the engine offers her
  -- and the whole upkeep step runs.
  --
  -- Goblin Piker is a 2/1, so +3/+3 (CR 613.4c, layer 7c) makes it a 5/4, and CR
  -- 702.19's trample arrives with it. The Augur is gone: CR 602.1a puts the
  -- sacrifice in the activation cost, so it left the battlefield on the way to
  -- the stack and the ability resolved all the same.
  Spec.it s "CR 307.5 whole card: the Augur pumps in its controller's upkeep" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    augur <- S.printingOf s registry "Llanowar Augur"
    let (augurId, pikerId, board) = augurBoard piker augur
        after = S.runPure pumpAnswer (augurUpkeep S.alice board) Engine.runStep
    Spec.assertEqWith s "the Piker is a 5/4" (S.powerToughnessOf pikerId after) (Just (5, 4))
    Spec.assertBool s (Projection.hasKeyword Keyword.Trample pikerId after) "and has trample"
    Spec.assertBool s (not (Set.member augurId (GameState.battlefield after))) "the Augur paid itself"

  -- The same board, the same interpreter, the same step -- on bob's turn. The
  -- gameplay-level twin of the enumeration test above, and the one an
  -- implementation that reads only the phase cannot pass: nothing happens at all.
  Spec.it s "CR 307.5/109.5 whole card: the Augur does nothing in the opponent's upkeep" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    augur <- S.printingOf s registry "Llanowar Augur"
    let (augurId, pikerId, board) = augurBoard piker augur
        after = S.runPure pumpAnswer (augurUpkeep S.bob board) Engine.runStep
    Spec.assertEqWith s "the Piker is still a 2/1" (S.powerToughnessOf pikerId after) (Just (2, 1))
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Trample pikerId after)) "and has no trample"
    Spec.assertBool s (Set.member augurId (GameState.battlefield after)) "and the Augur is still on the battlefield"

-- Alice with `n` untapped Mountains and a settled Cinder Elemental, holding
-- priority. `n` is the whole of what the {X} is measured against: the activation
-- cost is {X}{R}, so n Mountains pay every X up to n-1 and nothing above it.
cinderBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Int ->
  m (Printing.Printing, ObjectId.ObjectId, GameState.GameState)
cinderBoard s registry n = do
  cinder <- S.printingOf s registry "Cinder Elemental"
  mountain <- S.printingOf s registry "Mountain"
  let (srcId, g0) = S.addCreature cinder S.alice (S.landsInPlay mountain n)
  pure (cinder, srcId, g0 {GameState.priority = Just S.alice})

-- The name of Tovolar's back face, which is where the {X} ability is printed.
tovolarBackName :: CardName.CardName
tovolarBackName = CardName.MkCardName (Text.pack "Tovolar, the Midnight Scourge")

-- theAbility for an object showing a face that is NOT its card's combined view.
-- A transforming card's combined view is its front face (CR 712.8a), so reading
-- the printing gets Tovolar, Dire Overlord's empty list of activated abilities;
-- what the permanent actually offers is read off the face it is showing.
--
-- Total for theAbility's reason, and the fallback is inert: it costs nothing and
-- does nothing, so a fixture that reached it would fail its own assertions.
shownAbility :: ObjectId.ObjectId -> GameState.GameState -> ActivatedAbility.ActivatedAbility Card.Type.Card
shownAbility oid gs = case Game.faceOf oid gs >>= Maybe.listToMaybe . Face.activatedAbilities of
  Just ab -> ab
  Nothing -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (singleModeAbility [] Map.empty) [] Nothing

-- alice controls Tovolar showing his BACK face -- "{X}{R}{G}: Target Wolf or
-- Werewolf you control gets +X/+0 and gains trample until end of turn" -- a
-- Russet Wolves to aim it at, and two Mountains and two Forests, which pay
-- {2}{R}{G} and nothing above it.
tovolarNightBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
tovolarNightBoard s registry = do
  tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
  wolves <- S.printingOf s registry "Russet Wolves"
  mountain <- S.printingOf s registry "Mountain"
  forest <- S.printingOf s registry "Forest"
  let (tovolarId, g0) = S.addCreature tovolar S.alice (S.landsFor mountain S.alice 2 (S.landsInPlay forest 2))
      (wolfId, g1) = S.addCreature wolves S.alice g0
      turnedOver o = o {Object.face = Just tovolarBackName}
      g2 = g1 {GameState.objects = Map.adjust turnedOver tovolarId (GameState.objects g1)}
  pure (tovolarId, wolfId, g2 {GameState.priority = Just S.alice})

-- Announces X and aims every target slot at `who`.
answerXAt :: Natural -> PlayerId.PlayerId -> Prompt.Prompt r -> r
answerXAt x who p = case p of
  Prompt.ChooseX {} -> x
  _ -> aimAt who p

-- Announces X and narrows every target slot to one object, by FILTERING the
-- offered set rather than building a recipient: the pool decides which flavour
-- of Recipient a candidate arrives as, and a hand-built one of another flavour
-- is stored, looks targeted, and is then dropped by CR 608.2b's re-read.
answerXTargeting :: Natural -> ObjectId.ObjectId -> Prompt.Prompt r -> r
answerXTargeting x oid p = case p of
  Prompt.ChooseX {} -> x
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- Answers Prompt.ChooseX with the affordability bound the prompt carries, and
-- records that bound in the State -- the CastSpec answerer, aimed at a player.
-- The log is how a test sees a payload nothing on the board records; answering
-- WITH it is what proves the bound is payable rather than merely reported.
answerAtBound :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State [Natural] r
answerAtBound who p = case p of
  Prompt.ChooseX _ _ _ bound -> do
    State.modify' (\seen -> seen <> [bound])
    pure bound
  _ -> pure (aimAt who p)

-- Announces ONE MORE than the bound -- legal under CR 601.2b and unaffordable by
-- construction, whatever the board is.
answerAboveBound :: PlayerId.PlayerId -> Prompt.Prompt r -> r
answerAboveBound who p = case p of
  Prompt.ChooseX _ _ _ bound -> bound + 1
  _ -> aimAt who p

-- answerAtBound and answerAboveBound in one, COUNTING the CR 601.2c target
-- questions the activation asks: `offset` 0 announces the bound and 1 announces
-- one past it. The count is the only way a test can see a step the activation did
-- NOT take, since an activation reversed at CR 601.2h leaves a board identical to
-- one reversed earlier.
answerAtBoundOffsetCounting :: Natural -> PlayerId.PlayerId -> Prompt.Prompt r -> State.State Int r
answerAtBoundOffsetCounting offset who p = case p of
  Prompt.ChooseX _ _ _ bound -> pure (bound + offset)
  Prompt.ChooseTargets {} -> do
    State.modify' (+ 1)
    pure (aimAt who p)
  _ -> pure (aimAt who p)

-- Counts the CR 601.2b value-of-X questions asked, so a test can assert one was
-- put to the player -- and that none is put where the cost has no {X}.
countingX :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State Int r
countingX who p = case p of
  Prompt.ChooseX {} -> do
    State.modify' (+ 1)
    pure (aimAt who p)
  _ -> pure (aimAt who p)

-- CR 602.2b: "The remainder of the process for activating an ability is identical
-- to the process for casting a spell listed in rules 601.2b-i. Those rules apply
-- to activating an ability just as they apply to casting a spell. An activated
-- ability's analog to a spell's mana cost (as referenced in rule 601.2f) is its
-- activation cost." So CR 601.2b's "If the spell has a variable cost that will be
-- paid as it's being cast (such as an {X} in its mana cost; see rule 107.3), the
-- player announces the value of that variable" reaches an ACTIVATION cost's {X}
-- exactly as it reaches a spell's.
--
-- Cinder Elemental -- "{X}{R}, {T}, Sacrifice this creature: It deals X damage to
-- any target" -- is the pool's first card with one (#544), and it exercises both
-- halves at once: the X is paid AND read, so an engine that dropped it would
-- both undercharge and underdeal.
--
-- THE FALSIFIER for the whole group is the engine answering 0 on the player's
-- behalf. An unannounced ManaSymbol.Variable demands nothing at payment
-- (Mana.waysOf's Variable arm is [(Nothing, 0, 0)]), so a missing announcement is
-- not a missing question but a free {X}: Cinder Elemental would cost {R} and deal
-- nothing, and every mana assertion below would read 1 instead of n.
variableActivationCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
variableActivationCostSpec s registry = Spec.describe s "VariableActivationCost" $ do
  Spec.it s "CR 601.2b/602.2b whole card: Cinder Elemental at X=2 pays {2}{R} and deals 2" $ do
    (cinder, srcId, g1) <- cinderBoard s registry 3
    let after =
          snd
            ( Engine.runGamePure
                (answerXAt 2 S.bob)
                g1
                (do Activate.activateAbility S.alice srcId (theAbility cinder); Stack.resolveTop)
            )
    Spec.assertEqWith s "bob took the announced 2, not 0" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "all three Mountains paid the {2}{R}" (S.tappedCount S.alice after) 3
    Spec.assertBool s (not (Set.member srcId (GameState.battlefield after))) "and the cost really sacrificed it"
    Spec.assertEqWith s "stack empty after resolution" (GameState.stack after) []

  -- CR 601.2b's announced value is STAMPED, and this is the read half of #544's
  -- gap: the value reaches the effect through the ABILITY object's bindings, and
  -- the source permanent it names is in a graveyard by then, sacrificed to pay.
  Spec.it s "CR 601.2b the announced X is stamped on the ability object, not the source" $ do
    (cinder, srcId, g1) <- cinderBoard s registry 4
    let after = snd (Engine.runGamePure (answerXAt 3 S.bob) g1 (Activate.activateAbility S.alice srcId (theAbility cinder)))
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject srcId after)) "the source id names nothing"
    case GameState.stack after of
      [] -> Spec.assertFailure s "expected the ability on the stack"
      top : _ -> case Game.lookupObject top after of
        Nothing -> Spec.assertFailure s "stack id should resolve"
        Just obj -> Spec.assertEqWith s "amount bound" (Binding.amountOf Binding.variableX (Object.bindings obj)) (Just 3)

  -- The bound is what the BOARD can pay, so it moves with the board: {X}{R} off
  -- four Mountains admits X=3, off six admits X=5, and off the one Mountain that
  -- only just makes the ability activatable admits nothing but CR 601.2b's floor.
  -- No constant, and nothing read off the printed cost, satisfies all three.
  Spec.it s "CR 601.2b the ChooseX bound is the greatest X the board can pay" $ do
    let boundsOff n = do
          (cinder, srcId, g1) <- cinderBoard s registry n
          pure (State.execState (Engine.runGame (answerAtBound S.bob) g1 (Activate.activateAbility S.alice srcId (theAbility cinder))) [])
    four <- boundsOff 4
    six <- boundsOff 6
    one <- boundsOff 1
    Spec.assertEqWith s "four Mountains bound X at 3" four [3]
    Spec.assertEqWith s "six Mountains bound X at 5" six [5]
    Spec.assertEqWith s "one Mountain bounds X at 0" one [0]

  -- The bound is PAYABLE and not merely reported: announcing exactly it activates
  -- the ability, taps every Mountain, and resolves. An off-by-one bound would
  -- reverse the activation here (CR 602.2) and leave bob at 20.
  Spec.it s "CR 601.2b announcing X at the bound activates the ability and resolves it" $ do
    (cinder, srcId, g1) <- cinderBoard s registry 4
    let act = do Activate.activateAbility S.alice srcId (theAbility cinder); Stack.resolveTop
        after = snd (State.evalState (Engine.runGame (answerAtBound S.bob) g1 act) [])
    Spec.assertEqWith s "bob at 17, so the bound of 3 was announced and paid" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "all four Mountains paid the {3}{R}" (S.tappedCount S.alice after) 4

  -- The assertion that keeps the bound honest. It is ADVISORY: CR 601.2b lets the
  -- player announce the value of the variable freely, so one more than the bound
  -- is announced, is unaffordable, and reverses the whole activation -- CR 602.2's
  -- "the activation is illegal; the game returns to the moment before that ability
  -- started to be activated". A bound quietly turned into a clamp would deal 3
  -- damage and tap four Mountains here.
  Spec.it s "CR 602.2 the bound does not clamp: X one above it is a no-op" $ do
    (cinder, srcId, g1) <- cinderBoard s registry 4
    let after = snd (Engine.runGamePure (answerAboveBound S.bob) g1 (Activate.activateAbility S.alice srcId (theAbility cinder)))
    Spec.assertBool s (Set.member srcId (GameState.battlefield after)) "the Elemental was not sacrificed"
    Spec.assertEqWith s "and not tapped" (fmap Object.tapped (Game.lookupObject srcId after)) (Just TapState.Untapped)
    Spec.assertEqWith s "no mana spent" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "bob unharmed" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and nothing was left on the stack" (GameState.stack after) []

  -- WHERE the reversal happens, which the no-op above cannot see. CR 602.2 puts it
  -- at the step the player is unable to comply with, and the announced value of X
  -- is the first step that can be one -- activatability measured the cost at CR
  -- 601.2b's X=0 floor, the only value it can know before the announcement exists.
  -- So the activation ends there, and CR 601.2c's target question is never put to a
  -- player whose ability is already lost.
  Spec.it s "CR 602.2 an unaffordable X ends the activation before CR 601.2c's targets are asked for" $ do
    (cinder, srcId, g1) <- cinderBoard s registry 4
    let asked offset = State.execState (Engine.runGame (answerAtBoundOffsetCounting offset S.bob) g1 (Activate.activateAbility S.alice srcId (theAbility cinder))) 0
    Spec.assertEqWith s "at the bound the activation goes on and asks for its target" (asked 0) 1
    Spec.assertEqWith s "one above it, there is nothing left to target for" (asked 1) 0

  -- The question is asked exactly where the rules leave something to ask. Ghitu
  -- Fire-Eater's "{T}, Sacrifice this creature" has no variable in it, so no value
  -- is announced for it (CR 601.2b's clause is conditional on one).
  Spec.it s "CR 601.2b the value of X is asked for once, and only for a cost that has one" $ do
    (cinder, srcId, g1) <- cinderBoard s registry 4
    ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
    let (ghituId, g2) = S.addCreature ghitu S.alice g1
        asks oid ability = State.execState (Engine.runGame (countingX S.bob) g2 (Activate.activateAbility S.alice oid ability)) 0
    Spec.assertEqWith s "Cinder Elemental's {X}{R} is announced" (asks srcId (theAbility cinder)) 1
    Spec.assertEqWith s "the Fire-Eater's costs nothing to announce" (asks ghituId (theAbility ghitu)) 0

  -- CR 601.2b's X=0 FLOOR, which is what activatability can measure before any
  -- value exists: one Mountain pays {X}{R} at X=0 and the ability is on offer,
  -- while no Mountain pays the {R} and it is not. A floor that demanded {X} > 0
  -- would refuse the first board; a cost that never demanded the {X} at all would
  -- still accept the second.
  Spec.it s "CR 601.2b activatability is measured at the X=0 floor" $ do
    (cinder, srcId, one) <- cinderBoard s registry 1
    (_, noneId, none) <- cinderBoard s registry 0
    Spec.assertBool s (Activate.activatable S.alice srcId (theAbility cinder) one) "one Mountain admits X=0"
    Spec.assertBool s (not (Activate.activatable S.alice noneId (theAbility cinder) none)) "no Mountain pays even the {R}"

  -- Cinder Elemental READS the announced X in the instant the ability resolves;
  -- Tovolar's back face STORES it, and that is a second reader. CR 608.2h /
  -- 611.2d freeze a stored continuous effect's variable once, on resolution, and
  -- the value they freeze is the one CR 601.2b stamped on the object announcing
  -- it -- the ability on the stack, never CR 113.7a's source permanent, which
  -- never learned it.
  --
  -- The trample half of the same activation is the control: it carries no
  -- quantity, so it stored fine while the +X/+0 stored NOTHING AT ALL, which is
  -- what made the divergence silent -- the Wolf visibly gained trample and
  -- invisibly failed to grow.
  Spec.it s "CR 601.2b/611.2d an activated {X} pump freezes the announced X into the stored effect" $ do
    (tovolarId, wolfId, g1) <- tovolarNightBoard s registry
    Spec.assertEqWith s "Tovolar is showing his 4/4 back face, whose ability this is" (S.powerToughnessOf tovolarId g1) (Just (4, 4))
    Spec.assertEqWith s "and the Wolf starts a plain 3/3" (S.powerToughnessOf wolfId g1) (Just (3, 3))
    let after =
          snd
            ( Engine.runGamePure
                (answerXTargeting 2 wolfId)
                g1
                (do Activate.activateAbility S.alice tovolarId (shownAbility tovolarId g1); Stack.resolveTop)
            )
    Spec.assertEqWith s "the Wolf is a 5/3, so the announced 2 reached the stored pump" (S.powerToughnessOf wolfId after) (Just (5, 3))
    Spec.assertBool s (Projection.hasKeyword Keyword.Trample wolfId after) "and the quantity-free grant landed too"
    Spec.assertEqWith
      s
      "both halves are stored, the pump as a CR 611.2d Literal rather than a live X"
      (fmap ContinuousEffect.modification (GameState.continuousEffects after))
      [ Modification.GainKeyword Keyword.Trample,
        Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 0))
      ]
    Spec.assertEqWith s "all four lands paid the {2}{R}{G}" (S.tappedCount S.alice after) 4

-- Aims every target slot at one CREATURE. aimAt's counterpart for a board whose
-- point is that no PLAYER was targeted: CR 115.4's "any target" pool offers a
-- creature as Recipient.ToCreature, so a life total left at 20 is proof the
-- targeted instruction went elsewhere.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- alice's three untapped Mountains, `owner`'s Brothers of Fire and the other
-- player's Hill Giant, with `owner` holding priority. Returns the Brothers, the
-- Giant and the state.
brothersBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
brothersBoard brothers mountain hillGiant owner =
  let other = if owner == S.alice then S.bob else S.alice
      lands = List.foldl' (\g _ -> snd (S.addCreature mountain owner g)) (Setup.emptyGame S.bothPlayers) [1 :: Int .. 3]
      (srcId, g1) = S.addCreature brothers owner lands
      (giantId, g2) = S.addCreature hillGiant other g1
   in (srcId, giantId, g2 {GameState.priority = Just owner})

-- CR 109.5's ACTIVATED-ability sentence, on the path that had no answer for it
-- (#569): "The words 'you' and 'your' on an object refer to the object's
-- controller ... For an activated ability, this is the player who activated the
-- ability."
--
-- Brothers of Fire -- "{1}{R}{R}: This creature deals 1 damage to any target and
-- 1 damage to you" (checked against Scryfall) -- is the pool's first card whose
-- ACTIVATED ability names its controller through a reserved SLOT rather than
-- through an opcode carrying a PlayerRef: DealDamage takes an ObjectRef,
-- which reaches a player only through a bound recipient. CR 120.3a is what turns
-- that damage into the life loss these assertions read: "Damage dealt to a player
-- by a source without infect causes that player to lose that much life."
--
-- THE FALSIFIER for the group: an unbound `you` is a silent NO-OP rather than a
-- crash -- Resolve finds no recipient for the instruction and it does nothing --
-- so the controller sits at 20 while the targeted half resolves normally. Both
-- halves are asserted in every case, because a one-sided assertion would also
-- pass for an engine that aimed both instructions at the same recipient.
youOnActivatedAbilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
youOnActivatedAbilitySpec s registry = Spec.describe s "YouOnActivatedAbility" $ do
  -- The "any target" half is aimed at a CREATURE, never at a player, which is
  -- what makes the controller's life loss attributable: alice is not a recipient
  -- of the targeted instruction at all, so her 19 can only have come from the
  -- `you` one. Hill Giant is a 3/3, so it survives the ping and CR 120.3e's mark
  -- is readable straight off it.
  Spec.it s "CR 109.5/120.3a whole card: Brothers of Fire pings a creature and burns the player who activated it" $ do
    brothers <- S.printingOf s registry "Brothers of Fire"
    mountain <- S.printingOf s registry "Mountain"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (srcId, giantId, board) = brothersBoard brothers mountain hillGiant S.alice
        act = do Activate.activateAbility S.alice srcId (theAbility brothers); Stack.resolveTop
        after = snd (Engine.runGamePure (aimAtCreature giantId) board act)
    Spec.assertEqWith s "one damage marked on bob's Giant" (S.damageOf giantId after) (Just 1)
    Spec.assertEqWith s "and alice, who activated it, took the other one" (S.lifeOf S.alice after) (Just 19)
    Spec.assertEqWith s "bob was never a recipient of either half" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "all three Mountains paid the {1}{R}{R}" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "and nothing was left on the stack" (GameState.stack after) []

  -- WHICH player, asserted against the two constants that would pass the case
  -- above: the same card under bob's control burns BOB, while alice is still the
  -- active player and still at 20. An engine stamping GameState.activePlayer, or
  -- the game's first player, cannot pass both cases.
  Spec.it s "CR 109.5/602.2 the burn follows the player who activated it, not the active player" $ do
    brothers <- S.printingOf s registry "Brothers of Fire"
    mountain <- S.printingOf s registry "Mountain"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (srcId, giantId, board) = brothersBoard brothers mountain hillGiant S.bob
        act = do Activate.activateAbility S.bob srcId (theAbility brothers); Stack.resolveTop
        after = snd (Engine.runGamePure (aimAtCreature giantId) board act)
    Spec.assertEqWith s "alice is the active player throughout" (GameState.activePlayer after) S.alice
    Spec.assertEqWith s "one damage marked on alice's Giant" (S.damageOf giantId after) (Just 1)
    Spec.assertEqWith s "bob, who activated it, took the other one" (S.lifeOf S.bob after) (Just 19)
    Spec.assertEqWith s "and alice's life is untouched" (S.lifeOf S.alice after) (Just 20)

  -- The binding is on the ABILITY OBJECT, which is what lets it survive its
  -- source. CR 113.7a: "Once activated or triggered, an ability exists on the
  -- stack independently of its source." Asserted before resolution, so this reads
  -- the stamp itself rather than its consequence -- the twin of the CR 601.2b
  -- announced-X assertion above.
  Spec.it s "CR 109.5/113.7a the activator is bound on the ability object as it goes on the stack" $ do
    brothers <- S.printingOf s registry "Brothers of Fire"
    mountain <- S.printingOf s registry "Mountain"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (srcId, giantId, board) = brothersBoard brothers mountain hillGiant S.alice
        after = snd (Engine.runGamePure (aimAtCreature giantId) board (Activate.activateAbility S.alice srcId (theAbility brothers)))
    case GameState.stack after of
      [] -> Spec.assertFailure s "expected the ability on the stack"
      top : _ -> case Game.lookupObject top after of
        Nothing -> Spec.assertFailure s "stack id should resolve"
        Just obj -> do
          Spec.assertEqWith
            s
            "you names alice"
            (Map.lookup Binding.you (Object.bindings obj))
            (Just (Binding.toPlayer S.alice))
          Spec.assertEqWith
            s
            "and CR 113.7's source slot still names the Brothers"
            (Map.lookup Binding.triggerSource (Object.bindings obj))
            (Just (Binding.toObject srcId))

-- Answers the Hack: it targets `spellId` (rather than identityAnswer's
-- lowest-id land) and swaps `from` for `to`. Everything else falls through to
-- the identity, which aims the WARRIOR's own ability at the lowest-id land --
-- the Forest, added first for exactly that reason.
hackAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
hackAt spellId from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject spellId))) sets
  Prompt.ChooseLandTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- alice's board for the Tidal Warrior chain: a Forest (added FIRST, so it holds
-- the lowest object id and every unanswered ChooseTargets aims there), two
-- Islands for the two {U} costs, and Tidal Warrior plus Magical Hack in hand.
-- Returns the Forest, the two cards, and the state with alice holding priority.
tidalWarriorBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
tidalWarriorBoard forest island tidalWarrior magicalHack =
  let (forestId, g1) = S.addCreature forest S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addCreature island S.alice g1
      (_, g3) = S.addCreature island S.alice g2
      (warriorCardId, g4) = S.addHandCard tidalWarrior S.alice g3
      (hackId, g5) = S.addHandCard magicalHack S.alice g4
   in (forestId, warriorCardId, hackId, g5 {GameState.priority = Just S.alice})

-- The battlefield object alice controls that projects as a creature -- the
-- Warrior after its spell resolved. Its id is NOT the spell's: CR 400.7 minted a
-- new one, which is the whole point of the CR 400.7a case below.
soleCreatureOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe ObjectId.ObjectId
soleCreatureOf pid gs = case filter (`Projection.isCreatureOf` gs) (Game.zoneMembers Zone.Battlefield pid gs) of
  [only] -> Just only
  _ -> Nothing

-- The one activated ability the PROJECTION hands out for `oid` -- not
-- Face.activatedAbilities, which is the printed list a text change has not
-- reached. Projection.abilitiesGiven is the list Activate itself offers from, so
-- this is the same ability a player would be given.
soleProjectedAbility :: ObjectId.ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
soleProjectedAbility oid gs = case Projection.abilitiesOf oid gs of
  [only] -> Just only
  _ -> Nothing

-- CR 612.1 reaching an ACTIVATED ability, end to end through the real engine.
--
-- Tidal Warrior {U} Creature -- Merfolk Warrior 1/1, "{T}: Target land becomes
-- an Island until end of turn." (checked against Scryfall). Magical Hack changes
-- Island to Swamp, and the Warrior's ability must then make its target a Swamp.
--
-- CR 612.1: a text-changing effect "can apply to any words or symbols printed on
-- that object, but generally affects only that object's rules text (which
-- appears in its text box)". An activated ability is printed in that text box.
--
-- CR 113.7a is why the rewrite happens where the ability is ENUMERATED rather
-- than where it resolves: "once activated or triggered, an ability exists on the
-- stack independently of its source", so its text is fixed as it is put on the
-- stack. Pawl.ProjectionSpec's "hacking Tidal Warrior swaps the land type inside
-- its activated ability" is the projection-level half of this.
textChangedAbilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
textChangedAbilitySpec s registry = Spec.describe s "TextChangedActivatedAbility" $ do
  -- The control, and the one that needs no text change at all: unhacked, the
  -- printed word stands and the Forest becomes an Island.
  Spec.it s "CR 612 whole card: an unhacked Tidal Warrior makes its target an Island" $ do
    (forestId, after) <- tidalWarriorChain s registry Nothing
    Spec.assertEqWith s "the Forest became an Island" (Projection.subtypesOf forestId after) (Set.singleton Subtype.Island)

  -- Hacked on the PERMANENT, after its spell resolved: the plain CR 612.1 case.
  Spec.it s "CR 612.1 whole card: hacking the Tidal Warrior PERMANENT makes its target a Swamp" $ do
    (forestId, after) <- tidalWarriorChain s registry (Just OnThePermanent)
    Spec.assertEqWith s "the Forest became a Swamp" (Projection.subtypesOf forestId after) (Set.singleton Subtype.Swamp)

  -- Hacked on the SPELL, while it is still on the stack. CR 400.7a: "Effects
  -- from spells, activated abilities, and triggered abilities that change the
  -- characteristics or controller of a permanent spell on the stack continue to
  -- apply to the permanent that spell becomes" -- and rules text is a
  -- characteristic (CR 109.3). So the swap survives CR 400.7's new object and is
  -- still there when the ability is enumerated off the permanent. Wizards' own
  -- Magical Hack ruling says the same: "If you change the text of a spell which
  -- is to become a permanent, the permanent will retain the text change until
  -- the effect wears off."
  Spec.it s "CR 400.7a/612.1 whole card: hacking the Tidal Warrior SPELL makes its target a Swamp" $ do
    (forestId, after) <- tidalWarriorChain s registry (Just OnTheSpell)
    Spec.assertEqWith s "the Forest became a Swamp" (Projection.subtypesOf forestId after) (Set.singleton Subtype.Swamp)

-- Which incarnation of the Warrior the Hack is aimed at.
data HackTarget = OnTheSpell | OnThePermanent
  deriving (Eq, Ord, Show)

-- Cast Tidal Warrior; optionally cast Magical Hack (Island -> Swamp) at the
-- named incarnation; resolve the stack; settle alice's permanents (CR 302.6 --
-- running a whole turn to wear the sickness off would add nothing to what is
-- being proved here); activate the Warrior's projected ability, which the
-- identity answer aims at the lowest-id land (the Forest); resolve it. Returns
-- the Forest's id and the final state.
tidalWarriorChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe HackTarget -> m (ObjectId.ObjectId, GameState.GameState)
tidalWarriorChain s registry hackWhere = do
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  tidalWarrior <- S.printingOf s registry "Tidal Warrior"
  magicalHack <- S.printingOf s registry "Magical Hack"
  let (forestId, warriorCardId, hackId, g0) = tidalWarriorBoard forest island tidalWarrior magicalHack
      onStack = S.runPure S.identityAnswer g0 (S.cast S.alice warriorCardId)
      warriorSpellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      -- Hacking the SPELL: cast the Hack on top of the Warrior spell and resolve
      -- it, so the stored effect names the spell id. Then resolve the Warrior.
      hackedSpell = S.runPure (hackAt warriorSpellId Subtype.Island Subtype.Swamp) onStack (do S.cast S.alice hackId; Stack.resolveTop)
      resolveWarrior gs = S.runPure S.identityAnswer gs (do Stack.resolveTop; Engine.settleAll S.alice)
      -- Hacking the PERMANENT: resolve the Warrior first, then aim the Hack at
      -- the new incarnation.
      hackPermanent gs = case soleCreatureOf S.alice gs of
        Nothing -> gs
        Just permId -> S.runPure (hackAt permId Subtype.Island Subtype.Swamp) gs (do S.cast S.alice hackId; Stack.resolveTop)
      board = case hackWhere of
        Nothing -> resolveWarrior onStack
        Just OnTheSpell -> resolveWarrior hackedSpell
        Just OnThePermanent -> hackPermanent (resolveWarrior onStack)
  case soleCreatureOf S.alice board >>= \permId -> fmap ((,) permId) (soleProjectedAbility permId board) of
    Nothing -> Spec.assertFailure s "expected exactly one creature on the battlefield, carrying exactly one activated ability"
    Just (permId, ability) ->
      pure (forestId, S.runPure S.identityAnswer board (do Activate.activateAbility S.alice permId ability; Stack.resolveTop))

-- The board the two CR 612.1 cases below share: alice controls `subject`, one
-- Forest and one Island (both TAPPED), and a Seat of the Synod, and holds a
-- Magical Hack. Returns the subject's id, the Forest's, the Island's, the Hack's,
-- and the state with alice holding priority.
--
-- TWO distinct land types, and that is what makes either case discriminating: with
-- only a Forest on the board, "the Forest was hacked into an Island" and "the
-- filter was ignored" pick the same permanent.
--
-- Seat of the Synod ({T}: Add {U}, an Artifact Land with no land subtype at all,
-- checked against Scryfall) pays for the Hack rather than the Island. It is
-- neither a Forest nor an Island, so it can join neither the sacrifice candidates
-- nor the target set, and it leaves the two lands in the same state whether the
-- Hack was cast or not -- the hacked and unhacked runs are then a flip of one
-- fact rather than two different boards. It is also the ONLY untapped blue source,
-- so which permanent pays is not a coin toss.
textChangeBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
textChangeBoard subject forest island seat magicalHack =
  let (subjectId, g1) = S.addCreature subject S.alice (Setup.emptyGame S.bothPlayers)
      (forestId, g2) = S.addCreature forest S.alice g1
      (islandId, g3) = S.addCreature island S.alice g2
      (_, g4) = S.addCreature seat S.alice g3
      (hackId, g5) = S.addHandCard magicalHack S.alice g4
      g6 = S.tapObject islandId (S.tapObject forestId g5)
   in (subjectId, forestId, islandId, hackId, g6 {GameState.priority = Just S.alice})

-- Cast the Hack at `subjectId`, swapping Forest for Island, and resolve it -- or,
-- unhacked, hand the board straight back.
withForestHackedToIsland :: Bool -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withForestHackedToIsland hacked subjectId hackId gs =
  if hacked
    then S.runPure (hackAt subjectId Subtype.Forest Subtype.Island) gs (do S.cast S.alice hackId; Stack.resolveTop)
    else gs

-- alice activates `oid`'s one PROJECTED ability and the stack resolves. The
-- ability comes off Projection.abilitiesOf rather than Face.activatedAbilities,
-- so it is the text-changed one a player would be offered.
activateSole :: (Monad m) => Spec.Spec m n -> ObjectId.ObjectId -> GameState.GameState -> m GameState.GameState
activateSole s oid gs = case soleProjectedAbility oid gs of
  Nothing -> Spec.assertFailure s "expected the permanent to carry exactly one activated ability"
  Just ability -> do
    Spec.assertBool s (Activate.activatable S.alice oid ability gs) "the ability is activatable"
    pure (S.runPure S.identityAnswer gs (do Activate.activateAbility S.alice oid ability; Stack.resolveTop))

-- CR 612.1 reaching an activated ability's ACTIVATION COST, end to end.
--
-- Dark Heart of the Wood {B}{G} Enchantment, "Sacrifice a Forest: You gain 3
-- life." (checked against Scryfall). Magical Hack changes Forest to Island, and
-- the cost must then demand the Island.
--
-- CR 612.1: a text-changing effect "can apply to any words or symbols printed on
-- that object, but generally affects only that object's rules text (which appears
-- in its text box)". CR 118.1 makes the activation cost part of what is printed
-- there, and CR 602.2a is why fixing the projection fixes the PAYMENT: the ability
-- on the stack "has the text of the ability that created it".
--
-- An enchantment rather than a creature deliberately: CR 302.6's summoning
-- sickness gates no ability here, so nothing but the swap can decide which land
-- dies.
textChangedCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
textChangedCostSpec s registry =
  let forestName = CardName.MkCardName (Text.pack "Forest")
      islandName = CardName.MkCardName (Text.pack "Island")
      run hacked = do
        darkHeart <- S.printingOf s registry "Dark Heart of the Wood"
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        seat <- S.printingOf s registry "Seat of the Synod"
        magicalHack <- S.printingOf s registry "Magical Hack"
        let (subjectId, _, _, hackId, g0) = textChangeBoard darkHeart forest island seat magicalHack
            board = withForestHackedToIsland hacked subjectId hackId g0
        after <- activateSole s subjectId board
        pure (S.lifeOf S.alice board, after)
   in Spec.describe s "TextChangedActivationCost" $ do
        -- The control: unhacked, the printed word stands and the FOREST is the
        -- only thing that can pay.
        Spec.it s "CR 118.1 whole card: an unhacked Dark Heart of the Wood sacrifices the Forest" $ do
          (before, after) <- run False
          Spec.assertEqWith s "alice gained 3 life" (S.lifeOf S.alice after) (fmap (+ 3) before)
          Spec.assertEqWith s "the Forest is gone" (S.countOnBattlefieldByName forestName S.alice after) 0
          Spec.assertEqWith s "the Island survives" (S.countOnBattlefieldByName islandName S.alice after) 1
        -- The swap. alice's board did not move -- the same Forest and the same
        -- Island -- but the cost printed on the Dark Heart now reads "Sacrifice an
        -- Island", so the Island is what dies and the Forest is not even eligible.
        Spec.it s "CR 612.1 whole card: hacking Dark Heart of the Wood moves which land its cost demands" $ do
          (before, after) <- run True
          Spec.assertEqWith s "alice gained 3 life" (S.lifeOf S.alice after) (fmap (+ 3) before)
          Spec.assertEqWith s "the Island is gone" (S.countOnBattlefieldByName islandName S.alice after) 0
          Spec.assertEqWith s "the Forest survives" (S.countOnBattlefieldByName forestName S.alice after) 1

-- CR 612.1 reaching a mode's TARGET SLOT, end to end.
--
-- Arbor Elf {G} Creature -- Elf Druid 1/1, "{T}: Untap target Forest." (checked
-- against Scryfall). Magical Hack changes Forest to Island, and the ability must
-- then be able to target only the Island.
--
-- CR 601.2c, imported for an activated ability by CR 602.2b, is the step whose
-- candidate set the target slot defines, and CR 602.2a puts the projected text on
-- the ability object that does the untapping.
--
-- Arbor Elf IS a creature and its cost IS the tap symbol, so CR 302.6 gates it,
-- and `activateSole`'s Activate.activatable assertion is what keeps that gate
-- honest here. Measured, not assumed: landing the Elf Sickness.Sick leaves BOTH
-- cases below green once that one assertion is dropped, because
-- Activate.activateAbility trusts its caller and re-checks nothing (the gate lives
-- on the enumeration path, with `activatable`). So the tap states alone would not
-- have noticed a creature that could not legally have been activated at all.
--
-- Both lands start TAPPED, so "the ability never resolved" and "it untapped the
-- other land" are distinguishable board states.
textChangedTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
textChangedTargetSpec s registry =
  let untapped oid gs = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
      -- The legal recipients of the projected ability's one mode, as CR 601.2c
      -- would offer them. A candidate-set assertion alone can pass off a list
      -- nothing consumed, so each case below pairs it with the tap states.
      candidates elfId gs = case soleProjectedAbility elfId gs of
        Nothing -> []
        Just ability -> case Seq.lookup 0 (Modal.modes (ActivatedAbility.modal ability)) of
          Nothing -> []
          Just mode -> fmap Set.toList (Map.elems (Target.legalSets (Just S.alice) Map.empty elfId (Mode.targetSlots mode) gs))
      run hacked = do
        arborElf <- S.printingOf s registry "Arbor Elf"
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        seat <- S.printingOf s registry "Seat of the Synod"
        magicalHack <- S.printingOf s registry "Magical Hack"
        let (elfId, forestId, islandId, hackId, g0) = textChangeBoard arborElf forest island seat magicalHack
            board = withForestHackedToIsland hacked elfId hackId g0
        after <- activateSole s elfId board
        pure (elfId, forestId, islandId, board, after)
   in Spec.describe s "TextChangedTargetSpec" $ do
        -- The control: unhacked, "target Forest" admits the Forest alone, and the
        -- Forest is what wakes up.
        Spec.it s "CR 601.2c whole card: an unhacked Arbor Elf may untap only the Forest" $ do
          (elfId, forestId, islandId, board, after) <- run False
          Spec.assertEqWith s "only the Forest is a legal target" (candidates elfId board) [[Recipient.ToObject forestId]]
          Spec.assertBool s (untapped forestId after) "the Forest is untapped"
          Spec.assertBool s (not (untapped islandId after)) "the Island is still tapped"
        -- The swap: the printed "target Forest" now reads "target Island", so the
        -- Forest drops out of the candidate set entirely and the Island wakes up
        -- instead.
        Spec.it s "CR 612.1 whole card: hacking Arbor Elf moves which land its ability may target" $ do
          (elfId, forestId, islandId, board, after) <- run True
          Spec.assertEqWith s "only the Island is a legal target" (candidates elfId board) [[Recipient.ToObject islandId]]
          Spec.assertBool s (untapped islandId after) "the Island is untapped"
          Spec.assertBool s (not (untapped forestId after)) "the Forest is still tapped"

-- Alice with two untapped Swamps and one Reassembling Skeleton in her graveyard,
-- holding priority. Returns the graveyard card's id.
--
-- Two Swamps because the activation cost is {1}{B}; the Skeleton's own {1}{B} is
-- never paid, since it is in the graveyard rather than being cast.
skeletonBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
skeletonBoard skeleton swamp =
  let (gyId, withCard) = S.addGraveyardCard skeleton S.alice (S.landsInPlay swamp 2)
   in (gyId, withCard {GameState.priority = Just S.alice})

-- Is this object a Reassembling Skeleton? CR 400.7 mints a fresh id when the
-- card leaves the graveyard, so the returned permanent has to be found by name
-- rather than by the id the ability was activated from.
namedSkeleton :: GameState.GameState -> ObjectId.ObjectId -> Bool
namedSkeleton gs oid = case Game.faceOf oid gs of
  Nothing -> False
  Just face -> Face.name face == CardName.MkCardName (Text.pack "Reassembling Skeleton")

-- CR 113.6m's "or effect" half: "an ability whose cost OR EFFECT specifies that
-- it moves the object it's on out of a particular zone functions only in that
-- zone."
--
-- Reassembling Skeleton {1}{B} Creature -- Skeleton Warrior 1/1, "{1}{B}: Return
-- this card from your graveyard to the battlefield tapped." (checked against
-- Scryfall). Its cost is mana and nothing else, so Cost.zoneFunctionedFrom -- the
-- whole of pawl's CR 113.6m reading before this -- answers Nothing for it, and
-- the graveyard is named only by what the ability DOES. Loxodon Surveyor proves
-- the cost half in Pawl.SpeedSpec; this is the other half of the same sentence,
-- and the two cards divide it cleanly.
graveyardEffectZoneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graveyardEffectZoneSpec s registry = Spec.describe s "GraveyardEffectZone" $ do
  Spec.it s "CR 113.6m the Skeleton's ability is offered from the graveyard, on its effect's word alone" $ do
    skeleton <- S.printingOf s registry "Reassembling Skeleton"
    swamp <- S.printingOf s registry "Swamp"
    let (gyId, gs) = skeletonBoard skeleton swamp
    Spec.assertEqWith s "the card really is in the graveyard" (Game.zoneMembers Zone.Graveyard S.alice gs) [gyId]
    case Activate.abilitiesFor gyId gs of
      [ability] ->
        -- THE control this whole unit turns on: nothing in the cost names a zone,
        -- so the cost half cannot be what offered the ability. If this ever
        -- answers Just, the two cases below stop proving the effect half.
        Spec.assertEqWith s "and its cost names no zone (CR 113.6m's other half)" (Cost.zoneFunctionedFrom (ActivatedAbility.cost ability)) Nothing
      abilities -> Spec.assertEqWith s "exactly one ability from the graveyard" (length abilities) 1
    Spec.assertBool s (any (isActivationOf gyId) (Action.legalActions S.alice gs)) "and the activation is a legal action"
  -- The other direction of "functions ONLY in that zone", and the case the cost
  -- half got for free: a Loxodon Surveyor on the battlefield is withheld by its
  -- own unpayable cost, while this ability's {1}{B} is payable anywhere. So the
  -- zone gate is the only thing that can withhold it, and the board is built to
  -- make that the sole difference -- the same two Swamps, the same priority.
  Spec.it s "CR 113.6m the same card on the battlefield offers the ability to nobody" $ do
    skeleton <- S.printingOf s registry "Reassembling Skeleton"
    swamp <- S.printingOf s registry "Swamp"
    let (bfId, board) = S.addCreature skeleton S.alice (S.landsInPlay swamp 2)
        gs = board {GameState.priority = Just S.alice}
    Spec.assertEqWith s "the projection does hand it out" (length (Projection.abilitiesOf bfId gs)) 1
    Spec.assertBool s (not (any (isActivationOf bfId) (Action.legalActions S.alice gs))) "but no activation is offered"
  -- End to end through the real engine: the activation is announced, the {1}{B}
  -- is paid off the two Swamps, and CR 400.7's funnel puts the card onto the
  -- battlefield TAPPED (CR 110.5b, the rider the card prints). The falsifier for
  -- a gate that offered the action and could not carry it out.
  Spec.it s "CR 113.6m whole card: activating it returns the Skeleton tapped" $ do
    skeleton <- S.printingOf s registry "Reassembling Skeleton"
    swamp <- S.printingOf s registry "Swamp"
    let (gyId, gs) = skeletonBoard skeleton swamp
    case Activate.abilitiesFor gyId gs of
      [ability] -> do
        let after = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice gyId ability >> Stack.resolveTop)
            skeletons = filter (namedSkeleton after) (Game.zoneMembers Zone.Battlefield S.alice after)
        Spec.assertEqWith s "the graveyard is empty" (Game.zoneMembers Zone.Graveyard S.alice after) []
        Spec.assertEqWith s "and a Skeleton is on the battlefield instead" (length skeletons) 1
        -- CR 110.5b, the rider the card prints: it comes back TAPPED. The two
        -- Swamps are tapped too, having paid the {1}{B}, which is why this asks
        -- about the Skeleton by name rather than about the battlefield at large.
        Spec.assertEqWith s "returned tapped (CR 110.5b)" (fmap (\oid -> fmap Object.tapped (Game.lookupObject oid after)) skeletons) [Just TapState.Tapped]
      abilities -> Spec.assertEqWith s "exactly one ability to activate" (length abilities) 1

-- alice with Jarad, Golgari Lich Lord in her graveyard, one untapped Bayou, and
-- one extra land per printing in `extras`, holding priority.
--
-- Two boards that differ ONLY by that extra land is the whole design of this
-- group: the negative assertion below is "the ability is not offered", which
-- passes for a dozen unrelated reasons, and only a positive control drawn from
-- the same call on the same board plus one Forest discriminates.
jaradBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
jaradBoard jarad bayou extras =
  let add gs printing = snd (S.addCreature printing S.alice gs)
      withExtras = List.foldl' add (S.landsInPlay bayou 1) extras
      (jaradId, withJarad) = S.addGraveyardCard jarad S.alice withExtras
   in (jaradId, withJarad {GameState.priority = Just S.alice})

-- Is this object Jarad? CR 400.7 mints a fresh id when the card leaves the
-- graveyard, so the returned card has to be found by name -- namedSkeleton's
-- reason, one card over.
namedJarad :: GameState.GameState -> ObjectId.ObjectId -> Bool
namedJarad gs oid = case Game.faceOf oid gs of
  Nothing -> False
  Just face -> Face.name face == CardName.MkCardName (Text.pack "Jarad, Golgari Lich Lord")

-- CR 118.3 at the MENU, which is this unit's observable: an ability whose cost
-- cannot be paid in full is never offered.
--
-- Jarad, Golgari Lich Lord {B}{B}{G}{G} Legendary Creature -- Zombie Elf 2/2,
-- "Sacrifice a Swamp and a Forest: Return this card from your graveyard to your
-- hand" (Oracle text checked against Scryfall). A Bayou is `Land -- Forest
-- Swamp`, so one permanent matches both components and a per-component gate
-- offers the activation off it alone. Pawl.CostSpec proves the same refusal at
-- Cost.canPay; this is the same rule at gameplay level, through
-- Action.legalActions.
--
-- Not implemented: Jarad's other activated ability, "{1}{B}{G}, Sacrifice
-- another creature: Each opponent loses life equal to the sacrificed creature's
-- power" -- no quantity can read the power of a permanent sacrificed to pay a
-- COST, so the card file omits the ability (#1061).
twoSacrificeComponentSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
twoSacrificeComponentSpec s registry = Spec.describe s "TwoSacrificeComponents" $ do
  Spec.it s "CR 118.3 with one Bayou the activation is not offered, and a Forest offers it" $ do
    jarad <- S.printingOf s registry "Jarad, Golgari Lich Lord"
    bayou <- S.printingOf s registry "Bayou"
    forest <- S.printingOf s registry "Forest"
    let (loneId, lone) = jaradBoard jarad bayou []
        (pairId, pair) = jaradBoard jarad bayou [forest]
    -- The vacuity guard: on the lone-Bayou board CR 113.6m DID hand the ability
    -- out from the graveyard, so what withholds the action is the cost gate and
    -- not the zone gate. Without this, a typo in the card's origin zone would
    -- make the negative below pass and prove nothing.
    Spec.assertEqWith s "the ability is offered from the graveyard" (length (Activate.abilitiesFor loneId lone)) 1
    Spec.assertBool s (not (any (isActivationOf loneId) (Action.legalActions S.alice lone))) "one Bayou offers no activation"
    Spec.assertBool s (any (isActivationOf pairId) (Action.legalActions S.alice pair)) "a Forest beside it does"
  -- End to end through the real engine: the activation is announced, both lands
  -- pay the cost, and CR 400.7's funnel puts Jarad into alice's hand. The
  -- falsifier for a gate that offered the action and could not carry it out.
  Spec.it s "CR 118.3 whole card: activating it returns Jarad to hand and eats both lands" $ do
    jarad <- S.printingOf s registry "Jarad, Golgari Lich Lord"
    bayou <- S.printingOf s registry "Bayou"
    forest <- S.printingOf s registry "Forest"
    let (jaradId, gs) = jaradBoard jarad bayou [forest]
    case Activate.abilitiesFor jaradId gs of
      [ability] -> do
        let after = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice jaradId ability >> Stack.resolveTop)
            jarads = filter (namedJarad after) (Game.zoneMembers Zone.Hand S.alice after)
        Spec.assertEqWith s "Jarad is in alice's hand" (length jarads) 1
        Spec.assertBool s (not (any (namedJarad after) (Game.zoneMembers Zone.Graveyard S.alice after))) "and not still in the graveyard"
        Spec.assertEqWith s "both lands paid the cost" (length (GameState.battlefield after)) 0
        Spec.assertEqWith s "and both are in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
      abilities -> Spec.assertEqWith s "exactly one ability to activate" (length abilities) 1

-- CR 702.107: outlast, an activated ability rule 702 states in full and
-- Pawl.Engine.Keyword.outlast mints. Disowned Ancestor is the fixture: a {B} 0/4
-- Spirit Warrior whose ENTIRE printed text is "Outlast {1}{B}", so nothing else
-- it prints can make a case pass, and whose 0/4 makes the counter observable as
-- 1/5 -- a pair of numbers no other reading of the board produces.
--
-- Three of the rule's clauses are the engine's rather than the card's, and each
-- has its own case below: the {T} in the cost (so CR 302.6 reaches it), the
-- counter, and CR 602.5d's sorcery-speed window.
outlastBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m (ObjectId.ObjectId, GameState.GameState)
outlastBoard s registry lands = do
  ancestor <- S.printingOf s registry "Disowned Ancestor"
  swamp <- S.printingOf s registry "Swamp"
  let (oid, g0) = S.addCreature ancestor S.alice (S.landsInPlay swamp lands)
  pure (oid, g0 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain})

-- The +1/+1 counters on a permanent, 0 if it has none.
plusOnesOn :: ObjectId.ObjectId -> GameState.GameState -> Natural
plusOnesOn oid gs = case Game.lookupObject oid gs of
  Nothing -> 0
  Just o -> Map.findWithDefault 0 CounterKind.PlusOnePlusOne (Object.counters o)

outlastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
outlastSpec s registry = Spec.describe s "Outlast" $ do
  -- The ability rule 702.107a MEANS, rather than any the card prints: the
  -- Ancestor's own activatedAbilities list is empty, and what is offered is the
  -- printed {1}{B} with the rule's {T} appended and the rule's timing clause on
  -- it.
  Spec.it s "CR 702.107a the minted ability is '{1}{B}, {T}:' at sorcery speed" $ do
    ancestor <- S.printingOf s registry "Disowned Ancestor"
    (oid, gs) <- outlastBoard s registry 2
    Spec.assertEqWith s "the card itself prints no activated ability" (Face.activatedAbilities (S.combinedFace ancestor)) []
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        Spec.assertEqWith
          s
          "the printed {1}{B} plus rule 702.107a's tap symbol"
          (ActivatedAbility.cost ability)
          (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Black)])) [CostComponent.TapThis])
        Spec.assertEqWith
          s
          "CR 602.5d's clause and nothing else"
          (ActivatedAbility.restrictions ability)
          [ActivationRestriction.SorcerySpeed]
      abilities -> Spec.assertFailure s ("expected exactly one ability, got " <> show (length abilities))

  -- The whole card. 0/4 with one +1/+1 counter is 1/5 (CR 122.1a / 613.4c), which
  -- is neither the printed pair nor any other counter count.
  Spec.it s "CR 702.107a whole card: outlast taps the Ancestor and grows it to 1/5" $ do
    (oid, gs) <- outlastBoard s registry 2
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability)
            resolved = S.runPure S.identityAnswer activated Stack.resolveTop
            tapped g = length (filter (\o -> Object.tapped o == TapState.Tapped) (Maybe.mapMaybe (\o -> Game.lookupObject o g) (Set.toList (GameState.battlefield g))))
        Spec.assertEqWith s "0/4 before" (S.powerToughnessOf oid gs) (Just (0, 4))
        Spec.assertEqWith s "the ability is on the stack" (length (GameState.stack activated)) 1
        Spec.assertEqWith s "the Ancestor tapped itself as the cost was paid" (fmap Object.tapped (Game.lookupObject oid activated)) (Just TapState.Tapped)
        Spec.assertEqWith s "and no counter yet -- it is in the EFFECT, not the cost" (plusOnesOn oid activated) 0
        Spec.assertEqWith s "the Ancestor and both Swamps are tapped" (tapped activated) 3
        Spec.assertEqWith s "one +1/+1 counter once it resolves" (plusOnesOn oid resolved) 1
        Spec.assertEqWith s "so 1/5" (S.powerToughnessOf oid resolved) (Just (1, 5))
      abilities -> Spec.assertFailure s ("expected exactly one ability, got " <> show (length abilities))

  -- CR 602.5d, through CR 307.5's three conjuncts. One board with the mana for
  -- the cost, varied in nothing but the window, so a negative cannot be the cost
  -- gate refusing instead.
  Spec.it s "CR 602.5d outlast is offered only in your own main phase with an empty stack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    (oid, gs) <- outlastBoard s registry 2
    let (spellId, withSpell) = S.spellOnStack piker S.alice gs
    Spec.assertBool s (not (null (activationsOf oid (Action.legalActions S.alice gs)))) "offered in her precombat main phase"
    Spec.assertEqWith s "not during combat" (activationsOf oid (Action.legalActions S.alice gs {GameState.phase = Phase.Combat CombatStep.DeclareBlockers})) []
    Spec.assertEqWith s "not on bob's turn" (activationsOf oid (Action.legalActions S.alice gs {GameState.activePlayer = S.bob})) []
    Spec.assertBool s (elem spellId (GameState.stack withSpell)) "the stack really is occupied"
    Spec.assertEqWith s "not with a spell on the stack" (activationsOf oid (Action.legalActions S.alice withSpell)) []

  -- CR 302.6 reaches this ability and not crew's, because rule 702.107a puts the
  -- tap symbol in the cost of the creature's OWN ability. The control is the same
  -- board with the same two Swamps, settled.
  Spec.it s "CR 302.6 an Ancestor that arrived this turn cannot outlast" $ do
    (oid, gs) <- outlastBoard s registry 2
    let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
    Spec.assertEqWith s "the ability is minted all the same" (length (Activate.abilitiesFor oid sick)) 1
    Spec.assertEqWith s "but not offered" (activationsOf oid (Action.legalActions S.alice sick)) []
    Spec.assertBool s (not (null (activationsOf oid (Action.legalActions S.alice gs)))) "and offered once it has settled"

  -- CR 118.3: the printed half of the cost still has to be payable. One Swamp is
  -- not {1}{B}.
  Spec.it s "CR 118.3 outlast is not offered without the mana" $ do
    (oneId, one) <- outlastBoard s registry 1
    (twoId, two) <- outlastBoard s registry 2
    Spec.assertEqWith s "one Swamp cannot pay {1}{B}" (activationsOf oneId (Action.legalActions S.alice one)) []
    Spec.assertBool s (not (null (activationsOf twoId (Action.legalActions S.alice two)))) "two can"

  -- CR 113.6: rule 702.107a's ability functions on the battlefield, cycling's one
  -- zone over. A card in hand offers nothing.
  Spec.it s "CR 702.107a outlast is NOT offered from the hand" $ do
    ancestor <- S.printingOf s registry "Disowned Ancestor"
    swamp <- S.printingOf s registry "Swamp"
    let (g0, oid) = S.handOne ancestor (S.landsInPlay swamp 2)
        gs = g0 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain}
    Spec.assertEqWith s "no ability in a hand" (Activate.abilitiesFor oid gs) []
    Spec.assertEqWith s "and no activation offered" (activationsOf oid (Action.legalActions S.alice gs)) []

-- `owner`'s board: `lands` Mountains and the permanent whose ability is under
-- test, plus Heartstone under ALICE's control when one is passed. The positive and
-- the negative differ in that Maybe and in nothing else -- same seats, same mana,
-- same permanent, same priority -- which is what makes a tapped-land count
-- attributable to the reduction.
--
-- Heartstone sits with alice however `owner` reads, because its sentence is
-- symmetric ("activated abilities of creatures", PlayerScope.EachPlayer): the
-- board that proves that is the one where the activating player does not control
-- it.
heartstoneBoard ::
  Printing.Printing ->
  Int ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  Maybe Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
heartstoneBoard mountain lands source owner mHeartstone =
  let base = List.foldl' (\g _ -> snd (S.addCreature mountain owner g)) (Setup.emptyGame S.bothPlayers) [1 .. lands]
      (srcId, g1) = S.addCreature source owner base
      g2 = maybe g1 (\heartstone -> snd (S.addCreature heartstone S.alice g1)) mHeartstone
   in (srcId, g2 {GameState.priority = Just owner})

-- CR 601.2f reaching an ACTIVATION cost (#90), which nothing could do before:
-- Heartstone -- "Activated abilities of creatures cost {1} less to activate. This
-- effect can't reduce the mana in that cost to less than one mana" (checked
-- against Scryfall) -- is the first card in the pool that raises or lowers one.
--
-- Slivdrazi Monstrosity's "{3}: Create a 1/1 colorless Eldrazi Sliver creature
-- token" is the ability measured, because {3} and the {2} it reduces to are
-- DIFFERENT NUMBERS of lands:
-- an assertion on a cost the reduction rounded to the same figure would pass
-- either way.
--
-- THE FALSIFIER for the group is Mindslaver, on the same board as the positive: it
-- is a noncreature permanent with a {4} activation cost, so Heartstone's criterion
-- must turn it away -- and the criterion is all that can, since
-- PlayerEffect.matchesObject reads an OBJECT and a noncreature permanent matches
-- "noncreature" as readily as a noncreature spell does. That is the same trap
-- Pawl.CostSpec's Thalia case pins from the increase direction.
activationCostReductionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
activationCostReductionSpec s registry = Spec.describe s "ActivationCostReduction" $ do
  -- The gate and the payment, on one pair of boards. Two Mountains cannot pay the
  -- printed {3} and can pay the reduced {2}, so `activatable` flips; and the
  -- activation that goes through taps two lands out of three rather than all
  -- three, which is the reduction being PAID rather than merely measured.
  Spec.it s "CR 601.2f Heartstone reduces a creature's activation cost by {1}" $ do
    mountain <- S.printingOf s registry "Mountain"
    heartstone <- S.printingOf s registry "Heartstone"
    slivdrazi <- S.printingOf s registry "Slivdrazi Monstrosity"
    let ability = theAbility slivdrazi
        (twoWith, withStone) = heartstoneBoard mountain 2 slivdrazi S.alice (Just heartstone)
        (twoWithout, withoutStone) = heartstoneBoard mountain 2 slivdrazi S.alice Nothing
        (threeWith, threeWithStone) = heartstoneBoard mountain 3 slivdrazi S.alice (Just heartstone)
        (threeWithout, threeWithoutStone) = heartstoneBoard mountain 3 slivdrazi S.alice Nothing
        paid srcId gs = S.tappedCount S.alice (S.runPure S.identityAnswer gs (Activate.activateAbility S.alice srcId ability))
    Spec.assertBool s (Activate.activatable S.alice twoWith ability withStone) "two Mountains pay the reduced {2}"
    Spec.assertBool s (not (Activate.activatable S.alice twoWithout ability withoutStone)) "and cannot pay the printed {3}"
    Spec.assertEqWith s "two of the three Mountains paid it" (paid threeWith threeWithStone) 2
    Spec.assertEqWith s "where all three pay the printed cost" (paid threeWithout threeWithoutStone) 3
    -- The activation's OWN re-check (CR 602.2's "an activation a player cannot
    -- comply with is illegal") measures the same total: on two Mountains the
    -- printed {3} is unpayable, so an activation gated on the printed cost would
    -- be a no-op with nothing tapped rather than a paid ability on the stack.
    let activated = S.runPure S.identityAnswer withStone (Activate.activateAbility S.alice twoWith ability)
    Spec.assertEqWith s "both Mountains paid the reduced cost" (S.tappedCount S.alice activated) 2
    Spec.assertEqWith s "and the ability is on the stack" (length (GameState.stack activated)) 1

  -- Heartstone's own criterion, asserted on the SAME board as a reduction that
  -- works: three Mountains under Heartstone pay Slivdrazi's reduced {3} and do not
  -- pay Mindslaver's {4}, which they would if the reduction reached a noncreature
  -- permanent's ability.
  Spec.it s "CR 601.2f Heartstone does not reduce a noncreature permanent's activation cost" $ do
    mountain <- S.printingOf s registry "Mountain"
    heartstone <- S.printingOf s registry "Heartstone"
    slivdrazi <- S.printingOf s registry "Slivdrazi Monstrosity"
    mindslaver <- S.printingOf s registry "Mindslaver"
    let (creatureId, board) = heartstoneBoard mountain 3 slivdrazi S.alice (Just heartstone)
        (slaverId, withSlaver) = S.addCreature mindslaver S.alice board
    Spec.assertBool s (Activate.activatable S.alice creatureId (theAbility slivdrazi) withSlaver) "the creature's ability is reduced"
    Spec.assertBool s (not (Activate.activatable S.alice slaverId (theAbility mindslaver) withSlaver)) "and the artifact's {4} is not"

  -- The floor, which is card text (CR 101.1) and not a rule: Withered Wretch's
  -- "{1}: Exile target card from a graveyard" would be free without it. Nothing to
  -- exile is not what stops it -- bob's graveyard holds a card, so the target is
  -- fillable on both boards -- and the two differ only in whether alice has a
  -- Mountain.
  Spec.it s "Heartstone can't reduce the mana in that cost to less than one mana" $ do
    mountain <- S.printingOf s registry "Mountain"
    heartstone <- S.printingOf s registry "Heartstone"
    wretch <- S.printingOf s registry "Withered Wretch"
    piker <- S.printingOf s registry "Goblin Piker"
    let ability = theAbility wretch
        stocked lands =
          let (srcId, gs) = heartstoneBoard mountain lands wretch S.alice (Just heartstone)
           in (srcId, snd (S.addGraveyardCard piker S.bob gs))
        (noLandId, noLand) = stocked 0
        (oneLandId, oneLand) = stocked 1
    Spec.assertBool s (not (Activate.activatable S.alice noLandId ability noLand)) "the {1} is not reduced to {0}"
    Spec.assertBool s (Activate.activatable S.alice oneLandId ability oneLand) "and one Mountain still pays it"
    Spec.assertEqWith
      s
      "which is what gets tapped"
      (S.tappedCount S.alice (S.runPure S.identityAnswer oneLand (Activate.activateAbility S.alice oneLandId ability)))
      1

  -- CR 613.11 with PlayerScope.EachPlayer: Heartstone reduces the abilities of
  -- EVERY player's creatures, so the read site has to consult it for an activation
  -- its controller is not making. bob's three Mountains pay two, alice's Heartstone
  -- untouched -- and bob's own board is the one that would still cost {3} under
  -- Training Grounds' "creatures you control".
  Spec.it s "CR 613.11 Heartstone's reduction is symmetric" $ do
    mountain <- S.printingOf s registry "Mountain"
    heartstone <- S.printingOf s registry "Heartstone"
    slivdrazi <- S.printingOf s registry "Slivdrazi Monstrosity"
    let ability = theAbility slivdrazi
        (srcId, board) = heartstoneBoard mountain 3 slivdrazi S.bob (Just heartstone)
        after = S.runPure S.identityAnswer board (Activate.activateAbility S.bob srcId ability)
    Spec.assertBool s (Activate.activatable S.bob srcId ability board) "bob's creature is reduced too"
    Spec.assertEqWith s "two of bob's three Mountains paid it" (S.tappedCount S.bob after) 2
    Spec.assertEqWith s "and alice tapped nothing" (S.tappedCount S.alice after) 0

-- Mutavault's SECOND printed ability, the {1} animation. `theAbility` takes the
-- first, which here is the mana ability CR 605.3b keeps off the stack -- and one
-- with no mana in its cost, so nothing could be measured on it.
animationAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
animationAbility p = case drop 1 (Face.activatedAbilities (S.combinedFace p)) of
  ab : _ -> ab
  [] -> theAbility p

-- alice's board: Mutavault, `lands` Mountains and Heartstone, plus Blossoming
-- Tortoise when one is passed. The positive and the negative differ in that Maybe
-- and in nothing else -- same seats, same lands, same permanents, same priority --
-- which is what makes a tapped count attributable to the second reduction.
mutavaultBoard ::
  Printing.Printing ->
  Int ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
mutavaultBoard mountain lands mutavault heartstone mTortoise =
  let base = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) (Setup.emptyGame S.bothPlayers) [1 .. lands]
      (srcId, g1) = S.addCreature mutavault S.alice base
      g2 = snd (S.addCreature heartstone S.alice g1)
      g3 = maybe g2 (\tortoise -> snd (S.addCreature tortoise S.alice g2)) mTortoise
   in (srcId, g3 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice})

-- CR 601.2f's floor read PER REDUCING EFFECT rather than as a clamp on the pooled
-- result (#1243). The two readings agree on every board with one reduction, and on
-- every board whose reductions all state the same floor, so what tells them apart
-- is a FLOORED reduction beside an UNFLOORED one on one cost.
--
-- Heartstone {3} Artifact -- "Activated abilities of creatures cost {1} less to
-- activate. This effect can't reduce the mana in that cost to less than one mana"
-- -- is the floored half. Blossoming Tortoise {2}{G}{G} Creature -- Turtle 3/3 --
-- "Activated abilities of lands you control cost {1} less to activate" is the
-- unfloored half; it states no such sentence, so nothing forbids it taking the
-- mana Heartstone's floor left behind. Mutavault's "{1}: This land becomes a 2/2
-- creature with all creature types until end of turn. It's still a land" is the
-- cost both reduce, and the animation is what puts one permanent inside both
-- criteria at once. All three Oracle texts checked against api.scryfall.com.
--
-- The NUMBERS are why the board is honest: the printed {1} is at most what either
-- reduction takes, so the two orders CR 601.2f leaves the player agree -- the
-- Tortoise alone empties the cost, and Heartstone applied to an empty cost adds
-- nothing back (its own ruling) -- and the assertion is not reading pawl's choice
-- of order. A pooled clamp answers {1} on the same board, which is a different
-- number of lands.
unflooredActivationCostReductionSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
unflooredActivationCostReductionSpec s registry = Spec.describe s "UnflooredActivationCostReduction" $ do
  Spec.it s "CR 601.2f an unfloored reduction takes the mana a floored one may not" $ do
    mountain <- S.printingOf s registry "Mountain"
    mutavault <- S.printingOf s registry "Mutavault"
    heartstone <- S.printingOf s registry "Heartstone"
    tortoise <- S.printingOf s registry "Blossoming Tortoise"
    let animate = animationAbility mutavault
        (withId, withBoard) = mutavaultBoard mountain 2 mutavault heartstone (Just tortoise)
        (withoutId, withoutBoard) = mutavaultBoard mountain 2 mutavault heartstone Nothing
        activate srcId gs = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice srcId animate)
        animated srcId gs = S.runPure S.identityAnswer (activate srcId gs) Stack.resolveTop
        withAnimated = animated withId withBoard
        withoutAnimated = animated withoutId withoutBoard
    Spec.assertEqWith s "Mutavault prints two abilities, and the animation is the second" (length (Face.activatedAbilities (S.combinedFace mutavault))) 2
    -- The first activation, made while Mutavault is still no creature: only the
    -- Tortoise's criterion names it, so the board WITHOUT her pays the printed
    -- {1}. That is also what proves the two boards are the same board otherwise.
    Spec.assertEqWith s "the Tortoise alone pays the first activation" (S.tappedCount S.alice withAnimated) 0
    Spec.assertEqWith s "where the printed {1} costs a land" (S.tappedCount S.alice withoutAnimated) 1
    -- The animation resolved, so Mutavault is now a land AND a creature -- inside
    -- both criteria at once. The Tortoise's third sentence is what tells the two
    -- apart: "Land creatures you control get +1/+1".
    Spec.assertEqWith s "an animated Mutavault under the Tortoise" (S.powerToughnessOf withId withAnimated) (Just (3, 3))
    Spec.assertEqWith s "and 2/2 without her" (S.powerToughnessOf withoutId withoutAnimated) (Just (2, 2))
    let withSecond = activate withId withAnimated
        withoutSecond = activate withoutId withoutAnimated
    -- THE ASSERTION. Heartstone may not take the last mana; the Tortoise may, and
    -- does. A clamp on the pooled reduction leaves {1} here and taps a Mountain.
    Spec.assertEqWith s "the second activation costs nothing at all" (S.tappedCount S.alice withSecond) 0
    Spec.assertEqWith s "and is on the stack, so it was not merely refused" (length (GameState.stack withSecond)) 1
    -- The floor, on the same pair: Heartstone alone leaves the {1} it may not
    -- reduce, and one more land pays it. Not a refusal -- the ability reaches the
    -- stack on this board too, so the difference is the mana and nothing else.
    Spec.assertEqWith s "Heartstone's own floor leaves a mana to pay" (S.tappedCount S.alice withoutSecond) 2
    Spec.assertEqWith s "which is paid, not refused" (length (GameState.stack withoutSecond)) 1

-- alice's board: Saltfield Recluse, `lands` Plains, and a Goblin Piker under bob
-- to aim the Recluse's ability at -- plus Brutal Suppression under BOB when one
-- is passed. The positive and the negative differ in that Maybe and in nothing
-- else: same seats, same permanents, same priority, and the same mana on both
-- boards because the Recluse's activation cost has NO mana part at all, so no
-- reading of the negative can be "she could not afford it".
--
-- Brutal Suppression sits with bob however the activation goes, because its
-- sentence is symmetric ("activated abilities of nontoken Rebels", no
-- possessive, PlayerScope.EachPlayer) -- the board that proves that is the one
-- where the activating player does not control it.
suppressionBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  Maybe Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
suppressionBoard recluse plains piker lands mSuppression =
  let base = List.foldl' (\g _ -> snd (S.addCreature plains S.alice g)) (Setup.emptyGame S.bothPlayers) [1 .. lands]
      (srcId, g1) = S.addCreature recluse S.alice base
      g2 = snd (S.addCreature piker S.bob g1)
      g3 = maybe g2 (\suppression -> snd (S.addCreature suppression S.bob g2)) mSuppression
   in (srcId, g3 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice})

-- CR 601.2f's "plus all additional costs" reaching an ACTIVATION cost by CR
-- 602.2b (#1321), which nothing could do before: an adjustment carried mana
-- only, so no effect could add a tap, a sacrifice or a discard to an ability.
--
-- Brutal Suppression {R} Enchantment -- "Activated abilities of nontoken Rebels
-- cost an additional \"Sacrifice a land\" to activate" (Oracle text checked
-- against Scryfall) -- is the printing. Saltfield Recluse {2}{W} Creature --
-- Human Rebel Cleric 1/2, "{T}: Target creature gets -2/-0 until end of turn",
-- is the ability it taxes, and its cost is why the pair is honest: {T} and
-- nothing else, so the ONLY thing a land can be needed for on these boards is
-- the added component.
--
-- Asserted at GAMEPLAY level -- what Action.legalActions offers, and what
-- Activate.activateAbility actually does to the board -- rather than through
-- Activate.activatable, so a refusal is one a player would meet.
activationCostAdditionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
activationCostAdditionSpec s registry = Spec.describe s "ActivationCostAddition" $ do
  -- The gate, on a pair of boards differing only in the Suppression: with no
  -- land to sacrifice the added component cannot be paid, so the ability is not
  -- offered and an activation attempted anyway is the no-op CR 602.2 requires --
  -- nothing on the stack and the Recluse still UNTAPPED, which is the printed
  -- half of the cost proving it was rolled back rather than half-paid.
  Spec.it s "CR 601.2f Brutal Suppression's added cost stops an activation with no land" $ do
    recluse <- S.printingOf s registry "Saltfield Recluse"
    suppression <- S.printingOf s registry "Brutal Suppression"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let ability = theAbility recluse
        (withId, withSuppression) = suppressionBoard recluse plains piker 0 (Just suppression)
        (withoutId, withoutSuppression) = suppressionBoard recluse plains piker 0 Nothing
        activated srcId gs = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice srcId ability)
        after = activated withId withSuppression
        control = activated withoutId withoutSuppression
    Spec.assertEqWith s "not offered under the Suppression" (activationsOf withId (Action.legalActions S.alice withSuppression)) []
    Spec.assertBool s (not (null (activationsOf withoutId (Action.legalActions S.alice withoutSuppression)))) "and offered without it"
    Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack after)) 0
    Spec.assertEqWith s "and the Recluse was not tapped either" (fmap Object.tapped (Game.lookupObject withId after)) (Just TapState.Untapped)
    Spec.assertEqWith s "where the same activation without the Suppression is on the stack" (length (GameState.stack control)) 1
    Spec.assertEqWith s "having tapped the Recluse" (fmap Object.tapped (Game.lookupObject withoutId control)) (Just TapState.Tapped)

  -- The added component being PAID rather than merely measured, on a pair
  -- differing only in the Suppression again: one Plains under the Suppression is
  -- gone after the activation, and the same Plains without it is untouched. An
  -- ability that activated without paying is what this catches.
  Spec.it s "CR 601.2h the added \"Sacrifice a land\" is actually paid" $ do
    recluse <- S.printingOf s registry "Saltfield Recluse"
    suppression <- S.printingOf s registry "Brutal Suppression"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let ability = theAbility recluse
        plainsName = S.nameOf (Printing.card plains)
        (withId, withSuppression) = suppressionBoard recluse plains piker 1 (Just suppression)
        (withoutId, withoutSuppression) = suppressionBoard recluse plains piker 1 Nothing
        activated srcId gs = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice srcId ability)
        after = activated withId withSuppression
        control = activated withoutId withoutSuppression
    Spec.assertBool s (not (null (activationsOf withId (Action.legalActions S.alice withSuppression)))) "one Plains pays the added cost"
    Spec.assertEqWith s "the ability is on the stack" (length (GameState.stack after)) 1
    Spec.assertEqWith s "and the Plains was sacrificed" (S.countOnBattlefieldByName plainsName S.alice after) 0
    Spec.assertEqWith s "into her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "where the same activation without the Suppression keeps it" (S.countOnBattlefieldByName plainsName S.alice control) 1
    Spec.assertEqWith s "and sacrifices nothing" (length (Game.zoneMembers Zone.Graveyard S.alice control)) 0

  -- THE FALSIFIER for the criterion, both halves of it, on ONE board carrying a
  -- positive: Brutal Suppression names "nontoken REBELS", so a nonRebel's
  -- ability and a Rebel TOKEN's ability are both untaxed while the Recluse's is
  -- taxed. Prodigal Sorcerer is the nonRebel ("{T}: This creature deals 1 damage
  -- to any target"), and the token is a copy of the Recluse's own card -- so the
  -- two differ from the taxed permanent in exactly one atom each.
  Spec.it s "CR 601.2f Brutal Suppression's criterion spares a nonRebel and a token Rebel" $ do
    recluse <- S.printingOf s registry "Saltfield Recluse"
    recluseCard <- S.cardOf s registry "Saltfield Recluse"
    suppression <- S.printingOf s registry "Brutal Suppression"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (reclusId, board) = suppressionBoard recluse plains piker 0 (Just suppression)
        (sorcererId, withSorcerer) = S.addCreature sorcerer S.alice board
        (tokenId, withToken) = S.addToken recluseCard S.alice withSorcerer
        actions = Action.legalActions S.alice withToken
    Spec.assertEqWith s "the nontoken Rebel is taxed and cannot pay" (activationsOf reclusId actions) []
    Spec.assertBool s (not (null (activationsOf sorcererId actions))) "the nonRebel is untaxed"
    Spec.assertBool s (not (null (activationsOf tokenId actions))) "and so is the token Rebel"

-- Golden Egg ({2} Artifact -- Food): "When this artifact enters, draw a card. /
-- {1}, {T}, Sacrifice this artifact: Add one mana of any color. / {2}, {T},
-- Sacrifice this artifact: You gain 3 life." The pool's first permanent with CR
-- 205.3g's Food artifact type, added so that
-- Asmoranomardicadaistinaculdacar's "Sacrifice two Foods" has something to pay
-- with -- Pawl.CostSpec's asmorFoodSpec is where that cost is exercised, and
-- this group is the Egg's own three clauses, so a transcription short of the
-- printing fails here rather than passing unnoticed.
-- CR 613.1f layer 6, the ability-GRANTING half: Presence of Gond ({2}{G}
-- Enchantment -- Aura, "Enchant creature. Enchanted creature has '{T}: Create a
-- 1/1 green Elf Warrior creature token.'", checked against Scryfall) is the
-- smallest card that hands another object a whole quoted ability.
--
-- Three seats, and the two that matter are DIFFERENT players: alice controls the
-- Aura, bob controls the enchanted Prodigal Sorcerer, carol is the third. So
-- every claim about whose ability it is has a way to come out wrong. CR 303.4e
-- is explicit about this exact case: "if the Aura grants an ability to the
-- enchanted object (with 'gains' or 'has'), the enchanted object's controller is
-- the only one who can activate that ability". CR 113.7 makes the enchanted
-- creature the ability's source and CR 113.8 makes bob its controller, so the
-- token is bob's.
--
-- The Sorcerer is the receiver because it PRINTS an activated ability of its own
-- ("{T}: This creature deals 1 damage to any target"), so the granted one has to
-- be told apart from an ability the creature already had, from the Aura's own,
-- and from nothing at all.
presenceOfGondSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
presenceOfGondSpec s registry = Spec.describe s "Presence of Gond" $ do
  Spec.it s "CR 613.1f the enchanted creature has the granted ability ALONGSIDE its printed one" $ do
    gond <- S.printingOf s registry "Presence of Gond"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (_, sorcererId, enchanted) = gondBoard gond sorcerer True
        (_, unenchantedId, unenchanted) = gondBoard gond sorcerer False
    Spec.assertEqWith s "unenchanted: the printed ability alone" (Projection.abilitiesOf unenchantedId unenchanted) [theAbility sorcerer]
    Spec.assertEqWith s "enchanted: two abilities" (length (Projection.abilitiesOf sorcererId enchanted)) 2
    Spec.assertBool s (elem (theAbility sorcerer) (Projection.abilitiesOf sorcererId enchanted)) "the printed one survives the grant"
    Spec.assertBool s (grantedAbility sorcerer sorcererId enchanted /= Just (theAbility sorcerer)) "and the second one is not it"

  -- CR 303.4e / 113.7: the Aura holds the TEXT, never the ability. Presence of
  -- Gond prints no activated ability of its own, so tapping the Aura is not a
  -- thing any player may do.
  Spec.it s "CR 113.7 the Aura itself does not have the ability it grants" $ do
    gond <- S.printingOf s registry "Presence of Gond"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (gondId, _, enchanted) = gondBoard gond sorcerer True
    Spec.assertEqWith s "no ability on the granter" (Projection.abilitiesOf gondId enchanted) []
    Spec.assertEqWith s "and none offered to its controller" (activationsOf gondId (Action.legalActions S.alice enchanted)) []

  -- The gameplay-level proof. bob activates the granted ability and the stack
  -- resolves: the token is bob's, and the {T} that paid for it tapped BOB'S
  -- CREATURE rather than alice's Aura. Both are what make the ability the
  -- receiver's rather than the granter's.
  Spec.it s "CR 303.4e whole card: the granted {T} taps the enchanted creature and its controller gets the token" $ do
    gond <- S.printingOf s registry "Presence of Gond"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (gondId, sorcererId, enchanted) = gondBoard gond sorcerer True
    case grantedAbility sorcerer sorcererId enchanted of
      Nothing -> Spec.assertFailure s "the fixture should have granted an ability"
      Just granted -> do
        let activated = snd (Engine.runGamePure S.identityAnswer enchanted (Activate.activateAbility S.bob sorcererId granted))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            tokens = S.tokensOf resolved
        Spec.assertBool s (any (\a -> case a of A.Activate _ ab -> ab == granted; _ -> False) (activationsOf sorcererId (Action.legalActions S.bob enchanted))) "the granted ability is offered to bob"
        Spec.assertEqWith s "and not to the Aura's controller" (activationsOf sorcererId (Action.legalActions S.alice enchanted)) []
        Spec.assertEqWith s "one token" (length tokens) 1
        mapM_ (\oid -> Spec.assertEqWith s "controlled by bob, not by the Aura's controller" (Projection.controllerOf oid resolved) (Just S.bob)) tokens
        mapM_ (\oid -> Spec.assertEqWith s "a 1/1" (Projection.powerOf oid resolved, Projection.toughnessOf oid resolved) (Just 1, Just 1)) tokens
        mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf Warrior" (Projection.subtypesOf oid resolved) (Set.fromList [Subtype.Elf, Subtype.Warrior])) tokens
        mapM_ (\oid -> Spec.assertEqWith s "green" (Projection.colorsOf oid resolved) (Set.singleton Color.Green)) tokens
        Spec.assertEqWith s "the enchanted creature paid the {T}" (fmap Object.tapped (Game.lookupObject sorcererId resolved)) (Just TapState.Tapped)
        Spec.assertEqWith s "the Aura did not" (fmap Object.tapped (Game.lookupObject gondId resolved)) (Just TapState.Untapped)

  -- CR 613.1f puts the grant and Humility's strip in the SAME layer, so CR 613.7
  -- timestamp order alone decides. This is the pair, differing in nothing but
  -- which permanent arrived first -- and the granted ability is what the two
  -- boards disagree about, since Humility strips the Sorcerer's printed one
  -- either way.
  Spec.it s "CR 613.7 Humility BEFORE the Aura leaves the granted ability standing" $ do
    gond <- S.printingOf s registry "Presence of Gond"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    humility <- S.printingOf s registry "Humility"
    let (_, sorcererId, board) = gondBoardUnder (Just Before) gond sorcerer humility
    Spec.assertEqWith s "the printed ability is gone, the granted one is not" (length (Projection.abilitiesOf sorcererId board)) 1
    Spec.assertBool s (notElem (theAbility sorcerer) (Projection.abilitiesOf sorcererId board)) "and what is left is not the printed one"

  Spec.it s "CR 613.7 Humility AFTER the Aura takes the granted ability with the rest" $ do
    gond <- S.printingOf s registry "Presence of Gond"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    humility <- S.printingOf s registry "Humility"
    let (_, sorcererId, board) = gondBoardUnder (Just After) gond sorcerer humility
    Spec.assertEqWith s "no abilities at all" (Projection.abilitiesOf sorcererId board) []

  -- CR 612.1/612.2a reaching INSIDE the quoted ability: the words are printed on
  -- the Aura, so a text change affecting the AURA rewrites them, and the layer-3
  -- swap runs before the layer-6 grant hands them over. Artificial Evolution's
  -- "Change the text of target spell or permanent by replacing all instances of
  -- one creature type with another" (checked against Scryfall) aimed at Presence
  -- of Gond turns the Elf Warrior it mints into a Goblin Warrior -- name and
  -- type line both, which is CR 612.2a's whole-card clause.
  --
  -- The unevolved half is the control, on the same board bar the Evolution.
  Spec.it s "CR 612.2a an unevolved Presence of Gond's granted ability mints an Elf Warrior Token" $ do
    (tokens, after) <- gondEvolvedChain s registry Nothing
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf Warrior" (Projection.subtypesOf oid after) (Set.fromList [Subtype.Elf, Subtype.Warrior])) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Elf Warrior Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Elf Warrior Token")))) tokens

  Spec.it s "CR 612.2a an evolved Presence of Gond's granted ability mints a Goblin Warrior Token" $ do
    (tokens, after) <- gondEvolvedChain s registry (Just (Subtype.Elf, Subtype.Goblin))
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Goblin Warrior" (Projection.subtypesOf oid after) (Set.fromList [Subtype.Goblin, Subtype.Warrior])) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Goblin Warrior Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Goblin Warrior Token")))) tokens

-- alice's Aura, bob's settled Prodigal Sorcerer, carol as the third seat. The
-- two boards this builds differ in exactly one thing: whether the Aura is
-- attached (CR 303.4m is what Affected.Attached reads).
gondBoard :: Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
gondBoard gond sorcerer attached =
  let (sorcererId, g0) = S.addCreature sorcerer S.bob S.threePlayerGame
      -- CR 302.6: bob's creature has to have settled before its {T} is payable,
      -- and the granted ability's tap cost is the receiver's to pay.
      settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.bob)
      (gondId, g1) = S.addCreature gond S.alice settled
      g2 = if attached then S.attach gondId sorcererId g1 else g1
   in (gondId, sorcererId, g2 {GameState.priority = Just S.bob})

-- Which side of the Aura's timestamp Humility lands on (CR 613.7).
data HumilityOrder = Before | After

-- gondBoard with a Humility, placed either side of the Aura. Nothing but the
-- placement differs between the two boards.
gondBoardUnder :: Maybe HumilityOrder -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
gondBoardUnder order gond sorcerer humility =
  let (sorcererId, g0) = S.addCreature sorcerer S.bob S.threePlayerGame
      settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.bob)
      early = case order of
        Just Before -> S.withHumility humility settled
        _ -> settled
      (gondId, g1) = S.addCreature gond S.alice early
      late = case order of
        Just After -> S.withHumility humility g1
        _ -> g1
   in (gondId, sorcererId, (S.attach gondId sorcererId late) {GameState.priority = Just S.bob})

-- alice's Island and Presence of Gond, bob's settled Prodigal Sorcerer, the Aura
-- attached; optionally an Artificial Evolution resolved AT THE AURA first. Then
-- bob activates the granted ability and it resolves. Returns the tokens and the
-- final state.
gondEvolvedChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m ([ObjectId.ObjectId], GameState.GameState)
gondEvolvedChain s registry swap = do
  island <- S.printingOf s registry "Island"
  gond <- S.printingOf s registry "Presence of Gond"
  sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
  evolution <- S.printingOf s registry "Artificial Evolution"
  let (sorcererId, g0) = S.addCreature sorcerer S.bob (S.landsFor island S.alice 1 S.threePlayerGame)
      settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.bob)
      (gondId, g1) = S.addCreature gond S.alice settled
      (evolutionId, g2) = S.addHandCard evolution S.alice (S.attach gondId sorcererId g1)
      evolved = case swap of
        Nothing -> g2
        Just (from, to) ->
          S.runPure (evolveAt gondId from to) g2 $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      ready = evolved {GameState.priority = Just S.bob}
      after = case grantedAbility sorcerer sorcererId ready of
        Nothing -> ready
        Just granted ->
          S.runPure S.identityAnswer ready $ do
            Activate.activateAbility S.bob sorcererId granted
            Stack.resolveTop
  pure (S.tokensOf after, after)

-- Aims every target set at one object and answers the creature-type swap, the
-- Pawl.ResolveSpec helper of the same name.
evolveAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
evolveAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  Prompt.ChooseCreatureTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- The one ability on the object that the printing did not print: the grant.
grantedAbility :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
grantedAbility printed oid gs = case filter (/= theAbility printed) (Projection.abilitiesOf oid gs) of
  ability : _ -> Just ability
  [] -> Nothing

goldenEggSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
goldenEggSpec s registry = Spec.describe s "Golden Egg" $ do
  -- The clause every Food shares, and the one that makes the Egg a real card
  -- rather than a bare type line: {2} from two Islands, CR 107.5's tap and CR
  -- 701.21a's sacrifice, then three life.
  Spec.it s "CR 701.21a whole card: {2}, {T}, Sacrifice this artifact: You gain 3 life" $ do
    goldenEgg <- S.printingOf s registry "Golden Egg"
    island <- S.printingOf s registry "Island"
    let (eggId, g0) = S.addCreature goldenEgg S.alice (S.landsInPlay island 2)
        gs = g0 {GameState.priority = Just S.alice}
        activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice eggId (lifeGainAbility goldenEgg))
        resolved = S.runPure S.identityAnswer activated Stack.resolveTop
    Spec.assertBool s (Set.member Subtype.Food (Projection.subtypesOf eggId gs)) "CR 205.3g it is a Food"
    Spec.assertBool s (not (Set.member eggId (GameState.battlefield activated))) "the cost sacrificed it"
    Spec.assertEqWith s "and both Islands paid the {2}" (S.tappedCount S.alice activated) 2
    Spec.assertEqWith s "alice was at 20 while the ability was on the stack" (S.lifeOf S.alice activated) (Just 20)
    Spec.assertEqWith s "and gains three when it resolves" (S.lifeOf S.alice resolved) (Just 23)
  -- The other activated clause. CR 605.1a classifies it -- it adds mana, needs no
  -- target, is not a loyalty ability, and neither its cost nor its effect moves a
  -- card to or from a library -- which is what keeps it off the stack (CR
  -- 605.3b). Both abilities are classified here, so the pair pins them apart
  -- rather than assuming an order.
  Spec.it s "CR 605.1a/105.4 the {1}, {T}, Sacrifice ability is a mana ability offering the five colours" $ do
    goldenEgg <- S.printingOf s registry "Golden Egg"
    let (eggId, gs) = S.addCreature goldenEgg S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "the mana ability first, the life gain second"
      (fmap ManaAbility.isManaAbility (Face.activatedAbilities (S.combinedFace goldenEgg)))
      [True, False]
    Spec.assertEqWith
      s
      "CR 105.4 five colours, never colourless"
      (Set.fromList (Mana.manaTypesOf eggId gs))
      (Set.fromList (fmap ManaType.Colored [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green]))
  -- And the triggered clause, off the same enters event Pawl.ModalSpec's fixture
  -- uses. The card is drawn when the trigger RESOLVES, not when it is placed.
  Spec.it s "CR 603.2 its enters trigger draws a card" $ do
    goldenEgg <- S.printingOf s registry "Golden Egg"
    island <- S.printingOf s registry "Island"
    let (_, g0) = S.entersWithTrigger goldenEgg S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs) = S.addLibraryCard island S.alice g0
        placed = S.runPure S.identityAnswer gs Engine.placePendingTriggers
        resolved = S.runPure S.identityAnswer placed Stack.resolveTop
    Spec.assertEqWith s "the trigger is on the stack" (length (GameState.stack placed)) 1
    Spec.assertEqWith s "and nothing is drawn yet" (S.handSize S.alice placed) 0
    Spec.assertEqWith s "one card drawn when it resolves" (S.handSize S.alice resolved) 1
    Spec.assertEqWith s "out of her library" (length (Game.zoneMembers Zone.Library S.alice resolved)) 0

-- Golden Egg's SECOND activated ability -- the life gain, not the mana ability
-- theAbility above would pick. Total: the fallback is unreachable for a printing
-- with two.
lifeGainAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
lifeGainAbility p = case Face.activatedAbilities (S.combinedFace p) of
  _ : ab : _ -> ab
  _ -> theAbility p

-- CR 506.7g: "Rules 506.7 and 506.7a-f apply to abilities that state that they
-- may be activated only at certain times with respect to combat just as they
-- apply to spells." Trap Runner (Mercadian Masques) is the pool's producer -- a
-- {2}{W}{W} 2/3 Human Soldier, "{T}: Target unblocked attacking creature becomes blocked.
-- Activate only during combat after blockers are declared." -- and it prints the
-- clause Curtain of Light prints on a cast, word for word. That is why
-- ActivationRestriction.AfterBlockersDeclared and its casting twin share
-- Combat.afterBlockersDeclared instead of each deriving the window.
--
-- alice attacks with one Goblin Piker; bob holds priority and controls the Trap
-- Runner plus a Prodigal Sorcerer, the unrestricted {T} this module's other
-- rider groups use as their control. Blocks are declined throughout, so the
-- Piker stays an unblocked attacking creature and the ability keeps a legal
-- target on every leg -- a board that lost the target would refuse the
-- activation for a reason the rider has nothing to do with.
trapRunnerBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
trapRunnerBoard piker runner sorcerer =
  let (gs0, ours, theirs) = S.combatBoardOf [piker] [runner, sorcerer]
      declared = (S.runPure S.aggressiveAnswer gs0 (Combat.declareAttackers S.alice)) {GameState.priority = Just S.bob}
   in case (ours, theirs) of
        (attackerId : _, runnerId : sorcererId : _) -> (attackerId, runnerId, sorcererId, declared)
        -- combatBoardOf returns one id per printing given, so this is
        -- unreachable; bogus ids fail the assertions rather than the suite.
        _ -> (S.noSource, S.noSource, S.noSource, declared)

-- Decline every block, and otherwise answer as the aggressive interpreter does.
-- Declining is what keeps the attacker unblocked, so the only route into CR
-- 509.1h's blocked status is the Trap Runner's ability.
noBlocks :: Prompt.Prompt r -> r
noBlocks p = case p of
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- noBlocks, plus taking every activation the engine offers and aiming it at
-- `victim` -- the interpreter the gameplay-level leg runs a whole combat phase
-- with. The target is PINNED rather than searched for, so a broken gate cannot
-- repair the assertion by finding some other legal creature.
noBlocksActivating :: ObjectId.ObjectId -> Prompt.Prompt r -> r
noBlocksActivating victim p = case p of
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction _ _ options -> case filter isActivate options of
    a : _ -> a
    [] -> A.Pass
  _ -> S.aggressiveAnswer p

printedActivationCombatPointSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
printedActivationCombatPointSpec s registry = Spec.describe s "PrintedActivationCombatPoint" $ do
  -- One board, five readings, and the pair that carries the rule is
  -- `beforeDeclaration` against `declared`: identical states but for CR 509.1's
  -- turn-based action having run.
  Spec.it s "CR 506.7b/g the rider opens at the declaration and runs to the end of the combat phase" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    runner <- S.printingOf s registry "Trap Runner"
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (attackerId, runnerId, sorcererId, atAttackers) = trapRunnerBoard piker runner prodigalSorcerer
        beforeDeclaration = atAttackers {GameState.phase = Phase.Combat CombatStep.DeclareBlockers}
        declared = (S.runPure noBlocks beforeDeclaration Combat.declareBlockers) {GameState.priority = Just S.bob}
        inStep step = declared {GameState.phase = Phase.Combat step}
        offeredOn = Action.legalActions S.bob
    -- Anti-vacuity: the attack is real, the attacker is unblocked, and bob's
    -- unrestricted {T} is offered on every leg -- so a leg without the Trap
    -- Runner's activation is withholding that one ability.
    Spec.assertBool s (Map.member attackerId (Combat.Type.attackers (GameState.combat declared))) "the Piker is attacking"
    Spec.assertBool s (not (Combat.isBlocked attackerId declared)) "and unblocked, so the ability has a legal target"
    Spec.assertBool s (not (any (null . activationsOf sorcererId . offeredOn) [atAttackers, beforeDeclaration, declared, inStep CombatStep.CombatDamage, inStep CombatStep.EndOfCombat])) "bob's unrestricted {T} is offered on every leg"
    Spec.assertBool s (not (Combat.afterBlockersDeclared beforeDeclaration)) "the declaration has not happened yet"
    Spec.assertBool s (Combat.afterBlockersDeclared declared) "and it has after CR 509.1's turn-based action"
    -- Before the point CR 506.7b names.
    Spec.assertEqWith s "not offered in the declare attackers step" (activationsOf runnerId (offeredOn atAttackers)) []
    Spec.assertEqWith s "nor in the declare blockers step before blockers are declared" (activationsOf runnerId (offeredOn beforeDeclaration)) []
    -- After it, across all three steps the window spans.
    Spec.assertEqWith s "offered once blockers are declared" (length (activationsOf runnerId (offeredOn declared))) 1
    Spec.assertEqWith s "offered in the combat damage step" (length (activationsOf runnerId (offeredOn (inStep CombatStep.CombatDamage)))) 1
    Spec.assertEqWith s "offered in the end of combat step" (length (activationsOf runnerId (offeredOn (inStep CombatStep.EndOfCombat)))) 1

  -- The gameplay-level proof (design.md section 4), driven through the priority
  -- loop rather than by calling Activate.activateAbility. bob's life is the
  -- falsifier: the Piker is unblocked and declines to be blocked, so 2 damage is
  -- what reaches bob unless the ability makes it a blocked creature with nothing
  -- blocking it (CR 509.1h, then CR 510.1c assigning to no one).
  --
  -- Its own control is the same combat under the same declined blocks with no
  -- activation, so what saves bob is the ability and not the fixture.
  Spec.it s "CR 506.7b/g whole card: Trap Runner blocks the attacker in the declare blockers step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    runner <- S.printingOf s registry "Trap Runner"
    let (gs0, ours, theirs) = S.combatBoardOf [piker] [runner]
    case (ours, theirs) of
      ([attackerId], [runnerId]) -> do
        -- Stopped at the end of combat step rather than run past it: CR 511.3
        -- empties Combat as that step ENDS, and the blocked status is what these
        -- assertions read. Combat damage (CR 510) has already been dealt by
        -- here, so bob's life is settled.
        let used = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (noBlocksActivating attackerId) gs0
            idle = S.runToStep (Phase.Combat CombatStep.EndOfCombat) noBlocks gs0
        Spec.assertBool s (Combat.isBlocked attackerId used) "CR 509.1h: the ability made the attacker a blocked creature"
        Spec.assertEqWith s "with no creature blocking it" (Combat.blockersOf attackerId used) Set.empty
        Spec.assertEqWith s "so bob took nothing" (S.lifeOf S.bob used) (Just 20)
        Spec.assertEqWith s "and the Runner paid its {T}" (fmap Object.tapped (Game.lookupObject runnerId used)) (Just TapState.Tapped)
        Spec.assertBool s (not (Combat.isBlocked attackerId idle)) "control: without the activation the attacker is unblocked"
        Spec.assertEqWith s "control: so bob takes the Piker's 2" (S.lifeOf S.bob idle) (Just 18)
        Spec.assertEqWith s "control: and the Runner is untapped" (fmap Object.tapped (Game.lookupObject runnerId idle)) (Just TapState.Untapped)
      _ -> Spec.assertFailure s "fixture should have given alice an attacker and bob a Trap Runner"
