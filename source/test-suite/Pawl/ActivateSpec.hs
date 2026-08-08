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
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
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
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Finds the first matching library card on a search, else fails to find.
findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches -> case matches of
    m : _ -> Just m
    [] -> Nothing
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
singleModeAbility :: [Effect.Effect card] -> Map.Map SlotName.SlotName TargetSpec.TargetSpec -> Modal.Modal card
singleModeAbility effects specs =
  Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause (Seq.fromList effects))) specs Optionality.Mandatory Nothing)) (ModeSelection.ChooseExactly 1)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Activate" $ do
  printedActivationRestrictionSpec s registry
  printedActivationConjunctionSpec s registry
  printedActivationWholePhaseSpec s registry
  printedActivationTurnScopeSpec s registry
  variableActivationCostSpec s registry
  youOnActivatedAbilitySpec s registry
  textChangedAbilitySpec s registry
  graveyardEffectZoneSpec s registry

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
    Spec.assertBool s (not (Mana.isManaAbility ability)) "Evolving Wilds' ability is NOT a mana ability"
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
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer who)) sets
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
        pumped = S.withEffect srcId (Modification.ModifyPowerToughness (Quantity.Type.Literal 3) (Quantity.Type.Literal 3)) g0
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
        pumped = S.withEffect srcId (Modification.ModifyPowerToughness (Quantity.Type.Literal 3) (Quantity.Type.Literal 3)) g0
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
        pumped = S.withEffect srcId (Modification.ModifyPowerToughness (Quantity.Type.Literal 3) (Quantity.Type.Literal 3)) g0
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
          "the printed {2} plus rule 702.29a's discard"
          (ActivatedAbility.cost ability)
          (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) [CostComponent.DiscardThis])
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
  -- The log, not a per-player view: pawl has no such view yet (#322), and
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
        Spec.assertEqWith s "only the announcement reveal before the ability resolves" (S.revealsOf cycled) [(S.alice, CardName.MkCardName $ Text.pack "Ash Barrens")]
        Spec.assertEqWith
          s
          "then Alice revealed the Forest she found"
          (S.revealsOf after)
          [(S.alice, CardName.MkCardName $ Text.pack "Ash Barrens"), (S.alice, CardName.MkCardName $ Text.pack "Forest")]
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
        Spec.assertEqWith s "no basic land found, so only the announcement reveal" (S.revealsOf after) [(S.alice, CardName.MkCardName $ Text.pack "Ash Barrens")]
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
          (Just CostComponent.DiscardThis, Just CostComponent.DiscardThis)
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
    Spec.assertEqWith s "Alice revealed the Forest, and only the Forest" (S.revealsOf after) [(S.alice, CardName.MkCardName $ Text.pack "Forest")]
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
        Spec.assertEqWith s "Alice revealed Barkhide Mauler" (S.revealsOf activated) [(S.alice, CardName.MkCardName $ Text.pack "Barkhide Mauler")]
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
                (Seq.fromList [Mode.MkMode Seq.empty Map.empty Optionality.Mandatory Nothing, Mode.MkMode Seq.empty Map.empty Optionality.Mandatory Nothing])
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

-- Announces X and aims every target slot at `who`.
answerXAt :: Natural -> PlayerId.PlayerId -> Prompt.Prompt r -> r
answerXAt x who p = case p of
  Prompt.ChooseX {} -> x
  _ -> aimAt who p

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

-- Aims every target slot at one CREATURE. aimAt's counterpart for a board whose
-- point is that no PLAYER was targeted: CR 115.4's "any target" pool offers a
-- creature as Recipient.ToCreature, so a life total left at 20 is proof the
-- targeted instruction went elsewhere.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature oid)) sets
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
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject spellId)) sets
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
  deriving (Eq, Show)

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
    Nothing -> do
      Spec.assertFailure s "expected exactly one creature on the battlefield, carrying exactly one activated ability"
      pure (forestId, board)
    Just (permId, ability) ->
      pure (forestId, S.runPure S.identityAnswer board (do Activate.activateAbility S.alice permId ability; Stack.resolveTop))

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
