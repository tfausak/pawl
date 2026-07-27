{-# LANGUAGE GADTs #-}

-- Covers Pawl.Activate: activating an ability onto the stack, summoning-sickness
-- gating, and the CR 605 mana-ability exclusion from stack activations.
module Pawl.ActivateSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.ActivationTiming as ActivationTiming
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Regenerability as Regenerability
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Finds the first matching library card on a search, else fails to find.
findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches -> case matches of
    m : _ -> Just m
    [] -> Nothing
  _ -> S.identityAnswer p

-- The single ability of a printing (all M3e gates have exactly one). Total: the
-- empty-ability fallback is unreachable in these fixtures, and honors the
-- no-partial-functions rule (no `error`).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (singleModeAbility [] Map.empty) ActivationTiming.AnyTime

-- A single forced mode (ChooseExactly 1, M4g's non-modal shape) -- the fixture
-- shape every pre-M4h single-mode ActivatedAbility now takes.
singleModeAbility :: [Effect.Effect card] -> Map.Map SlotName.SlotName TargetSpec.TargetSpec -> Modal.Modal card
singleModeAbility effects specs =
  Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList effects) specs)) (ModeSelection.ChooseExactly 1)

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Pawl.Activate"
    [ HU.testCase "CR 602 activating Prodigal Sorcerer's {T} puts an ability on the stack and taps it" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
            after = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility prodigalSorcerer)))
        HU.assertEqual "one thing on the stack" 1 (length (GameState.stack after))
        HU.assertEqual "source tapped" (Just TapState.Tapped) (fmap Object.tapped (Game.lookupObject srcId after)),
      HU.testCase "CR 602.5/302.6 a summoning-sick creature's {T} ability is not offered" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) srcId (GameState.objects g0), GameState.priority = Just S.alice}
        HU.assertBool "no Activate offered" (not (any isActivate (Action.legalActions S.alice sick))),
      -- CR 302.6 keyed to the ACTIVATING player, not to the object: bob's
      -- Sorcerer has settled under bob, and alice's Control Magic does not
      -- inherit that settle along with the creature (#198).
      HU.testCase "CR 302.6 a stolen creature's {T} ability is not offered to the thief" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        controlMagic <- Registry.printing registry "Control Magic"
        let (srcId, g0) = S.addCreature prodigalSorcerer S.bob (Setup.emptyGame S.bothPlayers)
            settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.bob)
            (aura, withAura) = S.addCreature controlMagic S.alice settled
            stolen = (S.attach aura srcId withAura) {GameState.priority = Just S.alice}
        HU.assertBool "bob could have activated it" (any isActivate (Action.legalActions S.bob settled {GameState.priority = Just S.bob}))
        HU.assertBool "alice controls it now" (Projection.controllerOf srcId stolen == Just S.alice)
        HU.assertBool "but no Activate is offered to her" (not (any isActivate (Action.legalActions S.alice stolen))),
      -- CR 702.6a: "Activate only as a sorcery." CR 307.5 says what that means,
      -- and says it narrowly: "the player must have priority, it must be during
      -- the main phase of their turn, and the stack must be empty."
      HU.testCase "CR 307.5 equip is offered in your own main phase with an empty stack" $ do
        mountain <- Registry.printing registry "Mountain"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        piker <- Registry.printing registry "Goblin Piker"
        -- Lands, so the {1} equip cost is payable and the ONLY thing under test
        -- is the timing rider.
        let (_, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
            (_, g1) = S.addCreature bonesplitter S.alice g0
            gs = g1 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain}
        HU.assertBool "equip offered" (any isActivate (Action.legalActions S.alice gs)),
      HU.testCase "CR 307.5 equip is NOT offered outside a main phase" $ do
        mountain <- Registry.printing registry "Mountain"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        piker <- Registry.printing registry "Goblin Piker"
        -- Lands, so the {1} equip cost is payable and the ONLY thing under test
        -- is the timing rider.
        let (_, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
            (_, g1) = S.addCreature bonesplitter S.alice g0
            gs = g1 {GameState.priority = Just S.alice, GameState.phase = Phase.Combat CombatStep.DeclareBlockers}
        HU.assertBool "no equip during combat" (not (any isActivate (Action.legalActions S.alice gs))),
      HU.testCase "CR 307.5 equip is NOT offered on an opponent's turn" $ do
        mountain <- Registry.printing registry "Mountain"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        piker <- Registry.printing registry "Goblin Piker"
        -- Lands, so the {1} equip cost is payable and the ONLY thing under test
        -- is the timing rider.
        let (_, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
            (_, g1) = S.addCreature bonesplitter S.alice g0
            gs = g1 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.bob}
        HU.assertBool "not on bob's turn" (not (any isActivate (Action.legalActions S.alice gs))),
      HU.testCase "CR 307.5 equip is NOT offered while the stack is not empty" $ do
        mountain <- Registry.printing registry "Mountain"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        piker <- Registry.printing registry "Goblin Piker"
        -- Lands, so the {1} equip cost is payable and the ONLY thing under test
        -- is the timing rider.
        let (_, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
            (_, g1) = S.addCreature bonesplitter S.alice g0
            (spellId, g2) = S.spellOnStack piker S.alice g1
            gs = g2 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain}
        HU.assertBool "the stack really is occupied" (elem spellId (GameState.stack gs))
        HU.assertBool "no equip with a spell on the stack" (not (any isActivate (Action.legalActions S.alice gs))),
      -- The control: an UNRESTRICTED ability is unaffected by all three, so the
      -- gate cannot pass by refusing everything outside a main phase.
      HU.testCase "CR 602.2 an ability with no timing rider is still offered during combat" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (_, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.alice)
            gs = settled {GameState.priority = Just S.alice, GameState.phase = Phase.Combat CombatStep.DeclareBlockers}
        HU.assertBool "Prodigal Sorcerer still offered" (any isActivate (Action.legalActions S.alice gs)),
      HU.testCase "CR 602 a settled Prodigal Sorcerer's ability IS offered" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (_, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
        HU.assertBool "Activate offered" (any isActivate (Action.legalActions S.alice g1)),
      HU.testCase "CR 602 activating then resolving deals 1 damage and the ability ceases" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
            -- identityAnswer's ChooseTargets picks the lowest recipient; with no
            -- creatures but two players, it targets a player. Resolve the stack.
            activated = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility prodigalSorcerer)))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
        HU.assertEqual "stack empty after resolution" [] (GameState.stack resolved),
      HU.testCase "CR 605.3b a mana ability is not offered as a stack activation" $ do
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        let (_, g0) = S.addCreature llanowarElves S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
        HU.assertBool "no Activate for the mana ability" (not (any isActivate (Action.legalActions S.alice g1))),
      HU.testCase "CR 701.21/701.23 Evolving Wilds sacrifices itself and fetches a basic land tapped" $ do
        -- The fetched land gets a NEW id (CR 400.7); assert by count/tapped-count.
        evolvingWilds <- Registry.printing registry "Evolving Wilds"
        forest <- Registry.printing registry "Forest"
        let base = Setup.emptyGame S.bothPlayers
            (wildsId, g1) = S.addCreature evolvingWilds S.alice base
            (_, g2) = S.addLibraryCard forest S.alice g1
            g3 = g2 {GameState.priority = Just S.alice}
            ability = theAbility evolvingWilds
            activated = snd (Engine.runGamePure findFirst g3 (Activate.activateAbility S.alice wildsId ability))
            resolved = snd (Engine.runGamePure findFirst activated Stack.resolveTop)
        HU.assertBool "Evolving Wilds' ability is NOT a mana ability" (not (Mana.isManaAbility ability))
        HU.assertBool "Evolving Wilds sacrificed (gone from battlefield)" (not (Set.member wildsId (GameState.battlefield resolved)))
        HU.assertEqual "one permanent on the battlefield (the fetched land)" 1 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
        HU.assertEqual "the fetched land is tapped" 1 (S.tappedCount S.alice resolved),
      HU.testCase "CR 302.6 a freshly-added land can tap+sac immediately (no summoning sickness)" $ do
        evolvingWilds <- Registry.printing registry "Evolving Wilds"
        let base = Setup.emptyGame S.bothPlayers
            (wildsId, g1) = S.addCreature evolvingWilds S.alice base
            -- Force it Sick: a land ignores sickness, so the ability is still offered.
            g2 = g1 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) wildsId (GameState.objects g1), GameState.priority = Just S.alice}
        HU.assertBool "land ability offered despite sickness" (any isActivate (Action.legalActions S.alice g2)),
      HU.testCase "CR 613/602 a Humility'd Prodigal Sorcerer's ability is not offered" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        humility <- Registry.printing registry "Humility"
        let (_, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            gs = (S.withHumility humility g0) {GameState.priority = Just S.alice}
        HU.assertBool "no Activate under Humility" (not (any isActivate (Action.legalActions S.alice gs))),
      HU.testCase "CR 602.1b: an activation with a mana cost needs the mana" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
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
                  ActivatedAbility.timing = ActivationTiming.AnyTime
                }
        HU.assertBool "one Mountain cannot pay {2}" (not (Activate.activatable S.alice srcId costlyAbility gs1)),
      HU.testCase "CR 701.19a Drudge Skeletons regenerates: activate, survive Murder, die to the next" $ do
        swamp <- Registry.printing registry "Swamp"
        drudgeSkeletons <- Registry.printing registry "Drudge Skeletons"
        let base = S.landsInPlay swamp 1
            (skel, gs0) = S.addCreature drudgeSkeletons S.alice base
            ability = theAbility drudgeSkeletons -- the local ActivateSpec helper
            activated = snd (Engine.runGamePure S.identityAnswer gs0 (Activate.activateAbility S.alice skel ability))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            -- First Murder: replaced by the shield.
            firstKill = S.settleSba (S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable skel))
            -- Second Murder: no shield -> dies.
            secondKill = S.settleSba (S.runPure S.identityAnswer firstKill (Event.destroy Regenerability.Regenerable skel))
        HU.assertEqual
          "the shield's source is the skeleton itself"
          [skel]
          (fmap ActiveReplacement.source (GameState.replacements resolved))
        HU.assertEqual "survived the first destruction (regenerated)" True (Set.member skel (GameState.battlefield firstKill))
        HU.assertEqual "died to the second (one-shot shield consumed)" False (Set.member skel (GameState.battlefield secondKill)),
      -- CR 113.8: the controller of an activated ability on the stack is the
      -- player who activated it; the controller of a triggered ability on the
      -- stack is whoever controlled its source when it triggered. Fixed once,
      -- at activation (Activate.activateAbility sets Object.owner = pid), and
      -- never revisited -- so a control change to the SOURCE PERMANENT after
      -- activation must not move the ability to the new controller.
      --
      -- Prodigal Sorcerer's own ability (a flat DealDamage 1; CR 115.4's "any
      -- target" has no controller-sensitive restriction) has no
      -- controller-sensitive OUTPUT, so it cannot make "whose control"
      -- observable here. A GainControl/ForAsLongAs effect does -- the same
      -- shape Master Thief's ETB uses in Pawl.ExpirySpec's masterThiefTests --
      -- attached to a SYNTHETIC ability (labeled crutch) on the same creature,
      -- standing in for the pool's lack of a controller-sensitive printed
      -- activated ability (#82), so this exercises the ACTIVATED path rather
      -- than the TRIGGERED one that card already covers.
      HU.testCase "CR 113.8 an activated ability resolves under whoever activated it, not a later controller" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let base = Setup.emptyGame S.bothPlayers
            -- The Myr is ALICE's own artifact (not bob's): if control ever
            -- moved to bob it would be a genuine change, not a fixture
            -- coincidence, so this assertion actually discriminates the bug.
            (myrId, g0) = S.addCreature darksteelMyr S.alice base
            (srcId, g1) = S.addCreature prodigalSorcerer S.alice g0
            g2 = g1 {GameState.priority = Just S.alice}
            targetSlot = SlotName.MkSlotName (Text.pack "target")
            ability =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
                  ActivatedAbility.modal =
                    singleModeAbility
                      [Effect.GainControl (Duration.ForAsLongAs S.youControlSource) targetSlot]
                      (Map.singleton targetSlot (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact)))),
                  ActivatedAbility.timing = ActivationTiming.AnyTime
                }
            activated = snd (Engine.runGamePure S.identityAnswer g2 (Activate.activateAbility S.alice srcId ability))
            -- Control of the SOURCE CREATURE (not the ability object) moves to bob
            -- while the ability sits on the stack.
            taken = S.giveControl srcId S.bob activated
            resolved = snd (Engine.runGamePure S.identityAnswer taken Stack.resolveTop)
        HU.assertEqual "one thing on the stack before resolution" 1 (length (GameState.stack taken))
        HU.assertEqual "stack empty after resolution" [] (GameState.stack resolved)
        HU.assertEqual "control of the artifact never changed" (Just S.alice) (Projection.controllerOf myrId resolved)
        -- Filtered to the artifact: S.giveControl's own AtCleanup
        -- SetController effect on srcId itself is already in this list and
        -- is not what CR 611.2b's "never starts" is about.
        HU.assertEqual
          "nothing was stored for the artifact -- the duration never starts for alice, the ability's frozen activator"
          []
          (filter (S.continuousEffectAffects myrId) (GameState.continuousEffects resolved)),
      -- The stolen-creature case the OLD (deleted) Resolve.hs comment named --
      -- rebuilt as the POSITIVE mirror of the test above, with the same
      -- SYNTHETIC ability (#82): control of the source moves to bob FIRST, bob
      -- then activates, and the effect must arm and store under bob, the
      -- ability's frozen (and only) controller (CR 113.8). Object.owner is
      -- stamped with bob at activation time, so there is no later re-read to
      -- get wrong.
      HU.testCase "CR 113.8 a stolen creature's ability, activated by the new controller, resolves under them" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let base = Setup.emptyGame S.bothPlayers
            (myrId, g0) = S.addCreature darksteelMyr S.alice base
            (srcId, g1) = S.addCreature prodigalSorcerer S.alice g0
            targetSlot = SlotName.MkSlotName (Text.pack "target")
            ability =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
                  ActivatedAbility.modal =
                    singleModeAbility
                      [Effect.GainControl (Duration.ForAsLongAs S.youControlSource) targetSlot]
                      (Map.singleton targetSlot (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact)))),
                  ActivatedAbility.timing = ActivationTiming.AnyTime
                }
            -- Control of the SOURCE CREATURE moves to bob BEFORE activation.
            taken = S.giveControl srcId S.bob g1
            g2 = taken {GameState.priority = Just S.bob}
            activated = snd (Engine.runGamePure S.identityAnswer g2 (Activate.activateAbility S.bob srcId ability))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            stored = filter (S.continuousEffectAffects myrId) (GameState.continuousEffects resolved)
        HU.assertEqual "bob controls the stolen sorcerer" (Just S.bob) (Projection.controllerOf srcId activated)
        HU.assertEqual "one thing on the stack" 1 (length (GameState.stack activated))
        HU.assertEqual "stack empty after resolution" [] (GameState.stack resolved)
        HU.assertEqual
          "control of the artifact changed to bob, the new controller who activated it"
          (Just S.bob)
          (Projection.controllerOf myrId resolved)
        HU.assertEqual "exactly one stored effect names the artifact" 1 (length stored)
    ]

isActivate :: A.Action -> Bool
isActivate a = case a of
  A.Activate _ _ -> True
  _ -> False
