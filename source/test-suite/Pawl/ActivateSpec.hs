{-# LANGUAGE GADTs #-}

-- Covers Pawl.Activate: activating an ability onto the stack, summoning-sickness
-- gating, and the CR 605 mana-ability exclusion from stack activations.
module Pawl.ActivateSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
import qualified Pawl.Combat as Combat
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.GameState as GameState
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
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Finds the first matching library card on a search, else fails to find.
findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches -> case matches of
    m : _ -> Just m
    [] -> Nothing
  _ -> S.identityAnswer p

-- Answers ChooseModes with the empty set, which is an illegal answer to any
-- selection demanding one or more -- the reject-not-repair trigger.
chooseNoModes :: Prompt.Prompt r -> r
chooseNoModes p = case p of
  Prompt.ChooseModes {} -> Set.empty
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
  Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList effects) specs Optionality.Mandatory)) (ModeSelection.ChooseExactly 1)

tests :: Registry.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Pawl.Activate"
    [ printedActivationTimingTests registry,
      HU.testCase "CR 602 activating Prodigal Sorcerer's {T} puts an ability on the stack and taps it" $ do
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
            firstKill = S.settleSba (S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable [skel]))
            -- Second Murder: no shield -> dies.
            secondKill = S.settleSba (S.runPure S.identityAnswer firstKill (Event.destroy Regenerability.Regenerable [skel]))
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
      -- controller-sensitive OUTPUT, so it could not make "whose control"
      -- observable. Aladdin's can: "{1}{R}{R}, {T}: Gain control of target
      -- artifact for as long as you control this creature" is the same
      -- GainControl/ForAsLongAs shape Master Thief's ETB uses in
      -- Pawl.ExpirySpec's masterThiefTests, but on the ACTIVATED path -- which
      -- is what retired the synthetic ability these two tests used to carry.
      HU.testCase "CR 113.8 an activated ability resolves under whoever activated it, not a later controller" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        mountain <- Registry.printing registry "Mountain"
        aladdin <- Registry.printing registry "Aladdin"
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
      HU.testCase "CR 113.8 a stolen creature's ability, activated by the new controller, resolves under them" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        mountain <- Registry.printing registry "Mountain"
        aladdin <- Registry.printing registry "Aladdin"
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
        HU.assertEqual "bob controls the stolen Aladdin" (Just S.bob) (Projection.controllerOf srcId activated)
        HU.assertEqual "one thing on the stack" 1 (length (GameState.stack activated))
        HU.assertEqual "stack empty after resolution" [] (GameState.stack resolved)
        HU.assertEqual
          "control of the artifact changed to bob, the new controller who activated it"
          (Just S.bob)
          (Projection.controllerOf myrId resolved)
        HU.assertEqual "exactly one stored effect names the artifact" 1 (length stored),
      lastKnownTests registry
    ]

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
lastKnownTests :: Registry.Registry -> Tasty.TestTree
lastKnownTests registry =
  Tasty.testGroup
    "LastKnownInformation"
    [ HU.testCase "CR 113.7a whole card: a sacrificed Ghitu Fire-Eater still deals damage equal to its power" $ do
        ghituFireEater <- Registry.printing registry "Ghitu Fire-Eater"
        let (srcId, g0) = S.addCreature ghituFireEater S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
            activated = snd (Engine.runGamePure (aimAt S.bob) g1 (Activate.activateAbility S.alice srcId (theAbility ghituFireEater)))
            resolved = snd (Engine.runGamePure (aimAt S.bob) activated Stack.resolveTop)
        HU.assertBool "the cost really sacrificed it" (not (Set.member srcId (GameState.battlefield activated)))
        HU.assertBool "and the id it left behind names nothing" (Maybe.isNothing (Game.lookupObject srcId activated))
        HU.assertEqual "bob took the Fire-Eater's 2" (Just 18) (S.lifeOf S.bob resolved),
      HU.testCase "CR 608.2h the value is LAST KNOWN, not printed: a pumped Fire-Eater deals 5" $ do
        -- The discriminator between reading last known information and reading
        -- the printed card. Both answer 2 for a vanilla Fire-Eater; only last
        -- known information answers 5 for one that was pumped before it left.
        ghituFireEater <- Registry.printing registry "Ghitu Fire-Eater"
        let (srcId, g0) = S.addCreature ghituFireEater S.alice (Setup.emptyGame S.bothPlayers)
            pumped = S.withEffect srcId (Modification.ModifyPowerToughness (Quantity.Type.Literal 3) (Quantity.Type.Literal 3)) g0
            g1 = pumped {GameState.priority = Just S.alice}
            activated = snd (Engine.runGamePure (aimAt S.bob) g1 (Activate.activateAbility S.alice srcId (theAbility ghituFireEater)))
            resolved = snd (Engine.runGamePure (aimAt S.bob) activated Stack.resolveTop)
        HU.assertEqual "it was a 5/5 while it was on the battlefield" (Just 5) (Projection.powerOf srcId g1)
        HU.assertEqual "bob took 5, not the printed 2" (Just 15) (S.lifeOf S.bob resolved),
      HU.testCase "CR 608.2h leaving a zone files last known information under the OLD id" $ do
        -- The substrate, on its own. CR 400.7 mints a fresh id for the graveyard
        -- incarnation, so the id an ability on the stack still holds is the one
        -- this map has to be keyed by.
        ghituFireEater <- Registry.printing registry "Ghitu Fire-Eater"
        let (srcId, g0) = S.addCreature ghituFireEater S.alice (Setup.emptyGame S.bothPlayers)
            pumped = S.withEffect srcId (Modification.ModifyPowerToughness (Quantity.Type.Literal 3) (Quantity.Type.Literal 3)) g0
        HU.assertEqual "nothing filed before it moves" Nothing (Map.lookup srcId (GameState.lastKnown pumped))
        let moved = S.runPure S.identityAnswer pumped (Event.changeZone srcId Zone.Graveyard)
        HU.assertEqual
          "the snapshot is the projected power it had, not the printed one"
          (Just (Just 5))
          (fmap (PC.power . LastKnown.characteristics) (Map.lookup srcId (GameState.lastKnown moved)))
        -- CR 613.1b / 603.3a: the record keeps who controlled it as it left, not
        -- only what it looked like -- the half Event.eventTriggers needs to hand
        -- a dead entrant's trigger to the right player.
        HU.assertEqual
          "and who controlled it as it left"
          (Just S.alice)
          (fmap LastKnown.controller (Map.lookup srcId (GameState.lastKnown moved))),
      HU.testCase "CR 608.2h the fallback is only a fallback: a source still there reads LIVE" $ do
        -- Discriminating against a viewWithLastKnown that always consults the
        -- map: a Fire-Eater that has not moved must read its current projection,
        -- and the map has nothing filed for it at all.
        ghituFireEater <- Registry.printing registry "Ghitu Fire-Eater"
        let (srcId, g0) = S.addCreature ghituFireEater S.alice (Setup.emptyGame S.bothPlayers)
            pumped = S.withEffect srcId (Modification.ModifyPowerToughness (Quantity.Type.Literal 3) (Quantity.Type.Literal 3)) g0
        HU.assertEqual
          "the live projection is what the source-aware view returns"
          (Just 5)
          (Projection.viewWithLastKnown srcId pumped srcId >>= Filter.power)
        HU.assertEqual
          "and every other id is untouched by the substitution"
          (Projection.fullView pumped S.noSource >>= Filter.power)
          (Projection.viewWithLastKnown srcId pumped S.noSource >>= Filter.power),
      cyclingTests registry
    ]

-- CR 702.29: cycling, the first activated ability in the pool that is activated
-- from a zone other than the battlefield. Barkhide Mauler is a {4}{G} 4/4 whose
-- only text is "Cycling {2}", so every test below isolates the keyword: the
-- creature half is never cast.
--
-- Alice holds it, two Forests pay the {2}, and her library has a card to draw --
-- otherwise CR 704.5b would end the game around the assertion rather than the
-- draw being observable.
cyclingBoard :: Registry.Registry -> IO (ObjectId.ObjectId, GameState.GameState)
cyclingBoard registry = do
  mauler <- Registry.printing registry "Barkhide Mauler"
  forest <- Registry.printing registry "Forest"
  piker <- Registry.printing registry "Goblin Piker"
  let (_, g0) = S.addLibraryCard piker S.alice (S.landsInPlay forest 2)
      (g1, oid) = S.handOne mauler g0
  pure (oid, g1 {GameState.priority = Just S.alice})

cyclingTests :: Registry.Registry -> Tasty.TestTree
cyclingTests registry =
  Tasty.testGroup
    "Cycling"
    [ -- CR 702.29a: "Cycling is an activated ability that functions only while
      -- the card with cycling is in a player's hand."
      HU.testCase "CR 702.29a cycling is offered from the hand" $ do
        (_, gs) <- cyclingBoard registry
        HU.assertBool "an Activate is offered" (any isActivate (Action.legalActions S.alice gs)),
      -- The ability rule 702.29a MEANS, rather than any the card prints: Barkhide
      -- Mauler's own activatedAbilities list is empty, and what is offered is
      -- minted from the keyword -- printed cost plus the rule's discard.
      HU.testCase "CR 702.29a the minted ability is '{2}, Discard this card: Draw a card'" $ do
        mauler <- Registry.printing registry "Barkhide Mauler"
        (oid, gs) <- cyclingBoard registry
        HU.assertEqual "the card itself prints no activated ability" [] (Card.Type.activatedAbilities (Printing.card mauler))
        case Activate.abilitiesFor oid gs of
          [ability] -> do
            HU.assertEqual
              "the printed {2} plus rule 702.29a's discard"
              (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) [CostComponent.DiscardThis])
              (ActivatedAbility.cost ability)
            HU.assertEqual "instant speed" ActivationTiming.AnyTime (ActivatedAbility.timing ability)
          abilities -> HU.assertFailure ("expected exactly one ability, got " <> show (length abilities)),
      -- The whole card: pay {2}, discard it, draw. The Mauler is in the graveyard
      -- BEFORE the draw resolves, which is rule 702.29a putting the discard in
      -- the cost rather than in the effect.
      HU.testCase "CR 702.29a whole card: cycling discards the Mauler and draws" $ do
        (oid, gs) <- cyclingBoard registry
        case Activate.abilitiesFor oid gs of
          [ability] -> do
            let activated = snd (Engine.runGamePure S.identityAnswer gs (Activate.activateAbility S.alice oid ability))
                resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
                tapped g = length (filter (\o -> Object.tapped o == TapState.Tapped) (Maybe.mapMaybe (\o -> Game.lookupObject o g) (Set.toList (GameState.battlefield g))))
            HU.assertEqual "the Mauler left the hand as the cost was paid" 0 (length (Game.zoneMembers Zone.Hand S.alice activated))
            HU.assertEqual "and is in the graveyard while the draw is still on the stack" 1 (length (Game.zoneMembers Zone.Graveyard S.alice activated))
            HU.assertEqual "the draw is on the stack" 1 (length (GameState.stack activated))
            HU.assertEqual "both Forests paid" 2 (tapped activated)
            HU.assertEqual "then the draw resolves into her hand" 1 (length (Game.zoneMembers Zone.Hand S.alice resolved))
            HU.assertEqual "her library is empty now" 0 (length (Game.zoneMembers Zone.Library S.alice resolved))
            HU.assertEqual "the stack is empty" [] (GameState.stack resolved)
          _ -> HU.assertFailure "expected exactly one cycling ability",
      -- The zone gate, from the other side. CR 702.29b: the ability still EXISTS
      -- on the battlefield ("it continues to exist ... in all other zones"), so
      -- this is not the ability being absent -- it is not being activatable
      -- there. A Mauler that resolved as a creature cannot be cycled.
      HU.testCase "CR 702.29a cycling is NOT offered from the battlefield" $ do
        mauler <- Registry.printing registry "Barkhide Mauler"
        forest <- Registry.printing registry "Forest"
        let (_, g0) = S.addCreature mauler S.alice (S.landsInPlay forest 2)
            gs = g0 {GameState.priority = Just S.alice}
        HU.assertBool "no Activate offered" (not (any isActivate (Action.legalActions S.alice gs))),
      -- "In a PLAYER's hand" is that player's, and CR 108.4 is why this needs
      -- saying: a card in a hand has no controller at all, so an activation gate
      -- written against control would have offered it to nobody -- or, read
      -- loosely, to anybody.
      HU.testCase "CR 702.29a the other player cannot cycle a card in your hand" $ do
        (_, gs) <- cyclingBoard registry
        HU.assertBool "not offered to bob" (not (any isActivate (Action.legalActions S.bob gs {GameState.priority = Just S.bob}))),
      -- CR 118.3: the cost still has to be payable. One Forest is not {2}.
      HU.testCase "CR 118.3 cycling is not offered without the mana" $ do
        mauler <- Registry.printing registry "Barkhide Mauler"
        forest <- Registry.printing registry "Forest"
        let (g0, _) = S.handOne mauler (S.landsInPlay forest 1)
            gs = g0 {GameState.priority = Just S.alice}
        HU.assertBool "one Forest cannot pay {2}" (not (any isActivate (Action.legalActions S.alice gs))),
      -- CR 702.29e: typecycling is the same ability with a search in place of the
      -- draw. Ash Barrens' basic landcycling {1} finds a basic land card and puts
      -- it in HAND -- not onto the battlefield, which is the destination Evolving
      -- Wilds' search has and the one this rule does not.
      HU.testCase "CR 702.29e whole card: basic landcycling Ash Barrens fetches a Forest to hand" $ do
        barrens <- Registry.printing registry "Ash Barrens"
        forest <- Registry.printing registry "Forest"
        island <- Registry.printing registry "Island"
        let (_, g0) = S.addLibraryCard forest S.alice (S.landsInPlay island 1)
            (g1, oid) = S.handOne barrens g0
            gs = g1 {GameState.priority = Just S.alice}
        case Activate.abilitiesFor oid gs of
          [ability] -> do
            let cycled = S.runPure findFirst gs (Activate.activateAbility S.alice oid ability)
                after = S.runPure findFirst cycled Stack.resolveTop
            HU.assertEqual "Ash Barrens paid its own cost into the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice cycled))
            HU.assertEqual "the Forest is in hand" 1 (length (Game.zoneMembers Zone.Hand S.alice after))
            HU.assertEqual "and out of the library" 0 (length (Game.zoneMembers Zone.Library S.alice after))
            HU.assertEqual "nothing was put onto the battlefield: one Island, no Forest" 1 (length (Set.toList (GameState.battlefield after)))
          abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
      -- CR 702.29e's filter is the card's, and it is a real narrowing: a library
      -- with no basic land in it finds nothing, and the cycler is still discarded
      -- (CR 701.23b -- a player "isn't required to find" a card, and here cannot).
      HU.testCase "CR 702.29e basic landcycling finds nothing in a library of nonbasics" $ do
        barrens <- Registry.printing registry "Ash Barrens"
        piker <- Registry.printing registry "Goblin Piker"
        island <- Registry.printing registry "Island"
        let (_, g0) = S.addLibraryCard piker S.alice (S.landsInPlay island 1)
            (g1, oid) = S.handOne barrens g0
            gs = g1 {GameState.priority = Just S.alice}
        case Activate.abilitiesFor oid gs of
          [ability] -> do
            let cycled = S.runPure findFirst gs (Activate.activateAbility S.alice oid ability)
                after = S.runPure findFirst cycled Stack.resolveTop
            HU.assertEqual "the Piker is not a basic land, so it stays in the library" 1 (length (Game.zoneMembers Zone.Library S.alice after))
            HU.assertEqual "the hand is empty" 0 (length (Game.zoneMembers Zone.Hand S.alice after))
            HU.assertEqual "and Ash Barrens was still discarded" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
          abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
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
      HU.testCase "CR 701.20a basic landcycling reveals the Forest it fetches" $ do
        barrens <- Registry.printing registry "Ash Barrens"
        forest <- Registry.printing registry "Forest"
        island <- Registry.printing registry "Island"
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
            HU.assertEqual "only the announcement reveal before the ability resolves" [(S.alice, Text.pack "Ash Barrens")] (S.revealsOf cycled)
            HU.assertEqual
              "then Alice revealed the Forest she found"
              [(S.alice, Text.pack "Ash Barrens"), (S.alice, Text.pack "Forest")]
              (S.revealsOf after)
          abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
      -- CR 701.20a reveals "that card", so a search that finds none reveals
      -- nothing. The negative half of the test above: CR 701.23b lets a player
      -- fail to find, and a failed find must not put an empty or filler reveal
      -- into the log for an opponent to read something into. The CR 602.2a
      -- announcement reveal is unaffected -- it already happened, and whether
      -- the search finds anything cannot reach back and unshow the cycler.
      HU.testCase "CR 701.20a a search that finds nothing reveals nothing" $ do
        barrens <- Registry.printing registry "Ash Barrens"
        piker <- Registry.printing registry "Goblin Piker"
        island <- Registry.printing registry "Island"
        let (_, g0) = S.addLibraryCard piker S.alice (S.landsInPlay island 1)
            (g1, oid) = S.handOne barrens g0
            gs = g1 {GameState.priority = Just S.alice}
        case Activate.abilitiesFor oid gs of
          [ability] -> do
            let cycled = S.runPure findFirst gs (Activate.activateAbility S.alice oid ability)
                after = S.runPure findFirst cycled Stack.resolveTop
            HU.assertEqual "no basic land found, so only the announcement reveal" [(S.alice, Text.pack "Ash Barrens")] (S.revealsOf after)
          abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
      -- CR 702.29f: "typecycling abilities are cycling abilities, and typecycling
      -- costs are cycling costs." The engine gets that for free by minting both
      -- from one keyword arm -- the discard is in the cost either way, and the
      -- only difference is what resolves.
      HU.testCase "CR 702.29f a typecycling ability is a cycling ability" $ do
        barrens <- Registry.printing registry "Ash Barrens"
        mauler <- Registry.printing registry "Barkhide Mauler"
        island <- Registry.printing registry "Island"
        let (g0, barrensId) = S.handOne barrens (S.landsInPlay island 2)
            (g1, maulerId) = S.handOne mauler g0
            gs = g1 {GameState.priority = Just S.alice}
        case (Activate.abilitiesFor barrensId gs, Activate.abilitiesFor maulerId gs) of
          ([typecycler], [plain]) -> do
            HU.assertEqual
              "both costs end in rule 702.29a's discard"
              (Just CostComponent.DiscardThis, Just CostComponent.DiscardThis)
              (Maybe.listToMaybe (reverse (Cost.Type.components (ActivatedAbility.cost typecycler))), Maybe.listToMaybe (reverse (Cost.Type.components (ActivatedAbility.cost plain))))
            HU.assertEqual "and both are instant speed" (ActivationTiming.AnyTime, ActivationTiming.AnyTime) (ActivatedAbility.timing typecycler, ActivatedAbility.timing plain)
          _ -> HU.assertFailure "expected one cycling ability on each",
      -- CR 701.20a from the battlefield, and by a card that prints the word
      -- itself: "{2}, {T}, Sacrifice this artifact: Search your library for a
      -- basic land card, reveal that card, put it into your hand, then shuffle."
      -- Ash Barrens above reaches the same destination through rule 702.29e, so
      -- this is the half that proves the reveal belongs to the CARD's sentence
      -- (CR 701.23e) rather than to the keyword that happened to arrive first.
      HU.testCase "CR 701.20a whole card: Braidwood Sextant fetches a Forest and reveals it" $ do
        sextant <- Registry.printing registry "Braidwood Sextant"
        forest <- Registry.printing registry "Forest"
        island <- Registry.printing registry "Island"
        let (sextantId, g0) = S.addCreature sextant S.alice (S.landsInPlay island 2)
            (_, g1) = S.addLibraryCard forest S.alice g0
            gs = g1 {GameState.priority = Just S.alice}
            ability = theAbility sextant
            activated = S.runPure findFirst gs (Activate.activateAbility S.alice sextantId ability)
            after = S.runPure findFirst activated Stack.resolveTop
        HU.assertBool "the Sextant sacrificed itself paying the cost" (not (Set.member sextantId (GameState.battlefield activated)))
        -- ONE reveal, where Ash Barrens' typecycling produces two: the Sextant
        -- is announced from the BATTLEFIELD, which CR 400.2 makes a public zone,
        -- so CR 602.2a adds nothing. The contrast between these two tests is the
        -- whole of what that rule's zone condition does.
        HU.assertEqual "nothing revealed while the ability is still on the stack" [] (S.revealsOf activated)
        HU.assertEqual "Alice revealed the Forest, and only the Forest" [(S.alice, Text.pack "Forest")] (S.revealsOf after)
        HU.assertEqual "the Forest is in her hand" 1 (length (Game.zoneMembers Zone.Hand S.alice after))
        HU.assertEqual "and out of her library" 0 (length (Game.zoneMembers Zone.Library S.alice after))
        -- CR 701.20b: revealing does not move the card, so the two Islands that
        -- paid for this are the whole battlefield -- the Forest went to hand.
        HU.assertEqual "only the two Islands remain on the battlefield" 2 (length (Game.zoneMembers Zone.Battlefield S.alice after))
        -- "reveal that card, put it into your hand" is an ORDER, not two
        -- independent facts, and CR 701.20b is what gives it teeth: the reveal
        -- happens while the card is still in the library, so it precedes the
        -- move in the log. Swapping the two would leave every other assertion
        -- here passing.
        case (List.findIndex (Maybe.isJust . Event.revealOf) (Foldable.toList (GameState.events after)), List.findIndex ((== Just Zone.Hand) . fmap ZoneChange.to . Event.movedOf) (Foldable.toList (GameState.events after))) of
          (Just revealed, Just moved) -> HU.assertBool "the reveal is logged before the move into the hand" (revealed < moved)
          indices -> HU.assertFailure ("expected both a reveal and a move into the hand, got " <> show indices),
      -- CR 602.2a: "The player announces that they are activating the ability.
      -- If an activated ability is being activated from a hidden zone, the card
      -- that has that ability is revealed." CR 400.2 names the hidden zones:
      -- "Library and hand are hidden zones."
      --
      -- The reveal is at ANNOUNCEMENT, which for cycling is before its own cost
      -- discards the card -- so the log shows the Mauler being revealed and only
      -- then moving to the graveyard. Both halves are asserted: an engine that
      -- revealed at the wrong moment would still show the right card.
      HU.testCase "CR 602.2a cycling from hand reveals the Mauler as the ability is announced" $ do
        (oid, gs) <- cyclingBoard registry
        case Activate.abilitiesFor oid gs of
          [ability] -> do
            let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability)
                events = Foldable.toList (GameState.events activated)
            HU.assertEqual "Alice revealed Barkhide Mauler" [(S.alice, Text.pack "Barkhide Mauler")] (S.revealsOf activated)
            HU.assertEqual "and the draw has not resolved, so this is the announcement" 1 (length (GameState.stack activated))
            case (List.findIndex (Maybe.isJust . Event.revealOf) events, List.findIndex ((== Just Zone.Graveyard) . fmap ZoneChange.to . Event.movedOf) events) of
              (Just revealed, Just discarded) -> HU.assertBool "revealed before the cost discarded it" (revealed < discarded)
              indices -> HU.assertFailure ("expected both a reveal and a discard, got " <> show indices)
          abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
      -- The control, and the reason CR 602.2a needs a zone test rather than
      -- revealing unconditionally: CR 400.2 makes the battlefield a PUBLIC zone,
      -- where every player can already see the card. Prodigal Sorcerer's {T} is
      -- announced the same way and shows nobody anything.
      HU.testCase "CR 400.2 activating from the battlefield reveals nothing" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            gs = g0 {GameState.priority = Just S.alice}
            activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice srcId (theAbility prodigalSorcerer))
        HU.assertEqual "the ability is on the stack" 1 (length (GameState.stack activated))
        HU.assertEqual "and the battlefield is public, so nothing was revealed" [] (S.revealsOf activated),
      -- CR 602.2a's reveal belongs to an activation that HAPPENED. pawl's
      -- reject-not-repair guard (an interpreter answering with an illegal mode
      -- set) makes the whole activation a no-op, and the reveal has to go back
      -- with it -- otherwise the log claims Alice showed her opponent a card
      -- over an activation the engine refused.
      --
      -- The ability is hand-built because no card in the pool prints a MODAL
      -- ability activatable from a hidden zone; the Mauler is a real card in a
      -- real hand, which is the part under test.
      HU.testCase "CR 602.2a a rejected activation reveals nothing" $ do
        mauler <- Registry.printing registry "Barkhide Mauler"
        forest <- Registry.printing registry "Forest"
        let (g0, oid) = S.handOne mauler (S.landsInPlay forest 2)
            gs = g0 {GameState.priority = Just S.alice}
            -- Two fillable modes, choose one: more legal than the count, so
            -- ChooseModes is really asked and a bad answer can really be given.
            -- The modes are empty because what they DO is not under test.
            twoModes =
              ActivatedAbility.MkActivatedAbility
                (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [])
                ( Modal.MkModal
                    (Seq.fromList [Mode.MkMode Seq.empty Map.empty Optionality.Mandatory, Mode.MkMode Seq.empty Map.empty Optionality.Mandatory])
                    (ModeSelection.ChooseExactly 1)
                )
                ActivationTiming.AnyTime
            after = S.runPure chooseNoModes gs (Activate.activateAbility S.alice oid twoModes)
        HU.assertEqual "the activation was rejected: nothing on the stack" [] (GameState.stack after)
        HU.assertEqual "so no reveal survives it either" [] (S.revealsOf after)
        HU.assertEqual "and the Mauler is still in her hand" 1 (length (Game.zoneMembers Zone.Hand S.alice after)),
      -- The control: the gate cannot pass by offering every card in hand. A
      -- Piker has no cycling and nothing is minted for it.
      HU.testCase "CR 702.29a a card without cycling offers nothing from the hand" $ do
        piker <- Registry.printing registry "Goblin Piker"
        forest <- Registry.printing registry "Forest"
        let (g0, oid) = S.handOne piker (S.landsInPlay forest 2)
            gs = g0 {GameState.priority = Just S.alice}
        HU.assertEqual "no abilities minted" [] (Activate.abilitiesFor oid gs)
        HU.assertBool "and no Activate offered" (not (any isActivate (Action.legalActions S.alice gs)))
    ]

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
-- The mirror of Pawl.CastSpec's printedCastingRestrictionTests, and deliberately
-- not the same type -- see Pawl.Types.ActivationTiming for why the two gates
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

printedActivationTimingTests :: Registry.Registry -> Tasty.TestTree
printedActivationTimingTests registry =
  Tasty.testGroup
    "PrintedActivationTiming"
    [ -- The rider, isolated. bob is in the declare attackers step with an
      -- untapped Desert, a legal target (CR 508.1f has just tapped alice's
      -- attacker, and it is attacking) and no cost to pay beyond {T}. The only
      -- thing withholding the ability is the printed window.
      --
      -- Carries its own control in the same test, on the same board and for the
      -- same player: bob's Prodigal Sorcerer ("{T}: This creature deals 1 damage
      -- to any target", CR 602.2 and no rider) IS offered, so what stops the
      -- Desert is the rider and not the step being closed to bob altogether.
      HU.testCase "CR 307.5 the Desert's ping is NOT offered in the declare attackers step" $ do
        piker <- Registry.printing registry "Goblin Piker"
        desert <- Registry.printing registry "Desert"
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (gs0, _, theirs) = S.combatBoardOf [piker] [prodigalSorcerer]
            (desertId, gs1) = S.addCreature desert S.bob gs0
            attacked = desertAttacked gs1
            offered = Action.legalActions S.bob attacked
        HU.assertEqual "no activation of the Desert" [] (activationsOf desertId offered)
        case theirs of
          sorcererId : _ ->
            HU.assertBool "but bob's unrestricted ability is offered in the same step" (not (null (activationsOf sorcererId offered)))
          [] -> HU.assertFailure "fixture should have given bob a Prodigal Sorcerer",
      -- CR 511.1: "The end of combat step has no turn-based actions. Once it
      -- begins, the active player gets priority." The printed window, reached.
      --
      -- CR 511.3 is what makes the window useful at all: "As soon as the end of
      -- combat step ENDS, all creatures ... are removed from combat", so a
      -- creature declared as an attacker is still attacking throughout this step
      -- and the ability still has something to target.
      HU.testCase "CR 307.5 the Desert's ping IS offered in the end of combat step" $ do
        piker <- Registry.printing registry "Goblin Piker"
        desert <- Registry.printing registry "Desert"
        let (desertId, _, board) = desertBoard piker desert
            atEndOfCombat = (desertAttacked board) {GameState.phase = Phase.Combat CombatStep.EndOfCombat}
        HU.assertEqual "exactly one activation, the ping" 1 (length (activationsOf desertId (Action.legalActions S.bob atEndOfCombat))),
      -- The same board one step later, differing in nothing but the phase. The
      -- combat record still holds the attack -- asserted, so this cannot pass
      -- because the target pool emptied -- and the ability is gone all the same.
      HU.testCase "CR 307.5 the Desert's ping is NOT offered in the postcombat main phase" $ do
        piker <- Registry.printing registry "Goblin Piker"
        desert <- Registry.printing registry "Desert"
        let (desertId, _, board) = desertBoard piker desert
            attacked = desertAttacked board
            later = attacked {GameState.phase = Phase.PostcombatMain}
        HU.assertBool "the attack is still on the record" (Combat.Type.attackersJoined (GameState.combat later))
        HU.assertEqual "still offered in the step it names" 1 (length (activationsOf desertId (Action.legalActions S.bob (attacked {GameState.phase = Phase.Combat CombatStep.EndOfCombat}))))
        HU.assertEqual "and not one phase later" [] (activationsOf desertId (Action.legalActions S.bob later)),
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
      HU.testCase "CR 307.5 whole card: Desert pings the attacker dead in the end of combat step" $ do
        piker <- Registry.printing registry "Goblin Piker"
        desert <- Registry.printing registry "Desert"
        let (desertId, attackerId, board) = desertBoard piker desert
            after = S.runCombat pingAnswer board
        HU.assertEqual "the Piker connected first" (Just 18) (S.lifeOf S.bob after)
        HU.assertBool "and then died to the ping" (not (Set.member attackerId (GameState.battlefield after)))
        HU.assertEqual "the Desert paid its {T}" (Just TapState.Tapped) (fmap Object.tapped (Game.lookupObject desertId after)),
      -- The control for the whole-card test: the same board and the same combat,
      -- with an interpreter that never activates. The Piker survives, so what
      -- killed it above was the ability and not combat.
      HU.testCase "CR 307.5 whole card: the attacker survives a combat bob does not ping in" $ do
        piker <- Registry.printing registry "Goblin Piker"
        desert <- Registry.printing registry "Desert"
        let (_, attackerId, board) = desertBoard piker desert
            after = S.runCombat S.aggressiveAnswer board
        HU.assertEqual "the Piker still connected" (Just 18) (S.lifeOf S.bob after)
        HU.assertBool "and is still on the battlefield" (Set.member attackerId (GameState.battlefield after))
    ]
