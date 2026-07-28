{-# LANGUAGE GADTs #-}

-- Covers Pawl.Activate: activating an ability onto the stack, summoning-sickness
-- gating, and the CR 605 mana-ability exclusion from stack activations.
module Pawl.ActivateSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
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
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.ActivationTiming as ActivationTiming
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.LastKnown as LastKnown
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.Recipient as Recipient
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
lastKnownTests :: Registry.Type.Registry -> Tasty.TestTree
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
cyclingBoard :: Registry.Type.Registry -> IO (ObjectId.ObjectId, GameState.GameState)
cyclingBoard registry = do
  mauler <- Registry.printing registry "Barkhide Mauler"
  forest <- Registry.printing registry "Forest"
  piker <- Registry.printing registry "Goblin Piker"
  let (_, g0) = S.addLibraryCard piker S.alice (S.landsInPlay forest 2)
      (g1, oid) = S.handOne mauler g0
  pure (oid, g1 {GameState.priority = Just S.alice})

cyclingTests :: Registry.Type.Registry -> Tasty.TestTree
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
