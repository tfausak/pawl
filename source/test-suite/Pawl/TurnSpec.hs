{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Turn: turn structure, the phase schedule, the CR 508.8 skips, and
-- CR 500.8's added phases -- Aggravated Assault and Relentless Assault for the
-- combat-and-main pair, Aurelia, the Warleader for one added from INSIDE a
-- combat phase, and Full Throttle for two combat phases and none of main.
--
-- Also CR 500.7's extra TURNS, which Engine.handoffTurn deals out -- Time Warp,
-- the pool's one creator of any.
module Pawl.TurnSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Pawl.Activate as Activate
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Turn as Turn
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import Pawl.Types.Phase (Phase)
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Registry as Registry.Type
import qualified Pawl.Types.TapState as TapState
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

turnTests :: Tasty.TestTree
turnTests =
  Tasty.testGroup
    "Turn"
    [ HU.testCase "firstPhase is the untap step" $
        HU.assertEqual "firstPhase" (Phase.Beginning BeginningStep.Untap) Turn.firstPhase,
      HU.testCase "a turn has twelve steps" $
        HU.assertEqual "twelve" 12 (length Turn.allPhases),
      HU.testCase "firstPhase and laterPhases reconstruct the turn template" $
        HU.assertEqual "reconstruct" (Seq.fromList (drop 1 Turn.allPhases)) Turn.laterPhases,
      HU.testCase "untap and cleanup grant no priority"
        . HU.assertBool "no priority"
        $ not (Turn.grantsPriority (Phase.Beginning BeginningStep.Untap))
          && not (Turn.grantsPriority (Phase.Ending EndingStep.Cleanup)),
      QC.testProperty "a turn never revisits a phase" $
        QC.property (length Turn.allPhases == length (dedupe Turn.allPhases))
    ]

turnDataTests :: Tasty.TestTree
turnDataTests =
  Tasty.testGroup
    "TurnData"
    [ HU.testCase "advance pops the schedule head into the current phase" $
        let gs0 = Setup.emptyGame S.bothPlayers
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
                }
            after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.advance)
         in do
              HU.assertEqual "phase" (Phase.Combat CombatStep.BeginningOfCombat) (GameState.phase after)
              HU.assertEqual "remaining" (Seq.fromList [Phase.PostcombatMain]) (GameState.remaining after),
      HU.testCase "advance on an empty schedule hands off the turn" $
        let gs0 = Setup.emptyGame S.bothPlayers
            gs =
              gs0
                { GameState.phase = Phase.Ending EndingStep.Cleanup,
                  GameState.remaining = Seq.empty,
                  GameState.activePlayer = S.alice,
                  GameState.turnNumber = 1
                }
            after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.advance)
         in do
              HU.assertEqual "new active player" S.bob (GameState.activePlayer after)
              HU.assertEqual "phase reset" Turn.firstPhase (GameState.phase after)
              HU.assertEqual "schedule refilled" Turn.laterPhases (GameState.remaining after)
              HU.assertEqual "turn incremented" 2 (GameState.turnNumber after),
      HU.testCase "a fresh game starts at untap with the rest of the turn scheduled" $
        let gs = Setup.emptyGame S.bothPlayers
         in do
              HU.assertEqual "phase" Turn.firstPhase (GameState.phase gs)
              HU.assertEqual "remaining" Turn.laterPhases (GameState.remaining gs)
    ]

skipTests :: Registry.Type.Registry -> Tasty.TestTree
skipTests registry =
  Tasty.testGroup
    "Skip"
    [ HU.testCase "CR 511.3 thisPhase inside a combat phase ends at ITS end of combat" $
        -- Two whole combat phases back to back -- the arrangement CR 500.8
        -- permits and Aurelia, the Warleader builds. Splitting at the FIRST end
        -- of combat step is what leaves the second one whole.
        let remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.Combat CombatStep.BeginningOfCombat,
                  Phase.Combat CombatStep.DeclareAttackers,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
            expected =
              ( Seq.fromList
                  [ Phase.Combat CombatStep.DeclareBlockers,
                    Phase.Combat CombatStep.CombatDamage,
                    Phase.Combat CombatStep.EndOfCombat
                  ],
                Seq.fromList
                  [ Phase.Combat CombatStep.BeginningOfCombat,
                    Phase.Combat CombatStep.DeclareAttackers,
                    Phase.Combat CombatStep.EndOfCombat,
                    Phase.PostcombatMain
                  ]
              )
         in HU.assertEqual
              "split at the first end of combat, inclusive"
              expected
              (Turn.thisPhase (Phase.Combat CombatStep.DeclareAttackers) remaining),
      HU.testCase "CR 505.2 thisPhase in a main phase has no steps of its own" $
        -- "The main phase has no steps", so there is nothing of it left in the
        -- schedule and everything remaining is already after it.
        let remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
         in HU.assertEqual "empty prefix" (Seq.empty, remaining) (Turn.thisPhase Phase.PrecombatMain remaining),
      HU.testCase "thisPhase yields an empty prefix when this phase's final step is gone" $
        -- Unreachable from either caller, and asserted anyway: dropping nothing
        -- is the safer failure than treating the whole rest of the turn as this
        -- phase.
        let remaining = Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep]
         in HU.assertEqual
              "empty prefix"
              (Seq.empty, remaining)
              (Turn.thisPhase (Phase.Combat CombatStep.EndOfCombat) remaining),
      HU.testCase "CR 508.8 dropSkippedCombatSteps removes declare blockers and combat damage" $
        let full =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
            expected = Seq.fromList [Phase.Combat CombatStep.EndOfCombat, Phase.PostcombatMain]
         in HU.assertEqual "dropped" expected (Turn.dropSkippedCombatSteps (Phase.Combat CombatStep.DeclareAttackers) full),
      HU.testCase "CR 500.8 dropSkippedCombatSteps spares a LATER combat phase's steps" $
        -- The schedule a CR 500.8 additional combat phase leaves behind: this
        -- combat's tail, then an additional main phase, then the turn's normal
        -- combat phase in full. CR 508.8 skipped THIS combat, so only the two
        -- steps before this phase's end of combat step (CR 511.3: "after the end
        -- of combat step ends, the combat phase is over") may go.
        let full =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain,
                  Phase.Combat CombatStep.BeginningOfCombat,
                  Phase.Combat CombatStep.DeclareAttackers,
                  Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
            expected =
              Seq.fromList
                [ Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain,
                  Phase.Combat CombatStep.BeginningOfCombat,
                  Phase.Combat CombatStep.DeclareAttackers,
                  Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
         in HU.assertEqual
              "only this phase's steps dropped"
              expected
              (Turn.dropSkippedCombatSteps (Phase.Combat CombatStep.DeclareAttackers) full),
      HU.testCase "CR 508.8 no attacker declared skips to end of combat" $
        -- Nobody has a creature, so no attackers are declared: the declare
        -- blockers and combat damage steps must not run at all.
        let (gs, _, _) = S.combatBoardOf [] []
            after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.runStep)
         in HU.assertEqual "jumped past the two dead steps" (Phase.Combat CombatStep.EndOfCombat) (GameState.phase after),
      HU.testCase "CR 508.8 an attacker keeps the declare blockers step" $ do
        -- The control: with an attacker, the step after declare attackers is
        -- declare blockers, exactly as before. So the skip is not "always skip".
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [piker] []
            after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.runStep)
        HU.assertEqual "declare blockers still next" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase after),
      HU.testCase "CR 508.8 an attacker-less combat changes no life total" $
        -- End to end: run the whole combat region. No attackers means no damage,
        -- and the turn still leaves combat cleanly.
        let (gs, _, _) = S.combatBoardOf [] []
            after = S.runCombat S.aggressiveAnswer gs
         in do
              HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "alice untouched" (Just 20) (S.lifeOf S.alice after)
              HU.assertBool "left combat" (not (S.inCombatPhase (GameState.phase after))),
      HU.testCase "CR 508.8 the skip stands even when an instant could have been cast" $ do
        -- bob holds a castable Bolt; nobody attacks. The blockers and damage
        -- steps are still dropped -- the priority windows an instant would use
        -- in them do not exist (CR 500.11: proceed as though they don't).
        --
        -- The WHOLE step, not just its turn-based actions: CR 508.8's condition
        -- is settled as the declare attackers step ends, because its second
        -- clause ("or put onto the battlefield attacking") can only come true in
        -- that step's priority round -- which is also the round this test's Bolt
        -- would be cast in.
        mountain <- Registry.printing registry "Mountain"
        bolt <- Registry.printing registry "Lightning Bolt"
        let (base, _) = S.boltInHand mountain bolt 1 (Phase.Combat CombatStep.DeclareAttackers)
            armed = base {GameState.activePlayer = S.bob}
            after = snd (Engine.runGamePure S.identityAnswer armed Engine.runStep)
            remaining = foldr (:) [] (GameState.remaining after)
        HU.assertBool "no blockers step" (notElem (Phase.Combat CombatStep.DeclareBlockers) remaining)
        HU.assertBool "no damage step" (notElem (Phase.Combat CombatStep.CombatDamage) remaining),
      HU.testCase "CR 508.8 an attacker removed from combat still keeps the two steps" $ do
        -- The whole card: alice attacks with her only creature, and bob answers
        -- with Ray of Command, whose "gain control of it" is CR 506.4's
        -- control-change clause -- so the Goblin Piker is REMOVED FROM COMBAT
        -- before the step ends and Combat.Type.attackers is empty when CR 508.8 is
        -- asked.
        --
        -- The steps stay anyway, because CR 508.8's condition is HISTORICAL: "if
        -- no creatures are DECLARED as attackers". One was. CR 508.1k is the
        -- rule that keeps the two apart -- a declared creature "remains an
        -- attacking creature until it's removed from combat", which ends its
        -- attacking, not its having been declared.
        piker <- Registry.printing registry "Goblin Piker"
        island <- Registry.printing registry "Island"
        ray <- Registry.printing registry "Ray of Command"
        let (base, _, _) = S.combatBoardOf [piker] []
            -- {3}{U}, so four.
            withLands = List.foldl' (\g _ -> snd (S.addCreature island S.bob g)) base [1 :: Int .. 4]
            (_, armed) = S.addHandCard ray S.bob withLands
            after = snd (Engine.runGamePure castingDefender armed Engine.runStep)
        HU.assertEqual "the attacker really left combat" [] (Map.keys (Combat.Type.attackers (GameState.combat after)))
        HU.assertEqual "declare blockers still next" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase after),
      HU.testCase "CR 508.8 a creature put onto the battlefield attacking keeps the two steps" $ do
        -- The rule's SECOND clause on its own, with nothing declared. Direct
        -- call, because no card in the pool can put a creature onto the
        -- battlefield attacking without first attacking with something (#370);
        -- this is the strongest statement of the clause available, and it is
        -- what stops the fix above from being narrowed to the declaration.
        piker <- Registry.printing registry "Goblin Piker"
        let (base, ours, _) = S.combatBoardOf [piker] []
            joined = S.runPure S.identityAnswer base (Foldable.traverse_ Combat.putOntoBattlefieldAttacking ours)
            after = Combat.skipEmptyCombat joined
            remaining = foldr (:) [] (GameState.remaining after)
        HU.assertEqual "no declaration was made" [] (S.attackerDeclarationsOf after)
        HU.assertBool "blockers step kept" (elem (Phase.Combat CombatStep.DeclareBlockers) remaining)
        HU.assertBool "damage step kept" (elem (Phase.Combat CombatStep.CombatDamage) remaining)
    ]

-- aggressiveAnswer, except that it takes any cast on offer instead of passing --
-- the shape the Ray of Command fixture above needs, where alice must attack and
-- bob must then cast. Pawl.Support's castAnswer is the other half of the same
-- pair and declines to attack, so neither one alone will do.
castingDefender :: Prompt.Prompt r -> r
castingDefender p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast _ -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass
  _ -> S.aggressiveAnswer p

-- The schedule Engine.advance leaves once the precombat main phase is current:
-- everything after it in an ordinary turn (Turn.allPhases).
afterPrecombatMain :: Seq Phase
afterPrecombatMain =
  Seq.fromList
    [ Phase.Combat CombatStep.BeginningOfCombat,
      Phase.Combat CombatStep.DeclareAttackers,
      Phase.Combat CombatStep.DeclareBlockers,
      Phase.Combat CombatStep.CombatDamage,
      Phase.Combat CombatStep.EndOfCombat,
      Phase.PostcombatMain,
      Phase.Ending EndingStep.EndStep,
      Phase.Ending EndingStep.Cleanup
    ]

-- alice, in her precombat main phase with priority, controlling five untapped
-- Mountains (exactly the {3}{R}{R} activation), an Aggravated Assault, and one
-- TAPPED creature of the given printing. bob has a tapped Goblin Piker, which
-- the ability's "you control" must leave alone.
assaultBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId, ObjectId)
assaultBoard mountain assault mine piker =
  let (enchantment, gs1) = S.addCreature assault S.alice (S.landsInPlay mountain 5)
      (ours, gs2) = S.addCreature mine S.alice gs1
      (theirs, gs3) = S.addCreature piker S.bob gs2
      gs =
        (S.tapObject theirs (S.tapObject ours gs3))
          { GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.remaining = afterPrecombatMain
          }
   in (gs, enchantment, ours, theirs)

-- The card's one printed activated ability.
assaultAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
assaultAbility assault = case Card.Type.activatedAbilities (Printing.card assault) of
  [] -> Nothing
  ability : _ -> Just ability

-- Activate it and let it resolve, through the real activation path (the
-- Bonesplitter shape in Pawl.AuraSpec) -- not Resolve.applyEffect, so the
-- {3}{R}{R} is genuinely paid off the board. The CR 307.5 rider is NOT checked
-- here: Activate.timingOk gates Action.legalActions, and a direct
-- activateAbility call goes around it, so the test that cares asks
-- Activate.activatable itself.
activateAssault :: ActivatedAbility.ActivatedAbility Card.Type.Card -> ObjectId -> GameState.GameState -> GameState.GameState
activateAssault ability enchantment gs =
  let activated = snd (Engine.runGamePure S.identityAnswer gs (Activate.activateAbility S.alice enchantment ability))
   in snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)

-- Run whole steps until the turn hands off (Engine.advance on an empty
-- schedule) or the game ends, keeping the phase each step ran in alongside the
-- board the turn left. Bounded so a bug cannot loop forever.
runTurn :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> (GameState.GameState, [Phase])
runTurn answer gs0 =
  let turn = GameState.turnNumber gs0
      go n g =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || GameState.turnNumber g /= turn
          then (g, [])
          else
            let (final, later) = go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
             in (final, GameState.phase g : later)
   in go 32 gs0

-- alice at her declare attackers step, defending player bob, with two Settled
-- creatures of the given printing -- the first untapped and free to attack, the
-- second TAPPED so CR 508.1a keeps it out of combat -- four untapped Mountains
-- (exactly Relentless Assault's {2}{R}{R}) and the spell in hand.
relentlessBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId, ObjectId)
relentlessBoard mountain assault piker =
  let (base, ours, _) = S.combatBoardOf [piker, piker] []
      (attacker, bystander) = case ours of
        [a, b] -> (a, b)
        _ -> error "combatBoardOf should return two creatures"
      withLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) base [1 :: Int .. 4]
      (gs, spell) = S.handOne assault (S.tapObject bystander withLands)
   in ( gs
          { GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            GameState.combat = GameState.combat base,
            GameState.remaining = GameState.remaining base
          },
        spell,
        attacker,
        bystander
      )

-- alice in her precombat main phase with priority, six untapped Mountains
-- (exactly Full Throttle's {4}{R}{R}), one Settled creature of the given
-- printing, and the spell in hand.
throttleBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId)
throttleBoard mountain throttle piker =
  let (attacker, gs1) = S.addCreature piker S.alice (S.landsInPlay mountain 6)
      (gs2, spell) = S.handOne throttle gs1
   in ( gs2
          { GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.remaining = afterPrecombatMain
          },
        spell,
        attacker
      )

-- Cast a spell from alice's hand through the real path -- Cast.castSpell pays
-- its cost off the board -- and let it resolve.
castAndResolve :: ObjectId -> GameState.GameState -> GameState.GameState
castAndResolve = castAndResolveWith S.identityAnswer

castAndResolveWith :: (forall r. Prompt.Prompt r -> r) -> ObjectId -> GameState.GameState -> GameState.GameState
castAndResolveWith answer spell gs =
  let cast = snd (Engine.runGamePure answer gs (Cast.castSpell S.alice spell))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- CR 500.8's added phases, end to end, through the cards in the pool that add
-- any: Aggravated Assault ({3}{R}{R}: Untap all creatures you control. After
-- this main phase, there is an additional combat phase followed by an additional
-- main phase. Activate only as a sorcery.) and Relentless Assault ({2}{R}{R}:
-- Untap all creatures that attacked this turn. After this main phase, there is
-- an additional combat phase followed by an additional main phase.)
extraPhaseTests :: Registry.Type.Registry -> Tasty.TestTree
extraPhaseTests registry =
  Tasty.testGroup
    "ExtraPhase"
    [ HU.testCase "CR 500.8 splicePhases from a MAIN phase goes at the head" $
        -- CR 505.2: a main phase has no steps, so nothing of it is left in the
        -- schedule and "directly after the specified phase" is the head. This is
        -- Aggravated Assault's case, and the reason its behaviour is unchanged.
        let remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
         in HU.assertEqual
              "directly after this phase"
              (Turn.combatAndMainPhase <> remaining)
              (Turn.splicePhases Phase.PrecombatMain [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain] remaining),
      HU.testCase "CR 500.8 splicePhases from INSIDE a combat phase goes after its end of combat" $
        -- Aurelia, the Warleader's case. Her trigger resolves in the declare
        -- attackers step, where this phase's own later steps are still in
        -- `remaining` -- so the head is INSIDE the phase the added one must
        -- follow, and CR 511.3's boundary is what puts it in the right place.
        let remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
            expected =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat
                ]
                <> Turn.expandExtraPhase ExtraPhase.ExtraCombat
                <> Seq.fromList [Phase.PostcombatMain]
         in HU.assertEqual
              "not inside the current phase"
              expected
              (Turn.splicePhases (Phase.Combat CombatStep.DeclareAttackers) [ExtraPhase.ExtraCombat] remaining),
      HU.testCase "CR 500.8 splicePhases inserts a multi-phase list in written order" $
        -- Full Throttle's "there are two additional combat phases": two whole
        -- combat phases, back to back, and no main phase between them.
        HU.assertEqual
          "two combat phases, back to back"
          (Turn.expandExtraPhase ExtraPhase.ExtraCombat <> Turn.expandExtraPhase ExtraPhase.ExtraCombat)
          (Turn.splicePhases Phase.PrecombatMain [ExtraPhase.ExtraCombat, ExtraPhase.ExtraCombat] Seq.empty),
      HU.testCase "CR 500.8 whole card: Aggravated Assault untaps your creatures and adds a combat and a main phase" $ do
        mountain <- Registry.printing registry "Mountain"
        assault <- Registry.printing registry "Aggravated Assault"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, enchantment, ours, theirs) = assaultBoard mountain assault piker piker
        case assaultAbility assault of
          Nothing -> HU.assertFailure "Aggravated Assault should print one activated ability"
          Just ability -> do
            -- CR 307.5: "Activate only as a sorcery" is a real gate, not decoration
            -- -- offered in alice's main phase with an empty stack, withheld in her
            -- combat phase. Asked of Activate.activatable, because that is what
            -- Action.legalActions consults; activateAbility itself goes around it.
            HU.assertBool "offered in the main phase" (Activate.activatable S.alice enchantment ability gs)
            HU.assertBool
              "withheld in the combat phase"
              (not (Activate.activatable S.alice enchantment ability gs {GameState.phase = Phase.Combat CombatStep.DeclareAttackers}))
            let after = activateAssault ability enchantment gs
            -- CR 701.26b over the swept set: alice's creature, and only it.
            HU.assertEqual "alice's creature untapped" (Just TapState.Untapped) (fmap Object.tapped (Game.lookupObject ours after))
            HU.assertEqual "bob's creature still tapped" (Just TapState.Tapped) (fmap Object.tapped (Game.lookupObject theirs after))
            -- The five Mountains paid the cost, so they are tapped -- and they
            -- are lands, so "all creatures you control" must leave them alone.
            HU.assertEqual "the lands that paid stay tapped" 5 (S.tappedCount S.alice after)
            -- CR 500.8: "directly after the specified phase", not at the end of
            -- the turn. The whole of the ordinary remainder still follows.
            HU.assertEqual
              "the phases went in directly after this main phase"
              (Turn.combatAndMainPhase <> afterPrecombatMain)
              (GameState.remaining after)
            HU.assertEqual "and the main phase it was activated in is still current" Phase.PrecombatMain (GameState.phase after),
      HU.testCase "CR 508.8 + 500.8 skipping the added combat phase leaves the turn's own combat phase whole" $ do
        -- The falsifier for #31. The added combat phase runs FIRST (it goes in
        -- directly after the precombat main phase), nobody attacks in it, so CR
        -- 508.8 skips its declare blockers and combat damage steps -- and the
        -- turn's own combat phase, still ahead in the schedule, must keep both.
        mountain <- Registry.printing registry "Mountain"
        assault <- Registry.printing registry "Aggravated Assault"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, enchantment, _, _) = assaultBoard mountain assault piker piker
        case assaultAbility assault of
          Nothing -> HU.assertFailure "Aggravated Assault should print one activated ability"
          Just ability -> do
            -- Three steps: the main phase ends, then beginning of combat, then
            -- declare attackers -- which is where Combat.skipEmptyCombat fires.
            let resolved = activateAssault ability enchantment gs
                step g = snd (Engine.runGamePure S.identityAnswer g Engine.runStep)
                after = step (step (step resolved))
            HU.assertEqual "the added combat jumped to its end of combat step" (Phase.Combat CombatStep.EndOfCombat) (GameState.phase after)
            HU.assertEqual
              "the turn's own combat phase kept every step"
              (Seq.fromList [Phase.PostcombatMain] <> afterPrecombatMain)
              (GameState.remaining after),
      HU.testCase "CR 500.8 whole card: a vigilant creature attacks in the added combat phase AND the turn's own" $ do
        -- CR 506.1's five steps really run twice: Windseeker Centaur has
        -- vigilance (CR 702.20b: attacking does not tap it), so it is a legal
        -- attacker again in the second declare attackers step, and bob takes 2
        -- in each combat damage step.
        mountain <- Registry.printing registry "Mountain"
        assault <- Registry.printing registry "Aggravated Assault"
        centaur <- Registry.printing registry "Windseeker Centaur"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, enchantment, _, _) = assaultBoard mountain assault centaur piker
        case assaultAbility assault of
          Nothing -> HU.assertFailure "Aggravated Assault should print one activated ability"
          Just ability -> do
            let (played, ran) = runTurn (S.attackTo S.bob) (activateAssault ability enchantment gs)
            HU.assertEqual
              "the turn ran a whole extra combat phase and main phase"
              [ Phase.PrecombatMain,
                Phase.Combat CombatStep.BeginningOfCombat,
                Phase.Combat CombatStep.DeclareAttackers,
                Phase.Combat CombatStep.DeclareBlockers,
                Phase.Combat CombatStep.CombatDamage,
                Phase.Combat CombatStep.EndOfCombat,
                Phase.PostcombatMain,
                Phase.Combat CombatStep.BeginningOfCombat,
                Phase.Combat CombatStep.DeclareAttackers,
                Phase.Combat CombatStep.DeclareBlockers,
                Phase.Combat CombatStep.CombatDamage,
                Phase.Combat CombatStep.EndOfCombat,
                Phase.PostcombatMain,
                Phase.Ending EndingStep.EndStep,
                Phase.Ending EndingStep.Cleanup
              ]
              ran
            HU.assertEqual "bob took 2 in each of the two combats" (Just 16) (S.lifeOf S.bob played),
      HU.testCase "CR 500.8 whole card: Relentless Assault untaps only what ATTACKED" $ do
        -- The assertion that distinguishes Filter.AttackedThisTurn from
        -- "creatures you control": both creatures are alice's and both are
        -- tapped by the time the spell resolves, and only the one that was
        -- DECLARED as an attacker (CR 508.3a) may untap.
        mountain <- Registry.printing registry "Mountain"
        assault <- Registry.printing registry "Relentless Assault"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, spell, attacker, bystander) = relentlessBoard mountain assault piker
            fought = S.runCombat (S.attackTo S.bob) gs
            after = castAndResolve spell fought
        HU.assertEqual "the spell was cast in the postcombat main phase" Phase.PostcombatMain (GameState.phase fought)
        HU.assertEqual "it really attacked" [attacker] (S.attackerDeclarationsOf fought)
        HU.assertEqual "the attacker untapped" (Just TapState.Untapped) (fmap Object.tapped (Game.lookupObject attacker after))
        HU.assertEqual "the non-attacker stayed tapped" (Just TapState.Tapped) (fmap Object.tapped (Game.lookupObject bystander after))
        HU.assertEqual
          "and the phases went in directly after this main phase"
          (Turn.combatAndMainPhase <> Seq.fromList [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup])
          (GameState.remaining after),
      HU.testCase "CR 500.8 Aurelia's added combat phase goes AFTER this one, not inside it" $ do
        -- The falsifier for splicing at the head of GameState.remaining.
        -- Aurelia's trigger resolves in the declare attackers step, where this
        -- combat phase's own declare blockers, combat damage and end of combat
        -- steps are all still in `remaining` -- so the head is INSIDE the phase
        -- the added one has to follow. CR 511.3 is what bounds it.
        aurelia <- Registry.printing registry "Aurelia, the Warleader"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, ours, _) = S.combatBoardOf [aurelia, piker] []
            after = snd (Engine.runGamePure (S.attackTo S.bob) gs Engine.runStep)
        -- Under a head-cons the added phase's beginning of combat step would be
        -- popped here instead.
        HU.assertEqual "this combat phase's own next step ran next" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase after)
        HU.assertEqual
          "the rest of this phase still comes before the added one"
          ( Seq.fromList [Phase.Combat CombatStep.CombatDamage, Phase.Combat CombatStep.EndOfCombat]
              <> Turn.expandExtraPhase ExtraPhase.ExtraCombat
              <> Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
          )
          (GameState.remaining after)
        -- And the trigger's other clause really ran: the Piker tapped to attack
        -- (CR 508.1f) and Aurelia untapped it.
        HU.assertEqual
          "untapped all creatures you control"
          (Just TapState.Untapped)
          (fmap Object.tapped (Game.lookupObject (ours !! 1) after)),
      HU.testCase "CR 500.8 Aurelia's second attack adds no third combat phase" $ do
        -- "For the first time each turn" is load-bearing: without it Aurelia
        -- attacks in the phase she added, adds another, and the turn never ends.
        aurelia <- Registry.printing registry "Aurelia, the Warleader"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _, _) = S.combatBoardOf [aurelia, piker] []
            (played, ran) = runTurn (S.attackTo S.bob) gs
        HU.assertEqual
          "exactly two combat phases, and the second adds nothing"
          [ Phase.Combat CombatStep.DeclareAttackers,
            Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.Combat CombatStep.BeginningOfCombat,
            Phase.Combat CombatStep.DeclareAttackers,
            Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain,
            Phase.Ending EndingStep.EndStep,
            Phase.Ending EndingStep.Cleanup
          ]
          ran
        -- 3 + 2 in each of the two combats.
        HU.assertEqual "bob took both combats" (Just 10) (S.lifeOf S.bob played),
      HU.testCase "CR 500.8 whole card: Full Throttle adds two combat phases and NO main phase" $ do
        -- The one two-element payload in the pool, and the one that adds a
        -- combat phase directly after a combat phase. CR 500.8 fixes neither the
        -- number nor the kind, which is why the opcode carries a list.
        mountain <- Registry.printing registry "Mountain"
        throttle <- Registry.printing registry "Full Throttle"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, spell, _) = throttleBoard mountain throttle piker
            after = castAndResolve spell gs
        HU.assertEqual
          "two whole combat phases, back to back, then the ordinary rest of the turn"
          ( Turn.expandExtraPhase ExtraPhase.ExtraCombat
              <> Turn.expandExtraPhase ExtraPhase.ExtraCombat
              <> afterPrecombatMain
          )
          (GameState.remaining after)
        HU.assertEqual "one delayed ability armed" 1 (Seq.length (GameState.delayedTriggers after)),
      HU.testCase "CR 603.7b whole card: Full Throttle's delayed trigger fires at EVERY combat this turn" $ do
        -- The falsifier for CR 603.7b's one shot. "At the beginning of each
        -- combat this turn" is a STATED duration, so the ability stays armed and
        -- fires at all three of the turn's beginning of combat steps -- the two
        -- it added and the turn's own. Under the old store it would fire once,
        -- the Piker would stay tapped from its first attack (CR 508.1f), and bob
        -- would take 2 instead of 6.
        mountain <- Registry.printing registry "Mountain"
        throttle <- Registry.printing registry "Full Throttle"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, spell, _) = throttleBoard mountain throttle piker
            (played, ran) = runTurn (S.attackTo S.bob) (castAndResolve spell gs)
            combatPhase = Turn.expandExtraPhase ExtraPhase.ExtraCombat
        HU.assertEqual
          "three whole combat phases ran"
          ( [Phase.PrecombatMain]
              <> concat (replicate 3 (Foldable.toList combatPhase))
              <> [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
          )
          ran
        -- The Piker attacked in every one of them, which it could only do if the
        -- delayed ability untapped it before each -- and it found it by
        -- Filter.AttackedThisTurn, since CR 511.3 had cleared the combat record.
        HU.assertEqual "bob took 2 in each of the three combats" (Just 14) (S.lifeOf S.bob played)
        -- CR 514.2 ends the stated duration, so nothing is left armed.
        HU.assertEqual "and the store is empty by the end of the turn" 0 (Seq.length (GameState.delayedTriggers played)),
      HU.testCase "CR 511.3 Relentless Assault still finds an attacker after clearCombat" $ do
        -- The reason the atom reads the turn-scoped event log rather than the
        -- live combat record. By the postcombat main phase the end of combat
        -- step has ended, so CR 511.3 has removed every creature from combat and
        -- Combat.Type.attackers is empty -- an IsAttacking-shaped implementation would
        -- untap nothing here and this test would fail.
        mountain <- Registry.printing registry "Mountain"
        assault <- Registry.printing registry "Relentless Assault"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, spell, attacker, _) = relentlessBoard mountain assault piker
            fought = S.runCombat (S.attackTo S.bob) gs
        HU.assertEqual "combat really was cleared" [] (Map.keys (Combat.Type.attackers (GameState.combat fought)))
        HU.assertEqual
          "and the attacker untapped anyway"
          (Just TapState.Untapped)
          (fmap Object.tapped (Game.lookupObject attacker (castAndResolve spell fought)))
    ]

-- Aim every target slot at one player. Time Warp's spec is Pool.Players, so a
-- recipient tagged for any other pool is not in its legal set at all.
aimPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer pid)) sets
  _ -> S.identityAnswer p

-- alice in her precombat main phase with priority, five untapped Islands per
-- Time Warp (exactly {3}{U}{U} each) and that many Time Warps in hand. Both
-- libraries are stocked, because unlike every other case in this file these run
-- SEVERAL whole turns and each one's draw step takes a card -- an empty library
-- would end the game by CR 704.5b before the turn order could be read off.
warpBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (GameState.GameState, [ObjectId])
warpBoard island warp piker n =
  let addOne (ids, g) _ = let (oid, g1) = S.addHandCard warp S.alice g in (ids <> [oid], g1)
      (held, withHand) = List.foldl' addOne ([], S.landsInPlay island (5 * n)) [1 .. n]
      stock g pid = List.foldl' (\g1 _ -> snd (S.addLibraryCard piker pid g1)) g [1 .. (10 :: Int)]
   in ( (stock (stock withHand S.alice) S.bob)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held
      )

-- The seats the next `n` turns are dealt out to, in order, by running each turn
-- to its handoff. The one observable CR 500.7 is about.
turnTakers :: Int -> GameState.GameState -> [PlayerId.PlayerId]
turnTakers n gs =
  if n <= 0
    then []
    else
      let next = fst (runTurn S.identityAnswer gs)
       in GameState.activePlayer next : turnTakers (n - 1) next

-- CR 500.7's extra turns, end to end, through the one card in the pool that
-- creates any: Time Warp ({3}{U}{U} Sorcery, "Target player takes an extra turn
-- after this one.").
extraTurnTests :: Registry.Type.Registry -> Tasty.TestTree
extraTurnTests registry =
  let boardOf n = do
        island <- Registry.printing registry "Island"
        warp <- Registry.printing registry "Time Warp"
        piker <- Registry.printing registry "Goblin Piker"
        pure (warpBoard island warp piker n)
   in Tasty.testGroup
        "ExtraTurn"
        [ -- The control: without the spell the turn hands off down the seating
          -- order, which is what every case below is measured against.
          HU.testCase "CR 103.1 without an extra turn the seating order alternates" $ do
            (gs, _) <- boardOf 1
            HU.assertEqual "bob, alice, bob follow alice's turn" [S.bob, S.alice, S.bob] (turnTakers 3 gs),
          HU.testCase "CR 500.7 whole card: Time Warp aimed at yourself takes the next turn" $ do
            (gs, held) <- boardOf 1
            case held of
              [spell] -> do
                let resolved = castAndResolveWith (aimPlayer S.alice) spell gs
                HU.assertEqual "one turn was created" [S.alice] (GameState.extraTurns resolved)
                -- CR 500.7: "directly after the specified turn" -- alice takes
                -- it immediately, and it is a TURN, not a continuation of this
                -- one, so the turn number moves.
                let (next, _) = runTurn S.identityAnswer resolved
                HU.assertEqual "alice takes it rather than bob" S.alice (GameState.activePlayer next)
                HU.assertEqual "and it is a turn of its own" 2 (GameState.turnNumber next)
                HU.assertEqual "the pending stack is spent" [] (GameState.extraTurns next)
                -- CR 500.7 ADDS a turn; it does not reorder the rest. Once the
                -- extra turn is taken the seating order picks up where it was.
                HU.assertEqual "then the ordinary order resumes" [S.bob, S.alice] (turnTakers 2 next)
              _ -> HU.assertFailure "warpBoard should deal exactly one Time Warp",
          -- THE ANCHOR CASE. CR 500.7 adds a turn "directly after the specified
          -- turn" and takes none away, so bob's OWN turn still follows alice's.
          -- Fails against a handoff anchored on GameState.activePlayer, which
          -- would walk from bob after the extra turn and hand back to alice --
          -- silently eating the ordinary turn the rules never touched.
          HU.testCase "CR 500.7 an opponent's extra turn does not consume their ordinary one" $ do
            (gs, held) <- boardOf 1
            case held of
              [spell] -> do
                let resolved = castAndResolveWith (aimPlayer S.bob) spell gs
                HU.assertEqual "bob's turn was created" [S.bob] (GameState.extraTurns resolved)
                HU.assertEqual
                  "bob's extra turn, then bob's own, and only then alice's"
                  [S.bob, S.bob, S.alice]
                  (turnTakers 3 resolved)
              _ -> HU.assertFailure "warpBoard should deal exactly one Time Warp",
          -- THE PROVING CASE for the last sentence of CR 500.7: "the most
          -- recently created turn will be taken first." Two Time Warps resolve
          -- in alice's turn, the first naming BOB and the second naming ALICE,
          -- so alice's turn is the more recent one and goes first.
          --
          -- Discriminating in both directions, which is why the two are aimed
          -- this way round rather than the other: a QUEUE would deal bob's turn
          -- out first and give [bob, alice, bob, alice] -- which is also exactly
          -- what an engine with NO extra turns at all produces, since the
          -- seating order alternates. Only the stack reading answers alice
          -- first.
          HU.testCase "CR 500.7 two extra turns are a stack: most recently created first" $ do
            (gs, held) <- boardOf 2
            case held of
              [first_, second] -> do
                let resolved =
                      castAndResolveWith (aimPlayer S.alice) second $
                        castAndResolveWith (aimPlayer S.bob) first_ gs
                HU.assertEqual "pending, most recent at the head" [S.alice, S.bob] (GameState.extraTurns resolved)
                HU.assertEqual
                  "alice's extra turn, then bob's, then bob's ordinary one"
                  [S.alice, S.bob, S.bob, S.alice]
                  (turnTakers 4 resolved)
              _ -> HU.assertFailure "warpBoard should deal exactly two Time Warps",
          -- CR 500.7's APNAP clause, which no card in the pool reaches: Time
          -- Warp names one player, so the order between takers is vacuous for
          -- it. Applied directly, as the emblem and Aura cases in the sibling
          -- specs do, because the rule is real and the opcode's PlayerRef can
          -- name a set.
          --
          -- bob is the active player, so CR 101.4 orders the takers [bob,
          -- alice]; they are added one at a time in that order, which leaves
          -- ALICE's turn most recently created and therefore first. Fails
          -- against a push in seating order, which would answer [bob, alice].
          HU.testCase "CR 500.7 several extra turns from one effect are added in APNAP order" $ do
            island <- Registry.printing registry "Island"
            let gs = (S.landsInPlay island 1) {GameState.activePlayer = S.bob}
                source = ObjectId.MkObjectId 0
                after =
                  S.runPure
                    S.identityAnswer
                    gs
                    (Resolve.applyEffect source S.bob Map.empty Map.empty Map.empty (Effect.TakeExtraTurn PlayerRef.EachPlayer))
            HU.assertEqual "added in APNAP order, so taken in reverse" [S.alice, S.bob] (GameState.extraTurns after)
        ]

dedupe :: (Eq a) => [a] -> [a]
dedupe xs = case xs of
  [] -> []
  h : t -> h : dedupe (filter (/= h) t)

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Turn" [turnTests, turnDataTests, skipTests registry, extraPhaseTests registry, extraTurnTests registry]
