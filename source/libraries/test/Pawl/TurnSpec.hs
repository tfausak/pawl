{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Turn: turn structure, the phase schedule, the CR 508.8 skips, and
-- CR 500.8's added phases -- Aggravated Assault and Relentless Assault for the
-- combat-and-main pair, Aurelia, the Warleader for one added from INSIDE a
-- combat phase, and Full Throttle for two combat phases and none of main.
--
-- Also CR 500.7's extra TURNS, which Engine.handoffTurn deals out -- Time Warp
-- and Savor the Moment, the pool's creators of any -- and with Savor the Moment
-- CR 500.11's skip scoped to ONE of them.
module Pawl.TurnSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import Pawl.Types.Phase (Phase)
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

turnSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
turnSpec s = Spec.describe s "Turn" $ do
  Spec.it s "firstPhase is the untap step" $
    Spec.assertEqWith s "firstPhase" Turn.firstPhase (Phase.Beginning BeginningStep.Untap)
  Spec.it s "a turn has twelve steps" $
    Spec.assertEqWith s "twelve" (length Turn.allPhases) 12
  Spec.it s "firstPhase and laterPhases reconstruct the turn template" $
    Spec.assertEqWith s "reconstruct" Turn.laterPhases (Seq.fromList (drop 1 Turn.allPhases))
  -- CR 502.4 outright, CR 514.3's "normally" for the cleanup step -- whose
  -- CR 514.3a exception is Engine.grantsPriorityNow's question, not this
  -- one's (Pawl.GameSpec's "extra cleanup step").
  Spec.it s "untap grants no priority, and cleanup none by the phase alone" $
    Spec.assertBool
      s
      ( not (Turn.grantsPriority (Phase.Beginning BeginningStep.Untap))
          && not (Turn.grantsPriority (Phase.Ending EndingStep.Cleanup))
      )
      "no priority"
  -- CR 514.3a: "another cleanup step begins" -- at the head, so it runs next,
  -- exactly as CR 510.4's second combat damage step does.
  Spec.it s "CR 514.3a spliceExtraCleanup puts another cleanup step next" $ do
    Spec.assertEqWith
      s
      "the ordinary case: the cleanup step is the last of the turn"
      (Turn.spliceExtraCleanup Seq.empty)
      (Seq.singleton (Phase.Ending EndingStep.Cleanup))
    Spec.assertEqWith
      s
      "and it goes IN FRONT of a CR 500.8 phase added after the ending phase"
      (Turn.spliceExtraCleanup Turn.combatAndMainPhase)
      (Phase.Ending EndingStep.Cleanup Seq.<| Turn.combatAndMainPhase)
  Spec.it s "a turn never revisits a phase" $
    Spec.assertEqWith s "no repeats" (List.nub Turn.allPhases) Turn.allPhases

turnDataSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
turnDataSpec s = Spec.describe s "TurnData" $ do
  Spec.it s "advance pops the schedule head into the current phase" $
    let gs0 = Setup.emptyGame S.bothPlayers
        gs =
          gs0
            { GameState.phase = Phase.PrecombatMain,
              GameState.remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
            }
        after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.advance)
     in do
          Spec.assertEqWith s "phase" (GameState.phase after) (Phase.Combat CombatStep.BeginningOfCombat)
          Spec.assertEqWith s "remaining" (GameState.remaining after) (Seq.fromList [Phase.PostcombatMain])
  Spec.it s "advance on an empty schedule hands off the turn" $
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
          Spec.assertEqWith s "new active player" (GameState.activePlayer after) S.bob
          Spec.assertEqWith s "phase reset" (GameState.phase after) Turn.firstPhase
          Spec.assertEqWith s "schedule refilled" (GameState.remaining after) Turn.laterPhases
          Spec.assertEqWith s "turn incremented" (GameState.turnNumber after) 2
  Spec.it s "a fresh game starts at untap with the rest of the turn scheduled" $
    let gs = Setup.emptyGame S.bothPlayers
     in do
          Spec.assertEqWith s "phase" (GameState.phase gs) Turn.firstPhase
          Spec.assertEqWith s "remaining" (GameState.remaining gs) Turn.laterPhases
  -- CR 500.1: only the FIRST step of a phase that has steps opens that
  -- phase, and both main phases open none -- CR 505.2 makes each of them a
  -- single schedule entry, which PhaseSelector.Step already names.
  --
  -- This is what keeps CR 614.10's "once a step, phase, or turn has started,
  -- it can no longer be skipped" true at phase grain: Engine.runStep asks the
  -- whole-phase question only where this answers Just.
  Spec.it s "CR 500.1 phaseBeginningAt opens a phase only at its first step" $
    Spec.assertEqWith
      s
      "one Just per stepped phase"
      (fmap Turn.phaseBeginningAt Turn.allPhases)
      [ Just PhaseSelector.BeginningPhase,
        Nothing,
        Nothing,
        Nothing,
        Just PhaseSelector.CombatPhase,
        Nothing,
        Nothing,
        Nothing,
        Nothing,
        Nothing,
        Just PhaseSelector.EndingPhase,
        Nothing
      ]
  -- CR 500.5's other grain, and phaseBeginningAt's exact mirror: a phase ENDS
  -- as its LAST step ends. CR 511.3 names combat's ("After the end of combat
  -- step ends, the combat phase is over"), CR 501.1 the beginning phase's three
  -- steps and CR 512.1 the ending phase's two.
  --
  -- The two lists must not agree anywhere except on the Nothings: a phase begins
  -- at one step and ends at another, and an implementation that confused them
  -- would expire an "until end of combat" effect at the START of combat.
  Spec.it s "CR 500.5 phaseEndingAt closes a phase only at its last step" $
    Spec.assertEqWith
      s
      "one Just per stepped phase"
      (fmap Turn.phaseEndingAt Turn.allPhases)
      [ Nothing,
        Nothing,
        Just PhaseSelector.BeginningPhase,
        Nothing,
        Nothing,
        Nothing,
        Nothing,
        Nothing,
        Just PhaseSelector.CombatPhase,
        Nothing,
        Nothing,
        Just PhaseSelector.EndingPhase
      ]
  -- CR 500.1: "during" is CONTAINMENT, and it is what tells a rider naming a
  -- whole phase (Jade Statue's "only during combat") from one naming a step of
  -- it (Desert's "only during the end of combat step"). Both are PhaseSelectors,
  -- so nothing but this function distinguishes them.
  Spec.it s "CR 500.1/506.1 inWindow: a whole-phase window covers every step of it" $ do
    Spec.assertEqWith
      s
      "CombatPhase covers all five of CR 506.1's steps and nothing else"
      (filter (Turn.inWindow PhaseSelector.CombatPhase) Turn.allPhases)
      [ Phase.Combat CombatStep.BeginningOfCombat,
        Phase.Combat CombatStep.DeclareAttackers,
        Phase.Combat CombatStep.DeclareBlockers,
        Phase.Combat CombatStep.CombatDamage,
        Phase.Combat CombatStep.EndOfCombat
      ]
    Spec.assertEqWith
      s
      "CR 501.1 BeginningPhase covers untap, upkeep and draw"
      (filter (Turn.inWindow PhaseSelector.BeginningPhase) Turn.allPhases)
      [ Phase.Beginning BeginningStep.Untap,
        Phase.Beginning BeginningStep.Upkeep,
        Phase.Beginning BeginningStep.DrawStep
      ]
    Spec.assertEqWith
      s
      "CR 512.1 EndingPhase covers the end step and the cleanup step"
      (filter (Turn.inWindow PhaseSelector.EndingPhase) Turn.allPhases)
      [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
  -- The other arm, and the reason Desert goes on working unchanged: a Step
  -- window matches exactly its own schedule entry. CR 505.2 makes a main phase
  -- one such entry, so naming it and naming "the phase" are the same act -- and
  -- there is deliberately no whole-phase spelling of either main phase to
  -- disagree with it.
  Spec.it s "CR 500.1 inWindow: a Step window matches exactly one schedule entry" $ do
    Spec.assertEqWith
      s
      "Desert's end of combat step, and only it"
      (filter (Turn.inWindow (PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat))) Turn.allPhases)
      [Phase.Combat CombatStep.EndOfCombat]
    Spec.assertEqWith
      s
      "CR 505.2 a stepless main phase is its own window"
      (filter (Turn.inWindow (PhaseSelector.Step Phase.PostcombatMain)) Turn.allPhases)
      [Phase.PostcombatMain]
  -- CR 500.11: "to skip a step, phase, or turn is to proceed past it as
  -- though it didn't exist" -- past the PHASE, so its four remaining steps go
  -- and the postcombat main phase (CR 511.3) is what is left.
  --
  -- Positional, like dropSkippedCombatSteps: a SECOND combat phase later in
  -- the turn (CR 500.8) survives untouched, which is what a filter over the
  -- whole schedule would get wrong.
  Spec.it s "CR 500.11 dropRestOfPhase drops this combat phase and leaves a later one" $
    let remaining =
          Seq.fromList
            [ Phase.Combat CombatStep.DeclareAttackers,
              Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain,
              Phase.Combat CombatStep.BeginningOfCombat,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.Ending EndingStep.EndStep
            ]
     in Spec.assertEqWith
          s
          "only the current phase's steps go"
          (Turn.dropRestOfPhase (Phase.Combat CombatStep.BeginningOfCombat) remaining)
          ( Seq.fromList
              [ Phase.PostcombatMain,
                Phase.Combat CombatStep.BeginningOfCombat,
                Phase.Combat CombatStep.EndOfCombat,
                Phase.Ending EndingStep.EndStep
              ]
          )

skipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
skipSpec s registry = Spec.describe s "Skip" $ do
  Spec.it s "CR 511.3 thisPhase inside a combat phase ends at ITS end of combat" $
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
     in Spec.assertEqWith
          s
          "split at the first end of combat, inclusive"
          (Turn.thisPhase (Phase.Combat CombatStep.DeclareAttackers) remaining)
          expected
  Spec.it s "CR 505.2 thisPhase in a main phase has no steps of its own" $
    -- "The main phase has no steps", so there is nothing of it left in the
    -- schedule and everything remaining is already after it.
    let remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
     in Spec.assertEqWith s "empty prefix" (Turn.thisPhase Phase.PrecombatMain remaining) (Seq.empty, remaining)
  Spec.it s "thisPhase yields an empty prefix when this phase's final step is gone" $
    -- Unreachable from either caller, and asserted anyway: dropping nothing
    -- is the safer failure than treating the whole rest of the turn as this
    -- phase.
    let remaining = Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep]
     in Spec.assertEqWith
          s
          "empty prefix"
          (Turn.thisPhase (Phase.Combat CombatStep.EndOfCombat) remaining)
          (Seq.empty, remaining)
  Spec.it s "CR 508.8 dropSkippedCombatSteps removes declare blockers and combat damage" $
    let full =
          Seq.fromList
            [ Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain
            ]
        expected = Seq.fromList [Phase.Combat CombatStep.EndOfCombat, Phase.PostcombatMain]
     in Spec.assertEqWith s "dropped" (Turn.dropSkippedCombatSteps (Phase.Combat CombatStep.DeclareAttackers) full) expected
  Spec.it s "CR 500.8 dropSkippedCombatSteps spares a LATER combat phase's steps" $
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
     in Spec.assertEqWith
          s
          "only this phase's steps dropped"
          (Turn.dropSkippedCombatSteps (Phase.Combat CombatStep.DeclareAttackers) full)
          expected
  Spec.it s "CR 508.8 no attacker declared skips to end of combat" $
    -- Nobody has a creature, so no attackers are declared: the declare
    -- blockers and combat damage steps must not run at all.
    let (gs, _, _) = S.combatBoardOf [] []
        after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.runStep)
     in Spec.assertEqWith s "jumped past the two dead steps" (GameState.phase after) (Phase.Combat CombatStep.EndOfCombat)
  Spec.it s "CR 508.8 an attacker keeps the declare blockers step" $ do
    -- The control: with an attacker, the step after declare attackers is
    -- declare blockers, exactly as before. So the skip is not "always skip".
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] []
        after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.runStep)
    Spec.assertEqWith s "declare blockers still next" (GameState.phase after) (Phase.Combat CombatStep.DeclareBlockers)
  Spec.it s "CR 508.8 an attacker-less combat changes no life total" $
    -- End to end: run the whole combat region. No attackers means no damage,
    -- and the turn still leaves combat cleanly.
    let (gs, _, _) = S.combatBoardOf [] []
        after = S.runCombat S.aggressiveAnswer gs
     in do
          Spec.assertEqWith s "bob untouched" (S.lifeOf S.bob after) (Just 20)
          Spec.assertEqWith s "alice untouched" (S.lifeOf S.alice after) (Just 20)
          Spec.assertBool s (not (S.inCombatPhase (GameState.phase after))) "left combat"
  Spec.it s "CR 508.8 the skip stands even when an instant could have been cast" $ do
    -- bob holds a castable Bolt; nobody attacks. The blockers and damage
    -- steps are still dropped -- the priority windows an instant would use
    -- in them do not exist (CR 500.11: proceed as though they don't).
    --
    -- The WHOLE step, not just its turn-based actions: CR 508.8's condition
    -- is settled as the declare attackers step ends, because its second
    -- clause ("or put onto the battlefield attacking") can only come true in
    -- that step's priority round -- which is also the round this test's Bolt
    -- would be cast in.
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (base, _) = S.boltInHand mountain bolt 1 (Phase.Combat CombatStep.DeclareAttackers)
        armed = base {GameState.activePlayer = S.bob}
        after = snd (Engine.runGamePure S.identityAnswer armed Engine.runStep)
        remaining = foldr (:) [] (GameState.remaining after)
    Spec.assertBool s (notElem (Phase.Combat CombatStep.DeclareBlockers) remaining) "no blockers step"
    Spec.assertBool s (notElem (Phase.Combat CombatStep.CombatDamage) remaining) "no damage step"
  Spec.it s "CR 508.8 an attacker removed from combat still keeps the two steps" $ do
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
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    ray <- S.printingOf s registry "Ray of Command"
    let (base, _, _) = S.combatBoardOf [piker] []
        -- {3}{U}, so four.
        withLands = List.foldl' (\g _ -> snd (S.addCreature island S.bob g)) base [1 :: Int .. 4]
        (_, armed) = S.addHandCard ray S.bob withLands
        after = snd (Engine.runGamePure castingDefender armed Engine.runStep)
    Spec.assertEqWith s "the attacker really left combat" (Map.keys (Combat.Type.attackers (GameState.combat after))) []
    Spec.assertEqWith s "declare blockers still next" (GameState.phase after) (Phase.Combat CombatStep.DeclareBlockers)
  Spec.it s "CR 508.8 a creature put onto the battlefield attacking keeps the two steps" $ do
    -- The rule's SECOND clause on its own, with nothing declared. A DIRECT
    -- call, so the clause is stated with no card in the way: it is what
    -- stops the fix above from being narrowed to the declaration.
    -- Pawl.CombatSpec's MeanderingTowershell group reaches the same clause
    -- through a card, and the two are kept because they fail for different
    -- reasons -- this one for a flag dropped from putOntoBattlefieldAttacking,
    -- that one for anything between the attack and the return going wrong.
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, ours, _) = S.combatBoardOf [piker] []
        joined = S.runPure S.identityAnswer base (Foldable.traverse_ Combat.putOntoBattlefieldAttacking ours)
        after = Combat.skipEmptyCombat joined
        remaining = foldr (:) [] (GameState.remaining after)
    Spec.assertEqWith s "no declaration was made" (S.attackerDeclarationsOf after) []
    Spec.assertBool s (elem (Phase.Combat CombatStep.DeclareBlockers) remaining) "blockers step kept"
    Spec.assertBool s (elem (Phase.Combat CombatStep.CombatDamage) remaining) "damage step kept"

-- aggressiveAnswer, except that it takes any cast on offer instead of passing --
-- the shape the Ray of Command fixture above needs, where alice must attack and
-- bob must then cast. Pawl.Support's castAnswer is the other half of the same
-- pair and declines to attack, so neither one alone will do.
castingDefender :: Prompt.Prompt r -> r
castingDefender p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast {} -> True
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
assaultAbility assault = case Face.activatedAbilities (S.combinedFace assault) of
  [] -> Nothing
  ability : _ -> Just ability

-- Activate it and let it resolve, through the real activation path (the
-- Bonesplitter shape in Pawl.AuraSpec) -- not Resolve.applyEffect, so the
-- {3}{R}{R} is genuinely paid off the board. The CR 307.5 rider is NOT checked
-- here: ActivationRestriction.restrictionsOk gates Action.legalActions, and a direct
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
  let cast = snd (Engine.runGamePure answer gs (S.cast S.alice spell))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- CR 500.8's added phases, end to end, through the cards in the pool that add
-- any: Aggravated Assault ({3}{R}{R}: Untap all creatures you control. After
-- this main phase, there is an additional combat phase followed by an additional
-- main phase. Activate only as a sorcery.) and Relentless Assault ({2}{R}{R}:
-- Untap all creatures that attacked this turn. After this main phase, there is
-- an additional combat phase followed by an additional main phase.)
extraPhaseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
extraPhaseSpec s registry = Spec.describe s "ExtraPhase" $ do
  Spec.it s "CR 500.8 splicePhases from a MAIN phase goes at the head" $
    -- CR 505.2: a main phase has no steps, so nothing of it is left in the
    -- schedule and "directly after the specified phase" is the head. This is
    -- Aggravated Assault's case, and the reason its behaviour is unchanged.
    let remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
     in Spec.assertEqWith
          s
          "directly after this phase"
          (Turn.splicePhases Phase.PrecombatMain [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain] remaining)
          (Turn.combatAndMainPhase <> remaining)
  Spec.it s "CR 500.8 splicePhases from INSIDE a combat phase goes after its end of combat" $
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
     in Spec.assertEqWith
          s
          "not inside the current phase"
          (Turn.splicePhases (Phase.Combat CombatStep.DeclareAttackers) [ExtraPhase.ExtraCombat] remaining)
          expected
  Spec.it s "CR 500.8 splicePhases inserts a multi-phase list in written order" $
    -- Full Throttle's "there are two additional combat phases": two whole
    -- combat phases, back to back, and no main phase between them.
    Spec.assertEqWith
      s
      "two combat phases, back to back"
      (Turn.splicePhases Phase.PrecombatMain [ExtraPhase.ExtraCombat, ExtraPhase.ExtraCombat] Seq.empty)
      (Turn.expandExtraPhase ExtraPhase.ExtraCombat <> Turn.expandExtraPhase ExtraPhase.ExtraCombat)
  Spec.it s "CR 500.8 whole card: Aggravated Assault untaps your creatures and adds a combat and a main phase" $ do
    mountain <- S.printingOf s registry "Mountain"
    assault <- S.printingOf s registry "Aggravated Assault"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, enchantment, ours, theirs) = assaultBoard mountain assault piker piker
    case assaultAbility assault of
      Nothing -> Spec.assertFailure s "Aggravated Assault should print one activated ability"
      Just ability -> do
        -- CR 307.5: "Activate only as a sorcery" is a real gate, not decoration
        -- -- offered in alice's main phase with an empty stack, withheld in her
        -- combat phase. Asked of Activate.activatable, because that is what
        -- Action.legalActions consults; activateAbility itself goes around it.
        Spec.assertBool s (Activate.activatable S.alice enchantment ability gs) "offered in the main phase"
        Spec.assertBool
          s
          (not (Activate.activatable S.alice enchantment ability gs {GameState.phase = Phase.Combat CombatStep.DeclareAttackers}))
          "withheld in the combat phase"
        let after = activateAssault ability enchantment gs
        -- CR 701.26b over the swept set: alice's creature, and only it.
        Spec.assertEqWith s "alice's creature untapped" (fmap Object.tapped (Game.lookupObject ours after)) (Just TapState.Untapped)
        Spec.assertEqWith s "bob's creature still tapped" (fmap Object.tapped (Game.lookupObject theirs after)) (Just TapState.Tapped)
        -- The five Mountains paid the cost, so they are tapped -- and they
        -- are lands, so "all creatures you control" must leave them alone.
        Spec.assertEqWith s "the lands that paid stay tapped" (S.tappedCount S.alice after) 5
        -- CR 500.8: "directly after the specified phase", not at the end of
        -- the turn. The whole of the ordinary remainder still follows.
        Spec.assertEqWith
          s
          "the phases went in directly after this main phase"
          (GameState.remaining after)
          (Turn.combatAndMainPhase <> afterPrecombatMain)
        Spec.assertEqWith s "and the main phase it was activated in is still current" (GameState.phase after) Phase.PrecombatMain
  Spec.it s "CR 508.8 + 500.8 skipping the added combat phase leaves the turn's own combat phase whole" $ do
    -- The falsifier for #31. The added combat phase runs FIRST (it goes in
    -- directly after the precombat main phase), nobody attacks in it, so CR
    -- 508.8 skips its declare blockers and combat damage steps -- and the
    -- turn's own combat phase, still ahead in the schedule, must keep both.
    mountain <- S.printingOf s registry "Mountain"
    assault <- S.printingOf s registry "Aggravated Assault"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, enchantment, _, _) = assaultBoard mountain assault piker piker
    case assaultAbility assault of
      Nothing -> Spec.assertFailure s "Aggravated Assault should print one activated ability"
      Just ability -> do
        -- Three steps: the main phase ends, then beginning of combat, then
        -- declare attackers -- which is where Combat.skipEmptyCombat fires.
        let resolved = activateAssault ability enchantment gs
            step g = snd (Engine.runGamePure S.identityAnswer g Engine.runStep)
            after = step (step (step resolved))
        Spec.assertEqWith s "the added combat jumped to its end of combat step" (GameState.phase after) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertEqWith
          s
          "the turn's own combat phase kept every step"
          (GameState.remaining after)
          (Seq.fromList [Phase.PostcombatMain] <> afterPrecombatMain)
  Spec.it s "CR 500.8 whole card: a vigilant creature attacks in the added combat phase AND the turn's own" $ do
    -- CR 506.1's five steps really run twice: Windseeker Centaur has
    -- vigilance (CR 702.20b: attacking does not tap it), so it is a legal
    -- attacker again in the second declare attackers step, and bob takes 2
    -- in each combat damage step.
    mountain <- S.printingOf s registry "Mountain"
    assault <- S.printingOf s registry "Aggravated Assault"
    centaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, enchantment, _, _) = assaultBoard mountain assault centaur piker
    case assaultAbility assault of
      Nothing -> Spec.assertFailure s "Aggravated Assault should print one activated ability"
      Just ability -> do
        let (played, ran) = runTurn (S.attackTo S.bob) (activateAssault ability enchantment gs)
        Spec.assertEqWith
          s
          "the turn ran a whole extra combat phase and main phase"
          ran
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
        Spec.assertEqWith s "bob took 2 in each of the two combats" (S.lifeOf S.bob played) (Just 16)
  Spec.it s "CR 500.8 whole card: Relentless Assault untaps only what ATTACKED" $ do
    -- The assertion that distinguishes Filter.AttackedThisTurn from
    -- "creatures you control": both creatures are alice's and both are
    -- tapped by the time the spell resolves, and only the one that was
    -- DECLARED as an attacker (CR 508.3a) may untap.
    mountain <- S.printingOf s registry "Mountain"
    assault <- S.printingOf s registry "Relentless Assault"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, spell, attacker, bystander) = relentlessBoard mountain assault piker
        fought = S.runCombat (S.attackTo S.bob) gs
        after = castAndResolve spell fought
    Spec.assertEqWith s "the spell was cast in the postcombat main phase" (GameState.phase fought) Phase.PostcombatMain
    Spec.assertEqWith s "it really attacked" (S.attackerDeclarationsOf fought) [attacker]
    Spec.assertEqWith s "the attacker untapped" (fmap Object.tapped (Game.lookupObject attacker after)) (Just TapState.Untapped)
    Spec.assertEqWith s "the non-attacker stayed tapped" (fmap Object.tapped (Game.lookupObject bystander after)) (Just TapState.Tapped)
    Spec.assertEqWith
      s
      "and the phases went in directly after this main phase"
      (GameState.remaining after)
      (Turn.combatAndMainPhase <> Seq.fromList [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup])
  Spec.it s "CR 500.8 Aurelia's added combat phase goes AFTER this one, not inside it" $ do
    -- The falsifier for splicing at the head of GameState.remaining.
    -- Aurelia's trigger resolves in the declare attackers step, where this
    -- combat phase's own declare blockers, combat damage and end of combat
    -- steps are all still in `remaining` -- so the head is INSIDE the phase
    -- the added one has to follow. CR 511.3 is what bounds it.
    aurelia <- S.printingOf s registry "Aurelia, the Warleader"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, ours, _) = S.combatBoardOf [aurelia, piker] []
        after = snd (Engine.runGamePure (S.attackTo S.bob) gs Engine.runStep)
    -- Under a head-cons the added phase's beginning of combat step would be
    -- popped here instead.
    Spec.assertEqWith s "this combat phase's own next step ran next" (GameState.phase after) (Phase.Combat CombatStep.DeclareBlockers)
    Spec.assertEqWith
      s
      "the rest of this phase still comes before the added one"
      (GameState.remaining after)
      ( Seq.fromList [Phase.Combat CombatStep.CombatDamage, Phase.Combat CombatStep.EndOfCombat]
          <> Turn.expandExtraPhase ExtraPhase.ExtraCombat
          <> Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
      )
    -- And the trigger's other clause really ran: the Piker tapped to attack
    -- (CR 508.1f) and Aurelia untapped it.
    Spec.assertEqWith
      s
      "untapped all creatures you control"
      (fmap Object.tapped (Game.lookupObject (ours !! 1) after))
      (Just TapState.Untapped)
  Spec.it s "CR 500.8 Aurelia's second attack adds no third combat phase" $ do
    -- "For the first time each turn" is load-bearing: without it Aurelia
    -- attacks in the phase she added, adds another, and the turn never ends.
    aurelia <- S.printingOf s registry "Aurelia, the Warleader"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [aurelia, piker] []
        (played, ran) = runTurn (S.attackTo S.bob) gs
    Spec.assertEqWith
      s
      "exactly two combat phases, and the second adds nothing"
      ran
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
    -- 3 + 2 in each of the two combats.
    Spec.assertEqWith s "bob took both combats" (S.lifeOf S.bob played) (Just 10)
  Spec.it s "CR 500.8 whole card: Full Throttle adds two combat phases and NO main phase" $ do
    -- The one two-element payload in the pool, and the one that adds a
    -- combat phase directly after a combat phase. CR 500.8 fixes neither the
    -- number nor the kind, which is why the opcode carries a list.
    mountain <- S.printingOf s registry "Mountain"
    throttle <- S.printingOf s registry "Full Throttle"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, spell, _) = throttleBoard mountain throttle piker
        after = castAndResolve spell gs
    Spec.assertEqWith
      s
      "two whole combat phases, back to back, then the ordinary rest of the turn"
      (GameState.remaining after)
      ( Turn.expandExtraPhase ExtraPhase.ExtraCombat
          <> Turn.expandExtraPhase ExtraPhase.ExtraCombat
          <> afterPrecombatMain
      )
    Spec.assertEqWith s "one delayed ability armed" (Seq.length (GameState.delayedTriggers after)) 1
  Spec.it s "CR 603.7b whole card: Full Throttle's delayed trigger fires at EVERY combat this turn" $ do
    -- The falsifier for CR 603.7b's one shot. "At the beginning of each
    -- combat this turn" is a STATED duration, so the ability stays armed and
    -- fires at all three of the turn's beginning of combat steps -- the two
    -- it added and the turn's own. Under the old store it would fire once,
    -- the Piker would stay tapped from its first attack (CR 508.1f), and bob
    -- would take 2 instead of 6.
    mountain <- S.printingOf s registry "Mountain"
    throttle <- S.printingOf s registry "Full Throttle"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, spell, _) = throttleBoard mountain throttle piker
        (played, ran) = runTurn (S.attackTo S.bob) (castAndResolve spell gs)
        combatPhase = Turn.expandExtraPhase ExtraPhase.ExtraCombat
    Spec.assertEqWith
      s
      "three whole combat phases ran"
      ran
      ( [Phase.PrecombatMain]
          <> concat (replicate 3 (Foldable.toList combatPhase))
          <> [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
      )
    -- The Piker attacked in every one of them, which it could only do if the
    -- delayed ability untapped it before each -- and it found it by
    -- Filter.AttackedThisTurn, since CR 511.3 had cleared the combat record.
    Spec.assertEqWith s "bob took 2 in each of the three combats" (S.lifeOf S.bob played) (Just 14)
    -- CR 514.2 ends the stated duration, so nothing is left armed.
    Spec.assertEqWith s "and the store is empty by the end of the turn" (Seq.length (GameState.delayedTriggers played)) 0
  Spec.it s "CR 511.3 Relentless Assault still finds an attacker after clearCombat" $ do
    -- The reason the atom reads the turn-scoped event log rather than the
    -- live combat record. By the postcombat main phase the end of combat
    -- step has ended, so CR 511.3 has removed every creature from combat and
    -- Combat.Type.attackers is empty -- an IsAttacking-shaped implementation would
    -- untap nothing here and this test would fail.
    mountain <- S.printingOf s registry "Mountain"
    assault <- S.printingOf s registry "Relentless Assault"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, spell, attacker, _) = relentlessBoard mountain assault piker
        fought = S.runCombat (S.attackTo S.bob) gs
    Spec.assertEqWith s "combat really was cleared" (Map.keys (Combat.Type.attackers (GameState.combat fought))) []
    Spec.assertEqWith
      s
      "and the attacker untapped anyway"
      (fmap Object.tapped (Game.lookupObject attacker (castAndResolve spell fought)))
      (Just TapState.Untapped)

-- Aim every target slot at one player. Time Warp's slot is Pool.Players, so a
-- recipient tagged for any other pool is not in its legal set at all.
aimPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer pid))) sets
  _ -> S.identityAnswer p

-- alice in her precombat main phase with priority, five untapped Islands per
-- Time Warp (exactly {3}{U}{U} each) and that many Time Warps in hand. Both
-- libraries are stocked, because these run SEVERAL whole turns and each one's
-- draw step takes a card -- an empty library would end the game by CR 704.5b
-- before the turn order could be read off. `savorBoard` below stocks them for the
-- same reason; nothing else in this file runs past one turn.
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

-- The seats the pending extra turns will be dealt out to, most recently created
-- first -- GameState.extraTurns with each entry's own skips (which only Savor the
-- Moment writes) set aside.
takersOf :: GameState.GameState -> [PlayerId.PlayerId]
takersOf = fmap ExtraTurn.taker . GameState.extraTurns

-- The seats the next `n` turns are dealt out to, in order, by running each turn
-- to its handoff. The one observable CR 500.7 is about.
turnTakers :: Int -> GameState.GameState -> [PlayerId.PlayerId]
turnTakers n gs =
  if n <= 0
    then []
    else
      let next = fst (runTurn S.identityAnswer gs)
       in GameState.activePlayer next : turnTakers (n - 1) next

-- CR 500.7's extra turns, end to end, through the card that creates one for
-- ANOTHER player: Time Warp ({3}{U}{U} Sorcery, "Target player takes an extra
-- turn after this one."). Savor the Moment creates one too, and what it adds --
-- a skip scoped to that turn -- is turnScopedSkipSpec below.
extraTurnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
extraTurnSpec s registry = Spec.describe s "ExtraTurn" $ do
  let boardOf n = do
        island <- S.printingOf s registry "Island"
        warp <- S.printingOf s registry "Time Warp"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (warpBoard island warp piker n)
  -- The control: without the spell the turn hands off down the seating
  -- order, which is what every case below is measured against.
  Spec.it s "CR 103.1 without an extra turn the seating order alternates" $ do
    (gs, _) <- boardOf 1
    Spec.assertEqWith s "bob, alice, bob follow alice's turn" (turnTakers 3 gs) [S.bob, S.alice, S.bob]
  Spec.it s "CR 500.7 whole card: Time Warp aimed at yourself takes the next turn" $ do
    (gs, held) <- boardOf 1
    case held of
      [spell] -> do
        let resolved = castAndResolveWith (aimPlayer S.alice) spell gs
        Spec.assertEqWith s "one turn was created" (takersOf resolved) [S.alice]
        -- CR 500.7: "directly after the specified turn" -- alice takes
        -- it immediately, and it is a TURN, not a continuation of this
        -- one, so the turn number moves.
        let (next, _) = runTurn S.identityAnswer resolved
        Spec.assertEqWith s "alice takes it rather than bob" (GameState.activePlayer next) S.alice
        Spec.assertEqWith s "and it is a turn of its own" (GameState.turnNumber next) 2
        Spec.assertEqWith s "the pending stack is spent" (takersOf next) []
        -- CR 500.7 ADDS a turn; it does not reorder the rest. Once the
        -- extra turn is taken the seating order picks up where it was.
        Spec.assertEqWith s "then the ordinary order resumes" (turnTakers 2 next) [S.bob, S.alice]
      _ -> Spec.assertFailure s "warpBoard should deal exactly one Time Warp"
  -- THE ANCHOR CASE. CR 500.7 adds a turn "directly after the specified
  -- turn" and takes none away, so bob's OWN turn still follows alice's.
  -- Fails against a handoff anchored on GameState.activePlayer, which
  -- would walk from bob after the extra turn and hand back to alice --
  -- silently eating the ordinary turn the rules never touched.
  Spec.it s "CR 500.7 an opponent's extra turn does not consume their ordinary one" $ do
    (gs, held) <- boardOf 1
    case held of
      [spell] -> do
        let resolved = castAndResolveWith (aimPlayer S.bob) spell gs
        Spec.assertEqWith s "bob's turn was created" (takersOf resolved) [S.bob]
        Spec.assertEqWith
          s
          "bob's extra turn, then bob's own, and only then alice's"
          (turnTakers 3 resolved)
          [S.bob, S.bob, S.alice]
      _ -> Spec.assertFailure s "warpBoard should deal exactly one Time Warp"
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
  Spec.it s "CR 500.7 two extra turns are a stack: most recently created first" $ do
    (gs, held) <- boardOf 2
    case held of
      [first_, second] -> do
        let resolved =
              castAndResolveWith (aimPlayer S.alice) second $
                castAndResolveWith (aimPlayer S.bob) first_ gs
        Spec.assertEqWith s "pending, most recent at the head" (takersOf resolved) [S.alice, S.bob]
        Spec.assertEqWith
          s
          "alice's extra turn, then bob's, then bob's ordinary one"
          (turnTakers 4 resolved)
          [S.alice, S.bob, S.bob, S.alice]
      _ -> Spec.assertFailure s "warpBoard should deal exactly two Time Warps"
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
  Spec.it s "CR 500.7 several extra turns from one effect are added in APNAP order" $ do
    island <- S.printingOf s registry "Island"
    let gs = (S.landsInPlay island 1) {GameState.activePlayer = S.bob}
        source = ObjectId.MkObjectId 0
        after =
          S.runPure
            S.identityAnswer
            gs
            (Resolve.applyEffect source source S.bob Map.empty Map.empty (Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn PlayerRef.EachPlayer Set.empty)))
    Spec.assertEqWith s "added in APNAP order, so taken in reverse" (takersOf after) [S.alice, S.bob]

-- alice in her precombat main phase with priority, eight untapped Islands
-- (Savor the Moment's {1}{U}{U} plus Time Warp's {3}{U}{U}, so both can be cast
-- in the one main phase), one TAPPED creature of hers, and both spells in hand.
-- Libraries stocked because these cases run several whole turns and each one's
-- draw step takes a card.
--
-- The tapped creature is the observable. CR 502.3: "the active player determines
-- which permanents they control will untap. Then they untap them all
-- simultaneously." So a turn whose untap step happened leaves it untapped and a
-- turn whose untap step was skipped leaves it tapped -- and unlike the Islands,
-- nothing in these cases taps or untaps it for any other reason.
savorBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId, ObjectId)
savorBoard island savor warp piker =
  let (bystander, withPiker) = S.addCreature piker S.alice (S.landsInPlay island 8)
      (savorId, withSavor) = S.addHandCard savor S.alice withPiker
      (warpId, withWarp) = S.addHandCard warp S.alice withSavor
      stock g pid = List.foldl' (\g1 _ -> snd (S.addLibraryCard piker pid g1)) g [1 .. (10 :: Int)]
   in ( (stock (stock (S.tapObject bystander withWarp) S.alice) S.bob)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        savorId,
        warpId,
        bystander
      )

-- Run `n` whole turns. `runTurn` stops at the handoff, so the board it hands
-- back is positioned AT the next turn's untap step with that step not yet run --
-- which is why each assertion below is read after the turn it is about has been
-- run by one more call, not after the call that begins it.
runTurns :: Int -> GameState.GameState -> GameState.GameState
runTurns n gs = if n <= 0 then gs else runTurns (n - 1) (fst (runTurn S.identityAnswer gs))

tapStateOf :: ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid = fmap Object.tapped . Game.lookupObject oid

-- CR 500.11's skip scoped to ONE identified turn, through the only card in the
-- pool that writes one: Savor the Moment ({1}{U}{U} Sorcery, "Take an extra turn
-- after this one. Skip the untap step of that turn.").
--
-- The whole point of the group is the second case. "Skip the untap step of that
-- turn" is NOT "skip your next untap step": CR 500.7's last sentence -- "the most
-- recently created turn will be taken first" -- lets another extra turn be
-- created after this one and taken BEFORE it, and the printed card skips the
-- untap step of the turn IT made, not of whichever turn comes next. Two cards
-- already in the pool reach that divergence.
turnScopedSkipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
turnScopedSkipSpec s registry = Spec.describe s "TurnScopedSkip" $ do
  let boardOf = do
        island <- S.printingOf s registry "Island"
        savor <- S.printingOf s registry "Savor the Moment"
        warp <- S.printingOf s registry "Time Warp"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (savorBoard island savor warp piker)
      tapped = Just TapState.Tapped
      untapped = Just TapState.Untapped
  -- The control that keeps the two below from passing vacuously: an extra
  -- turn carrying NO skip runs its untap step like any other turn.
  Spec.it s "CR 502.3 the control: an extra turn with no skip untaps" $ do
    (gs, _, warp, piker) <- boardOf
    let resolved = castAndResolveWith (aimPlayer S.alice) warp gs
        atExtra = runTurns 1 resolved
        afterExtra = runTurns 1 atExtra
    Spec.assertEqWith s "the extra turn is alice's" (GameState.activePlayer atExtra) S.alice
    Spec.assertEqWith s "still tapped going into it" (tapStateOf piker atExtra) tapped
    Spec.assertEqWith s "and Time Warp's extra turn untaps it" (tapStateOf piker afterExtra) untapped
  Spec.it s "CR 500.11 whole card: Savor the Moment's extra turn skips its untap step" $ do
    (gs, savor, _, piker) <- boardOf
    let resolved = castAndResolve savor gs
        atExtra = runTurns 1 resolved
        -- CR 500.11: "to skip a step, phase, or turn is to proceed past
        -- it as though it didn't exist", and CR 614.1b replaces it with
        -- nothing -- so CR 502.3's turn-based action never happens.
        afterExtra = runTurns 1 atExtra
    Spec.assertEqWith
      s
      "one turn was created, alice's, carrying the untap skip"
      (fmap (\e -> (ExtraTurn.taker e, ExtraTurn.skipped e)) (GameState.extraTurns resolved))
      [(S.alice, Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap)))]
    Spec.assertEqWith s "the extra turn is alice's" (GameState.activePlayer atExtra) S.alice
    Spec.assertEqWith s "and it untapped nothing" (tapStateOf piker afterExtra) tapped
    -- The skip named ONE turn (CR 500.11's "skip a step ... of that
    -- turn", as Savor the Moment prints it) and is gone with it -- NOT CR
    -- 614.10a's "next", which is the reading these cases exist to rule
    -- out. bob's turn untaps BOB's permanents (CR 502.3 is about the
    -- ACTIVE player's), so it is alice's own next turn that has to put
    -- this right.
    Spec.assertEqWith s "the turn after it is bob's" (GameState.activePlayer afterExtra) S.bob
    Spec.assertEqWith s "which is not alice's untap step" (tapStateOf piker (runTurns 1 afterExtra)) tapped
    Spec.assertEqWith s "alice's next ordinary turn untaps it" (tapStateOf piker (runTurns 2 afterExtra)) untapped
  -- THE PROVING CASE. Savor the Moment creates extra turn A; Time Warp,
  -- cast after it in the same turn, creates extra turn B. CR 500.7: "the
  -- most recently created turn will be taken first", so B is taken first
  -- and A second -- and the skip belongs to A.
  --
  -- Fails against a "skip your NEXT untap step" reading, which is what
  -- Effect.SkipNextPhase's CR 614.10a floating replacement would give: it
  -- would take B's untap step (the next one there is) and leave A's
  -- alone, inverting both assertions below.
  Spec.it s "CR 500.7 the skip stays on Savor's own extra turn, not on a later-created one" $ do
    (gs, savor, warp, piker) <- boardOf
    let resolved = castAndResolveWith (aimPlayer S.alice) warp (castAndResolve savor gs)
        atWarpTurn = runTurns 1 resolved
        -- Time Warp's turn has now run, and Savor's is the one about to
        -- begin. Tapped again here, by hand, so that Savor's turn has
        -- something to untap -- the previous turn's untap step is what
        -- undid it, and nothing in these cases taps it back (identityAnswer
        -- declares no attackers, so CR 508.1f never fires).
        atSavorTurn = S.tapObject piker (runTurns 1 atWarpTurn)
        afterSavorTurn = runTurns 1 atSavorTurn
    -- Both turns are alice's, so the active player cannot tell them
    -- apart. The pending stack can: Time Warp's entry is at the head
    -- (created last) and carries no skip, and Savor's is behind it,
    -- carrying the one skip either of them has.
    Spec.assertEqWith
      s
      "Time Warp's turn is at the head with no skip, Savor's behind it with the untap skip"
      (fmap (\e -> (ExtraTurn.taker e, ExtraTurn.skipped e)) (GameState.extraTurns resolved))
      [ (S.alice, Set.empty),
        (S.alice, Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap)))
      ]
    Spec.assertEqWith s "so Time Warp's is turn 2, and alice's" (GameState.turnNumber atWarpTurn, GameState.activePlayer atWarpTurn) (2, S.alice)
    Spec.assertEqWith s "and it DID untap" (tapStateOf piker (runTurns 1 atWarpTurn)) untapped
    Spec.assertEqWith s "Savor's is turn 3, and alice's" (GameState.turnNumber atSavorTurn, GameState.activePlayer atSavorTurn) (3, S.alice)
    Spec.assertEqWith s "but Savor's own turn did NOT untap" (tapStateOf piker afterSavorTurn) tapped

-- Casts the first castable spell offered and passes otherwise, deferring every
-- other prompt to S.identityAnswer. This is the CR 724.1f discriminator: under a
-- correct implementation alice never gets a priority window with an empty stack
-- in her precombat main phase, so her sorcery stays in hand; an implementation
-- that rewrites the schedule but grants the window anyway lets her cast it.
castingFirst :: Prompt.Prompt r -> r
castingFirst p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast {} -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass
  _ -> S.identityAnswer p

-- The cards in one player's zone, by name.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
namesIn zone pid gs = fmap (`S.soleFaceName` gs) (Game.zoneMembers zone pid gs)

-- alice in her precombat main phase with priority, nine untapped Islands (Time
-- Stop's {4}{U}{U} and Divination's {2}{U} together), a Sinister Gnarlbark, and
-- both spells in hand; bob with one Mountain and a Burst Lightning ALREADY ON THE
-- STACK aimed at alice.
--
-- Every element earns its place:
--
--   * Sinister Gnarlbark's only ability is "at the beginning of your end step,
--     draw a card and blight 1" -- unconditional, and observable as a library
--     count and a -1/-1 counter rather than as a flag. It is CR 724.1e's
--     negative.
--   * bob's Burst Lightning is a SECOND object on the stack, owned by the other
--     seat, so CR 724.1b's exile cannot be confused with the resolving spell's
--     own CR 608.2n move to a graveyard.
--   * Divination is a SORCERY, so it is uncastable while bob's spell sits on the
--     stack and becomes castable only in a priority window CR 724.1f forbids.
--   * The precombat main phase leaves more than one entry between here and the
--     cleanup step, so an implementation that advanced once would not pass.
--
-- Both libraries are stocked with lands so no draw decks anyone out (CR 704.5b),
-- and alice's hand stays under seven so CR 514.1's discard moves nothing the
-- assertions read.
timeStopBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId, ObjectId)
timeStopBoard island mountain gnarlbark timeStop divination burst =
  let (tree, gs1) = S.addCreature gnarlbark S.alice (S.landsFor mountain S.bob 1 (S.landsInPlay island 9))
      (stop, gs2) = S.addHandCard timeStop S.alice gs1
      (divine, gs3) = S.addHandCard divination S.alice gs2
      (bolt, gs4) = S.addHandCard burst S.bob gs3
      stock g pid = List.foldl' (\g1 _ -> snd (S.addLibraryCard mountain pid g1)) g [1 .. (10 :: Int)]
      staged =
        (stock (stock gs4 S.alice) S.bob)
          { GameState.activePlayer = S.alice,
            GameState.priority = Just S.bob,
            GameState.phase = Phase.PrecombatMain,
            GameState.remaining = afterPrecombatMain
          }
      onStack = snd (Engine.runGamePure (aimPlayer S.alice) staged (S.cast S.bob bolt))
   in (onStack {GameState.priority = Just S.alice, GameState.passes = 0}, tree, stop, divine)

-- CR 724.1, end to end, through the pool's simplest producer: Time Stop
-- ({4}{U}{U} Instant, "End the turn.").
endTurnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
endTurnSpec s registry = Spec.describe s "EndTheTurn" $ do
  let board = do
        island <- S.printingOf s registry "Island"
        mountain <- S.printingOf s registry "Mountain"
        gnarlbark <- S.printingOf s registry "Sinister Gnarlbark"
        timeStop <- S.printingOf s registry "Time Stop"
        divination <- S.printingOf s registry "Divination"
        burst <- S.printingOf s registry "Burst Lightning"
        pure (timeStopBoard island mountain gnarlbark timeStop divination burst, S.printingName timeStop, S.printingName burst)
  -- THE CONTROL, and the same board differing in exactly one thing: whether alice
  -- casts anything. Without it every negative below is satisfied by a board where
  -- the end step trigger never existed.
  Spec.it s "CR 500.1 the control turn runs its steps and the end step trigger fires" $ do
    ((gs, tree, _, _), _, _) <- board
    let (after, phases) = runTurn S.identityAnswer gs
    Spec.assertEqWith s "the whole turn ran" phases [Phase.PrecombatMain, Phase.Combat CombatStep.BeginningOfCombat, Phase.Combat CombatStep.DeclareAttackers, Phase.Combat CombatStep.EndOfCombat, Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
    Spec.assertEqWith s "so Sinister Gnarlbark's end step trigger drew" (length (Game.zoneMembers Zone.Library S.alice after)) 9
    Spec.assertEqWith s "and blighted itself" (S.counterOf CounterKind.MinusOneMinusOne tree after) 1
    Spec.assertEqWith s "and bob's Burst Lightning resolved" (S.lifeOf S.alice after) (Just 18)
  -- CR 724.1d/724.1e: the schedule assertions, in their own case so no zone
  -- assertion can absorb a mutation to the jump.
  Spec.it s "CR 724.1d ending the turn skips straight to the cleanup step" $ do
    ((gs, tree, _, _), _, _) <- board
    let (after, phases) = runTurn castingFirst gs
    Spec.assertEqWith s "the postcombat main phase and the end step never ran" phases [Phase.PrecombatMain, Phase.Ending EndingStep.Cleanup]
    -- CR 724.1e: no end step began, so Sinister Gnarlbark's ability never
    -- triggered -- read as a library count and a counter, against the control.
    Spec.assertEqWith s "so nothing drew" (length (Game.zoneMembers Zone.Library S.alice after)) 10
    Spec.assertEqWith s "and nothing blighted" (S.counterOf CounterKind.MinusOneMinusOne tree after) 0
  -- CR 724.1b against CR 608.2n: both spells are EXILED, and neither reaches a
  -- graveyard. Asserting Time Stop's own destination is also what makes a card
  -- that failed to parse unable to pass this group.
  Spec.it s "CR 724.1b it exiles the whole stack, the resolving spell included" $ do
    ((gs, _, _, _), stopName, burstName) <- board
    let after = fst (runTurn castingFirst gs)
    Spec.assertEqWith s "alice's exile holds Time Stop" (namesIn Zone.Exile S.alice after) [stopName]
    Spec.assertEqWith s "bob's exile holds Burst Lightning" (namesIn Zone.Exile S.bob after) [burstName]
    Spec.assertEqWith s "and neither graveyard has either" (namesIn Zone.Graveyard S.alice after, namesIn Zone.Graveyard S.bob after) ([], [])
    Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
    Spec.assertEqWith s "and Burst Lightning never resolved" (S.lifeOf S.alice after) (Just 20)
  -- CR 724.1f: no player gets priority during this process, and CR 724.1d has
  -- ended the step, so the window a sorcery would need never opens. The one
  -- assertion that separates a correct implementation from one that rewrites the
  -- schedule and settles anyway.
  Spec.it s "CR 724.1f no player gets priority once the turn has ended" $ do
    ((gs, _, _, divine), _, _) <- board
    let after = fst (runTurn castingFirst gs)
    Spec.assertEqWith s "alice's Divination is still in her hand" (fmap Object.zone (Game.lookupObject divine after)) (Just Zone.Hand)
    Spec.assertEqWith s "her hand holds only it" (S.handSize S.alice after) 1

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Turn" $ do
  turnSpec s
  turnDataSpec s
  skipSpec s registry
  extraPhaseSpec s registry
  extraTurnSpec s registry
  turnScopedSkipSpec s registry
  endTurnSpec s registry
