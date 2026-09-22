{-# LANGUAGE GADTs #-}

-- Covers: CR 801's limited range of influence option -- Pawl.Types.RangeOfInfluence,
-- the Pawl.Types.GameSettings field that carries it, Pawl.Engine.Game.inRangeOf,
-- and its readers: Pawl.Engine.Combat's attackableOpponents (CR 801.3),
-- Pawl.Engine.Target's legalRecipientsGiven (CR 801.4),
-- Pawl.Engine.Activate's activatableGiven (CR 801.6) and Pawl.Engine.Sba's
-- fallsOff and becomesUnattached (CR 801.8 / CR 801.9).
--
-- FOUR SEATS, at range 1 unless a case says otherwise, turn order [alice, bob,
-- carol, dave]: bob and dave sit next to alice and carol sits two seats away.
-- Three seats would cut nothing, every seat being adjacent there. Each negative
-- is paired with the same board at an unlimited range.
module Pawl.RangeOfInfluenceSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Range of influence" $ do
  -- CR 801.3: alice's Goblin Piker may attack only the opponents within one
  -- seat. ONE board declared at two ranges, and at range 1 a declaration against
  -- dave -- the adjacent seat reached round the end of the turn order -- is legal
  -- where the one against carol is not.
  Spec.it s "CR 801.3 a creature cannot attack an opponent outside its controller's range" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (mine, staged) = S.addPermanent piker S.alice S.fourPlayerGame
        board =
          staged
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
              GameState.remaining =
                Seq.fromList
                  [ Phase.Combat CombatStep.DeclareAttackers,
                    Phase.Combat CombatStep.DeclareBlockers,
                    Phase.Combat CombatStep.CombatDamage,
                    Phase.Combat CombatStep.EndOfCombat,
                    Phase.PostcombatMain,
                    Phase.Ending EndingStep.EndStep,
                    Phase.Ending EndingStep.Cleanup
                  ]
            }
        settle gs = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
        declares at gs = Combat.legalAttackDeclarationAs S.alice [(mine, AttackTarget.OfPlayer at)] (settle gs)
    Spec.assertBool s (not (declares S.carol (S.withRange 1 board))) "CR 801.3 at range 1 alice's creature may not attack carol, two seats away"
    Spec.assertBool s (declares S.carol board) "at an unlimited range it may"
    Spec.assertBool s (declares S.dave (S.withRange 1 board)) "CR 801.2 and at range 1 it may attack dave, one seat away the other way round"

  -- CR 801.4 through a target slot's offer. Ravenous Rats, {1}{B} Rat: "When this
  -- creature enters, target opponent discards a card." The offer is the engine's
  -- own output, read as Pawl.TeamSpec's teammate case reads it.
  Spec.it s "CR 801.4 a target opponent slot does not offer an opponent outside the controller's range" $ do
    rats <- S.printingOf s registry "Ravenous Rats"
    swamp <- S.printingOf s registry "Swamp"
    let lands = S.landsFor swamp S.alice 2 S.fourPlayerGame
        (held, staged) = S.addHandCard rats S.alice lands
        board =
          staged
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        recording :: Prompt.Prompt r -> State.State [[Recipient.Recipient]] r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.modify' (<> fmap (Set.toAscList . snd) (Map.elems sets))
            pure (S.preferring (const True) sets)
          _ -> pure (S.identityAnswer p)
        offered gs = State.execState (Engine.runGame recording (S.runPure S.identityAnswer gs (S.cast S.alice held)) Engine.priorityLoop) []
    Spec.assertEqWith
      s
      "CR 801.4 at range 1 only bob and dave are offered"
      (offered (S.withRange 1 board))
      [[Recipient.ToPlayer S.bob, Recipient.ToPlayer S.dave]]
    Spec.assertEqWith
      s
      "and at an unlimited range carol is too"
      (offered board)
      [[Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol, Recipient.ToPlayer S.dave]]

  -- CR 801.2d for objects under CR 801.4: Lightning Bolt's "any target" over a
  -- Goblin Piker and Invasion of Dominaria, both controlled by carol. The Piker is
  -- out of alice's range; the battle is back in it once bob, one seat away,
  -- protects it -- CR 801.2d's second sentence.
  Spec.it s "CR 801.2d an object is in range by its controller, and a battle by its protector" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    piker <- S.printingOf s registry "Goblin Piker"
    invasion <- S.printingOf s registry "Invasion of Dominaria"
    let (theirs, g0) = S.addPermanent piker S.carol S.fourPlayerGame
        (battle, g1) = S.addPermanent invasion S.carol g0
        protectedBy who gs = gs {GameState.objects = Map.adjust (\o -> o {Object.protector = who}) battle (GameState.objects gs)}
    case S.spellTargetSlot bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot
        Spec.assertBool s (not (Set.member (Recipient.ToCreature theirs) (legal (S.withRange 1 g1)))) "CR 801.4 at range 1 alice may not bolt carol's creature"
        Spec.assertBool s (Set.member (Recipient.ToCreature theirs) (legal g1)) "at an unlimited range she may"
        Spec.assertBool s (Set.member (Recipient.ToBattle battle) (legal (S.withRange 1 (protectedBy (Just S.bob) g1)))) "CR 801.2d at range 1 she may bolt carol's battle while bob protects it"
        Spec.assertBool s (not (Set.member (Recipient.ToBattle battle) (legal (S.withRange 1 (protectedBy Nothing g1))))) "and not while nobody in range does"

  -- CR 801.6: Glittering Lion ({2}{W} Creature -- Cat 2/2), "{3}: Until end of
  -- turn, this creature loses 'Prevent all damage that would be dealt to this
  -- creature.' Any player may activate this ability." carol controls it; alice
  -- and bob each have three Plains to pay with.
  Spec.it s "CR 801.6 a player cannot activate an ability of an object outside their range" $ do
    plains <- S.printingOf s registry "Plains"
    lionPrinting <- S.printingOf s registry "Glittering Lion"
    let lands = S.landsFor plains S.bob 3 (S.landsFor plains S.alice 3 S.fourPlayerGame)
        (lion, g0) = S.addPermanent lionPrinting S.carol lands
        board = g0 {GameState.phase = Phase.PrecombatMain}
        offeredTo pid gs = any (\a -> case a of A.Activate o _ -> o == lion; _ -> False) (Action.legalActions pid gs {GameState.priority = Just pid})
    Spec.assertBool s (not (offeredTo S.alice (S.withRange 1 board))) "CR 801.6 at range 1 alice is not offered the Lion carol controls"
    Spec.assertBool s (offeredTo S.alice board) "at an unlimited range she is"
    Spec.assertBool s (offeredTo S.bob (S.withRange 1 board)) "and bob, one seat from carol, is at range 1"

  -- CR 801.8 / CR 801.9: alice's Goblin Piker (2/1) wears her Unholy Strength
  -- (+2/+1) and her Bonesplitter (+2/+0), and carol's Control Magic has taken
  -- it. Carol reaches two seats and alice one, so the Piker now sits outside
  -- alice's range while carol's own Aura stays legal. Paired with the same board
  -- at an unlimited range.
  Spec.it s "CR 801.8 / 801.9 an Aura or Equipment on a host outside its controller's range falls off" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    unholy <- S.printingOf s registry "Unholy Strength"
    splitter <- S.printingOf s registry "Bonesplitter"
    controlMagic <- S.printingOf s registry "Control Magic"
    let (creature, g0) = S.addPermanent piker S.alice S.fourPlayerGame
        (aura, g1) = S.addPermanent unholy S.alice g0
        (equipment, g2) = S.addPermanent splitter S.alice g1
        (steal, g3) = S.addPermanent controlMagic S.carol g2
        board = S.attach steal creature (S.attach equipment creature (S.attach aura creature g3))
        ranged gs =
          let ranges = RangeOfInfluence.MkRangeOfInfluence (Map.fromList [(S.alice, 1), (S.bob, 1), (S.carol, 2), (S.dave, 1)])
           in gs {GameState.settings = (GameState.settings gs) {GameSettings.rangeOfInfluence = ranges}}
        limited = S.settleSba (ranged board)
        unlimited = S.settleSba board
    Spec.assertEqWith s "CR 801.8 / 801.9 at alice's range 1 the Piker loses both bonuses" (S.powerToughnessOf creature limited) (Just (2, 1))
    Spec.assertBool s (not (S.onBattlefield aura limited)) "CR 801.8 alice's Unholy Strength leaves the battlefield"
    Spec.assertEqWith s "CR 801.9 alice's Bonesplitter is unattached" (fmap Object.attachedTo (Game.lookupObject equipment limited)) (Just Nothing)
    Spec.assertBool s (S.onBattlefield equipment limited) "CR 801.9 and stays on the battlefield"
    Spec.assertBool s (S.onBattlefield steal limited) "carol's Control Magic, on a creature she controls, stays"
    Spec.assertEqWith s "at an unlimited range the Piker keeps both" (S.powerToughnessOf creature unlimited) (Just (6, 2))
