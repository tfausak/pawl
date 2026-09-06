{-# LANGUAGE GADTs #-}

-- Covers: CR 701.66 EARTHBEND -- Pawl.Engine.Earthbend, Effect.Earthbend's arm in
-- Pawl.Engine.Resolve.Effect (and the Binding.earthbentLand stamp it writes),
-- TriggerCondition.BoundDiesOrIsExiled's arms in Pawl.Engine.Event.Match and
-- Pawl.Engine.Event.Binding, and the row rule 701.66a's delayed ability holds on
-- Pawl.Engine.Keyword.mintedDelayedAbilities.
--
-- Earthbending Lesson ({3}{G} Sorcery -- Lesson, whose whole text is "Earthbend
-- 4") is the fixture for the keyword action itself: it states nothing the
-- rulebook does not, so every assertion below is about rule 701.66a. Aang, at the
-- Crossroads // Aang, Destined Savior's back face ("At the beginning of combat on
-- your turn, earthbend 2") is the second producer, and the one that proves the
-- opcode is reachable from a triggered ability carrying its own count.
--
-- THE BOARD SHAPE that makes the cases discriminating: alice controls FIVE
-- Forests, identical in every respect, and the spell names ONE of them. So an
-- assertion about the earthbent land failing while a twin beside it succeeds is
-- rule 701.66a and nothing else -- not the phase, not the controller, and not a
-- board on which every land was animated. Every characteristic case is asked of
-- both, on the one board.
--
-- BOTH FORESTS ARE Sickness.Sick, which is not tidiness: S.landsInPlay settles
-- what it places, and CR 302.6 is unobservable on a settled permanent, so the
-- attack case would prove the fixture rather than rule 701.66a's haste. A Goblin
-- Piker, equally sick, is the paired control there -- same board, same clock, no
-- haste.
--
-- The counts DIFFER between the two producers -- four for the Lesson, two for
-- Aang -- and neither equals a power, a toughness or a land count on its board,
-- so no assertion here can pass on a numeric coincidence.
module Pawl.EarthbendSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as View
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- Aim a target slot at this permanent, PINNED rather than searched: an answerer
-- that took whatever was legal would find another Forest after a mutation and
-- keep the case green.
--
-- FILTERED out of the offered set rather than built from the id: CR 115.1's pool
-- of permanents offers its own Recipient, and a hand-built one of the same
-- permanent need not be what CR 608.2b re-reads at resolution.
aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just victim) . Recipient.objectOf) offered) sets
  _ -> S.castAnswer p

-- Make one permanent Sickness.Sick, the board a land played this turn is on.
sicken :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
sicken oid gs = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}

-- Alice's Forests, in ObjectId order.
forestsOf :: GameState.GameState -> [ObjectId.ObjectId]
forestsOf gs = List.sort (filter (\oid -> Set.member Subtype.Forest (Projection.subtypesOf oid gs)) (Game.zoneMembers Zone.Battlefield S.alice gs))

-- alice's five Forests, `extras` more lands beside them, Earthbending Lesson in
-- hand and `others` in hand after it. Five Forests and not four: the spell costs
-- four mana, so paying for it can tap at most four of them, and the LAST one is
-- the target -- which is what leaves an untapped animated land for the attack
-- case to declare.
--
-- Returns (target, twin, spell, the other cards in hand, state).
lessonBoard :: Printing.Printing -> Printing.Printing -> [(Printing.Printing, Int)] -> [Printing.Printing] -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
lessonBoard forest lesson extras others =
  let base = List.foldl' (\acc (printing, n) -> S.landsFor printing S.alice n acc) (S.landsInPlay forest 5) extras
      (withSpell, spell) = S.handOne lesson base
      (otherIds, gs) = List.foldl' (\(acc, g) printing -> let (oid, g') = S.addHandCard printing S.alice g in (acc <> [oid], g')) ([], withSpell) others
   in case reverse (forestsOf gs) of
        target : twin : _ -> (target, twin, spell, otherIds, sicken twin (sicken target gs))
        _ -> (S.noSource, S.noSource, spell, otherIds, gs)

-- Cast one spell from alice's hand at one victim and resolve it.
castAt :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castAt victim spell gs =
  let cast = S.runPure (aimedAt victim) gs (S.cast S.alice spell)
   in S.runPure (aimedAt victim) cast Stack.resolveTop

-- The Lesson cast at `target` and resolved: everything rule 701.66a's first two
-- sentences do has happened, and its third has been created.
earthbent :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
earthbent forest lesson =
  let (target, twin, spell, _, gs) = lessonBoard forest lesson [] []
   in (target, twin, castAt target spell gs)

-- The paired control: the same board with the spell never cast, so the twin and
-- the target are still indistinguishable.
uncast :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
uncast forest lesson =
  let (target, twin, _, _, gs) = lessonBoard forest lesson [] []
   in (target, twin, gs)

-- Lethal damage on the 0/0 with four +1/+1 counters, then CR 704.5g, then the
-- delayed ability onto the stack and off it.
killed :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
killed oid gs =
  let dead = S.settleSba (S.markDamage oid 4 gs)
   in S.runPure S.identityAnswer dead (Engine.placePendingTriggers *> Stack.resolveTop)

-- The delayed ability placed and resolved without anything having killed
-- anything: what a zone change the condition does not admit leaves behind.
settled :: GameState.GameState -> GameState.GameState
settled gs = S.runPure S.identityAnswer (S.settleSba gs) (Engine.placePendingTriggers *> Stack.resolveTop)

-- Alice's battlefield permanents that were not there before.
arrivals :: GameState.GameState -> GameState.GameState -> [ObjectId.ObjectId]
arrivals before after =
  List.sort
    ( Set.toList
        ( Set.difference
            (Set.fromList (Game.zoneMembers Zone.Battlefield S.alice after))
            (Set.fromList (Game.zoneMembers Zone.Battlefield S.alice before))
        )
    )

-- alice mid-declaration against bob. Stated rather than run, DetainSpec's and
-- GoadSpec's posture: CR 507.1's choice is a turn-based action this fixture does
-- not need.
attacking :: GameState.GameState -> GameState.GameState
attacking gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]},
      GameState.remaining =
        Seq.fromList
          [ Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain,
            Phase.Ending EndingStep.EndStep,
            Phase.Ending EndingStep.Cleanup
          ]
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Earthbend" $ do
  animationSpec s registry
  returnSpec s registry
  producerSpec s registry

-- CR 701.66a's first two sentences.
animationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
animationSpec s registry = Spec.describe s "Animation" $ do
  Spec.it s "CR 701.66a / 205.1b the land becomes a 0/0 land creature with four +1/+1 counters" $ do
    forest <- S.printingOf s registry "Forest"
    lesson <- S.printingOf s registry "Earthbending Lesson"
    let (target, twin, after) = earthbent forest lesson
        (control, _, before) = uncast forest lesson
    -- The gameplay reading first: a 0/0 base with four +1/+1 counters is a 4/4.
    Spec.assertEqWith s "CR 701.66a the earthbent land is a 4/4" (S.powerToughnessOf target after) (Just (4, 4))
    Spec.assertEqWith s "CR 122.6 four +1/+1 counters" (S.counterOf CounterKind.PlusOnePlusOne target after) 4
    -- CR 205.1b: IN ADDITION TO, so the land types survive the animation.
    Spec.assertBool s (Set.member CardType.Creature (Projection.cardTypesOf target after)) "CR 701.66a it is a creature"
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf target after)) "CR 205.1b and still a land"
    Spec.assertBool s (Set.member Subtype.Forest (Projection.subtypesOf target after)) "CR 205.1b and still a Forest"
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste target after) "CR 701.66a with haste"
    -- The twin on the SAME board is untouched, so none of the above is a fact
    -- about Forests.
    Spec.assertBool s (not (Set.member CardType.Creature (Projection.cardTypesOf twin after))) "the Forest beside it is no creature"
    Spec.assertEqWith s "and carries no counters" (S.counterOf CounterKind.PlusOnePlusOne twin after) 0
    -- And the same land on the board where the spell was never cast.
    Spec.assertEqWith s "uncast, the target is no creature at all" (S.powerToughnessOf control before) Nothing
  -- CR 611.2a: rule 701.66a states no duration, so the animation is not "until end
  -- of turn". Nothing but the CR 514.2 sweep can tell the two apart, and the sweep
  -- is what a card printing the shorter duration would be caught by.
  Spec.it s "CR 611.2a the animation does not end at cleanup" $ do
    forest <- S.printingOf s registry "Forest"
    lesson <- S.printingOf s registry "Earthbending Lesson"
    let (target, _, after) = earthbent forest lesson
        swept = Expiry.dropAtCleanup after
    Spec.assertEqWith s "CR 611.2a the earthbent land is still a 4/4 after cleanup" (S.powerToughnessOf target swept) (Just (4, 4))
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste target swept) "and still has haste"
    -- CR 514.2 really did run on this board: the animation surviving it is the
    -- duration and not a sweep that reached nothing.
    Spec.assertEqWith s "CR 514.2 the sweep did run" (length (GameState.continuousEffects swept)) (length (GameState.continuousEffects after))
  -- CR 302.6 read against CR 702.10b: the land was played this turn, so haste is
  -- the only thing that lets it attack. The Piker is the control -- equally sick,
  -- equally alice's, and without haste.
  Spec.it s "CR 702.10b the earthbent land can attack the turn it was animated" $ do
    forest <- S.printingOf s registry "Forest"
    lesson <- S.printingOf s registry "Earthbending Lesson"
    piker <- S.printingOf s registry "Goblin Piker"
    let (target, _, after) = earthbent forest lesson
        (pikerId, withPiker) = S.addPermanent piker S.alice after
        gs = attacking (sicken pikerId withPiker)
    Spec.assertBool
      s
      (Combat.legalAttackDeclarationAs S.alice [(target, AttackTarget.OfPlayer S.bob)] gs)
      "CR 701.66a the earthbent land attacks the turn it was animated"
    -- CR 302.6 is live on this very board, so the assertion above is haste.
    Spec.assertBool
      s
      (not (Combat.legalAttackDeclarationAs S.alice [(pikerId, AttackTarget.OfPlayer S.bob)] gs))
      "CR 302.6 a sick creature without haste on the same board cannot"

-- CR 701.66a's third sentence, and CR 603.7's delayed ability behind it.
returnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
returnSpec s registry = Spec.describe s "Return" $ do
  Spec.it s "CR 701.66a the land that dies comes back tapped under its controller's control" $ do
    forest <- S.printingOf s registry "Forest"
    lesson <- S.printingOf s registry "Earthbending Lesson"
    let (target, twin, after) = earthbent forest lesson
        returned = killed target after
    case arrivals after returned of
      [back] -> do
        Spec.assertEqWith s "CR 701.66a it returns tapped" (fmap Object.tapped (Game.lookupObject back returned)) (Just TapState.Tapped)
        Spec.assertEqWith s "CR 701.66a under alice's control" (View.controllerOf back returned) (Just S.alice)
        Spec.assertBool s (Set.member Subtype.Forest (Projection.subtypesOf back returned)) "and it is the Forest that died"
        -- CR 400.7: a new object, so rule 701.66a's animation is gone with the old
        -- one rather than following the card back.
        Spec.assertBool s (not (Set.member CardType.Creature (Projection.cardTypesOf back returned))) "CR 400.7 and it is a land again, not a creature"
      other -> Spec.assertEqWith s "CR 701.66a exactly one permanent returned to the battlefield" (length other) 1
    -- The twin dying on the same board returns nothing, so the arrival above is
    -- the earthbend rather than a rule about Forests.
    Spec.assertEqWith s "CR 701.66a the land nobody earthbent stays dead" (arrivals after (killed twin after)) []
  Spec.it s "CR 701.66a the land that is exiled comes back too" $ do
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    lesson <- S.printingOf s registry "Earthbending Lesson"
    edict <- S.printingOf s registry "Angelic Edict"
    let (target, _, spell, others, gs) = lessonBoard forest lesson [(plains, 5)] [edict]
        after = castAt target spell gs
        exiled = case others of
          edictId : _ -> settled (castAt target edictId after)
          [] -> after
    case arrivals after exiled of
      [back] -> do
        Spec.assertEqWith s "CR 701.66a the exiled land returns tapped" (fmap Object.tapped (Game.lookupObject back exiled)) (Just TapState.Tapped)
        Spec.assertEqWith s "under alice's control" (View.controllerOf back exiled) (Just S.alice)
      other -> Spec.assertEqWith s "CR 701.66a exactly one permanent returned to the battlefield" (length other) 1
  -- CR 110.2a's "unless the effect states otherwise", which rule 701.66a does:
  -- "under YOUR control". alice earthbends a Forest bob OWNS and she controls, so
  -- the owner reading and the earthbender reading name different seats -- the one
  -- board on which that rider is observable at all. Without it this case would
  -- pass under either reading, alice owning everything else she controls.
  Spec.it s "CR 110.2a the land returns under the earthbender's control, not its owner's" $ do
    forest <- S.printingOf s registry "Forest"
    lesson <- S.printingOf s registry "Earthbending Lesson"
    let (_, _, spell, _, base) = lessonBoard forest lesson [] []
        lent = S.landsFor forest S.bob 1 base
        borrowed = case List.sort (Game.zoneMembers Zone.Battlefield S.bob lent) of
          oid : _ -> oid
          [] -> S.noSource
        gs = S.giveControl borrowed S.alice lent
        after = castAt borrowed spell gs
        returned = killed borrowed after
        back = List.sort (Game.zoneMembers Zone.Battlefield S.bob returned)
    -- The precondition the rider turns on, asserted rather than assumed.
    Spec.assertEqWith s "alice controls the Forest bob owns" (View.controllerOf borrowed after) (Just S.alice)
    Spec.assertEqWith s "CR 400.7 the card came back as one new permanent bob owns" (length back) 1
    case back of
      [oid] -> do
        Spec.assertEqWith s "CR 701.66a it is alice who controls it, not bob who owns it" (View.controllerOf oid returned) (Just S.alice)
        Spec.assertEqWith s "CR 701.66a and it is tapped" (fmap Object.tapped (Game.lookupObject oid returned)) (Just TapState.Tapped)
      _ -> pure ()
  -- CR 701.66a names two destinations and no third. A bounce is the reading that
  -- would be wrong if the condition were "leaves the battlefield".
  Spec.it s "CR 701.66a a land returned to hand is not returned to the battlefield" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    lesson <- S.printingOf s registry "Earthbending Lesson"
    unsummon <- S.printingOf s registry "Unsummon"
    let (target, _, spell, others, gs) = lessonBoard forest lesson [(island, 1)] [unsummon]
        after = castAt target spell gs
        bounced = case others of
          unsummonId : _ -> settled (castAt target unsummonId after)
          [] -> after
    Spec.assertEqWith s "CR 701.66a a bounce is neither of rule 701.66a's two destinations" (arrivals after bounced) []
    Spec.assertEqWith s "and the land really did leave" (length (forestsOf bounced)) 4

-- Aang's back face, showing rather than transformed into: CR 701.27a's transform
-- is Pawl.TransformSpec's subject, and nothing here is about it.
aangBackName :: CardName.CardName
aangBackName = CardName.MkCardName (Text.pack "Aang, Destined Savior")

-- The beginning of combat on alice's turn arrives, the trigger goes on the stack
-- aimed at `target`, and it resolves. Pawl.TransformSpec's atNextUpkeep, one step
-- over.
atBeginningOfCombat :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
atBeginningOfCombat target gs =
  let step = Phase.Combat CombatStep.BeginningOfCombat
      began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan step S.alice)) (gs {GameState.phase = step, GameState.activePlayer = S.alice})
   in S.runPure (aimedAt target) began (Engine.placePendingTriggers *> Stack.resolveTop)

-- The two printings that write the opcode, and the one thing that separates them.
producerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
producerSpec s registry = Spec.describe s "Producers" $ do
  -- Aang's back face carries earthbend 2 rather than the Lesson's 4: the count
  -- comes off the card, so a resolver that had baked one in would put four
  -- counters here.
  Spec.it s "CR 701.66a Aang's back face earthbends 2 at the beginning of combat" $ do
    forest <- S.printingOf s registry "Forest"
    aang <- S.printingOf s registry "Aang, at the Crossroads"
    let base = S.landsInPlay forest 5
        (aangId, withAang) = S.addPermanent aang S.alice base
        turned = withAang {GameState.objects = Map.adjust (\o -> o {Object.face = Just aangBackName}) aangId (GameState.objects withAang)}
        target = case reverse (forestsOf turned) of
          t : _ -> t
          [] -> S.noSource
        after = atBeginningOfCombat target turned
        twin = case forestsOf turned of
          t : _ -> t
          [] -> S.noSource
    -- The face really is the back one, so what follows is the back face's trigger.
    Spec.assertEqWith s "the permanent is showing its 4/4 back face" (S.powerToughnessOf aangId turned) (Just (4, 4))
    Spec.assertEqWith s "CR 701.66a Aang's trigger left a 2/2, which is Aang's count and not the Lesson's" (S.powerToughnessOf target after) (Just (2, 2))
    Spec.assertEqWith s "CR 122.6 two +1/+1 counters" (S.counterOf CounterKind.PlusOnePlusOne target after) 2
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf target after)) "CR 205.1b and still a land"
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste target after) "CR 701.66a and it has haste"
    -- One land and not every land: rule 701.66a names one target.
    Spec.assertEqWith s "the Forest beside it is untouched" (S.powerToughnessOf twin after) Nothing
