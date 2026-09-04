{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Cast over restrictions on casting (CR 601.2, CR 601.3): printed
-- restrictions, flash timing, split cards and fuse, Victor Mancha, and the
-- cards from Drought to Shell of the Last Kappa. Split out of Pawl.CastSpec,
-- which keeps the machinery.
module Pawl.CastRestrictionSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.CastSpec (aliceOnTurn, isPlaneswalkerTarget, rallyBoard, tapStateOf)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

printedCastingRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
printedCastingRestrictionSpec s registry = Spec.describe s "PrintedCastingRestriction" $ do
  -- Both clauses satisfied: bob is the defending player (CR 506.2), attackers
  -- have joined (CR 508.8), and the game is in the declare attackers step.
  Spec.it s "CR 601.3 castable once bob has been attacked in the declare attackers step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
        attacked = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertBool s (S.castable S.bob bobsRally attacked) "castable"
    Spec.assertBool s (elem (A.Cast bobsRally (S.printingName rally) Facing.FaceUp) (Action.legalActions S.bob attacked)) "and offered as a legal action"
  -- CR 306.6 / CR 508.1b: the same board, with the attack aimed at bob's
  -- planeswalker instead of at bob. Eightfold Maze's ruling is the reading being
  -- pinned -- "If all the attacking creatures attack your planeswalkers, you
  -- can't cast Eightfold Maze. To cast it, a creature needs to have attacked
  -- _you_" -- so this is the case that says "you've been attacked" is a question
  -- about the ATTACK TARGET and not about whether a declaration happened.
  --
  -- Its own control is the test above: one Piker, one step, one declaration; the
  -- only difference is what it was announced as attacking.
  Spec.it s "CR 601.3 not castable when the only attacker attacked bob's planeswalker instead" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    jace <- S.printingOf s registry "Jace Beleren"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
        (jaceId, withJace) = S.addCreature jace S.bob board
        loyal = S.addCounter CounterKind.Loyalty 3 jaceId withJace
        atPlaneswalker :: Prompt.Prompt r -> r
        atPlaneswalker p = case p of
          Prompt.ChooseAttackTarget _ _ _ options -> case filter isPlaneswalkerTarget (NonEmpty.toList options) of
            target : _ -> target
            [] -> NonEmpty.head options
          _ -> S.aggressiveAnswer p
        attacked = S.runPure atPlaneswalker loyal (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertEqWith
      s
      "the Piker really was declared, attacking the planeswalker"
      (Map.elems (Combat.Type.attackers (GameState.combat attacked)))
      [AttackTarget.OfPlaneswalker jaceId]
    Spec.assertBool s (not (S.castable S.bob bobsRally attacked)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf bobsRally) (Action.legalActions S.bob attacked))) "and not offered"
  -- The "only if you've been attacked this step" clause, isolated: the step is
  -- right and nobody has attacked yet.
  Spec.it s "CR 601.3 not castable in the declare attackers step before attackers are declared" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
    Spec.assertBool s (not (S.castable S.bob bobsRally board)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf bobsRally) (Action.legalActions S.bob board))) "and not offered"
  -- The same clause from the other side, and the reason the check cannot be a
  -- question about the step alone: Eightfold Maze's ruling is "To cast it, a
  -- creature needs to have attacked _you_", and nothing attacked alice.
  Spec.it s "CR 601.3 the ATTACKING player is not offered it in the same step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (_, alicesRally, _, board) = rallyBoard piker plains rally
        attacked = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertBool s (not (S.castable S.alice alicesRally attacked)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf alicesRally) (Action.legalActions S.alice attacked))) "and not offered"
  -- Both of Rally's clauses fail in the declare blockers step, and this case
  -- pins the wider one: CR 511.3 keeps the PHASE-scoped record live until the
  -- end of combat step ends, so bob is still on it, and the window has passed.
  -- (The step-scoped record is already empty by then -- the case below is what
  -- proves that -- so this is a conjunction failing rather than one clause
  -- isolated.)
  --
  -- Carries its own control, in the same step and for the same player: bob's
  -- Bolt is still offered, so what stops the Rally is the clauses and not the
  -- step being closed to bob altogether.
  Spec.it s "CR 601.3 not castable in the declare blockers step, though bob was attacked" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
        (boltId, withBolt) = S.addHandCard bolt S.bob (snd (S.addCreature mountain S.bob board))
        attacked = S.runPure S.aggressiveAnswer withBolt (Combat.declareAttackers S.manaPerformer S.alice)
        later = attacked {GameState.phase = Phase.Combat CombatStep.DeclareBlockers}
    Spec.assertBool s (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.attacked (GameState.combat later))) "still attacked"
    Spec.assertBool s (not (S.castable S.bob bobsRally later)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf bobsRally) (Action.legalActions S.bob later))) "and not offered"
    Spec.assertBool s (elem (A.Cast boltId (S.printingName bolt) Facing.FaceUp) (Action.legalActions S.bob later)) "bob's unrestricted instant still is"
  -- CR 508.6 on CR 500.1's span: "you've been attacked this step" asks about ONE
  -- STEP, and no printed card tells that from "this combat phase" -- Scryfall
  -- `o:"been attacked this step"`, 2026-08-21, returns fifteen cards and every
  -- one of them also prints "only during the declare attackers step", where the
  -- two spans coincide. So the discriminating card is Synthetic Belated Rally,
  -- Rally the Troops with the DuringPhase clause removed and nothing else
  -- changed; a printing that drops that clause would refute this and replace it.
  --
  -- The boundary is crossed by RUNNING the engine rather than by writing
  -- GameState.phase, as the case above does: what makes the answer flip is a
  -- reset at the end of every step (Pawl.Engine.Combat.clearAttackedThisStep),
  -- which a hand-set phase never reaches.
  Spec.it s "CR 508.6 / 500.1 the record is empty in the declare blockers step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    belated <- S.printingOf s registry "Synthetic Belated Rally"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, _, _, board) = rallyBoard piker plains rally
        (bobsBelated, withBelated) = S.addHandCard belated S.bob board
        (boltId, withBolt) = S.addHandCard bolt S.bob (snd (S.addCreature mountain S.bob withBelated))
        attacked = S.runPure S.aggressiveAnswer withBolt (Combat.declareAttackers S.manaPerformer S.alice)
        later = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer withBolt
    Spec.assertBool s (S.castable S.bob bobsBelated attacked) "castable in the step the attack was declared"
    Spec.assertEqWith s "the engine reached the next step" (GameState.phase later) (Phase.Combat CombatStep.DeclareBlockers)
    Spec.assertBool s (not (S.castable S.bob bobsBelated later)) "not castable once that step has ended"
    Spec.assertBool s (not (any (S.isCastOf bobsBelated) (Action.legalActions S.bob later))) "and not offered"
    -- What separates "the step ended" from "combat ended": the phase-scoped
    -- record still names bob, CR 511.3 emptying it only as the combat phase
    -- ends, so the answer changed because of the step and nothing else.
    Spec.assertBool s (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.declaredAttacked (GameState.combat later))) "bob is still on the phase-scoped record"
    Spec.assertBool s (Set.null (Combat.Type.declaredAttackedThisStep (GameState.combat later))) "and off the step-scoped one"
    -- CR 117.1a, as for Rally above: the step is not closed to bob.
    Spec.assertBool s (elem (A.Cast boltId (S.printingName bolt) Facing.FaceUp) (Action.legalActions S.bob later)) "bob's unrestricted instant is castable there"
  -- CR 508.4's last-but-one sentence: "Such creatures are 'attacking' but, for
  -- the purposes of trigger events and effects, they never 'attacked.'" The
  -- words that reach a printed casting restriction are "AND EFFECTS" -- a
  -- restriction is an effect, not a trigger. CR 508.3b works the same principle
  -- out for the trigger case ("Whenever [a player, planeswalker, or battle] is
  -- attacked" "won't trigger if a creature is put onto the battlefield attacking
  -- that player or permanent"), so it corroborates rather than governs.
  --
  -- So Rally's "only if you've been attacked this step" must NOT be satisfied by
  -- a creature that arrived attacking. A DIRECT call, the shape TurnSpec's CR
  -- 508.8 case takes for the same clause, so the rule is stated with no card in
  -- the way; the pool reaches it through Meandering Towershell, whose delayed
  -- ability returns it attacking on a turn its controller declares nothing.
  --
  -- The two records this separates are both live: CR 508.8's skip counts a
  -- creature put onto the battlefield attacking ("or put onto the battlefield
  -- attacking"), and this rule does not. Asserting both here is what keeps a fix
  -- from collapsing them again.
  Spec.it s "CR 508.4 a creature put onto the battlefield attacking never attacked, so Rally stays uncastable" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
        mine = Projection.controls S.alice board
        joined = S.runPure S.identityAnswer board (Foldable.traverse_ Combat.putOntoBattlefieldAttacking mine)
        combat = GameState.combat joined
    Spec.assertEqWith s "nothing was DECLARED" (S.attackerDeclarationsOf joined) []
    -- CR 508.8's record does count it -- that is the rule's second clause, and
    -- the skip depends on it.
    Spec.assertBool s (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.attacked combat)) "CR 508.8 counts it: something is attacking bob"
    -- CR 508.3b/508.4's record does not.
    Spec.assertBool s (not (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.declaredAttacked combat))) "but bob was never DECLARED-attacked"
    Spec.assertBool s (not (S.castable S.bob bobsRally joined)) "so Rally is not castable"
    Spec.assertBool s (not (any (S.isCastOf bobsRally) (Action.legalActions S.bob joined))) "and not offered"
    -- The control twin: the SAME board with a real declaration makes it
    -- castable, so what the assertions above measure is the declaration and not
    -- something else about the fixture.
    let declared = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertBool s (S.castable S.bob bobsRally declared) "declared instead, it IS castable"

  -- CR 117.1a is not what is stopping it: an unrestricted instant with the
  -- same cost, in the same hand, in the same step, is castable. Without this
  -- the negatives above would also pass on an engine that refused every cast
  -- in the declare attackers step.
  Spec.it s "CR 117.1a an unrestricted instant is castable in the same step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, _, _, board) = rallyBoard piker plains rally
        (boltId, withBolt) = S.addHandCard bolt S.alice (snd (S.addCreature mountain S.alice board))
    Spec.assertBool s (S.castable S.alice boltId withBolt) "castable"
    Spec.assertBool s (elem (A.Cast boltId (S.printingName bolt) Facing.FaceUp) (Action.legalActions S.alice withBolt)) "and offered as a legal action"
  -- Gameplay level, through the stack: the permitted cast resolves and its
  -- effect lands, so the gate is a gate and not a silent no-op.
  Spec.it s "CR 601.3 the permitted cast resolves and untaps bob's creatures" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (bobsRally, _, bobsPiker, board) = rallyBoard piker plains rally
        attacked = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
        cast = S.runPure S.identityAnswer attacked (S.cast S.bob bobsRally)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "tapped before" (tapStateOf bobsPiker attacked) (Just TapState.Tapped)
    Spec.assertEqWith s "untapped after" (tapStateOf bobsPiker resolved) (Just TapState.Untapped)
    -- "creatures YOU control" is the CASTER's, not everyone's. CR 508.1f taps
    -- alice's attacker as it is declared, and alice's only other permanent is
    -- an untapped Plains, so her tapped count is exactly her attacker -- before
    -- the spell and after it.
    Spec.assertEqWith s "alice's attacker was tapped to attack" (S.tappedCount S.alice attacked) 1
    Spec.assertEqWith s "and Rally did not untap it" (S.tappedCount S.alice resolved) 1

  necrologiaSpec s registry

-- Necrologia {3}{B}{B} Instant: "Cast this spell only during your end step. As
-- an additional cost to cast this spell, pay X life. Draw X cards."
--
-- The card for the TURN axis of CastingRestriction.DuringPhase (CR 109.5's
-- "your"), where Rally the Troops above is the card for a window every player
-- shares.
--
-- alice holds Necrologia and a Lightning Bolt, with five Swamps and a Mountain
-- untapped and three cards in her library. The Bolt is the CONTROL on every
-- board below: an unrestricted instant in the same hand at the same moment, so a
-- board that refuses Necrologia for want of priority or mana refuses the Bolt
-- too, and the negative cases would not pass for that reason unnoticed.
necrologiaBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Phase.Phase -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
necrologiaBoard swamp mountain necrologia bolt ph =
  let (base, necrologiaId) = S.boltInHand swamp necrologia 5 ph
      (boltId, withBolt) = S.addHandCard bolt S.alice (snd (S.addCreature mountain S.alice base))
      stocked = List.foldl' (\gs _ -> snd (S.addLibraryCard swamp S.alice gs)) withBolt [1 :: Int, 2, 3]
   in (necrologiaId, boltId, stocked)

-- Announces this X for Necrologia; every other prompt takes the identity
-- fallback. CostSpec's answerHatredXOf, for the other card whose only X is a
-- CostComponent.PayLifeX.
answerNecrologiaXOf :: Natural -> Prompt.Prompt r -> r
answerNecrologiaXOf n p = case p of
  Prompt.ChooseX {} -> n
  _ -> S.identityAnswer p

necrologiaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
necrologiaSpec s registry = Spec.describe s "Necrologia" $ do
  -- CR 512.1 / CR 513.1: the end step is a step of the ending phase, and alice
  -- is the active player, so both conjuncts hold.
  Spec.it s "CR 601.3 castable in its controller's own end step" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, boltId, board) = necrologiaBoard swamp mountain necrologia bolt (Phase.Ending EndingStep.EndStep)
    Spec.assertBool s (S.castable S.alice oid board) "castable"
    Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.alice board)) "and offered as a legal action"
    Spec.assertBool s (S.castable S.alice boltId board) "the control instant is castable too"
  -- CR 109.5: the TURN half, isolated. The same board with ONE field changed --
  -- bob is the active player. alice still holds priority, still has the same
  -- five Swamps, and the game is still in an end step.
  Spec.it s "CR 109.5 the same card is NOT castable in an opponent's end step" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, boltId, board) = necrologiaBoard swamp mountain necrologia bolt (Phase.Ending EndingStep.EndStep)
        bobsTurn = board {GameState.activePlayer = S.bob}
    Spec.assertBool s (not (S.castable S.alice oid bobsTurn)) "TurnScope.ControllersTurn refuses it"
    Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice bobsTurn))) "and it is not offered"
    Spec.assertBool s (S.castable S.alice boltId bobsTurn) "though the control instant still is"
  -- CR 500.1: the WINDOW half, isolated. alice's own turn, wrong phase.
  Spec.it s "CR 601.3 not castable in its controller's precombat main phase" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, boltId, board) = necrologiaBoard swamp mountain necrologia bolt Phase.PrecombatMain
    Spec.assertBool s (not (S.castable S.alice oid board)) "the end-step window is closed"
    Spec.assertBool s (S.castable S.alice boltId board) "though the control instant is castable"
  -- CR 512.1: the cleanup step is the OTHER step of the same phase, so a reader
  -- comparing PhaseSelector.EndingPhase rather than the step would admit it.
  -- This is the case that keeps Turn.inWindow's containment honest for a Step.
  Spec.it s "CR 512.1 not castable in the cleanup step of the same phase" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, boltId, board) = necrologiaBoard swamp mountain necrologia bolt (Phase.Ending EndingStep.Cleanup)
    Spec.assertBool s (not (S.castable S.alice oid board)) "a Step window names one step"
    Spec.assertBool s (S.castable S.alice boltId board) "though the control instant is castable"
  -- Gameplay level, through the stack: the permitted cast resolves, CR 119.4
  -- takes the announced life and CR 121.3 draws that many, so the gate admits a
  -- card that then plays. Falsifiers: an X read as 0 leaves 20 life and one card
  -- drawn short of nothing; an X paid but not read back leaves 18 life and no
  -- draw.
  Spec.it s "CR 601.2b/107.3a the permitted cast pays 2 life and draws 2" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, _, board) = necrologiaBoard swamp mountain necrologia bolt (Phase.Ending EndingStep.EndStep)
        after = S.runPure (answerNecrologiaXOf 2) board (do S.cast S.alice oid; Stack.resolveTop)
    Spec.assertEqWith s "CR 119.4 subtracted the announced 2" (S.lifeOf S.alice after) (Just 18)
    -- One Bolt left in hand plus the two drawn; Necrologia itself has left it.
    Spec.assertEqWith s "two cards drawn" (S.handSize S.alice after) 3
    Spec.assertEqWith s "and the library is two shorter" (length (Game.zoneMembers Zone.Library S.alice after)) 1

-- alice holds one Pouncing Cheetah and one War Mammoth, with four untapped
-- Forests -- enough for either one alone ({2}{G} and {3}{G}), so nothing below
-- turns on affordability. Returns the Cheetah's hand id and the Mammoth's.
--
-- The Mammoth is the CONTROL, and it is in the same hand and the same state on
-- purpose: it is a green creature spell whose only difference from the Cheetah
-- is the keyword, so a case that passed for both would be the timing gate
-- opening for every creature rather than for flash.
cheetahAndMammothInHand ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
cheetahAndMammothInHand forest cheetah warMammoth =
  let (gs0, cheetahId) = S.handOne cheetah (S.landsInPlay forest 4)
      (mammothId, gs1) = S.addHandCard warMammoth S.alice gs0
   in (gs1, cheetahId, mammothId)

-- CR 702.8a: "Flash is a static ability that functions in any zone from which
-- you could play the card it's on. 'Flash' means 'You may play this card any
-- time you could cast an instant.'"
--
-- Pouncing Cheetah is the whole producer: a {2}{G} 3/2 Cat whose entire rules
-- text is the keyword, so every case here is the keyword and nothing else.
--
-- The CAST half of rule 702.8a's "you may play this card". The other half is CR
-- 116.2a's land play, which CR 601.1a makes the same sentence reach: those cases
-- are Pawl.GameSpec's Action group, where Teferi grants flash to Dryad Arbor in
-- a hand and the gate is Action.legalActions rather than Cast.timingOk.
flashSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
flashSpec s registry = Spec.describe s "Flash" $ do
  -- The baseline both halves start from: with no flash in the question at all,
  -- alice's own main phase and an empty stack is a window BOTH creatures pass.
  -- Without this the negatives below would also hold on an engine that refused
  -- the Mammoth everywhere.
  Spec.it s "CR 302.1 the control: in alice's own main phase with an empty stack, both are castable" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
    Spec.assertBool s (S.castable S.alice cheetahId gs) "the Cheetah"
    Spec.assertBool s (S.castable S.alice mammothId gs) "and the Mammoth"
  -- CR 302.1's "during a main phase of THEIR turn", lifted for the Cheetah and
  -- not for the Mammoth.
  Spec.it s "CR 702.8a castable on an opponent's turn, where a creature without flash is not" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
        bobsTurn = gs {GameState.activePlayer = S.bob}
    Spec.assertBool s (S.castable S.alice cheetahId bobsTurn) "the Cheetah is castable"
    Spec.assertBool s (elem (A.Cast cheetahId (S.printingName pouncingCheetah) Facing.FaceUp) (Action.legalActions S.alice bobsTurn)) "and offered as a legal action"
    Spec.assertBool s (not (S.castable S.alice mammothId bobsTurn)) "the Mammoth is not"
    Spec.assertBool s (not (any (S.isCastOf mammothId) (Action.legalActions S.alice bobsTurn))) "and is not offered"
  -- CR 302.1's "when the stack is empty", same pair.
  Spec.it s "CR 702.8a castable with a non-empty stack, where a creature without flash is not" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
        busy = gs {GameState.stack = [ObjectId.MkObjectId 999]}
    Spec.assertBool s (S.castable S.alice cheetahId busy) "the Cheetah is castable"
    Spec.assertBool s (not (S.castable S.alice mammothId busy)) "the Mammoth is not"
  -- CR 302.1's "during a MAIN PHASE", same pair.
  Spec.it s "CR 702.8a castable in the upkeep, where a creature without flash is not" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
        upkeep = gs {GameState.phase = Phase.Beginning BeginningStep.Upkeep}
    Spec.assertBool s (S.castable S.alice cheetahId upkeep) "the Cheetah is castable"
    Spec.assertBool s (not (S.castable S.alice mammothId upkeep)) "the Mammoth is not"
  -- Rule 702.8a's second sentence is about WHEN, and says nothing about WHERE:
  -- it lets a player play the card any time they could cast an instant, not
  -- from anywhere they could not already. A graveyard needs a CR 601.3
  -- permission (flashback's), which flash is not.
  Spec.it s "CR 601.3 flash is a timing window and not a zone permission: a buried Cheetah is uncastable" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    let (gs, cheetahId) = S.handOne pouncingCheetah (S.landsInPlay forest 4)
        buried = S.runPure S.identityAnswer gs (Event.changeZone cheetahId Zone.Graveyard)
    Spec.assertBool s (S.castable S.alice cheetahId gs) "castable from the hand"
    Spec.assertEqWith s "and nothing castable once it is in the graveyard" (Cast.castableSpells S.alice buried) []
  -- Flash moves the window the cast is PROPOSED in and nothing else. Two rules
  -- say what is left untouched, and they are two:
  --
  --   * CR 601.2a, the stack half: "To propose the casting of a spell, a player
  --     first moves that card ... from where it is to the stack. It becomes the
  --     topmost object on the stack." So the Cheetah is a spell before it is a
  --     permanent, exactly as a sorcery-speed creature spell is.
  --   * CR 117.3c, the response half: "If a player has priority when they cast a
  --     spell ... that player receives priority afterward" -- and then CR 117.1a
  --     lets the opponent cast an instant when priority reaches them.
  Spec.it s "CR 601.2a / 117.3c an instant-speed creature spell still uses the stack and can be responded to" $ do
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs0, cheetahId) = S.handOne pouncingCheetah (S.landsInPlay forest 4)
        (boltId, gs1) = S.addHandCard lightningBolt S.bob (snd (S.addCreature mountain S.bob gs0))
        bobsTurn = gs1 {GameState.activePlayer = S.bob}
        cast = S.runPure S.identityAnswer bobsTurn (S.cast S.alice cheetahId)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "one object on the stack" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "and not on the battlefield yet" (S.creaturesInPlay S.alice cast) 0
    Spec.assertBool s (S.castable S.bob boltId cast) "bob may respond to it"
    Spec.assertEqWith s "it resolves into a creature like any other" (S.creaturesInPlay S.alice resolved) 1
  -- Cast.instantSpeed reads the CR 613 projection, and this is the case that says
  -- the printed keyword still reaches it: a card whose flash is printed rather
  -- than granted is castable on the same board.
  --
  -- Humility is why the projection is the RIGHT reader rather than a coincidence
  -- here: CR 109.2 makes its "all creatures" mean permanents on the battlefield,
  -- and a card in a hand is not one of them, so the window stays open and the
  -- projection says so.
  Spec.it s "CR 702.8a the projection of a card in hand carries flash, and Humility does not reach it" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    humility <- S.printingOf s registry "Humility"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
        humbled = (S.withHumility humility gs) {GameState.activePlayer = S.bob}
    Spec.assertBool s (Projection.hasKeyword Keyword.Flash cheetahId humbled) "the Cheetah projects flash"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flash mammothId humbled)) "the Mammoth does not"
    Spec.assertBool s (S.castable S.alice cheetahId humbled) "and it is still castable on bob's turn"

  -- CR 613.1f layer 6 over a card in a HAND. Teferi, Mage of Zhalfir's "creature
  -- cards you own that aren't on the battlefield have flash" is the pool's first
  -- effect to change a card's keywords while it sits in a hand, which is what
  -- makes Cast.instantSpeed's projected read observable at all.
  --
  -- A PAIR of boards differing in exactly one thing: whether Teferi is on the
  -- battlefield. Same four Forests, same Mammoth in the same hand, same seat
  -- active, and S.castAnswer takes whatever cast it is OFFERED -- so on the bare
  -- board alice is offered none and passes.
  --
  -- Three readings the pair separates. War Mammoth prints no flash, so "the card
  -- always had it" would put it onto the battlefield on the bare board too. It is
  -- in the hand before Teferi arrives, so "the effect applied as it was drawn"
  -- puts it there on neither. Only an effect applying to a card SITTING in a hand
  -- puts it there on exactly one.
  --
  -- Teferi's third clause is on the card and reaches nothing here: it is scoped
  -- to his controller's opponents, and alice both controls him and takes every
  -- cast below. teferiSorcerySpeedSpec is where that clause is proved.
  Spec.it s "CR 702.8a/613.1f Teferi gives a creature card in hand flash, and it is cast on bob's turn" $ do
    forest <- S.printingOf s registry "Forest"
    warMammoth <- S.printingOf s registry "War Mammoth"
    teferi <- S.printingOf s registry "Teferi, Mage of Zhalfir"
    let (mammothId, bare) = teferiBoard forest warMammoth Nothing
        withTeferi = snd (teferiBoard forest warMammoth (Just teferi))
        play gs = S.runPure S.castAnswer gs Engine.priorityLoop
        after = play withTeferi
    Spec.assertEqWith s "the Mammoth is on the battlefield" (S.countOnBattlefieldByName (S.printingName warMammoth) S.alice after) 1
    Spec.assertEqWith s "and without Teferi it never left her hand" (S.countOnBattlefieldByName (S.printingName warMammoth) S.alice (play bare)) 0
    Spec.assertEqWith s "bob was the active player throughout" (GameState.activePlayer after) S.bob
    Spec.assertBool s (Projection.hasKeyword Keyword.Flash mammothId withTeferi) "the card in hand projects flash"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flash mammothId bare)) "and does not without Teferi"

  -- The arm's own gate, read from the other side. Teferi's set is
  -- Affected.MatchingOffBattlefield, so a creature ON the battlefield is outside
  -- it -- which is the whole difference between that arm and MatchingAnywhere,
  -- and CR 702.8a is a permission to PLAY a card, so nothing else on the board
  -- would show it.
  --
  -- TWO War Mammoths on ONE board differing in exactly one thing: which zone each
  -- is in. Same printing, same owner, same Teferi, so a set that ignored the zone
  -- would answer alike for both.
  Spec.it s "CR 613.1f Teferi's off-battlefield set reaches the Mammoth in hand and not the one in play" $ do
    forest <- S.printingOf s registry "Forest"
    warMammoth <- S.printingOf s registry "War Mammoth"
    teferi <- S.printingOf s registry "Teferi, Mage of Zhalfir"
    let (inHand, gs0) = teferiBoard forest warMammoth (Just teferi)
        (inPlay, gs) = S.addCreature warMammoth S.alice gs0
    Spec.assertBool s (Projection.hasKeyword Keyword.Flash inHand gs) "the one in hand has flash"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flash inPlay gs)) "the one on the battlefield does not"

-- Four Forests and a War Mammoth in alice's hand, on BOB's turn with alice
-- holding priority, and Teferi on the battlefield or not. The Mammoth is {3}{G},
-- which the four Forests pay exactly, so affordability is identical either way
-- and the only thing that varies is the printing this takes.
teferiBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Maybe Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
teferiBoard forest warMammoth mTeferi =
  let (gs0, mammothId) = S.handOne warMammoth (S.landsInPlay forest 4)
      gs1 = maybe gs0 (\teferi -> snd (S.addCreature teferi S.alice gs0)) mTeferi
   in ( mammothId,
        gs1
          { GameState.activePlayer = S.bob,
            GameState.priority = Just S.alice
          }
      )

-- The two names Wax // Wane prints (CR 709.4a). Neither of them is "Wax//Wane",
-- which is the combined view's stand-in and not a name the card has.
waxName, waneName :: CardName.CardName
waxName = CardName.MkCardName (Text.pack "Wax")
waneName = CardName.MkCardName (Text.pack "Wane")

onwardName, victoryName :: CardName.CardName
onwardName = CardName.MkCardName (Text.pack "Onward")
victoryName = CardName.MkCardName (Text.pack "Victory")

-- The pool's first split card (CR 709.1), and so the first case here that
-- exercises CR 709.3-709.4 against a printed card rather than a fixture: Wax is
-- {G} "Target creature gets +2/+2 until end of turn", Wane is {W} "Destroy
-- target enchantment".
--
-- Every case names the half and calls Pawl.Engine.Cast directly. S.cast and
-- S.castable route through S.soleFaceName, which errors on a card with more
-- than one castable half precisely so a split card cannot silently exercise
-- nothing here.
waxWaneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
waxWaneSpec s registry = Spec.describe s "WaxWane" $ do
  -- CR 709.3: "A player chooses which half of a split card they are casting
  -- before putting it onto the stack."
  --
  -- Falsifier: an engine pricing the cast from CR 709.4b's COMBINED {G}{W}
  -- fails here. One Forest cannot pay it, so no candidate survives
  -- castProposed's payability filter, the cast rewinds (CR 601.2e), and the
  -- Piker stays a 2/1.
  --
  -- What this case does NOT catch, despite the shape suggesting it: an engine
  -- resolving the combined view's PAYLOAD. Pawl.Engine.Card.merge2 deliberately
  -- leaves Face.spell as the left half's, so the combined view carries Wax's
  -- effect and would pass. Nor does it catch one that always casts the first
  -- face, which Wax already is. The Wane case below is what discriminates
  -- against both.
  Spec.it s "CR 709.3 casting Wax gives the targeted creature +2/+2" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    waxWane <- S.printingOf s registry "Wax"
    let (pikerId, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
        (gs, oid) = S.handOne waxWane withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.manaPerformer S.alice oid waxName Facing.FaceUp))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the 2/1 Piker is a 4/3" (S.powerToughnessOf pikerId resolved) (Just (4, 3))
  -- The half that carries the weight. One Plains, an enchantment, and no
  -- creature at all -- so three broken engines fail here, two of which the Wax
  -- case above lets through:
  --
  --   * one that always casts the FIRST face resolves Wax, which has no legal
  --     target on a creatureless board;
  --   * one that resolves the COMBINED view's payload finds the same, since
  --     Pawl.Engine.Card.merge2 keeps Face.spell as the left half's -- Wax's;
  --   * one that prices from the combined {G}{W} cannot pay it with a Plains.
  --
  -- Each rewinds the cast (CR 601.2e) and leaves the Prison standing.
  Spec.it s "CR 709.3 casting Wane destroys the targeted enchantment" $ do
    plains <- S.printingOf s registry "Plains"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    waxWane <- S.printingOf s registry "Wane"
    let (prisonId, withPrison) = S.addCreature ghostlyPrison S.alice (S.landsInPlay plains 1)
        (gs, oid) = S.handOne waxWane withPrison
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.manaPerformer S.alice oid waneName Facing.FaceUp))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertBool s (S.onBattlefield prisonId gs) "the Prison starts on the battlefield"
    Spec.assertBool s (not (S.onBattlefield prisonId resolved)) "and Wane destroys it"
  -- CR 709.4: "In every zone except the stack, the characteristics of a split
  -- card are those of its two halves combined", and CR 709.4b: "The mana cost of
  -- a split card is the combined mana costs of its two halves. A split card's
  -- colors and mana value are determined from its combined mana cost." Read
  -- through the game state rather than off the card, so this is the combined
  -- view a resting object actually projects.
  Spec.it s "CR 709.4b a split card in a graveyard is green and white with mana value 2" $ do
    waxWane <- S.printingOf s registry "Wax"
    let (oid, gs) = S.addGraveyardCard waxWane S.alice (Setup.emptyGame S.bothPlayers)
    case Game.faceOf oid gs of
      Nothing -> Spec.assertFailure s "expected a card in the graveyard"
      Just face -> do
        Spec.assertEqWith s "both colours" (Projection.printedColorsOf face) (Set.fromList [Color.Green, Color.White])
        Spec.assertEqWith s "mana value 2" (Quantity.manaValueOf face) 2
  -- CR 709.3b: "While on the stack, only the characteristics of the half being
  -- cast exist. The other half's characteristics are treated as though they
  -- didn't exist." The same card in a hand is CR 709.4's combined view instead,
  -- which is the contrast that makes the stack reading a narrowing rather than
  -- the only answer the engine has.
  Spec.it s "CR 709.3b the Wax on the stack is named Wax, where the card in hand is not" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    waxWane <- S.printingOf s registry "Wax"
    let (_, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
        (gs, oid) = S.handOne waxWane withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.manaPerformer S.alice oid waxName Facing.FaceUp))
    -- CR 709.4a: "Each split card has two names." BOTH of them, asked one at a
    -- time, and the "Wax//Wane" the combined Face renders them as is not among
    -- them -- that string is how the CR's own Examples write the pair and not a
    -- name the card has.
    Spec.assertEqWith s "in hand, the combined view has both names" (Projection.namesOf oid gs) (Set.fromList [waxName, waneName])
    Spec.assertEqWith s "and has Wax" (Projection.hasName waxName oid gs) True
    Spec.assertEqWith s "and has Wane" (Projection.hasName waneName oid gs) True
    Spec.assertEqWith s "and does NOT have the two joined" (Projection.hasName (CardName.MkCardName (Text.pack "Wax//Wane")) oid gs) False
    case GameState.stack cast of
      [] -> Spec.assertFailure s "expected the spell on the stack"
      top : _ -> do
        -- CR 709.3b narrows the pair to one: the half being cast.
        Spec.assertEqWith s "on the stack, the half being cast" (Projection.namesOf top cast) (Set.singleton waxName)
        Spec.assertEqWith s "so the OTHER half's name is not one of the spell's" (Projection.hasName waneName top cast) False
  Spec.it s "CR 709.3a each half is offered and gated on its own" $ do
    waxWane <- S.printingOf s registry "Wax"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    -- Both halves need a legal target, or targeting gates them BOTH out and the
    -- offered list is empty for a reason that has nothing to do with mana. Wax
    -- wants a creature; Wane wants an enchantment.
    let targets g = snd (S.addCreature ghostlyPrison S.alice (snd (S.addCreature piker S.alice g)))
        namesOffered gs = [n | A.Cast _ n _ <- Action.legalActions S.alice gs]
        (green, _) = S.handOne waxWane (targets (S.landsInPlay forest 1))
        (both, _) = S.handOne waxWane (targets (snd (S.addCreature plains S.alice (S.landsInPlay forest 1))))
    -- CR 709.3a: "Only the chosen half is evaluated to see if it can be cast."
    -- One Forest pays Wax's {G} and cannot pay Wane's {W}. Falsifier: an engine
    -- pricing either half from CR 709.4b's combined {G}{W} would offer NEITHER.
    Spec.assertEqWith s "one Forest: only the affordable half" (namesOffered green) [waxName]
    -- The other direction, without which "each half is gated on its own" is
    -- indistinguishable from "the first face wins": with both halves payable,
    -- both are offered and CR 709.3's choice is left to the player.
    Spec.assertEqWith s "a Forest and a Plains: both halves" (namesOffered both) [waxName, waneName]
  -- The observer for CR 709.3a's half being part of the CR 601.2a MOVE rather
  -- than a stamp applied once the move has landed.
  --
  -- Synthetic Stack Interdiction is "If a green card would be put onto the stack,
  -- exile it instead" -- a CR 614.1a replacement of the one zone change CR 601.2a
  -- makes. Nothing printed watches that event: a sweep of Scryfall's oracle bulk
  -- data (38,542 cards) for "onto the stack" returns two, and neither replaces
  -- anything -- Grip of Chaos is a TRIGGER on the same moment, and Ertai's
  -- Meddling only says where a delayed card goes. So the redirect is synthetic
  -- while the split card it reads is a printing.
  --
  -- Two readings of the same card, one case:
  --
  --   * BEFORE the move, CR 616.1 asks the pattern about the card as it still
  --     sits in the hand, where CR 709.4 gives it both halves combined. The half
  --     being cast is WANE, which is white; the pattern names GREEN, which only
  --     Wax is. An engine reading CR 709.3b's chosen half here would find no
  --     green card, decline the redirect, and leave Wane on the stack.
  --   * AFTER it, the card is in EXILE and was never put onto the stack at all,
  --     so CR 709.3a ("only that half is considered to be put onto the stack")
  --     has nothing to say about it and CR 709.4's combined view is what it
  --     shows -- both names, and the colours of the combined mana cost (CR
  --     709.4b).
  --
  -- CR 614.6 is what makes the second reading follow from the first: the
  -- modified event is the event that happens, so the destination the CR 616.1
  -- loop settled on is the destination the move must answer for. Only a writer
  -- INSIDE the move can, which is what this proves -- restoring the pre-#781
  -- ordering (Event.changeZoneAttaching setting no face, Cast.castSpell stamping
  -- the chosen half onto whatever the move handed back) exiles a card named
  -- "Wane" whose only colour is white, and fails this case.
  Spec.it s "CR 709.4 a cast redirected off the stack keeps both halves" $ do
    plains <- S.printingOf s registry "Plains"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    interdiction <- S.printingOf s registry "Synthetic Stack Interdiction"
    waxWane <- S.printingOf s registry "Wax"
    -- The Prison is Wane's target (CR 601.2c), so the cast does not rewind for
    -- want of one before it ever reaches the move.
    let (_, withPrison) = S.addCreature ghostlyPrison S.alice (S.landsInPlay plains 1)
        (_, withInterdiction) = S.addCreature interdiction S.alice withPrison
        (gs, oid) = S.handOne waxWane withInterdiction
        after = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.manaPerformer S.alice oid waneName Facing.FaceUp))
    -- Not asserted: what CR 601.2b-i do afterwards. castSpell announces, prices
    -- and pays for a spell the redirect has already moved off the stack, and
    -- which of the two defensible readings is right is not implemented (#816).
    Spec.assertEqWith s "the redirect fired, so nothing reached the stack" (length (GameState.stack after)) 0
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiled] -> do
        Spec.assertEqWith s "in exile, CR 709.4's combined view has both names" (Projection.namesOf exiled after) (Set.fromList [waxName, waneName])
        Spec.assertEqWith s "and CR 709.4b's combined colours" (Projection.colorsOf exiled after) (Set.fromList [Color.Green, Color.White])
      _ -> Spec.assertFailure s "expected the redirected card in exile"

-- Wear // Tear {1}{R} // {W}, the pool's first card with fuse (CR 702.102):
-- Wear is "Destroy target artifact", Tear is "Destroy target enchantment", and
-- both halves print "Fuse (You may cast one or both halves of this card from
-- your hand.)".
--
-- The card that tells a fused cast from either half: two halves that destroy
-- DIFFERENT permanents, so "both halves resolved" is a board no single half
-- reaches, and two mana costs of different colours, so rule 702.102c's combined
-- cost is a board too.
--
-- Every case casts by NAME, as the WaxWane group above does. The fused spell's
-- name is neither half's -- CR 709.4a's pair rendered, which
-- Pawl.Engine.Card.merge2 joins and Pawl.Engine.Game.resolveFace turns back into
-- the fused face.
wearName, tearName, fusedName :: CardName.CardName
wearName = CardName.MkCardName (Text.pack "Wear")
tearName = CardName.MkCardName (Text.pack "Tear")
fusedName = CardName.MkCardName (Text.pack "Wear//Tear")

sphereName, prisonName :: CardName.CardName
sphereName = CardName.MkCardName (Text.pack "Chromatic Sphere")
prisonName = CardName.MkCardName (Text.pack "Ghostly Prison")

wearTearSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wearTearSpec s registry = Spec.describe s "WearTear" $ do
  -- CR 702.102d: "As a fused split spell resolves, the controller of the spell
  -- follows the instructions of the left half and then follows the instructions
  -- of the right half."
  --
  -- The Prison assertion comes first because it is the one that discriminates:
  -- an engine resolving the combined view's payload as it stood before this
  -- change destroys the Sphere and nothing else (Pawl.Engine.Card.merge2 keeps
  -- Face.spell as the LEFT half's), so a Sphere-first ordering would report a
  -- pass on the very half that is not in question.
  --
  -- Asserted BY NAME rather than by object id: a destroyed permanent is a new
  -- object in its graveyard (CR 400.7), and a count of what is still on the
  -- battlefield asks the question the rule does.
  Spec.it s "CR 702.102d a fused cast destroys the enchantment and the artifact" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    wearTear <- S.printingOf s registry "Wear"
    let board = S.landsFor mountain S.alice 2 (S.landsInPlay plains 1)
        (_, withSphere) = S.addCreature sphere S.alice board
        (_, withPrison) = S.addCreature ghostlyPrison S.alice withSphere
        (gs, oid) = S.handOne wearTear withPrison
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.manaPerformer S.alice oid fusedName Facing.FaceUp))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the Prison starts on the battlefield" (S.countOnBattlefieldByName prisonName S.alice gs) 1
    Spec.assertEqWith s "and Tear, the RIGHT half, destroys it" (S.countOnBattlefieldByName prisonName S.alice resolved) 0
    Spec.assertEqWith s "the Sphere starts on the battlefield" (S.countOnBattlefieldByName sphereName S.alice gs) 1
    Spec.assertEqWith s "and Wear, the left half, destroys it" (S.countOnBattlefieldByName sphereName S.alice resolved) 0
  -- CR 702.102a offers a THIRD action and never replaces the two halves: "the
  -- player may choose to cast both halves of that split card rather than choose
  -- one half", a choice the player makes by picking an action.
  --
  -- CR 702.102c is the pair that follows: "the total cost of a fused split spell
  -- includes the mana cost of each half". Two boards differing in exactly one
  -- Plains -- {1}{R} pays Wear alone, {1}{R}{W} pays the fused spell too -- so
  -- the fused offer appearing is about the mana and not about the targets, which
  -- both boards supply.
  Spec.it s "CR 702.102c the fused cast is offered only where both halves' mana is payable" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    wearTear <- S.printingOf s registry "Wear"
    let targets g = snd (S.addCreature ghostlyPrison S.alice (snd (S.addCreature sphere S.alice g)))
        namesOffered gs = [n | A.Cast _ n _ <- Action.legalActions S.alice gs]
        (red, _) = S.handOne wearTear (targets (S.landsInPlay mountain 2))
        (both, _) = S.handOne wearTear (targets (S.landsFor plains S.alice 1 (S.landsInPlay mountain 2)))
    Spec.assertEqWith s "two Mountains: Wear alone, and no fused cast" (namesOffered red) [wearName]
    Spec.assertEqWith s "a Plains as well: both halves and the fused cast" (namesOffered both) [wearName, tearName, fusedName]
  -- The other half of that offer: CR 702.102a is a permission fuse GRANTS, so a
  -- split card without it is offered its two halves and nothing else. Wax // Wane
  -- is that card -- Scryfall reports no keywords on it, and Dragon's Maze is
  -- where fuse was printed -- and the WaxWane group above is where its halves are
  -- proved castable, so this case adds only the absence.
  Spec.it s "CR 702.102a a split card without fuse offers no fused cast" $ do
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    waxWane <- S.printingOf s registry "Wax"
    let targets g = snd (S.addCreature ghostlyPrison S.alice (snd (S.addCreature piker S.alice g)))
        namesOffered board = [n | A.Cast _ n _ <- Action.legalActions S.alice board]
        (unfused, _) = S.handOne waxWane (targets (S.landsFor plains S.alice 1 (S.landsInPlay forest 1)))
    Spec.assertEqWith s "both halves payable, and still two offers" (namesOffered unfused) [waxName, waneName]
    Spec.assertEqWith s "and no fused face to offer" (fmap Face.name (Card.fusedFace (Printing.card waxWane))) Nothing
  -- CR 601.2c, asked of a spell with TWO halves' target slots: "the player
  -- announces their choice of an appropriate . . . object for each target the
  -- spell requires", and CR 601.2e rewinds the cast where they cannot. So a board
  -- with an artifact and no enchantment offers Wear and refuses the fused spell,
  -- which needs a target for Tear as well.
  --
  -- The pair to the mana case above, and the same shape: two boards differing in
  -- exactly one permanent, so the refusal is about the missing target rather than
  -- about the mana, which both boards pay in full. It is also what holds the
  -- fused proposal to the ORDINARY gates -- a proposal exempt from `castable`
  -- would be offered on both boards.
  Spec.it s "CR 601.2c the fused cast is refused where one half has no legal target" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    wearTear <- S.printingOf s registry "Wear"
    let mana = S.landsFor plains S.alice 1 (S.landsInPlay mountain 2)
        namesOffered board = [n | A.Cast _ n _ <- Action.legalActions S.alice board]
        (artifactOnly, _) = S.handOne wearTear (snd (S.addCreature sphere S.alice mana))
        (bothTargets, _) = S.handOne wearTear (snd (S.addCreature ghostlyPrison S.alice (snd (S.addCreature sphere S.alice mana))))
    Spec.assertEqWith s "no enchantment: Wear alone, and no fused cast" (namesOffered artifactOnly) [wearName]
    Spec.assertEqWith s "an enchantment as well: the fused cast returns" (namesOffered bothTargets) [wearName, tearName, fusedName]

-- Synthetic Bounded Blaze {X}{R} // Synthetic Bounded Bounty {X}{W}, both halves
-- Instants with fuse
-- (data/cards/synthetic-bounded-blaze-synthetic-bounded-bounty.json): "X can't be
-- greater than the number of creatures you control. Synthetic Bounded Blaze deals
-- X damage to each opponent." // "X can't be greater than the number of artifacts
-- you control. You gain X life."
--
-- SYNTHETIC because no printing pairs fuse with X. Scryfall `keyword:fuse`,
-- 2026-09-01, returns seventeen cards and none of them prints an {X} anywhere;
-- `o:"X can't be greater than"` returns six and none of them is a split card. So
-- CR 709.4c's combined view of two halves that each bound X has no printing, and
-- CR 702.102b's fused split spell is the only cast that could be priced against
-- one.
--
-- The two ceilings count DIFFERENT things, and neither counts a land: the mana is
-- the same on every board below, so nothing here can pass because a cast was
-- unaffordable.
--
-- CR 101.2 is what makes both bind -- each sentence is a "can't" and beats the
-- permission on its own -- so the fused spell's ceiling is the lesser of them,
-- which Pawl.Engine.Cost.maximumX takes.
blazeName, bountyName, boundedFusedName :: CardName.CardName
blazeName = CardName.MkCardName (Text.pack "Synthetic Bounded Blaze")
bountyName = CardName.MkCardName (Text.pack "Synthetic Bounded Bounty")
boundedFusedName = CardName.MkCardName (Text.pack "Synthetic Bounded Blaze//Synthetic Bounded Bounty")

-- Three seats, so "each opponent" is not the same set as "each player": an
-- ObjectRef.EachPlayer in the left half's place would take life from alice too.
--
-- Alice controls three Goblin Pikers -- the left half's ceiling, 3 -- and
-- `artifacts` Chromatic Spheres, which is the right half's. Eight lands, four of
-- each colour, pay {4}{R}{W} with room to spare, so every case below is affordable
-- at every X it announces.
boundedBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
boundedBoard mountain plains piker sphere blaze artifacts =
  let withLands = S.landsFor plains S.alice 4 (S.landsFor mountain S.alice 4 S.threePlayerGame)
      place printing g = snd (S.addCreature printing S.alice g)
      withCreatures = List.foldl' (\g _ -> place piker g) withLands [1 .. 3 :: Int]
      withArtifacts = List.foldl' (\g _ -> place sphere g) withCreatures [1 .. artifacts]
      (spellId, withSpell) = S.addHandCard blaze S.alice withArtifacts
   in ( spellId,
        withSpell
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Announces this X and answers everything else the ordinary way.
answerBoundedX :: Natural -> Prompt.Prompt r -> r
answerBoundedX n p = case p of
  Prompt.ChooseX {} -> n
  _ -> S.identityAnswer p

boundedFuseXSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
boundedFuseXSpec s registry = Spec.describe s "BoundedFuseX" $ do
  -- The PROVING case, and the pair to the one below: two boards differing in
  -- exactly one Chromatic Sphere, so the refusal is the RIGHT half's ceiling and
  -- nothing else. An engine keeping only the left half's ceiling reads 3 here,
  -- permits the announcement, and deals two to each opponent.
  Spec.it s "CR 709.4c a fused cast is refused an X above the RIGHT half's ceiling" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    blaze <- S.printingOf s registry "Synthetic Bounded Blaze"
    let (spellId, board) = boundedBoard mountain plains piker sphere blaze 1
        after = S.runPure (answerBoundedX 2) board (do Cast.castSpell S.manaPerformer S.alice spellId boundedFusedName Facing.FaceUp; Stack.resolveTop)
    Spec.assertEqWith s "bob took nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "carol took nothing" (S.lifeOf S.carol after) (Just 20)
    Spec.assertEqWith s "and alice gained nothing" (S.lifeOf S.alice after) (Just 20)
    -- CR 601.2e's rewind: the whole casting is undone, not just the half that
    -- broke its own ceiling.
    Spec.assertEqWith s "the card is still in alice's hand" (S.handSize S.alice after) 1
  -- The CONTROL. One more artifact makes the right half's ceiling 2, so the same
  -- announcement on the same mana is legal and both halves resolve.
  Spec.it s "CR 702.102b an X within both halves' ceilings is announced and both halves resolve" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    blaze <- S.printingOf s registry "Synthetic Bounded Blaze"
    let (spellId, board) = boundedBoard mountain plains piker sphere blaze 2
        after = S.runPure (answerBoundedX 2) board (do Cast.castSpell S.manaPerformer S.alice spellId boundedFusedName Facing.FaceUp; Stack.resolveTop)
    Spec.assertEqWith s "bob took two" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "carol took two" (S.lifeOf S.carol after) (Just 18)
    Spec.assertEqWith s "and alice gained two" (S.lifeOf S.alice after) (Just 22)
    Spec.assertEqWith s "the card left alice's hand" (S.handSize S.alice after) 0
  -- The other direction, on the board the first case refuses: CR 709.3b puts ONE
  -- half on the stack, so a cast of the left half alone is priced against the left
  -- half's ceiling and the right half's does not reach it.
  Spec.it s "CR 709.3b one half alone is bound by that half's ceiling only" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    blaze <- S.printingOf s registry "Synthetic Bounded Blaze"
    let (spellId, board) = boundedBoard mountain plains piker sphere blaze 1
        after = S.runPure (answerBoundedX 3) board (do Cast.castSpell S.manaPerformer S.alice spellId blazeName Facing.FaceUp; Stack.resolveTop)
    Spec.assertEqWith s "bob took three" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "carol took three" (S.lifeOf S.carol after) (Just 17)
    -- The right half was never cast, so its life gain never happened.
    Spec.assertEqWith s "and alice gained nothing" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "the card left alice's hand" (S.handSize S.alice after) 0
  -- The right half alone, for the same reason in the other half's favour: its
  -- ceiling is 1 on this board, so an X of 2 is refused where the fused cast's
  -- control above allowed it on a board with a second artifact.
  Spec.it s "CR 709.3b the right half alone is refused an X above its own ceiling" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    blaze <- S.printingOf s registry "Synthetic Bounded Blaze"
    let (spellId, board) = boundedBoard mountain plains piker sphere blaze 1
        castAt n = S.runPure (answerBoundedX n) board (do Cast.castSpell S.manaPerformer S.alice spellId bountyName Facing.FaceUp; Stack.resolveTop)
        after = castAt 2
        within = castAt 1
    Spec.assertEqWith s "alice gained nothing" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and the card is still in her hand" (S.handSize S.alice after) 1
    -- The same board and the same half, one lower: the refusal above is the
    -- ceiling and not the mana, the timing or a missing action.
    Spec.assertEqWith s "an X of one is within the ceiling and gains one" (S.lifeOf S.alice within) (Just 21)

-- Victor Mancha, Runaway {5} Legendary Artifact Creature -- Human Hero 4/4:
-- "When Victor Mancha enters, exile target card from your graveyard. You may
-- play it for as long as you control Victor Mancha." The pool's only
-- Effect-granted permission to play a card from exile (CR 601.3), and the only
-- one of either kind with a STATED duration -- CR 715.3d's states none, so
-- Pawl.AdventureSpec cannot reach the sweep this group exercises.
--
-- Its target slot names CardsInGraveyard with no filter, so the permitted card
-- may be a LAND -- which is played and never cast (CR 305.1), and is the last
-- two cases here.
victorName, benalishHeroName, swampName :: CardName.CardName
victorName = CardName.MkCardName (Text.pack "Victor Mancha, Runaway")
benalishHeroName = CardName.MkCardName (Text.pack "Benalish Hero")
swampName = CardName.MkCardName (Text.pack "Swamp")

-- The adventurer card the last case below permits out of exile. Named here
-- rather than imported from Pawl.AdventureSpec, which is the module's own
-- convention for every other card name in this file.
shieldbreakerName, battleDisplayName :: CardName.CardName
shieldbreakerName = CardName.MkCardName (Text.pack "Embereth Shieldbreaker")
battleDisplayName = CardName.MkCardName (Text.pack "Battle Display")

-- The battlefield objects answering to a name -- how a test reaches the
-- permanent a cast produced, whose id is neither the card's in hand nor the
-- spell's on the stack (CR 400.7).
namedOnBattlefield :: CardName.CardName -> GameState.GameState -> [ObjectId.ObjectId]
namedOnBattlefield name gs = filter (\o -> Projection.hasName name o gs) (Set.toList (GameState.battlefield gs))

-- Six Plains, `victim` in alice's graveyard, and Victor Mancha cast out of her
-- hand and resolved, with his ETB waiting on the stack.
--
-- FIVE Plains pay Victor's {5} and the SIXTH is left untapped. That is the whole
-- answer to the cast-gate vacuity trap: every assertion below about the
-- graveyard card being castable or not is made on a board where the {W} for the
-- Benalish Hero is untapped and the window is the same precombat main phase, so
-- a missing offer is about the permission and can be about nothing else.
--
-- `victim` is the ONLY card in alice's graveyard, so CR 603.3d's target choice is
-- forced and S.identityAnswer suffices. The cast cases pass a Benalish Hero, a
-- 1/1 whose only text is banding, so nothing it prints can be the reason a cast
-- succeeds or fails; the land-play cases pass a Swamp, which is the same board
-- with a card type the cast path cannot serve.
--
-- Returns the victim's graveyard id, the battlefield objects named Victor Mancha
-- (a list, so a case that removes him can assert it emptied), and the state.
victorTriggered :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
victorTriggered plains victor victim =
  let (heroId, board) = S.addGraveyardCard victim S.alice (S.landsInPlay plains 6)
      (gs, victorCard) = S.handOne victor board
      cast = S.runPure S.identityAnswer gs (Cast.castSpell S.manaPerformer S.alice victorCard victorName Facing.FaceUp)
      entered = S.runPure S.identityAnswer cast Stack.resolveTop
      -- CR 603.3b: the enters trigger goes onto the stack the next time a player
      -- would receive priority.
      placed = S.runPure S.identityAnswer entered Engine.settleForPriority
   in (heroId, namedOnBattlefield victorName placed, placed)

-- Whether alice is offered the cast of this object under this name.
offeredCast :: ObjectId.ObjectId -> CardName.CardName -> GameState.GameState -> Bool
offeredCast oid name gs = elem (A.Cast oid name Facing.FaceUp) (Action.legalActions S.alice gs)

-- The land plays this player is offered, in the engine's own order. A list
-- rather than a membership test, so a negative below is read off something that
-- is never empty (see the land-play case's board).
offeredPlays :: PlayerId.PlayerId -> GameState.GameState -> [A.Action]
offeredPlays pid gs =
  let isPlay action = case action of
        A.Play {} -> True
        A.Pass -> False
        A.Cast {} -> False
        A.Activate _ _ -> False
        A.TurnFaceUp {} -> False
        A.Unlock _ _ -> False
        A.DiscardFromHand _ -> False
        A.Plot _ -> False
        A.Foretell _ -> False
        A.Ignore _ -> False
        A.EndEffect _ -> False
        A.ActivateManaAbility _ -> False
   in filter isPlay (Action.legalActions pid gs)

-- The board victorTriggered leaves, moved to alice's precombat main phase with
-- the stack empty: CR 305.1's window, which that fixture's untap step is not.
inHerMainPhase :: GameState.GameState -> GameState.GameState
inHerMainPhase gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = S.alice,
      GameState.priority = Just S.alice
    }

victorManchaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
victorManchaSpec s registry = Spec.describe s "VictorMancha" $ do
  -- CR 608.2c: two instructions in one clause, in written order -- the exile
  -- happens and the permission is written over the incarnation CR 400.7 minted
  -- at the destination, which is the slot the move bound.
  Spec.it s "CR 601.3 the exiled card becomes castable from exile, and only by the permitted player" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    hero <- S.printingOf s registry "Benalish Hero"
    let (heroId, victors, placed) = victorTriggered plains victor hero
        after = S.runPure S.identityAnswer placed Stack.resolveTop
    Spec.assertEqWith s "Victor is on the battlefield" (length victors) 1
    Spec.assertEqWith s "the graveyard is empty" (Game.zoneMembers Zone.Graveyard S.alice after) []
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertBool s (exiledId /= heroId) "CR 400.7: exile holds a new incarnation"
        -- The ACTION LIST, not the field: a field read would pass against an
        -- engine where Pawl.Engine.Cast never consulted the permission at all.
        Spec.assertBool s (offeredCast exiledId benalishHeroName after) "alice is offered the cast from exile"
        -- Five Plains paid Victor's {5}; the sixth is still untapped, which is
        -- what the negative cases below rest on.
        Spec.assertEqWith s "one Plains is still untapped" (S.tappedCount S.alice after) 5
        case Game.faceOf exiledId after of
          Nothing -> Spec.assertFailure s "expected a face on the exiled card"
          Just face -> do
            -- CR 109.5: the permission names the ability's controller and no one
            -- else. Asked of permitsCastFromExile directly, because bob's cast
            -- would be refused on this board for a second reason -- he has no
            -- mana -- so a gameplay-level negative here would be over-determined
            -- and would discriminate nothing. Cast.zoneCandidates reads exile as
            -- the shared zone, so bob does reach alice's card.
            Spec.assertBool s (Cast.permitsCastFromExile S.alice exiledId face after) "alice is permitted"
            Spec.assertBool s (not (Cast.permitsCastFromExile S.bob exiledId face after)) "bob is not"
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- CR 611.2b's "for as long as": the duration ends when its condition stops
  -- holding, and the permission goes with it. The SAME board and the SAME step as
  -- the case above, differing only in whether Victor is on the battlefield.
  Spec.it s "CR 611.2b the permission ends when its source leaves, and the card stays in exile" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    hero <- S.printingOf s registry "Benalish Hero"
    let (_, victors, placed) = victorTriggered plains victor hero
        after = S.runPure S.identityAnswer placed Stack.resolveTop
        dead = S.runPure S.identityAnswer after (Event.destroy Regenerability.Regenerable victors)
        swept = S.runPure S.identityAnswer dead Engine.settleForPriority
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertBool s (offeredCast exiledId benalishHeroName after) "offered while Victor stands"
        Spec.assertEqWith s "Victor is gone" (namedOnBattlefield victorName swept) []
        -- Both halves matter. Still in exile says the offer's disappearance is
        -- the permission ending and not the card moving, which CR 400.7 would
        -- have ended for free.
        Spec.assertBool s (elem exiledId (Game.zoneMembers Zone.Exile S.alice swept)) "the card is still in exile"
        Spec.assertBool s (not (offeredCast exiledId benalishHeroName swept)) "and no longer offered"
        -- The mana did not move either, so the absent offer is not about cost.
        Spec.assertEqWith s "the same Plains is still untapped" (S.tappedCount S.alice swept) 5
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- CR 611.2b's first sentence: "if the 'for as long as' duration never starts,
  -- the effect does nothing". Victor dies while his own trigger is on the stack,
  -- so the trigger still resolves (its target is legal, CR 608.2b) and still
  -- exiles the card -- but no permission is ever written.
  --
  -- The discriminator a sweep cannot launder: this state is reached without any
  -- sweep running over a stored permission, so a green result here means nothing
  -- was ever stored rather than that nothing survived.
  Spec.it s "CR 611.2b a duration that never starts stores no permission, and the exile still happens" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    hero <- S.printingOf s registry "Benalish Hero"
    let (_, victors, placed) = victorTriggered plains victor hero
        dead = S.runPure S.identityAnswer placed (Event.destroy Regenerability.Regenerable victors)
        after = S.runPure S.identityAnswer dead Stack.resolveTop
    Spec.assertBool s (not (null (GameState.stack placed))) "the trigger really was on the stack"
    Spec.assertEqWith s "Victor died before it resolved" (namedOnBattlefield victorName after) []
    case filter (\o -> Projection.hasName benalishHeroName o after) (Game.zoneMembers Zone.Exile S.alice after) of
      [exiledId] -> do
        Spec.assertEqWith
          s
          "the card was exiled with no permission on it"
          (fmap Object.playableFromExile (Game.lookupObject exiledId after))
          (Just Nothing)
        Spec.assertBool s (not (offeredCast exiledId benalishHeroName after)) "and it is not castable from exile"
      other -> Spec.assertFailure s ("expected exactly one exiled Hero, got " <> show (length other))
  -- CR 715.3d's verb is PLAY, and CR 305.1 makes playing a land a special action
  -- rather than a cast (CR 601.1 is the rule that "play" once meant casting).
  -- Victor's target slot is CardsInGraveyard with no filter, so a land card is a
  -- legal target and this is the board that follows.
  --
  -- The SAME fixture with a Swamp where the Hero was, and a Mountain added to
  -- each seat's hand: every list below therefore holds that seat's own land
  -- play whatever the permission does, so an absent Swamp is the permission and
  -- not an empty menu.
  Spec.it s "CR 305.1 an exiled LAND is offered as a land play, and only to the permitted player" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    let (_, _, placed) = victorTriggered plains victor swamp
        resolved = S.runPure S.identityAnswer placed Stack.resolveTop
        (herMountain, withHers) = S.addHandCard mountain S.alice (inHerMainPhase resolved)
        (hisMountain, after) = S.addHandCard mountain S.bob withHers
        bobsTurn = after {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertEqWith
          s
          "her hand's Mountain and the exiled Swamp, in that order"
          (offeredPlays S.alice after)
          [A.Play herMountain Nothing, A.Play exiledId Nothing]
        -- The permission is to PLAY it, and a land has no castable face, so the
        -- cast path offers it nothing. Both readings of "play" would pass the
        -- assertion above; only this one separates them.
        Spec.assertBool s (not (offeredCast exiledId swampName after)) "and it is not offered as a cast"
        -- CR 109.5 again: exile is a SHARED zone, so bob can reach the object --
        -- what stops him is that the permission names alice.
        Spec.assertEqWith s "bob is offered his own hand and nothing else" (offeredPlays S.bob bobsTurn) [A.Play hisMountain Nothing]
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- The negative of the pair, and the same one the cast side takes above: Victor
  -- leaves, CR 611.2b's duration ends, and the same board with the same Swamp in
  -- the same zone offers nothing but the hand.
  Spec.it s "CR 611.2b the land play goes with the permission" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    let (_, victors, placed) = victorTriggered plains victor swamp
        resolved = S.runPure S.identityAnswer placed Stack.resolveTop
        (herMountain, after) = S.addHandCard mountain S.alice (inHerMainPhase resolved)
        dead = S.runPure S.identityAnswer after (Event.destroy Regenerability.Regenerable victors)
        swept = S.runPure S.identityAnswer dead Engine.settleForPriority
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertBool s (elem (A.Play exiledId Nothing) (offeredPlays S.alice after)) "offered while Victor stands"
        Spec.assertBool s (elem exiledId (Game.zoneMembers Zone.Exile S.alice swept)) "the Swamp is still in exile"
        Spec.assertEqWith s "and only her hand is offered now" (offeredPlays S.alice swept) [A.Play herMountain Nothing]
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- CR 715.3d's closing clause: "it can't be cast as an Adventure THIS WAY,
  -- although other effects that allow a player to cast it may allow a player to
  -- cast it as an Adventure". Victor is one of those other effects, so the
  -- adventurer card he exiles offers BOTH halves -- CR 715.3 having the player
  -- choose between them wherever the card is playable.
  --
  -- The paired negative is Pawl.AdventureSpec's "CR 715.3d from exile the
  -- creature is castable and the Adventure is not": the SAME card, in exile, in
  -- a sorcery window, asked of by the same player -- differing only in which
  -- rule wrote the permission. Neither board alone tells "the origin is read"
  -- from "the exclusion was dropped".
  Spec.it s "CR 715.3d another effect's permission allows the Adventure half" $ do
    mountain <- S.printingOf s registry "Mountain"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    let (_, victors, placed) = victorTriggered mountain victor shieldbreaker
        resolved = S.runPure S.identityAnswer placed Stack.resolveTop
        -- Mountains rather than the fixture's Plains, and a SEVENTH added: five
        -- paid Victor's {5}, Battle Display's {R} comes off the sixth, and the
        -- creature half's {1}{R} needs the seventh. Both halves are therefore
        -- affordable, so a missing offer of either is about the permission --
        -- Cast.castable gates on payability, which is the trap this dodges.
        after = S.landsFor mountain S.alice 1 resolved
    -- Battle Display targets an artifact, and Victor is a Legendary ARTIFACT
    -- Creature standing on this board -- so CR 601.2c is satisfied and cannot be
    -- the reason for an absent offer either.
    Spec.assertEqWith s "Victor is on the battlefield, and is the artifact Battle Display can target" (length victors) 1
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertEqWith s "two Mountains untapped, so neither half is priced out" (S.tappedCount S.alice after) 5
        Spec.assertBool s (offeredCast exiledId shieldbreakerName after) "the creature half is offered, as it would be under CR 715.3d's own permission too"
        Spec.assertBool s (offeredCast exiledId battleDisplayName after) "and so is the Adventure half, which CR 715.3d's own permission would refuse"
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))

-- Dire Fleet Daredevil {1}{R} Creature -- Human Pirate 2/1: "First strike. When
-- this creature enters, exile target instant or sorcery card from an opponent's
-- graveyard. You may cast it this turn ..." The pool's only card that lets a
-- player cast a card somebody else OWNS, which is the one board where CR 405.4's
-- controller and CR 108.3's owner name different players (#83).
--
-- Both riders are expressed. "If that spell would be put into a graveyard, exile
-- it instead" is a floating CR 614.1a redirect whose pattern names the object
-- the ability's own MoveToZone bound, and CR 400.7h is what carries that name
-- from the exiled card to the spell it becomes; the case below proves it.
--
-- The other rider: "and mana of any type can be spent to cast that
-- spell" is CR 118.14, carried by the grant as ManaSpending.AnyType, and the two
-- cases at the end of this group are what prove it.
daredevilName, renewedFaithName :: CardName.CardName
daredevilName = CardName.MkCardName (Text.pack "Dire Fleet Daredevil")
renewedFaithName = CardName.MkCardName (Text.pack "Renewed Faith")

-- Three seats, because "its controller" and "its owner" collapse onto the two
-- seats of a duel: carol holds nothing and is asked for nothing, so an engine
-- that credited the spell to any player but alice is visible whichever wrong
-- player it picked.
--
-- alice holds the Daredevil and five lands -- two Mountains for its {1}{R} and
-- three Plains for Renewed Faith's {2}{W}. Two Mountains is the whole answer to
-- the cast-gate vacuity trap: {R} can only come from a Mountain, so the worst
-- payment for the Daredevil leaves a Mountain and two Plains, which pays {2}{W}
-- on any split. bob's graveyard holds Renewed Faith ({2}{W} Instant, "You gain 6
-- life") and nothing else, so CR 603.3d's target choice is forced and
-- S.identityAnswer cannot re-find a different one after a mutation.
--
-- Returns the board with the Daredevil's enters trigger already resolved: the
-- Faith exiled, and alice permitted to cast it until end of turn.
daredevilExiled :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
daredevilExiled mountain plains daredevil faith =
  let lands = S.landsFor plains S.alice 3 (S.landsFor mountain S.alice 2 S.threePlayerGame)
      (_, stocked) = S.addGraveyardCard faith S.bob lands
      (handId, board) = S.addHandCard daredevil S.alice stocked
      cast = S.runPure S.identityAnswer board (Cast.castSpell S.manaPerformer S.alice handId daredevilName Facing.FaceUp)
      entered = S.runPure S.identityAnswer cast Stack.resolveTop
      -- CR 603.3b/603.3d: the enters trigger goes onto the stack the next time a
      -- player would receive priority, and its target is chosen there.
      placed = S.runPure S.identityAnswer entered Engine.settleForPriority
   in S.runPure S.identityAnswer placed Stack.resolveTop

-- The one object named `name` in the shared exile zone. Not Game.zoneMembers,
-- which files exile by OWNER -- the whole point of this board is that the owner
-- is not the player doing anything with the card.
exiledNamed :: CardName.CardName -> GameState.GameState -> [ObjectId.ObjectId]
exiledNamed name gs = filter (\o -> Projection.hasName name o gs) (Set.toList (GameState.exile gs))

-- daredevilExiled with the Faith cast and still on the stack: alice's SPELL off
-- bob's CARD. Exported because Pawl.DepartureSpec wants the same board -- CR
-- 800.4a's fourth clause is about an object whose controller is not its owner,
-- and this is the only one in the pool that is on the stack.
daredevilFaithCast :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
daredevilFaithCast mountain plains daredevil faith =
  let exiled = daredevilExiled mountain plains daredevil faith
   in case exiledNamed renewedFaithName exiled of
        [exiledId] -> S.runPure S.identityAnswer exiled (Cast.castSpell S.manaPerformer S.alice exiledId renewedFaithName Facing.FaceUp)
        -- Left to the caller's own assertion about the stack: a board that never
        -- exiled the card is one where nothing was cast either.
        _ -> exiled

-- daredevilExiled's board with the white mana taken away and a SECOND Renewed
-- Faith put into alice's hand -- CR 118.14's board, and the pair the negative
-- rests on.
--
-- alice's five lands are all Mountains: two pay the Daredevil's {1}{R} and the
-- three that are left are the only mana she has for a {2}{W}. Nothing on this
-- board can make white (asserted, rather than assumed, in the case below), so
-- the exiled Faith is payable only under the permission's rider and the copy in
-- her hand is payable not at all.
--
-- THE PAIR IS ON ONE BOARD, which is what makes it a pair: the two Faiths are
-- the same card at the same cost, held by the same player, in the same step with
-- the same empty stack, and both are instants so CR 117.1a permits either at
-- this moment. The single difference is that one of them is being cast under CR
-- 118.14's permission. Returns the exiled card's id, the hand copy's, and the
-- board.
daredevilRedOnly :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (Maybe ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
daredevilRedOnly mountain daredevil faith =
  let lands = S.landsFor mountain S.alice 5 S.threePlayerGame
      (_, stocked) = S.addGraveyardCard faith S.bob lands
      (handFaithId, withFaith) = S.addHandCard faith S.alice stocked
      (handId, board) = S.addHandCard daredevil S.alice withFaith
      cast = S.runPure S.identityAnswer board (Cast.castSpell S.manaPerformer S.alice handId daredevilName Facing.FaceUp)
      entered = S.runPure S.identityAnswer cast Stack.resolveTop
      placed = S.runPure S.identityAnswer entered Engine.settleForPriority
      after = S.runPure S.identityAnswer placed Stack.resolveTop
   in (Maybe.listToMaybe (exiledNamed renewedFaithName after), handFaithId, after)

-- daredevilExiled's board with a SECOND Renewed Faith in alice's hand and enough
-- lands to cast both: nine, since the Daredevil takes two and each Faith takes
-- three. Three Mountains, so the worst payment for its {1}{R} still leaves six
-- Plains for two {2}{W}. Returns the hand copy's id and the board.
--
-- The hand copy is what makes the exiled card's exile-instead a claim about ONE
-- OBJECT rather than about a card name: bob owns the exiled Faith and alice owns
-- the one in her hand, so the two also differ in which graveyard CR 608.2n would
-- reach.
daredevilTwoFaiths :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
daredevilTwoFaiths mountain plains daredevil faith =
  let lands = S.landsFor plains S.alice 6 (S.landsFor mountain S.alice 3 S.threePlayerGame)
      (_, stocked) = S.addGraveyardCard faith S.bob lands
      (handFaithId, withFaith) = S.addHandCard faith S.alice stocked
      (handId, board) = S.addHandCard daredevil S.alice withFaith
      cast = S.runPure S.identityAnswer board (Cast.castSpell S.manaPerformer S.alice handId daredevilName Facing.FaceUp)
      entered = S.runPure S.identityAnswer cast Stack.resolveTop
      placed = S.runPure S.identityAnswer entered Engine.settleForPriority
   in (handFaithId, S.runPure S.identityAnswer placed Stack.resolveTop)

-- One symbol of the colour the board cannot make, as a cost to ask canPay about.
whiteCost :: ManaCost.ManaCost
whiteCost = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.White)]

direFleetDaredevilSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
direFleetDaredevilSpec s registry = Spec.describe s "DireFleetDaredevil" $ do
  -- CR 601.3: the permission names a player, so the search for castable cards in
  -- exile has to consult it rather than filtering the zone by owner first (#668).
  Spec.it s "CR 601.3 a player is offered a card in exile that an opponent owns" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let after = daredevilExiled mountain plains daredevil faith
    Spec.assertEqWith s "bob's graveyard is empty" (Game.zoneMembers Zone.Graveyard S.bob after) []
    case exiledNamed renewedFaithName after of
      [exiledId] -> do
        Spec.assertEqWith s "CR 108.3: bob still owns it" (fmap Object.owner (Game.lookupObject exiledId after)) (Just S.bob)
        -- The ACTION LIST, not the permission field: a field read would pass
        -- against an engine that never let alice reach the card.
        Spec.assertBool s (offeredCast exiledId renewedFaithName after) "alice is offered the cast"
        case Game.faceOf exiledId after of
          Nothing -> Spec.assertFailure s "expected a face on the exiled card"
          Just face -> do
            -- CR 109.5: the permission names the granting ability's controller
            -- and nobody else -- asked of the gate directly, because bob's cast
            -- would also fail for want of mana on this board and a gameplay-level
            -- negative would discriminate nothing.
            Spec.assertBool s (Cast.permitsCastFromExile S.alice exiledId face after) "alice is permitted"
            Spec.assertBool s (not (Cast.permitsCastFromExile S.bob exiledId face after)) "its owner is not"
      other -> Spec.assertFailure s ("expected exactly one exiled Faith, got " <> show (length other))
  -- CR 405.4: "the controller of a spell is the player who cast it". The spell
  -- says "YOU gain 6 life" (CR 109.5), so the life total that moves is the whole
  -- assertion -- and it moves on a board where the caster and the owner are
  -- different players, which is the only board the two readings disagree on.
  Spec.it s "CR 405.4 a spell cast off an opponent's card resolves under its caster" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let cast = daredevilFaithCast mountain plains daredevil faith
        after = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertBool s (not (null (GameState.stack cast))) "the Faith really was cast"
    Spec.assertEqWith s "alice cast it, so alice gains the life" (S.lifeOf S.alice after) (Just 26)
    Spec.assertEqWith s "its owner gains nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and the third seat is untouched" (S.lifeOf S.carol after) (Just 20)
    -- CR 608.2n would send the card to its OWNER's graveyard, and the card's own
    -- replacement sends it to exile instead (the case below) -- either way it
    -- lands under bob, which is what keeps the life assertion above from being
    -- readable as "owner and controller are the same player after all".
    Spec.assertEqWith s "the card ends up under bob, not under its caster" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
    Spec.assertEqWith s "and alice's own zones hold none of it" (length (Game.zoneMembers Zone.Exile S.alice after)) 0
  -- CR 400.7h with CR 614.1a: "If that spell would be put into a graveyard,
  -- exile it instead". The clause names the SPELL, which CR 400.7 made a new
  -- object when the card was cast -- so the printed sentence is a claim about an
  -- id that did not exist when the ability resolved.
  --
  -- The pair is on ONE board: the same card, at the same cost, cast by the same
  -- player in the same step, differing only in whether it was cast off the
  -- Daredevil's exile or out of alice's hand.
  Spec.it s "CR 400.7h a spell cast off the exiled card is exiled as it resolves, and one from hand is not" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let (handFaithId, board) = daredevilTwoFaiths mountain plains daredevil faith
    case exiledNamed renewedFaithName board of
      [exiledId] -> do
        let cast = S.runPure S.identityAnswer board (Cast.castSpell S.manaPerformer S.alice exiledId renewedFaithName Facing.FaceUp)
            after = S.runPure S.identityAnswer cast Stack.resolveTop
        Spec.assertBool s (not (null (GameState.stack cast))) "the exiled Faith really was cast"
        Spec.assertEqWith s "and it resolved, so alice gained its 6 life" (S.lifeOf S.alice after) (Just 26)
        Spec.assertEqWith s "CR 400.7h: the SPELL was exiled, so its owner's graveyard is empty" (Game.zoneMembers Zone.Graveyard S.bob after) []
        Spec.assertEqWith s "and the card the spell became is in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
        -- The other half of the pair, cast from alice's hand on this same board:
        -- the replacement names one object, not the card's name, so this Faith
        -- goes where CR 608.2n sends it.
        let fromHand = S.runPure S.identityAnswer after (Cast.castSpell S.manaPerformer S.alice handFaithId renewedFaithName Facing.FaceUp)
            resolved = S.runPure S.identityAnswer fromHand Stack.resolveTop
        Spec.assertBool s (not (null (GameState.stack fromHand))) "the hand Faith really was cast"
        Spec.assertEqWith s "CR 608.2n: the hand copy goes to alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
        Spec.assertEqWith s "and alice gained the second Faith's life too" (S.lifeOf S.alice resolved) (Just 32)
      other -> Spec.assertFailure s ("expected exactly one exiled Faith, got " <> show (length other))
  -- CR 118.14: "mana of any type can be spent to cast that spell". The offer
  -- side, on a board with no white mana on it at all -- and the same board's
  -- second Renewed Faith, in alice's hand, is the negative: same cost, same
  -- player, same step, no permission.
  Spec.it s "CR 118.14 an off-colour spell in exile is offered where the same card in hand is not" $ do
    mountain <- S.printingOf s registry "Mountain"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let (exiled, handFaithId, after) = daredevilRedOnly mountain daredevil faith
    -- The board's own claim, asked of the mana engine rather than assumed from
    -- the land names: nothing alice controls can pay {W}.
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice whiteCost after)) "alice can make no white mana"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 3]) after) "but she has three mana"
    case exiled of
      Nothing -> Spec.assertFailure s "expected the Faith to be exiled"
      Just exiledId -> do
        Spec.assertBool s (offeredCast exiledId renewedFaithName after) "the exiled Faith is offered: CR 118.14 pays its {W} with red"
        Spec.assertBool s (not (offeredCast handFaithId renewedFaithName after)) "the copy in her hand is not, and nothing but the rider tells them apart"
  -- CR 609.4b: the permission "affects only how the player may pay a cost. It
  -- doesn't change that cost, and it doesn't change what mana was actually
  -- spent" -- so the payment goes through, and what pays it is three RED mana
  -- off three Mountains.
  Spec.it s "CR 609.4b the off-colour cost is paid with red mana and resolves" $ do
    mountain <- S.printingOf s registry "Mountain"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let (exiled, _, board) = daredevilRedOnly mountain daredevil faith
    case exiled of
      Nothing -> Spec.assertFailure s "expected the Faith to be exiled"
      Just exiledId -> do
        Spec.assertEqWith s "two Mountains paid for the Daredevil" (S.tappedCount S.alice board) 2
        let cast = S.runPure S.identityAnswer board (Cast.castSpell S.manaPerformer S.alice exiledId renewedFaithName Facing.FaceUp)
            after = S.runPure S.identityAnswer cast Stack.resolveTop
        Spec.assertBool s (not (null (GameState.stack cast))) "the Faith really was cast"
        Spec.assertEqWith s "CR 118.14: the {2}{W} was paid, and alice gains the 6 life" (S.lifeOf S.alice after) (Just 26)
        -- WHAT WAS SPENT, which is rule 609.4b's second clause: three more
        -- Mountains, so the mana that paid the {W} was red and stayed red.
        Spec.assertEqWith s "all five Mountains are tapped" (S.tappedCount S.alice after) 5
        Spec.assertEqWith s "and nothing is left floating" (Game.poolOf S.alice after) (Mana.Type.MkMana [])

-- alice with `n` untapped Swamps and one spell in hand, a Goblin Piker under BOB
-- for the spells that target a creature, and Drought under bob when one is
-- passed. The positive and the negative differ in that Maybe and in nothing
-- else: same seats, same permanents, same Swamps.
--
-- Drought sits with BOB however the cast goes, because its sentence is symmetric
-- ("Spells cost an additional ...", no possessive, PlayerScope.EachPlayer) --
-- the board that proves that is the one where the caster does not control it.
droughtBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe Printing.Printing ->
  Int ->
  (GameState.GameState, ObjectId.ObjectId)
droughtBoard swamp piker spell mDrought n =
  let withPiker = snd (S.addCreature piker S.bob (S.landsInPlay swamp n))
      board = maybe withPiker (\drought -> snd (S.addCreature drought S.bob withPiker)) mDrought
   in S.handOne spell board

-- Drought {2}{W}{W} Enchantment (ICE), Oracle text checked against Scryfall:
-- "At the beginning of your upkeep, sacrifice this enchantment unless you pay
-- {W}{W}. / Spells cost an additional \"Sacrifice a Swamp\" to cast for each
-- black mana symbol in their mana costs. / Activated abilities cost an
-- additional \"Sacrifice a Swamp\" to activate for each black mana symbol in
-- their activation costs."
--
-- The SPELL sentence, which is CR 118.8's "or applied to a spell or ability from
-- another effect" -- the half a spell's own card text cannot state -- reaching CR
-- 601.2f's total. The activation sentence is Pawl.ActivateSpec's droughtSpec;
-- line one is droughtUpkeepSpec below.
--
-- STRICTLY MORE Swamps than any case consumes on every board, so a cast that
-- succeeded for lack of anything to sacrifice cannot pass: the assertion is the
-- SURVIVOR count and never zero.
--
-- NO SINGLE CASE HERE DISCRIMINATES. An implementation that adds the component
-- unconditionally passes the one-symbol case and fails the zero-symbol one; one
-- that adds it exactly once for a black spell passes both and fails the
-- two-symbol case; one that saturates at two fails only Stalker Hag's three. The
-- ladder 0, 1, 2, 3 is the proof, and the counting RULE is what Dismember and
-- the Hag add on top of it.
droughtSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
droughtSpec s registry = Spec.describe s "Drought" $ do
  -- ONE black mana symbol, so one Swamp. Doom Blade is {1}{B}, and the generic
  -- half is what shows the count is over SYMBOLS OF A COLOUR and not over the
  -- cost's size.
  Spec.it s "CR 601.2f a spell with one black symbol costs a Swamp to cast" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    blade <- S.printingOf s registry "Doom Blade"
    let (taxed, taxedId) = droughtBoard swamp piker blade (Just drought) 5
        (free, freeId) = droughtBoard swamp piker blade Nothing 5
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
        control = S.runPure S.identityAnswer free (S.cast S.alice freeId)
    Spec.assertEqWith s "one of the five Swamps was sacrificed" (S.countOnBattlefieldByName swampName S.alice after) 4
    Spec.assertEqWith s "where the same cast without Drought keeps all five" (S.countOnBattlefieldByName swampName S.alice control) 5
    Spec.assertEqWith s "and the Blade is on the stack, not refused" (length (GameState.stack after)) 1
  -- ZERO black mana symbols, so nothing at all -- not a Sacrifice component of
  -- count zero. Bonesplitter is {1}, and the board is carried far enough that a
  -- component added regardless of the count would have taken a Swamp.
  Spec.it s "CR 601.2f a spell with no black symbol sacrifices nothing" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    splitter <- S.printingOf s registry "Bonesplitter"
    let (taxed, taxedId) = droughtBoard swamp piker splitter (Just drought) 5
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
    Spec.assertEqWith s "all five Swamps survive" (S.countOnBattlefieldByName swampName S.alice after) 5
    Spec.assertEqWith s "and the Bonesplitter is on the stack" (length (GameState.stack after)) 1
  -- TWO black mana symbols, so two Swamps: the multiplier, which the one-symbol
  -- case above cannot tell from "add it once". Sign in Blood is {B}{B}.
  Spec.it s "CR 601.2f two black symbols cost two Swamps" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    sign <- S.printingOf s registry "Sign in Blood"
    let (taxed, taxedId) = droughtBoard swamp piker sign (Just drought) 5
        (free, freeId) = droughtBoard swamp piker sign Nothing 5
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
        control = S.runPure S.identityAnswer free (S.cast S.alice freeId)
    Spec.assertEqWith s "two of the five Swamps were sacrificed" (S.countOnBattlefieldByName swampName S.alice after) 3
    Spec.assertEqWith s "where the same cast without Drought keeps all five" (S.countOnBattlefieldByName swampName S.alice control) 5
    Spec.assertEqWith s "and Sign in Blood is on the stack" (length (GameState.stack after)) 1
  -- CR 107.4f: "Phyrexian mana symbols are colored mana symbols ... {B/P} is
  -- black", so Dismember's {1}{B/P}{B/P} holds two BLACK mana symbols and
  -- demands two Swamps -- where an implementation counting only CR 107.4a's
  -- five primary symbols reads it as zero.
  Spec.it s "CR 107.4f two Phyrexian black symbols cost two Swamps too" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    dismember <- S.printingOf s registry "Dismember"
    let (taxed, taxedId) = droughtBoard swamp piker dismember (Just drought) 5
        (free, freeId) = droughtBoard swamp piker dismember Nothing 5
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
        control = S.runPure S.identityAnswer free (S.cast S.alice freeId)
    Spec.assertEqWith s "two of the five Swamps were sacrificed" (S.countOnBattlefieldByName swampName S.alice after) 3
    Spec.assertEqWith s "where the same cast without Drought keeps all five" (S.countOnBattlefieldByName swampName S.alice control) 5
    Spec.assertEqWith s "and Dismember is on the stack" (length (GameState.stack after)) 1
  -- Drought's own 2008-08-01 ruling, on the symbols it was written about: "A
  -- hybrid symbol that is both black and another type is a black mana symbol,
  -- regardless of what cost is paid for it." Stalker Hag is {B/G}{B/G}{B/G}, so
  -- it is THREE black mana symbols -- and the ruling's last clause is what the
  -- board pins: every one of them is paid with black mana here (alice has only
  -- Swamps), and the count would be the same off Forests, because CR 107.4e
  -- makes a hybrid symbol all of its component colours whatever pays it.
  --
  -- THREE, so this is also the multiplier past two: a scale that saturated at
  -- one or two would leave a Swamp standing.
  Spec.it s "CR 107.4e three black hybrid symbols cost three Swamps" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    hag <- S.printingOf s registry "Stalker Hag"
    let (taxed, taxedId) = droughtBoard swamp piker hag (Just drought) 6
        (free, freeId) = droughtBoard swamp piker hag Nothing 6
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
        control = S.runPure S.identityAnswer free (S.cast S.alice freeId)
    Spec.assertEqWith s "three of the six Swamps were sacrificed" (S.countOnBattlefieldByName swampName S.alice after) 3
    Spec.assertEqWith s "where the same cast without Drought keeps all six" (S.countOnBattlefieldByName swampName S.alice control) 6
    Spec.assertEqWith s "and the Hag is on the stack" (length (GameState.stack after)) 1

-- CR 115.6's "up to one target", read at cast time. Rat Out {B} Instant is "Up
-- to one target creature gets -1/-1 until end of turn. You create a 1/1 black
-- Rat creature token ...", and Dismember is the falsifier: {1}{B/P}{B/P} "Target
-- creature gets -5/-5 until end of turn", the same clause with the slot
-- REQUIRED. One creatureless board tells the two apart, and the same board with
-- a creature on it is what proves the mana was never the reason.
upToOneTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
upToOneTargetSpec s registry = Spec.describe s "UpToOneTargetCast" $ do
  Spec.it s "CR 115.6 a slot that may be left empty does not gate castability" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ratOut <- S.printingOf s registry "Rat Out"
    dismember <- S.printingOf s registry "Dismember"
    -- One base board and one card apiece: S.handOne replaces the hand, so the
    -- two spells cannot sit in it together.
    let base = S.landsInPlay swamp 3
        (bareRat, rat) = S.handOne ratOut base
        (bareCut, cut) = S.handOne dismember base
        (_, peopledCut) = S.addCreature piker S.bob bareCut
    Spec.assertBool s (S.castable S.alice rat bareRat) "up to one target: castable with no creature"
    Spec.assertBool s (not (S.castable S.alice cut bareCut)) "one required target: not castable"
    Spec.assertBool s (S.castable S.alice cut peopledCut) "and castable once a creature exists, so the mana was fine"
  -- CR 601.2c's count announcement is a real choice when the slot has a
  -- candidate, and no question at all when it has none.
  Spec.it s "CR 601.2c the number of targets is announced only when there is one to choose" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ratOut <- S.printingOf s registry "Rat Out"
    let (bare, spellId) = S.handOne ratOut (S.landsInPlay swamp 1)
        (_, peopled) = S.addCreature piker S.bob bare
        announced gs = any isAnnouncement (snd (Replay.record S.identityAnswer gs (S.cast S.alice spellId)))
        isAnnouncement r = case r of
          Response.AnnouncedTargets _ -> True
          _ -> False
    Spec.assertBool s (not (announced bare)) "no candidate, no question"
    Spec.assertBool s (announced peopled) "a candidate, so the caster is asked"

-- CR 601.2c's count above one, read at cast time. Hearts on Fire {1}{R} Instant
-- is "One or two target creatures each get +2/+1 until end of turn": the number
-- is a real question with two creatures to choose between and no question at all
-- with one, because "in some cases, the number of targets will be defined by the
-- spell's text" -- and here the board defines it just as firmly.
multiTargetCastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
multiTargetCastSpec s registry = Spec.describe s "MultiTargetCast" $ do
  Spec.it s "CR 601.2c one candidate fixes the number, so nothing is announced" $ do
    (one_, _, spellId) <- heartsBoards s registry
    Spec.assertEqWith s "nothing asked" (announcedCounts spellId one_) []
  Spec.it s "CR 601.2c a second candidate makes the number a question" $ do
    (_, two, spellId) <- heartsBoards s registry
    Spec.assertEqWith s "asked, and answered here with the maximum" (announcedCounts spellId two) [2]

-- One Hearts on Fire in alice's hand over two Mountains, on two boards that
-- differ only in whether bob has a second creature.
heartsBoards :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, GameState.GameState, ObjectId.ObjectId)
heartsBoards s registry = do
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  hearts <- S.printingOf s registry "Hearts on Fire"
  let (one_, spellId) = S.handOne hearts (snd (S.addCreature piker S.bob (S.landsInPlay mountain 2)))
      (_, two) = S.addCreature rats S.bob one_
  pure (one_, two, spellId)

-- Every number CR 601.2c's announcement carried while casting this spell.
announcedCounts :: ObjectId.ObjectId -> GameState.GameState -> [Natural]
announcedCounts spellId gs =
  [ n
  | Response.AnnouncedTargets counts <- snd (Replay.record S.identityAnswer gs (S.cast S.alice spellId)),
    n <- Map.elems counts
  ]

-- CR 702.127a, aftermath, all three of the static abilities the one word stands
-- for -- on Onward // Victory, where the keyword is printed on the RIGHT half
-- only. That asymmetry is the point: every assertion here would pass vacuously on
-- a card whose halves agreed.
-- A board that can pay for EITHER half and has something for either to target:
-- four Mountains for Onward's {2}{R}, four Plains for Victory's {2}{W}, and a
-- Goblin Piker.
--
-- Both colours on purpose. With Mountains alone every "Victory is not castable"
-- assertion below would hold because {W} could not be paid, which is the vacuous
-- pass a cast gate invites -- the negatives have to fail on rule 702.127a and
-- nothing else.
aftermathBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
aftermathBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  let withPlains = foldr (\_ g -> snd (S.addCreature plains S.alice g)) (S.landsInPlay mountain 4) [1 :: Int .. 4]
  pure (snd (S.addCreature piker S.alice withPlains))

aftermathSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aftermathSpec s registry = Spec.describe s "Aftermath" $ do
  -- Rule 702.127a's SECOND ability: "this half of this split card can't be cast
  -- from any zone other than a graveyard". A hand is where every other card in
  -- the pool is castable from, so this is the prohibition doing real work -- and
  -- "this HALF" is why Onward, off the same card in the same hand, still is.
  Spec.it s "CR 702.127a an aftermath half can't be cast from a hand, and its sibling can" $ do
    onwardVictory <- S.printingOf s registry "Onward"
    board <- aftermathBoard s registry
    let (gs, oid) = S.handOne onwardVictory board
    Spec.assertEqWith s "Onward is castable from the hand" (Cast.castable S.alice oid onwardName Facing.FaceUp gs) True
    Spec.assertEqWith s "Victory is not" (Cast.castable S.alice oid victoryName Facing.FaceUp gs) False
  -- Rule 702.127a's FIRST ability: "you may cast this half of this split card from
  -- your graveyard". The mirror of the case above, and the falsifier for a
  -- prohibition that swallowed the permission too.
  Spec.it s "CR 702.127a an aftermath half can be cast from a graveyard, and its sibling cannot" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    onwardVictory <- S.printingOf s registry "Onward"
    board <- aftermathBoard s registry
    -- A main phase with priority: Victory is a SORCERY (CR 307.1), so without it
    -- this would fail on timing rather than on rule 702.127a.
    let (gs0, _) = S.handOne piker board
        (oid, gs) = S.addGraveyardCard onwardVictory S.alice gs0
    Spec.assertEqWith s "Victory is castable from the graveyard" (Cast.castable S.alice oid victoryName Facing.FaceUp gs) True
    -- Onward has no permission of its own, so the graveyard is closed to it --
    -- CR 702.127a grants the half that PRINTS aftermath, not the card.
    Spec.assertEqWith s "Onward is not" (Cast.castable S.alice oid onwardName Facing.FaceUp gs) False

-- Soul Immolation {3}{R}{R} Sorcery (data/cards/soul-immolation.json): "As an
-- additional cost to cast this spell, blight X. X can't be greater than the
-- greatest toughness among creatures you control. Soul Immolation deals X damage
-- to each opponent and each creature they control." Name, cost, type line and
-- oracle text checked against Scryfall 2026-08-20.
--
-- The pool's card for CR 101.1 read against CR 601.2b: the card's own sentence
-- overrides the rule that would otherwise leave the announced X free, and CR
-- 101.2 fixes the direction, the sentence being a "can't". CR 107.3a is only
-- where the announcement happens.
--
-- THREE SEATS, because "each opponent" and "each player" name the same set on a
-- two-seat board once alice is excluded from neither -- carol is what makes the
-- difference between reaching every opponent and reaching one observable.
--
-- The board: alice has five Mountains (exactly {3}{R}{R}, so nothing below turns
-- on mana), a Goblin Piker (2/1) and a Palace Guard (1/4); bob has Russet Wolves
-- (3/3); carol has nothing but a life total. The ceiling is therefore 4, and the
-- Piker is deliberately the LOWER-numbered object: a "least toughness" reading
-- and a "first creature" reading both answer 1, so both refuse an X of 4 that
-- CR 101.1 permits.
--
-- Nothing here turns on affordability, and that is the point: {3}{R}{R} carries
-- no {X}, so every X is equally payable and the ceiling is the only thing that
-- can refuse one.
soulImmolationBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
soulImmolationBoard mountain piker guard wolves immolation =
  let withLands = S.landsFor mountain S.alice 5 S.threePlayerGame
      (pikerId, withPiker) = S.addCreature piker S.alice withLands
      (guardId, withGuard) = S.addCreature guard S.alice withPiker
      (wolvesId, withWolves) = S.addCreature wolves S.bob withGuard
      (spellId, withSpell) = S.addHandCard immolation S.alice withWolves
   in ( spellId,
        pikerId,
        guardId,
        wolvesId,
        withSpell
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Announces this X, and pays the blight onto the Palace Guard -- alice controls
-- two creatures, so CR 701.68a's choice is a real prompt and pinning it keeps
-- the counters off the creature the ceiling is read from being an accident.
answerSoulImmolation :: ObjectId.ObjectId -> Natural -> Prompt.Prompt r -> r
answerSoulImmolation guardId n p = case p of
  Prompt.ChooseX {} -> n
  Prompt.ChooseBlight {} -> guardId
  _ -> S.identityAnswer p

soulImmolationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulImmolationSpec s registry = Spec.describe s "Soul Immolation" $ do
  -- The PROVING case. Five is one more than the greatest toughness among
  -- alice's creatures, so CR 101.1 refuses the announcement and CR 601.2
  -- returns the game to before the casting was proposed. An engine that
  -- honoured the answer would deal five to each opponent.
  Spec.it s "CR 101.1 an X above the card's stated maximum reverses the cast" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    guard <- S.printingOf s registry "Palace Guard"
    wolves <- S.printingOf s registry "Russet Wolves"
    immolation <- S.printingOf s registry "Soul Immolation"
    let (spellId, _, guardId, wolvesId, board) = soulImmolationBoard mountain piker guard wolves immolation
        after = S.runPure (answerSoulImmolation guardId 5) board (do S.cast S.alice spellId; Stack.resolveTop)
    Spec.assertEqWith s "bob took nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "carol took nothing" (S.lifeOf S.carol after) (Just 20)
    Spec.assertEqWith s "and bob's Wolves took nothing" (S.damageOf wolvesId after) (Just 0)
    -- CR 601.2e's rewind reaches the additional cost as well as the damage.
    Spec.assertEqWith s "no blight counters were paid" (S.counterOf CounterKind.MinusOneMinusOne guardId after) 0
    Spec.assertEqWith s "and the card is still in alice's hand" (S.handSize S.alice after) 1
  -- The CONTROL, and the same board with one thing changed: the answer. Four IS
  -- the greatest toughness among alice's creatures, so CR 101.1 permits it and
  -- everything the case above found missing happens.
  Spec.it s "CR 101.1 an X equal to the stated maximum is announced and paid" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    guard <- S.printingOf s registry "Palace Guard"
    wolves <- S.printingOf s registry "Russet Wolves"
    immolation <- S.printingOf s registry "Soul Immolation"
    let (spellId, _, guardId, wolvesId, board) = soulImmolationBoard mountain piker guard wolves immolation
        after = S.runPure (answerSoulImmolation guardId 4) board (do S.cast S.alice spellId; Stack.resolveTop)
    Spec.assertEqWith s "bob took four" (S.lifeOf S.bob after) (Just 16)
    Spec.assertEqWith s "carol took four" (S.lifeOf S.carol after) (Just 16)
    Spec.assertEqWith s "and bob's Wolves took four" (S.damageOf wolvesId after) (Just 4)
    Spec.assertEqWith s "the blight put four counters on the Palace Guard" (S.counterOf CounterKind.MinusOneMinusOne guardId after) 4
    Spec.assertEqWith s "and the card left alice's hand" (S.handSize S.alice after) 0
  -- Gameplay level, under the ceiling on both sides, so nothing here is about
  -- the ceiling: what it proves is that CR 601.2b's announced X is the number
  -- the blight charges and the number the damage deals, and that "each opponent
  -- and each creature they control" reaches neither alice nor her creatures.
  Spec.it s "CR 107.3a the announced X is blighted, dealt to each opponent, and dealt to their creatures" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    guard <- S.printingOf s registry "Palace Guard"
    wolves <- S.printingOf s registry "Russet Wolves"
    immolation <- S.printingOf s registry "Soul Immolation"
    let (spellId, pikerId, guardId, wolvesId, board) = soulImmolationBoard mountain piker guard wolves immolation
        after = S.runPure (answerSoulImmolation guardId 2) board (do S.cast S.alice spellId; Stack.resolveTop)
    Spec.assertEqWith s "bob took two" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "carol took two" (S.lifeOf S.carol after) (Just 18)
    -- CR 109.5: alice is not her own opponent, and neither of her creatures is
    -- one an opponent controls. An ObjectRef.EachPlayer in either instruction's
    -- place would have taken two from her and two from each of them.
    Spec.assertEqWith s "alice took nothing" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "her Palace Guard took no damage" (S.damageOf guardId after) (Just 0)
    Spec.assertEqWith s "her Goblin Piker took no damage" (S.damageOf pikerId after) (Just 0)
    Spec.assertEqWith s "bob's Wolves took two" (S.damageOf wolvesId after) (Just 2)
    -- CR 601.2f/601.2h: the additional cost was paid with the same X, on the
    -- creature the prompt was answered with rather than on the first candidate.
    Spec.assertEqWith s "two -1/-1 counters on the Palace Guard" (S.counterOf CounterKind.MinusOneMinusOne guardId after) 2
    Spec.assertEqWith s "and none on the Goblin Piker" (S.counterOf CounterKind.MinusOneMinusOne pikerId after) 0

-- Drannith Magistrate {1}{W} Creature -- Human Wizard 1/3 (IKO 12): "Your
-- opponents can't cast spells from anywhere other than their hands." CR 601.3's
-- prohibit half scoped to a ZONE, which PlayerEffect.CantCastMatching's Filter
-- states through Filter.IsInZone.
--
-- "From anywhere other than their hands" is `Not (IsInZone Hand)`, read at the
-- cast gate against the card WHERE IT LIES: CR 601.2 casts a spell "from where it
-- is", and Pawl.Engine.Cast.castable runs before CR 601.2a moves the card to the
-- stack.
--
-- "THEIR hands" is the caster's own, which is Hand simply: pawl's hand is indexed
-- by owner (Pawl.Engine.Game.zoneMembers, CR 108.3) and Pawl.Engine.Cast's
-- zoneCandidates offers a player only their own, so no cast from another player's
-- hand exists to tell the two readings apart. Not implemented: the possessive,
-- which wants an OwnedBy conjunct beside the zone atom and a card granting such a
-- cast (#2169).
--
-- Think Twice {1}{U} Instant "Draw a card." / "Flashback {2}{U}" is the spell,
-- and an INSTANT so that neither seat's cast turns on whose turn it is. SIX
-- Islands per seat, which is what keeps the mana from being the reason any cast
-- is refused: three pay the flashback {2}{U} and three more are left over, so the
-- graveyard copy stays affordable after a seat has already cast the hand one --
-- without the spare the last case's negative would hold for want of mana.
drannithMagistrateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
drannithMagistrateSpec s registry = Spec.describe s "Drannith Magistrate" $ do
  -- THE PROVING CASE, and it needs all three readings at once: the same spell is
  -- refused to bob from his graveyard, allowed to him from his hand, and allowed
  -- to alice from her graveyard. An implementation that prohibited every cast
  -- passes the first assertion alone; one that ignored the zone passes the third
  -- alone.
  Spec.it s "CR 601.3 an opponent's cast from a graveyard is refused, from a hand is not, and the controller's own graveyard cast stands" $ do
    magistrate <- S.printingOf s registry "Drannith Magistrate"
    island <- S.printingOf s registry "Island"
    thinkTwice <- S.printingOf s registry "Think Twice"
    let (bobGrave, bobHand, aliceGrave, carolGrave, open) = drannithBoard island thinkTwice
        board = withMagistrate magistrate open
        casting pid oid = elem (A.Cast oid (S.printingName thinkTwice) Facing.FaceUp) (offeredTo pid board)
    Spec.assertBool s (not (casting S.bob bobGrave)) "bob may not flash it back: the graveyard is not his hand"
    Spec.assertBool s (casting S.bob bobHand) "but the same card in his hand is still castable, off the same Islands"
    Spec.assertBool s (casting S.alice aliceGrave) "and alice, who controls the Magistrate, is no opponent of her own"
    Spec.assertBool s (not (casting S.carol carolGrave)) "while carol, the third seat, is prohibited exactly as bob is"
  -- The paired board, differing in exactly one permanent: without the Magistrate
  -- bob's flashback is offered, so neither his mana nor the timing was ever the
  -- reason it was refused above.
  Spec.it s "CR 601.3 the pair: with no Magistrate on the battlefield the same flashback is offered" $ do
    magistrate <- S.printingOf s registry "Drannith Magistrate"
    island <- S.printingOf s registry "Island"
    thinkTwice <- S.printingOf s registry "Think Twice"
    let (bobGrave, _, _, _, open) = drannithBoard island thinkTwice
        board = withMagistrate magistrate open
        offered gs = elem (A.Cast bobGrave (S.printingName thinkTwice) Facing.FaceUp) (offeredTo S.bob gs)
    Spec.assertBool s (offered open) "no Magistrate, so the flashback is legal"
    Spec.assertBool s (not (offered board)) "and the one permanent is the whole difference"
  -- The prohibition is a LIVE read rather than a one-shot: bob takes the cast he
  -- is allowed, and the one he is not is still refused with his own spell on the
  -- stack. The first assertion is a control and not a gate -- S.cast calls
  -- Pawl.Engine.Cast.castSpell, which performs the cast rather than asking
  -- `castable` -- so the second is what this case proves.
  Spec.it s "CR 601.3 the prohibition still stands while the opponent's own hand cast is on the stack" $ do
    magistrate <- S.printingOf s registry "Drannith Magistrate"
    island <- S.printingOf s registry "Island"
    thinkTwice <- S.printingOf s registry "Think Twice"
    let (bobGrave, bobHand, _, _, open) = drannithBoard island thinkTwice
        board = withMagistrate magistrate open
        cast = S.runPure S.identityAnswer board (S.cast S.bob bobHand)
    Spec.assertEqWith s "the hand cast is on the stack" (length (GameState.stack cast)) 1
    Spec.assertBool s (not (S.castable S.bob bobGrave cast)) "and the graveyard copy is still refused with it there"

-- Teferi, Mage of Zhalfir {2}{U}{U}{U} Legendary Creature -- Human Wizard 3/4:
-- "Flash / Creature cards you own that aren't on the battlefield have flash. /
-- Each opponent can cast spells only any time they could cast a sorcery." The
-- THIRD clause, which PlayerEffect.CastOnlyAtSorcerySpeed states and
-- Pawl.Engine.PlayerEffect.prohibitsCasting reads. The first two are flashSpec's
-- two Teferi cases above.
--
-- CR 307.5 defines the printed phrase, and as three conjuncts rather than as "a
-- sorcery could be cast": priority, a main phase of the player's own turn, and
-- an empty stack. That is Turn.sorcerySpeedWindow verbatim, which is why no
-- fourth copy of the window is written here.
--
-- Lightning Bolt {R} Instant is the spell, and an INSTANT deliberately: CR
-- 117.1a hands an instant every priority its controller has, so a wrong engine
-- offers it on alice's turn, where a creature card would be refused by CR 302.1
-- and the board could not tell Teferi from the rules.
--
-- THREE seats, so PlayerScope.Opponents is not a two-seat coincidence and a fix
-- that hardcoded "the player whose turn it isn't" fails carol. ONE Mountain and
-- ONE Bolt per seat, alice included: she controls Teferi and is no opponent of
-- her own, so her copy is what fails an over-broad fix, and the identical mana
-- is what keeps affordability from being any seat's reason.
teferiSorcerySpeedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
teferiSorcerySpeedSpec s registry = Spec.describe s "Teferi, Mage of Zhalfir" $ do
  -- THE PROVING CASE. The two refusals come first, because every assertion after
  -- them holds on today's engine too and would absorb a mutation.
  Spec.it s "CR 307.5 an opponent's instant is refused in alice's main phase, offered in his own, and alice's own is untouched" $ do
    teferi <- S.printingOf s registry "Teferi, Mage of Zhalfir"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (aliceBolt, bobBolt, carolBolt, open) = teferiSorceryBoard mountain bolt
        board = addTeferi teferi open
        bobsTurn gs = gs {GameState.activePlayer = S.bob}
        busy gs = gs {GameState.stack = [ObjectId.MkObjectId 999]}
        casting pid oid gs = elem (A.Cast oid (S.printingName bolt) Facing.FaceUp) (offeredTo pid gs)
    Spec.assertBool s (not (casting S.bob bobBolt board)) "bob may not cast an instant in alice's main phase"
    Spec.assertBool s (not (casting S.carol carolBolt board)) "and neither may carol, the third seat"
    Spec.assertBool s (casting S.alice aliceBolt board) "while alice, who controls Teferi, is no opponent of her own"
    -- The SAME control at a moment alice is outside CR 307.5's window, which is
    -- what tells PlayerScope.Opponents from PlayerScope.EachPlayer: on bob's turn
    -- her own instant is still hers to cast, and an EachPlayer carrier would take
    -- it away.
    Spec.assertBool s (casting S.alice aliceBolt (bobsTurn board)) "and still is on bob's turn, where the clause would bite if it reached her"
    Spec.assertBool s (casting S.bob bobBolt (bobsTurn board)) "and bob's own main phase with an empty stack still admits it"
    -- CR 307.5's third conjunct, which the two boards above share and so cannot
    -- separate: his own main phase is not enough while something is on the stack.
    Spec.assertBool s (not (casting S.bob bobBolt (busy (bobsTurn board)))) "not even on his own turn with a spell already on the stack"
  -- The paired board, differing in exactly one permanent: without Teferi bob's
  -- Bolt is offered off the same Mountain at the same moment, so neither his mana
  -- nor CR 117.1a was ever the reason it was refused above.
  Spec.it s "CR 307.5 the pair: with no Teferi on the battlefield the same instant is offered" $ do
    teferi <- S.printingOf s registry "Teferi, Mage of Zhalfir"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, bobBolt, _, open) = teferiSorceryBoard mountain bolt
        offered gs = elem (A.Cast bobBolt (S.printingName bolt) Facing.FaceUp) (offeredTo S.bob gs)
    Spec.assertBool s (offered open) "no Teferi, so the instant is legal"
    Spec.assertBool s (not (offered (addTeferi teferi open))) "and the one permanent is the whole difference"
  -- CR 305.1: the clause narrows CASTING, and playing a land is a special action
  -- that never uses the stack, so Teferi stops no land. bob's own turn is the
  -- moment that shows it -- CR 305.3 forbids a land play on anyone else's turn,
  -- so a board on alice's turn could not tell the two readings apart -- and this
  -- is what gives prohibitsPlayingLand's arm an observer.
  Spec.it s "CR 305.1 the clause stops no land play" $ do
    teferi <- S.printingOf s registry "Teferi, Mage of Zhalfir"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, _, _, open) = teferiSorceryBoard mountain bolt
        (bobLand, stocked) = S.addHandCard mountain S.bob open
        board = (addTeferi teferi stocked) {GameState.activePlayer = S.bob}
    Spec.assertBool s (elem (A.Play bobLand Nothing) (offeredTo S.bob board)) "bob may still play a land with Teferi standing"
  -- THE OTHER PATH, and the reason the clause is read in prohibitsCasting rather
  -- than conjoined onto Cast.timingOk: Cast.castableWhileSearching asks
  -- castableWhenOffered, which omits timingOk on purpose (CR 601.3's timing limb
  -- is what Panglacial Wurm's permission excepts) and keeps the prohibit limb.
  -- A timingOk conjunct would leave this offer standing.
  --
  -- THREE boards over one Wurm and one library: bob mid-search on alice's turn,
  -- the same without Teferi, and the same on bob's own turn. The last is what
  -- says the arm answers by CR 307.5's window rather than refusing every offer.
  Spec.it s "CR 307.5 the offered path: an opponent's mid-search cast is refused too" $ do
    teferi <- S.printingOf s registry "Teferi, Mage of Zhalfir"
    forest <- S.printingOf s registry "Forest"
    wurm <- S.printingOf s registry "Panglacial Wurm"
    let open = S.landsFor forest S.bob 7 (aliceOnTurn S.threePlayerGame)
        stocked = snd (S.addLibraryCard wurm S.bob open)
        board = addTeferi teferi stocked
        offers gs = length (Cast.castableWhileSearching S.bob gs)
    Spec.assertEqWith s "bob's mid-search cast is refused on alice's turn" (offers board) 0
    Spec.assertEqWith s "without Teferi the same offer stands, off the same seven Forests" (offers stocked) 1
    Spec.assertEqWith s "and with Teferi it stands again once it is bob's own turn" (offers (board {GameState.activePlayer = S.bob})) 1

-- Three seats, one untapped Mountain and one Lightning Bolt in hand each, alice
-- on her own turn in her precombat main phase with an empty stack. No Teferi yet
-- -- `addTeferi` adds him, so the two boards differ in exactly that permanent.
teferiSorceryBoard ::
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
teferiSorceryBoard mountain bolt =
  let gs1 = S.landsFor mountain S.alice 1 S.threePlayerGame
      gs2 = S.landsFor mountain S.bob 1 gs1
      gs3 = S.landsFor mountain S.carol 1 gs2
      (aliceBolt, gs4) = S.addHandCard bolt S.alice gs3
      (bobBolt, gs5) = S.addHandCard bolt S.bob gs4
      (carolBolt, gs6) = S.addHandCard bolt S.carol gs5
   in (aliceBolt, bobBolt, carolBolt, aliceOnTurn gs6)

addTeferi :: Printing.Printing -> GameState.GameState -> GameState.GameState
addTeferi teferi gs = snd (S.addCreature teferi S.alice gs)

-- Grafdigger's Cage {1} Artifact (DKA 150): "Creature cards in graveyards and
-- libraries can't enter the battlefield. / Players can't cast spells from
-- graveyards or libraries." The SECOND sentence, which Filter.IsInZone is what
-- makes writable -- the first is Pawl.EntryRestrictionSpec's.
--
-- The pair with Drannith Magistrate above is the point of putting it here: the
-- two sentences differ in the SCOPE and in nothing else, so one board tells
-- PlayerScope.EachPlayer from PlayerScope.Opponents. "Players" includes the Cage's
-- own controller (CR 109.5 has no "you" in that sentence to exclude her).
--
-- The LIBRARY half has its own case below, off Panglacial Wurm's mid-search
-- offer (Pawl.Engine.Cast.castableWhileSearching). The other road into a library
-- cast -- Garruk's Horde's standing permission -- is refused by the same
-- disjunct, proved in Pawl.PlayerEffectSpec's GarruksHorde group. Or is written
-- for both zones because the printed sentence names both, and each disjunct is
-- proved on its own board.
grafdiggersCageCastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
grafdiggersCageCastSpec s registry = Spec.describe s "Grafdigger's Cage" $ do
  Spec.it s "CR 601.3 no player may cast from a graveyard, the Cage's own controller included, and a hand cast is untouched" $ do
    cage <- S.printingOf s registry "Grafdigger's Cage"
    magistrate <- S.printingOf s registry "Drannith Magistrate"
    island <- S.printingOf s registry "Island"
    thinkTwice <- S.printingOf s registry "Think Twice"
    let (bobGrave, bobHand, aliceGrave, _, open) = drannithBoard island thinkTwice
        board = snd (S.addCreature cage S.alice open)
        casting pid oid gs = elem (A.Cast oid (S.printingName thinkTwice) Facing.FaceUp) (offeredTo pid gs)
    Spec.assertBool s (not (casting S.alice aliceGrave board)) "alice controls the Cage and is prohibited by it anyway"
    Spec.assertBool s (not (casting S.bob bobGrave board)) "and so is bob, on the same board"
    Spec.assertBool s (casting S.bob bobHand board) "while the hand cast the sentence does not name is still offered"
    Spec.assertBool s (casting S.alice aliceGrave open) "the pair: with no Cage on the battlefield alice's flashback is legal"
    -- The scope is what separates the two cards: Drannith Magistrate's
    -- PlayerScope.Opponents leaves its controller alone where this leaves nobody
    -- alone, on the same board and the same graveyard cast.
    Spec.assertBool s (casting S.alice aliceGrave (withMagistrate magistrate open)) "and the Magistrate's PlayerScope.Opponents, on the same board, spares her"
  -- The Or's second disjunct on the mid-search road, where the Cage is the one
  -- permanent between the two readings. The standing top-of-library permission
  -- is the same disjunct's other road, in Pawl.PlayerEffectSpec.
  Spec.it s "CR 601.3 the library disjunct: a mid-search cast is refused too" $ do
    cage <- S.printingOf s registry "Grafdigger's Cage"
    forest <- S.printingOf s registry "Forest"
    wurm <- S.printingOf s registry "Panglacial Wurm"
    let (_, open) = S.addLibraryCard wurm S.alice (S.landsInPlay forest 7)
        caged = snd (S.addCreature cage S.alice open)
    Spec.assertEqWith s "without the Cage the mid-search cast is offered" (length (Cast.castableWhileSearching S.alice open)) 1
    Spec.assertEqWith s "with it the same offer is gone, off the same seven Forests" (length (Cast.castableWhileSearching S.alice caged)) 0

-- Three seats, six Islands each, and one Think Twice in each of alice's, bob's and
-- carol's graveyards plus one in bob's hand. No Magistrate yet --
-- `withMagistrate` adds it, so the two boards differ in exactly that permanent.
drannithBoard ::
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
drannithBoard island thinkTwice =
  let gs1 = S.landsFor island S.alice 6 S.threePlayerGame
      gs2 = S.landsFor island S.bob 6 gs1
      gs3 = S.landsFor island S.carol 6 gs2
      (bobGrave, gs4) = S.addGraveyardCard thinkTwice S.bob gs3
      (bobHand, gs5) = S.addHandCard thinkTwice S.bob gs4
      (aliceGrave, gs6) = S.addGraveyardCard thinkTwice S.alice gs5
      (carolGrave, gs7) = S.addGraveyardCard thinkTwice S.carol gs6
   in (bobGrave, bobHand, aliceGrave, carolGrave, aliceOnTurn gs7)

withMagistrate :: Printing.Printing -> GameState.GameState -> GameState.GameState
withMagistrate magistrate gs = snd (S.addCreature magistrate S.alice gs)

-- What this player is offered with priority in hand, the shape
-- Pawl.SpecialActionSpec's Damping Engine cases use: legalActions answers for the
-- player holding priority, and every seat here is asked the same question.
offeredTo :: PlayerId.PlayerId -> GameState.GameState -> [A.Action]
offeredTo pid gs = Action.legalActions pid (gs {GameState.priority = Just pid})

-- Aven Interrupter (OTJ 5) {1}{W}{W} Creature -- Bird Rogue 2/2, "Flash /
-- Flying / When this creature enters, exile target spell. It becomes plotted.
-- (Its owner may cast it as a sorcery on a later turn without paying its mana
-- cost.) / Spells your opponents cast from graveyards or from exile cost {2}
-- more to cast."
--
-- The third sentence is Filter.WasCastFrom, one atom under an Or, and it is
-- deliberately not Filter.IsInZone: that atom reads the zone the object is in
-- NOW, which CR 601.2a has made the stack by the time CR 601.2f prices the
-- spell, so a card written with it would have a gate and a payment that
-- disagree; see #2363. The two cases below assert the OFFER and the mana TAPPED on
-- one board for exactly that reason.
--
-- The first sentence is the effect DSL's road out of the stack into exile: a
-- Pool.Spells target slot feeding Effect.MoveToZone with a zone of Exile. The two nearest things
-- data/cards had before it reach neither end of that -- reprieve.json is the
-- same slot into Hand, and CR 724.1b's Time Stop sweep reaches exile with no
-- slot and no target. The group is here rather than beside Kellan Joins Up's
-- plot cases because what it proves is a SPELL leaving the stack; the plot stamp
-- rides along on the same MoveToZone.
--
-- Each seat gets three basics of its OWN colour, so neither seat's mana pays for
-- the other's spell and bob's three are all tapped by the victim he casts --
-- which is what makes the cast from exile below discriminating. `answer` is
-- alice's card in hand: the Bird in the cases about exiling, and a Cancel in the
-- controls that show the same board countering.
avenBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
avenBoard aliceLand bobLand answer victim =
  let (gs0, answerId) = S.handOne answer (S.landsFor bobLand S.bob 3 (S.landsInPlay aliceLand 3))
      (victimId, gs1) = S.addHandCard victim S.bob gs0
   in (answerId, victimId, gs1)

-- Aims every target choice at the named object and passes on everything else.
-- The aim is PINNED though the victim spell is the stack's only spell when the
-- trigger goes on it: an answerer that searched for a legal recipient would find
-- it again after a mutation, which is the repair Pawl.Support's default answerer
-- would silently make.
avenAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
avenAnswer victimId prompt = case prompt of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just victimId) . Recipient.objectOf) sets
  _ -> S.identityAnswer prompt

-- avenAnswer, plus CR 603.5's "may" taken: Baral's trigger draws only if its
-- optional is exercised, and Pawl.Support's default answerer declines every one.
-- The Bird's own trigger is mandatory, so this changes nothing on its board.
baralAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
baralAnswer victimId prompt = case prompt of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> avenAnswer victimId prompt

-- The cards of one name a player owns in one zone, by name rather than by id:
-- CR 400.7 mints a new incarnation on every move, so the id the spell had on the
-- stack names nothing in exile.
avenNamed :: Printing.Printing -> Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> Int
avenNamed printing zone pid gs =
  length (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName printing)) (Game.zoneMembers zone pid gs))

-- Take every permanent of that name off the battlefield without routing it
-- anywhere, so a case can price the same cast with and without the Bird's static
-- ability gathered. Not a zone change: nothing here is a CR 400.7 move, and no
-- case using it reads a graveyard.
--
-- BY NAME rather than by id, for `avenNamed` above's reason: the incarnation that
-- reaches the battlefield is not the one the hand held.
avenRemoved :: Printing.Printing -> GameState.GameState -> GameState.GameState
avenRemoved printing gs =
  let gone = filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName printing)) (Set.toList (GameState.battlefield gs))
   in gs
        { GameState.battlefield = foldr Set.delete (GameState.battlefield gs) gone,
          GameState.objects = foldr Map.delete (GameState.objects gs) gone
        }

-- `islands` untapped Islands for bob, one Think Twice in his graveyard and one
-- in his hand, with alice on Plains she never spends. The Bird is added by the
-- case rather than here, so the taxed and untaxed boards differ in that
-- permanent alone, and bob's mana is what separates the offer cases from the
-- payment ones.
--
-- Two seats where the Drannith Magistrate group above wants three: nothing here
-- tells "your opponents" from "each player", only a taxed zone from an untaxed
-- one.
avenTaxBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
avenTaxBoard plains island thinkTwice islands =
  let gs0 = S.landsFor island S.bob islands (S.landsInPlay plains 3)
      (yardId, gs1) = S.addGraveyardCard thinkTwice S.bob gs0
      (handId, gs2) = S.addHandCard thinkTwice S.bob gs1
      -- CR 104.3c: Think Twice draws, and an empty library would lose bob the
      -- game out from under every assertion.
      (_, gs3) = S.addLibraryCard island S.bob gs2
   in (yardId, handId, gs3)

avenInterrupterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
avenInterrupterSpec s registry = Spec.describe s "Aven Interrupter" $ do
  -- The headline road: bob's spell is on the stack, alice flashes the Bird in,
  -- and the spell it targets goes to bob's EXILE instead of resolving. Every
  -- other destination the rules could have sent it to is asserted empty, since
  -- a move to the wrong zone is the failure this proves against.
  Spec.it s "CR 400.7 the targeted spell leaves the stack for its owner's exile, and CR 702.170c plots it there" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    aven <- S.printingOf s registry "Aven Interrupter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (avenId, pikerId, gs) = avenBoard plains mountain aven piker
        waiting = S.runPure (avenAnswer pikerId) gs (S.cast S.bob pikerId)
        after = S.runPure (avenAnswer pikerId) waiting (S.cast S.alice avenId >> Engine.priorityLoop)
        -- CR 702.170d's "any turn after the turn in which it became plotted", on
        -- bob's own turn, with every land he has still tapped for the Piker.
        later =
          after
            { GameState.turnNumber = GameState.turnNumber after + 1,
              GameState.activePlayer = S.bob,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.bob
            }
        -- The Bird taxes the very card it plotted -- "spells your opponents cast
        -- from graveyards or from exile" names no exception for its own -- so
        -- the plot permission has to be shown on a board without it. The pair
        -- differs in that one permanent.
        birdGone = avenRemoved aven later
        -- The control that says the permission came from the plot stamp rather
        -- than from the card merely being in exile. Off `birdGone`, so it and
        -- the positive differ in the stamp alone.
        unplotted = birdGone {GameState.objects = Map.map (\o -> o {Object.plotted = Nothing}) (GameState.objects birdGone)}
    -- CR 702.8a, without which the whole case is unreachable: a creature with no
    -- flash could not be cast with bob's spell waiting on the stack, and there is
    -- no other moment at which a spell is there to target.
    Spec.assertBool s (S.castable S.alice avenId waiting) "CR 702.8a flash: the Bird is castable with bob's spell on the stack"
    Spec.assertEqWith s "the Piker left the stack without resolving: bob has no creature" (S.countOnBattlefieldByName (S.printingName piker) S.bob after) 0
    Spec.assertEqWith s "CR 406.2 it was exiled, and CR 108.4a files an exiled card under its owner" (avenNamed piker Zone.Exile S.bob after) 1
    Spec.assertEqWith s "and not under the player whose card exiled it" (avenNamed piker Zone.Exile S.alice after) 0
    Spec.assertEqWith s "and not in a graveyard, which is where CR 701.6a's countering would have put it" (avenNamed piker Zone.Graveyard S.bob after) 0
    Spec.assertEqWith s "the Bird itself resolved" (S.countOnBattlefieldByName (S.printingName aven) S.alice after) 1
    Spec.assertEqWith s "and the stack is empty, so the trigger resolved" (GameState.stack after) []
    case filter (\oid -> fmap S.nameOf (Game.cardOf oid after) == Just (S.printingName piker)) (Game.zoneMembers Zone.Exile S.bob after) of
      [exiledId] -> do
        -- Every land bob has is tapped for the Piker, so a permission that
        -- granted nothing and one that waived no cost both fail this.
        Spec.assertBool s (S.castable S.bob exiledId birdGone) "CR 702.170d bob casts it from exile on a later turn, with all three of his lands still tapped"
        Spec.assertBool s (not (S.castable S.bob exiledId later)) "and not while the Bird is still out: its own third sentence taxes the cast {2} and every land bob has is tapped"
        Spec.assertBool s (not (S.castable S.bob exiledId unplotted)) "the control: the same card in the same exile, unplotted, is castable by nobody"
        Spec.assertEqWith
          s
          "CR 702.170c the exiled card is stamped with the turn it became plotted"
          (fmap Object.plotted (Game.lookupObject exiledId after))
          (Just (Just (GameState.turnNumber after)))
      other -> Spec.assertFailure s ("expected one exiled Piker, got " <> show (length other))
  -- CR 701.6a's negative, as a PAIR that differs in one thing: the same
  -- Prowling Serpopard spell, on the same seats with the same mana, removed once
  -- by Cancel and once by the Bird. Cancel is the control that says CR 113.6g is
  -- live on this board at all -- without it, "the Serpopard was exiled" would
  -- pass on an engine that had never heard of Counterability.
  --
  -- Prowling Serpopard {1}{G}{G} rather than Blurred Mongoose, which prints the
  -- same "this spell can't be countered" beside a shroud that would give a
  -- refusal to TARGET it a second reading (CR 702.18a).
  --
  -- What refuses the countering here is the Serpopard's own FACE. Its other
  -- sentence, "creature spells you control can't be countered", is a static
  -- ability of the permanent and does not reach its own spell on the stack:
  -- disabling Pawl.Engine.Event.protectedFromCountering leaves this case green,
  -- and only forcing Face.counterability to Counterable reddens it.
  Spec.it s "CR 701.6a exiling a spell is not countering it, so a spell that can't be countered is exiled anyway" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    aven <- S.printingOf s registry "Aven Interrupter"
    cancel <- S.printingOf s registry "Cancel"
    serpopard <- S.printingOf s registry "Prowling Serpopard"
    let (avenId, victimA, boardA) = avenBoard plains forest aven serpopard
        (cancelId, victimB, boardB) = avenBoard island forest cancel serpopard
        exiled = S.runPure (avenAnswer victimA) boardA (S.cast S.bob victimA >> S.cast S.alice avenId >> Engine.priorityLoop)
        countered = S.runPure (avenAnswer victimB) boardB (S.cast S.bob victimB >> S.cast S.alice cancelId >> Engine.priorityLoop)
    Spec.assertEqWith s "the exile took the Serpopard off the stack, so it never resolved" (S.countOnBattlefieldByName (S.printingName serpopard) S.bob exiled) 0
    Spec.assertEqWith s "CR 701.6a and put it in exile, CR 113.6g notwithstanding: this was never a countering" (avenNamed serpopard Zone.Exile S.bob exiled) 1
    Spec.assertEqWith s "CR 113.6g the control: Cancel does not counter the same spell, so it resolves" (S.countOnBattlefieldByName (S.printingName serpopard) S.bob countered) 1
    Spec.assertEqWith s "and nothing of it is in exile there" (avenNamed serpopard Zone.Exile S.bob countered) 0
  -- CR 701.6a's other negative, the same way: nothing counters a spell that is
  -- exiled off the stack, so a "whenever a spell you control counters a spell"
  -- trigger must stay silent. Baral, Chief of Compliance is the observer, and
  -- alice's LIBRARY is where his trigger shows: he draws, so a fired trigger
  -- costs her a card off the top and a silent one does not.
  --
  -- The pair again: the same Goblin Piker, removed by Cancel on one board and by
  -- the Bird on the other. The Cancel half is what says Baral is watching -- an
  -- engine that never recorded the event at all would leave alice's library
  -- untouched on BOTH boards and the negative would be vacuous.
  Spec.it s "CR 701.6a a spell exiled off the stack was not countered, so a counter trigger stays silent" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    aven <- S.printingOf s registry "Aven Interrupter"
    cancel <- S.printingOf s registry "Cancel"
    baral <- S.printingOf s registry "Baral, Chief of Compliance"
    piker <- S.printingOf s registry "Goblin Piker"
    let watched land answer =
          let (answerId, victimId, gs0) = avenBoard land mountain answer piker
              (_, gs1) = S.addCreature baral S.alice gs0
              (_, gs2) = S.addLibraryCard mountain S.alice gs1
              (_, gs3) = S.addLibraryCard mountain S.alice gs2
           in (answerId, victimId, gs3)
        (avenId, victimA, boardA) = watched plains aven
        (cancelId, victimB, boardB) = watched island cancel
        exiled = S.runPure (baralAnswer victimA) boardA (S.cast S.bob victimA >> S.cast S.alice avenId >> Engine.priorityLoop)
        countered = S.runPure (baralAnswer victimB) boardB (S.cast S.bob victimB >> S.cast S.alice cancelId >> Engine.priorityLoop)
        librarySize gs = length (Game.zoneMembers Zone.Library S.alice gs)
        wasCountered gs = any (\e -> case e of GameEvent.SpellCountered _ -> True; _ -> False) (S.eventsOf gs)
    Spec.assertEqWith s "both boards start alice on two library cards" (librarySize boardA, librarySize boardB) (2, 2)
    Spec.assertEqWith s "CR 701.6a the control: Cancel counters the Piker, Baral's trigger draws, and alice's library is one shorter" (librarySize countered) 1
    Spec.assertEqWith s "CR 701.6a the Bird exiled the same spell instead, so Baral never triggered and alice drew nothing" (librarySize exiled) 2
    Spec.assertEqWith s "the Piker is in bob's exile rather than his graveyard" (avenNamed piker Zone.Exile S.bob exiled, avenNamed piker Zone.Graveyard S.bob exiled) (1, 0)
    Spec.assertEqWith s "where the countered one went to his graveyard rather than exile" (avenNamed piker Zone.Exile S.bob countered, avenNamed piker Zone.Graveyard S.bob countered) (0, 1)
    Spec.assertEqWith s "and the record agrees: a countering happened on the one board and not on the other" (wasCountered countered, wasCountered exiled) (True, False)
  -- The third sentence, on the zone the Bird itself never puts anything in: CR
  -- 702.34a's flashback cast out of a GRAVEYARD. Think Twice ({1}{U} Instant,
  -- "Draw a card." / "Flashback {2}{U}") is the spell, and the two zones it can
  -- be cast from off one board are what make the tax discriminating -- the hand
  -- copy is the same card, the same caster and the same mana, differing from the
  -- graveyard copy in the zone alone.
  --
  -- MANA TAPPED rather than a castability flag, and both rather than either:
  -- #2363's whole content is that a cost filter reading the wrong zone leaves
  -- Pawl.Engine.Cast.castable and Pawl.Engine.Cast.castSpell pricing one cast
  -- differently, so a case asserting only the offer would pass on an engine that
  -- taxed nobody at payment time, and one asserting only the payment would pass
  -- on an engine that never offered the cast at all.
  Spec.it s "CR 601.2f the third sentence taxes an opponent's cast from a graveyard and leaves their cast from hand alone" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    aven <- S.printingOf s registry "Aven Interrupter"
    thinkTwice <- S.printingOf s registry "Think Twice"
    let (yardId, handId, open) = avenTaxBoard plains island thinkTwice 5
        taxed = snd (S.addCreature aven S.alice open)
        tappedAfter gs oid = S.tappedCount S.bob (S.runPure S.identityAnswer gs (S.cast S.bob oid))
        -- Four Islands is one short of the taxed flashback and one over the
        -- untaxed one, which is where the OFFER and the payment have to agree.
        (shortYardId, _, shortOpen) = avenTaxBoard plains island thinkTwice 4
        shortTaxed = snd (S.addCreature aven S.alice shortOpen)
    Spec.assertEqWith s "CR 702.34a the flashback cost is {2}{U}, so bob taps three Islands with no Bird out" (tappedAfter open yardId) 3
    Spec.assertEqWith s "CR 601.2f the Bird adds {2} to that same cast, so the same board taps five" (tappedAfter taxed yardId) 5
    Spec.assertEqWith s "and the sentence names no hand: the {1}{U} cast from bob's hand still taps two with the Bird out" (tappedAfter taxed handId) 2
    Spec.assertBool s (S.castable S.bob shortYardId shortOpen) "the gate agrees with the payment: on four Islands the untaxed flashback is offered"
    Spec.assertBool s (not (S.castable S.bob shortYardId shortTaxed)) "and with the Bird out the same four Islands cannot pay it, so it is not offered"

-- Terror of the Peaks (OTJ 149) {3}{R}{R} Creature -- Dragon 5/4, Oracle text
-- checked against Scryfall: "Flying / Spells your opponents cast that target
-- this creature cost an additional 3 life to cast. / Whenever another creature
-- you control enters, this creature deals damage equal to that creature's power
-- to any target."
--
-- The second sentence is the customer: CR 601.2f prices a spell after CR 601.2c
-- has fixed its targets, and Filter.TargetsSource is what lets the tax ask what
-- they are. alice controls the Dragon with a Goblin Piker beside it, and the
-- caster holds one Lightning Bolt and the one Mountain that pays for it. Every
-- case is a PAIR differing in the aim alone -- the Dragon or the Piker -- and
-- reads the caster's LIFE once the Bolt is on the stack: the life component is
-- the only thing the tax costs, and nothing else on the board moves it before
-- the Bolt resolves.
terrorBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
terrorBoard mountain terror piker bolt caster =
  let (terrorId, g1) = S.addCreature terror S.alice (S.landsFor mountain caster 1 (Setup.emptyGame S.bothPlayers))
      (pikerId, g2) = S.addCreature piker S.alice g1
      (boltId, g3) = S.addHandCard bolt caster g2
   in (terrorId, pikerId, boltId, g3)

-- Aims every target choice at the named PLAYER; avenAnswer above is the object
-- half. Pinned rather than searched, for avenAnswer's reason.
aimAtPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimAtPlayer pid prompt = case prompt of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer pid) sets
  _ -> S.identityAnswer prompt

-- A player's life total set by hand, for the case that asks what CR 601.2h does
-- when the life is not there to pay.
withLife :: PlayerId.PlayerId -> Integer -> GameState.GameState -> GameState.GameState
withLife pid life gs = gs {GameState.players = Map.adjust (\p -> p {Player.life = life}) pid (GameState.players gs)}

terrorOfThePeaksSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
terrorOfThePeaksSpec s registry = Spec.describe s "Terror of the Peaks" $ do
  -- The headline pair. bob's Bolt at the Dragon costs him 3 life on top of the
  -- {R}; the same Bolt at the Piker beside it costs the {R} alone. The Piker
  -- half is what kills a reading of "any spell an opponent casts" -- same caster,
  -- same controller of the target, same mana -- and the Bolt on the stack is what
  -- says the cast went through rather than being refused.
  Spec.it s "CR 601.2f a spell an opponent aims at the Dragon costs 3 life more, and one aimed at another creature of the same controller does not" $ do
    mountain <- S.printingOf s registry "Mountain"
    terror <- S.printingOf s registry "Terror of the Peaks"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (terrorId, pikerId, boltId, board) = terrorBoard mountain terror piker bolt S.bob
        atDragon = S.runPure (avenAnswer terrorId) board (S.cast S.bob boltId)
        atPiker = S.runPure (avenAnswer pikerId) board (S.cast S.bob boltId)
    Spec.assertEqWith s "both seats start at 20" (S.lifeOf S.bob board) (Just 20)
    Spec.assertEqWith s "CR 601.2f / 118.8 the Bolt at the Dragon cost bob 3 life" (S.lifeOf S.bob atDragon) (Just 17)
    Spec.assertEqWith s "and it is on the stack, so the cast was made and not refused" (length (GameState.stack atDragon)) 1
    Spec.assertEqWith s "the same Bolt at the Piker cost no life" (S.lifeOf S.bob atPiker) (Just 20)
    Spec.assertEqWith s "and is on the stack too" (length (GameState.stack atPiker)) 1
    Spec.assertEqWith s "the {R} was paid on both boards" (S.tappedCount S.bob atDragon, S.tappedCount S.bob atPiker) (1, 1)
  -- CR 109.5 / PlayerScope.Opponents: "your opponents" excludes the Dragon's
  -- own controller, so alice's Bolt at her own Dragon costs her nothing. Same
  -- board with the Bolt and the Mountain moved to alice's seat.
  Spec.it s "CR 109.5 the Dragon's own controller pays no life to target it" $ do
    mountain <- S.printingOf s registry "Mountain"
    terror <- S.printingOf s registry "Terror of the Peaks"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (terrorId, _, boltId, board) = terrorBoard mountain terror piker bolt S.alice
        after = S.runPure (avenAnswer terrorId) board (S.cast S.alice boltId)
    Spec.assertEqWith s "alice's own Bolt at her own Dragon cost her no life" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and is on the stack" (length (GameState.stack after)) 1
  -- CR 601.2h: the tax is priced after CR 601.2c's targets are fixed, which is
  -- after the castability gate ran -- so the gate offers the Bolt to a player
  -- who cannot pay 3 life, and it is the PAYMENT that fails and CR 601.2's
  -- rewind that answers. The pair again: at 2 life bob's Bolt at the Dragon
  -- never leaves his hand, where the same Bolt at the Piker is cast.
  Spec.it s "CR 601.2h a caster without the life to pay has the cast rewound, and the same caster aims the Bolt elsewhere" $ do
    mountain <- S.printingOf s registry "Mountain"
    terror <- S.printingOf s registry "Terror of the Peaks"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (terrorId, pikerId, boltId, board) = terrorBoard mountain terror piker bolt S.bob
        poor = withLife S.bob 2 board
        atDragon = S.runPure (avenAnswer terrorId) poor (S.cast S.bob boltId)
        atPiker = S.runPure (avenAnswer pikerId) poor (S.cast S.bob boltId)
    Spec.assertEqWith s "CR 601.2h the Bolt at the Dragon was rewound: nothing is on the stack" (GameState.stack atDragon) []
    Spec.assertEqWith s "bob still holds it" (S.handSize S.bob atDragon) 1
    Spec.assertEqWith s "and paid nothing -- neither the life nor the Mountain" (S.lifeOf S.bob atDragon, S.tappedCount S.bob atDragon) (Just 2, 0)
    Spec.assertEqWith s "where the same Bolt at the Piker is on the stack" (length (GameState.stack atPiker)) 1
    Spec.assertBool s (S.castable S.bob boltId poor) "the gate offered the Bolt: CR 601.2c's choice is what decides whether the tax applies, and the gate runs before it"
  -- The third sentence, on its own: a Goblin Piker (2/1) entering under alice
  -- has the Dragon deal 2 -- the PIKER's power, not the Dragon's 5 -- to the
  -- target alice names. The Dragon entering with the Piker already out deals
  -- nothing, which is CR 603.2's "another".
  Spec.it s "CR 603.2 another creature entering under alice has the Dragon deal that creature's power to any target" $ do
    mountain <- S.printingOf s registry "Mountain"
    terror <- S.printingOf s registry "Terror of the Peaks"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g1) = S.addCreature terror S.alice (S.landsFor mountain S.alice 5 (Setup.emptyGame S.bothPlayers))
        (pikerId, pikerBoard) = S.addHandCard piker S.alice g1
        after = S.runPure (aimAtPlayer S.bob) pikerBoard (S.cast S.alice pikerId >> Engine.priorityLoop)
        (_, g2) = S.addCreature piker S.alice (S.landsFor mountain S.alice 5 (Setup.emptyGame S.bothPlayers))
        (terrorId, terrorBoard') = S.addHandCard terror S.alice g2
        itself = S.runPure (aimAtPlayer S.bob) terrorBoard' (S.cast S.alice terrorId >> Engine.priorityLoop)
    Spec.assertEqWith s "the Piker entered" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
    Spec.assertEqWith s "CR 603.2 and the Dragon dealt the Piker's 2 to bob" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "the Dragon entering beside a Piker already out dealt nothing" (S.lifeOf S.bob itself) (Just 20)
    Spec.assertEqWith s "though it did enter" (S.countOnBattlefieldByName (S.printingName terror) S.alice itself) 1

-- Shell of the Last Kappa (CHK 269) {3} Legendary Artifact, Oracle text checked
-- against Scryfall: "{3}, {T}: Exile target instant or sorcery spell that
-- targets you. (The spell has no effect.) / {3}, {T}, Sacrifice Shell of the
-- Last Kappa: You may cast a spell from among cards exiled with Shell of the
-- Last Kappa without paying its mana cost."
--
-- Not implemented: the second ability. Effect.OfferCast names a SLOT, and no
-- pool or effect binds the cards exiled with a source into one, so nothing can
-- make the offer (#2946). The omission takes a benefit off the Shell's
-- controller, which leaves the card stricter than printed.
--
-- THREE SEATS, so "you" is told from "a player": bob's Lightning Bolt aims at
-- alice on one board and at carol on the next, alice holds the Shell and three
-- Islands on both, and the question is whether the Shell can take the Bolt off
-- the stack. Read at gameplay level -- the Bolt's owner's exile, and the life
-- of whoever it was aimed at -- once the whole stack has resolved. The Piker
-- board is CR 115.10a's: a Bolt at a creature alice controls targets the
-- creature and not her.
shellBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
shellBoard island mountain shell piker bolt =
  let (shellId, g1) = S.addCreature shell S.alice (S.landsFor mountain S.bob 1 (S.landsFor island S.alice 3 S.threePlayerGame))
      (pikerId, g2) = S.addCreature piker S.alice g1
      (boltId, g3) = S.addHandCard bolt S.bob g2
   in (shellId, pikerId, boltId, g3)

-- bob casts the Bolt with `aim` answering its target, alice activates the Shell
-- at the Bolt, and the stack resolves down. The activation is DRIVEN rather than
-- offered, so a board on which the Bolt is not a legal target shows the
-- activation failing to happen at all -- the Shell untapped, the Bolt resolving.
shellRun :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
shellRun aim shellId boltId board =
  let cast = S.runPure aim board (S.cast S.bob boltId)
   in case Activate.abilitiesFor shellId cast of
        [ability] -> S.runPure (avenAnswer boltId) cast (Activate.activateAbility S.alice shellId ability >> Engine.priorityLoop)
        _ -> cast

shellOfTheLastKappaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
shellOfTheLastKappaSpec s registry = Spec.describe s "Shell of the Last Kappa" $ do
  Spec.it s "CR 115.1 a Bolt aimed at alice is exiled by her Shell, and one aimed at carol is not" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    shell <- S.printingOf s registry "Shell of the Last Kappa"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (shellId, _, boltId, board) = shellBoard island mountain shell piker bolt
        atAlice = shellRun (aimAtPlayer S.alice) shellId boltId board
        atCarol = shellRun (aimAtPlayer S.carol) shellId boltId board
    Spec.assertEqWith s "CR 115.1 the Bolt at alice never resolved: she is at 20" (S.lifeOf S.alice atAlice) (Just 20)
    Spec.assertEqWith s "CR 406.2 it is in bob's exile" (avenNamed bolt Zone.Exile S.bob atAlice) 1
    Spec.assertEqWith s "and the Shell tapped to do it" (S.tappedCount S.alice atAlice) 4
    Spec.assertEqWith s "the Bolt at carol resolved: she took 3" (S.lifeOf S.carol atCarol) (Just 17)
    Spec.assertEqWith s "nothing of it is in exile" (avenNamed bolt Zone.Exile S.bob atCarol) 0
    Spec.assertEqWith s "and the Shell never tapped, there being no spell targeting alice for it to aim at" (S.tappedCount S.alice atCarol) 0
    Spec.assertEqWith s "the stack is empty on both boards" (GameState.stack atAlice, GameState.stack atCarol) ([], [])
  Spec.it s "CR 115.10a a Bolt aimed at a creature alice controls does not target her" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    shell <- S.printingOf s registry "Shell of the Last Kappa"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (shellId, pikerId, boltId, board) = shellBoard island mountain shell piker bolt
        atPiker = shellRun (avenAnswer pikerId) shellId boltId board
    Spec.assertEqWith s "the Bolt resolved and the Piker died" (S.countOnBattlefieldByName (S.printingName piker) S.alice atPiker) 0
    Spec.assertEqWith s "nothing is in bob's exile" (avenNamed bolt Zone.Exile S.bob atPiker) 0
    Spec.assertEqWith s "and the Shell never tapped" (S.tappedCount S.alice atPiker) 0

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Cast" $ do
  droughtSpec s registry
  waxWaneSpec s registry
  wearTearSpec s registry
  boundedFuseXSpec s registry
  aftermathSpec s registry
  printedCastingRestrictionSpec s registry
  flashSpec s registry
  victorManchaSpec s registry
  direFleetDaredevilSpec s registry
  upToOneTargetSpec s registry
  multiTargetCastSpec s registry
  soulImmolationSpec s registry
  drannithMagistrateSpec s registry
  teferiSorcerySpeedSpec s registry
  grafdiggersCageCastSpec s registry
  avenInterrupterSpec s registry
  terrorOfThePeaksSpec s registry
  shellOfTheLastKappaSpec s registry
