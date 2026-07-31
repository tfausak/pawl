-- Covers Pawl.Engine.Expiry and Pawl.Types.Expiry: the printed Duration -> stored Expiry
-- arming (CR 611.2), the sweeps that end a duration (CR 514.2, 611.2a, 611.2b),
-- and the two gate cards (Master Thief, Hag of Inner Weakness).
module Pawl.ExpirySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Two distinct stand-in ids, named rather than spelled inline: nothing in this
-- file looks either object up, so the only thing that matters about them is that
-- they are not each other and not S.noSource.
effectSource :: ObjectId.ObjectId
effectSource = ObjectId.MkObjectId 998

effectTarget :: ObjectId.ObjectId
effectTarget = ObjectId.MkObjectId 997

-- A stored continuous effect with a chosen expiry, over a stand-in target.
effectWith :: Expiry.Type.Expiry -> GameState.GameState -> GameState.GameState
effectWith expiry gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = effectSource,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = expiry,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Flying,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton effectTarget)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- A stand-in state for the three arms that consult neither source nor state --
-- only Duration.ForAsLongAs reads them. They pass S.noSource for the source
-- rather than a bare literal, which is what "not consulted" actually means here.
armGs :: GameState.GameState
armGs = Setup.emptyGame S.bothPlayers

armSpec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
armSpec s = Spec.describe s "Arm" $ do
  Spec.it s "CR 514.2 an until-end-of-turn duration arms to AtCleanup" $
    Spec.assertEqWith s "armed" (Expiry.arm S.alice S.noSource Duration.UntilEndOfTurn armGs) (Just Expiry.Type.AtCleanup)
  Spec.it s "CR 611.2a an indefinite duration arms to Never" $
    Spec.assertEqWith s "armed" (Expiry.arm S.alice S.noSource Duration.Indefinite armGs) (Just Expiry.Type.Never)
  Spec.it s "CR 611.2a / 109.5 'until your next turn' bakes the controller" $
    Spec.assertEqWith s "armed" (Expiry.arm S.alice S.noSource Duration.UntilYourNextTurn armGs) (Just (Expiry.Type.AtTurnOf S.alice))

handoffSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
handoffSpec s = Spec.describe s "DropAtTurnOf" $ do
  Spec.it s "CR 611.2a an AtTurnOf effect ends as that player's turn begins, not before" $ do
    let gs0 = Setup.emptyGame S.bothPlayers
        -- alice is the active player; the effect ends at ALICE's next turn.
        armed = effectWith (Expiry.Type.AtTurnOf S.alice) gs0
        bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
        alicesTurn = S.runPure S.identityAnswer bobsTurn Engine.handoffTurn
    Spec.assertEqWith s "alice is active when it is created" (GameState.activePlayer armed) S.alice
    Spec.assertEqWith s "it survives the creating turn's handoff" (length (GameState.continuousEffects bobsTurn)) 1
    Spec.assertEqWith s "bob is active" (GameState.activePlayer bobsTurn) S.bob
    Spec.assertEqWith s "it ends as alice's next turn begins" (GameState.continuousEffects alicesTurn) []
  Spec.it s "CR 514.2 does not touch an AtTurnOf effect" $ do
    let gs0 = Setup.emptyGame S.bothPlayers
        armed = effectWith (Expiry.Type.AtTurnOf S.alice) gs0
    Spec.assertEqWith s "survives cleanup" (length (GameState.continuousEffects (Expiry.dropAtCleanup armed))) 1
  Spec.it s "CR 611.2a the sweep is scoped to the player whose turn began" $ do
    let gs0 = Setup.emptyGame S.bothPlayers
        armed = effectWith (Expiry.Type.AtTurnOf S.bob) gs0
        bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
    Spec.assertEqWith s "bob's turn ends bob's effect" (GameState.continuousEffects bobsTurn) []
  Spec.it s "CR 611.2a dropAtTurnOf ends the NAMED player's AtTurnOf effects, whoever is active" $ do
    -- alice is the active player throughout; the sweep is told to fire for
    -- BOB, and does. That divorce from GameState.activePlayer is the whole
    -- generalization -- it is what lets CR 800.4m fire at a seat whose turn
    -- never begins.
    let gs0 = S.threePlayerGame
        armed = effectWith (Expiry.Type.AtTurnOf S.alice) (effectWith (Expiry.Type.AtTurnOf S.bob) gs0)
        after = Expiry.dropAtTurnOf S.bob armed
    Spec.assertEqWith s "alice is still the active player" (GameState.activePlayer after) S.alice
    Spec.assertEqWith s "only alice's effect survives" (fmap ContinuousEffect.expiry (GameState.continuousEffects after)) [Expiry.Type.AtTurnOf S.alice]
  Spec.it s "CR 611.2a dropAtTurnOf touches no other expiry" $ do
    let gs0 = S.threePlayerGame
        armed = effectWith Expiry.Type.Never (effectWith Expiry.Type.AtCleanup (effectWith (Expiry.Type.AtTurnOf S.bob) gs0))
        after = Expiry.dropAtTurnOf S.bob armed
    Spec.assertEqWith s "Never and AtCleanup both survive" (length (GameState.continuousEffects after)) 2
  Spec.it s "CR 800.4k/800.4m a departed seat is skipped, and its durations end there anyway" $ do
    -- alice, bob, carol. Bob departs, then alice's turn ends. CR 800.4k: bob's
    -- turn doesn't begin, so carol becomes active. CR 800.4m: bob's "until
    -- your next turn" effect ends AT BOB'S SEAT -- not immediately when he
    -- left, and not never.
    let gone = Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame
        armed = effectWith (Expiry.Type.AtTurnOf S.bob) gone
        after = S.runPure S.identityAnswer armed Engine.handoffTurn
    Spec.assertEqWith s "it survived bob's departure itself" (length (GameState.continuousEffects armed)) 1
    Spec.assertEqWith s "carol takes the turn, not bob" (GameState.activePlayer after) S.carol
    Spec.assertEqWith s "bob's effect ended at bob's seat" (GameState.continuousEffects after) []
  Spec.it s "CR 800.4m the walk sweeps only the seats it passes" $ do
    -- Same board. The walk goes bob's seat -> carol's seat and STOPS. Alice's
    -- own AtTurnOf effect must survive: her seat was not passed. The
    -- activePlayer assertion is load-bearing (fix round 1, #87 review): without
    -- it this case cannot fail against the OLD single-step
    -- `dropAtTurnOf newActive` code, because with bob departed immediately
    -- after alice, newActive is ALSO bob, so the old code happened to sweep
    -- the same seat as the walk and left continuousEffects byte-identical.
    let gone = Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame
        armed = effectWith (Expiry.Type.AtTurnOf S.alice) (effectWith (Expiry.Type.AtTurnOf S.bob) gone)
        after = S.runPure S.identityAnswer armed Engine.handoffTurn
    Spec.assertEqWith s "carol takes the turn, not bob" (GameState.activePlayer after) S.carol
    Spec.assertEqWith s "alice's survives, bob's does not" (fmap ContinuousEffect.expiry (GameState.continuousEffects after)) [Expiry.Type.AtTurnOf S.alice]
  Spec.it s "CR 800.4m the sweep reaches a seat the walk only passes through, not just the seat it lands next to" $ do
    -- alice, bob, carol; bob AND carol both departed, alice active. The walk
    -- passes bob's seat, then carol's seat, then begins alice's turn (wrapping
    -- around). CR 800.4m requires the sweep to fire at EVERY seat walked past,
    -- not just the one immediately after the active player -- carol's AtTurnOf
    -- effect must be gone even though carol's seat is two hops away, not one.
    -- This is the discriminating case fix round 1 added: the OLD single-step
    -- `dropAtTurnOf newActive` only ever swept the FIRST seat past the active
    -- player (bob here), so carol's effect survived under the old code. See
    -- the fix report for the RED proof.
    let gone =
          Departure.depart
            Departure.Type.Conceded
            S.carol
            (Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame)
        armed = effectWith (Expiry.Type.AtTurnOf S.carol) gone
        after = S.runPure S.identityAnswer armed Engine.handoffTurn
    Spec.assertEqWith s "alice takes the turn (wrapping past both departed seats)" (GameState.activePlayer after) S.alice
    Spec.assertEqWith s "carol's effect ended at carol's seat, two hops past alice" (GameState.continuousEffects after) []

cleanupSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
cleanupSpec s registry = Spec.describe s "DropAtCleanup" $ do
  Spec.it s "CR 514.2 cleanup drops an AtCleanup continuous effect and keeps a Never one" $ do
    let gs0 = Setup.emptyGame S.bothPlayers
        gs1 = effectWith Expiry.Type.Never (effectWith Expiry.Type.AtCleanup gs0)
        after = Expiry.dropAtCleanup gs1
    Spec.assertEqWith s "two stored before" (length (GameState.continuousEffects gs1)) 2
    Spec.assertEqWith s "one survives" (fmap ContinuousEffect.expiry (GameState.continuousEffects after)) [Expiry.Type.Never]
  Spec.it s "CR 514.2 the same sweep drops an AtCleanup floating replacement" $ do
    piker <- Registry.printing registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (oid, gs1) = S.addCreature piker S.alice gs0
        shielded = S.addRegenShield oid gs1
        after = Expiry.dropAtCleanup shielded
    Spec.assertEqWith s "one shield before" (length (GameState.replacements shielded)) 1
    Spec.assertEqWith s "none after" (fmap ActiveReplacement.expiry (GameState.replacements after)) []

-- A stored continuous effect whose expiry is a live condition over `src`,
-- affecting `target`. The Master Thief shape, hand-built so the sweep can be
-- tested before the card exists.
whileEffect :: ObjectId.ObjectId -> ObjectId.ObjectId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
whileEffect src target you gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = src,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.Type.While you S.youControlSource,
            ContinuousEffect.modification = Modification.SetController you,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- Event.stateHolds's retired (you, source, cond, gs) shape, over the FULL
-- projection (outside the layer fold, per Pawl.Engine.Condition's spec) and the one
-- Condition S.youControlSource replaces StateCondition.YouControlSource with.
holdsYouControlSource :: PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> Bool
holdsYouControlSource you source gs =
  Condition.holds (Projection.fullView gs) (Filter.MkContext (Just you) (Just source)) gs source S.youControlSource

-- The OTHER carrier's shape of whileEffect: a floating replacement whose expiry
-- is a live condition over `src`. The effect payload is irrelevant to what this
-- proves (only `expiry` and `source` are read by sweepConditional's
-- keepReplacement predicate), so it reuses Support.addRegenShield's
-- DestructionR Regenerate as the shortest available fixture.
whileReplacement :: ObjectId.ObjectId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
whileReplacement src you gs =
  let (ts, gs1) = Game.freshTimestamp gs
      active =
        ActiveReplacement.MkActiveReplacement
          { ActiveReplacement.effect = ReplacementEffect.DestructionR DestructionRewrite.Regenerate,
            ActiveReplacement.source = src,
            ActiveReplacement.timestamp = ts,
            ActiveReplacement.expiry = Expiry.Type.While you S.youControlSource,
            ActiveReplacement.uses = Uses.Unlimited
          }
   in S.addReplacement active gs1

-- The Master Thief shape (piker source, War Mammoth target), built from the
-- two loaded printings each test case supplies.
board :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
board piker warMammoth =
  let gs0 = Setup.emptyGame S.bothPlayers
      (srcId, gs1) = S.addCreature piker S.alice gs0
      (targetId, gs2) = S.addCreature warMammoth S.bob gs1
   in (srcId, targetId, whileEffect srcId targetId S.alice gs2)

conditionalSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
conditionalSpec s registry = Spec.describe s "Conditional" $ do
  Spec.it s "CR 611.2b YouControlSource holds while the source is controlled" $ do
    piker <- Registry.printing registry "Goblin Piker"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
    Spec.assertBool s (holdsYouControlSource S.alice srcId gs) "holds"
  Spec.it s "CR 613.1b it stops holding when another player gains control of the source" $ do
    piker <- Registry.printing registry "Goblin Piker"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
        stolen = S.giveControl srcId S.bob gs
    Spec.assertBool s (not (holdsYouControlSource S.alice srcId stolen)) "no longer holds"
  Spec.it s "CR 400.7 it stops holding when the source leaves the battlefield" $ do
    piker <- Registry.printing registry "Goblin Piker"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
        gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [srcId])
    Spec.assertBool s (not (holdsYouControlSource S.alice srcId gone)) "no longer holds"
  Spec.it s "CR 611.2b arm returns Nothing when the condition is already false" $ do
    piker <- Registry.printing registry "Goblin Piker"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
        gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [srcId])
    Spec.assertEqWith
      s
      "never starts"
      (Expiry.arm S.alice srcId (Duration.ForAsLongAs S.youControlSource) gone)
      Nothing
  Spec.it s "CR 611.2b arm returns a While when the condition holds now" $ do
    piker <- Registry.printing registry "Goblin Piker"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
    Spec.assertEqWith
      s
      "starts"
      (Expiry.arm S.alice srcId (Duration.ForAsLongAs S.youControlSource) gs)
      (Just (Expiry.Type.While S.alice S.youControlSource))
  Spec.it s "CR 611.2b the sweep DELETES the effect once the condition fails" $ do
    piker <- Registry.printing registry "Goblin Piker"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (srcId, targetId, gs) = board piker warMammoth
        gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [srcId])
        (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
    Spec.assertEqWith s "alice held it while the source stood" (Projection.controllerOf targetId gs) (Just S.alice)
    Spec.assertBool s changed "the sweep reports a change"
    Spec.assertEqWith s "the effect is gone, not masked" (GameState.continuousEffects swept) []
    Spec.assertEqWith s "control reverted" (Projection.controllerOf targetId swept) (Just S.bob)
  Spec.it s "CR 611.2b a sweep that changes nothing reports False" $ do
    piker <- Registry.printing registry "Goblin Piker"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (_, _, gs) = board piker warMammoth
        (changed, _) = Engine.runGamePure S.identityAnswer gs Expiry.sweepConditional
    Spec.assertBool s (not changed) "no change"
  Spec.it s "CR 704.3 settleForPriority runs the sweep" $ do
    piker <- Registry.printing registry "Goblin Piker"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (srcId, targetId, gs) = board piker warMammoth
        gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [srcId])
        settled = S.runPure S.identityAnswer gone Engine.settleForPriority
    Spec.assertEqWith s "control reverted at the settle" (Projection.controllerOf targetId settled) (Just S.bob)
  Spec.it s "CR 611.2b the sweep's replacements half survives while the source stands, then deletes once it doesn't" $ do
    piker <- Registry.printing registry "Goblin Piker"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (srcId, _, gs0) = board piker warMammoth
        gs = whileReplacement srcId S.alice gs0
        (unchanged, stillUp) = Engine.runGamePure S.identityAnswer gs Expiry.sweepConditional
        -- A direct zone change, NOT Event.destroy: the fixture's own
        -- payload is a DestructionR (Regenerate), so routing the removal
        -- through the destruction funnel would let it replace/regenerate
        -- itself instead of leaving the battlefield.
        gone = S.runPure S.identityAnswer gs (Event.changeZone srcId Zone.Graveyard)
        (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
    Spec.assertBool s (not unchanged) "no change while the source stands"
    Spec.assertEqWith s "the replacement survives" (length (GameState.replacements stillUp)) 1
    Spec.assertBool s changed "the sweep reports a change once the source is gone"
    Spec.assertEqWith s "the replacement is gone" (GameState.replacements swept) []

-- Master Thief {2}{U}{U} Creature -- Human Rogue 2/2: "When this creature
-- enters, gain control of target artifact for as long as you control this
-- creature." CR 611.2b's own printed example; the three assertions below in
-- tests 2-4 are its three Gatherer rulings, verbatim.
masterThiefSettle :: GameState.GameState -> GameState.GameState
masterThiefSettle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

masterThiefResolveAll :: GameState.GameState -> GameState.GameState
masterThiefResolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop

-- bob's Darksteel Myr (Artifact Creature -- Myr, 0/1) is the only artifact on
-- the board, so the CR 603.3d target choice is forced.
masterThiefBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
masterThiefBoard darksteelMyr masterThief =
  let gs0 = Setup.emptyGame S.bothPlayers
      (myrId, gs1) = S.addCreature darksteelMyr S.bob gs0
      (thiefId, gs2) = S.addCreature masterThief S.alice gs1
      entered = ZoneChange.MkZoneChange thiefId thiefId Zone.Stack Zone.Battlefield
      gs3 = S.withEvents [GameEvent.Moved entered (Projection.project thiefId gs2)] gs2
   in (thiefId, myrId, gs3)

-- The masterThiefBoard shape at three seats, resolved: alice's Master Thief has
-- entered and its ETB has taken bob's Darksteel Myr (the only artifact on the
-- board, so the CR 603.3d target choice is forced). Carol is the third seat, so
-- either departure leaves a game with two players still in it and CR 104.2a does
-- not end it -- which is the whole reason these two directions are only
-- distinguishable at three seats.
masterThiefThreeWay :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
masterThiefThreeWay darksteelMyr masterThief =
  let (myrId, gs1) = S.addCreature darksteelMyr S.bob S.threePlayerGame
      (thiefId, gs2) = S.addCreature masterThief S.alice gs1
      entered = ZoneChange.MkZoneChange thiefId thiefId Zone.Stack Zone.Battlefield
      gs3 = S.withEvents [GameEvent.Moved entered (Projection.project thiefId gs2)] gs2
   in (thiefId, myrId, masterThiefResolveAll (masterThiefSettle gs3))

-- Master Thief {2}{U}{U} Creature -- Human Rogue 2/2: "When this creature
-- enters, gain control of target artifact for as long as you control this
-- creature." CR 611.2b's own printed example; the three assertions below in
-- tests 2-4 are its three Gatherer rulings, verbatim.
masterThiefSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
masterThiefSpec s registry = Spec.describe s "MasterThief" $ do
  Spec.it s "CR 611.2b it works: the ETB resolves and control of the artifact changes" $ do
    darksteelMyr <- Registry.printing registry "Darksteel Myr"
    masterThief <- Registry.printing registry "Master Thief"
    let (_, myr, entering) = masterThiefBoard darksteelMyr masterThief
        stolen = masterThiefResolveAll (masterThiefSettle entering)
    Spec.assertEqWith s "alice controls the Myr" (Projection.controllerOf myr stolen) (Just S.alice)
    -- CR 302.6: the new controller has not controlled it continuously.
    Spec.assertEqWith s "and it is re-Sicked" (fmap Object.sickness (Game.lookupObject myr stolen)) (Just Sickness.Sick)
  -- Ruling: "If Master Thief leaves the battlefield, you no longer
  -- control it, and its control-change effect ends."
  Spec.it s "CR 611.2b leaving the battlefield ends it" $ do
    darksteelMyr <- Registry.printing registry "Darksteel Myr"
    masterThief <- Registry.printing registry "Master Thief"
    let (thief, myr, entering) = masterThiefBoard darksteelMyr masterThief
        stolen = masterThiefResolveAll (masterThiefSettle entering)
        dead = S.runPure S.identityAnswer stolen (Event.destroy Regenerability.Regenerable [thief])
        swept = masterThiefSettle dead
    Spec.assertEqWith s "control reverts at the next settle" (Projection.controllerOf myr swept) (Just S.bob)
    Spec.assertEqWith s "and stays reverted" (Projection.controllerOf myr (masterThiefSettle swept)) (Just S.bob)
  -- Ruling: "If Master Thief ceases to be under your control before its
  -- ability resolves, you won't gain control of the targeted artifact at
  -- all." CR 704.5g destroys it for lethal damage while the trigger is
  -- on the stack; the trigger still RESOLVES (its target is legal, CR
  -- 608.2b), but the duration never starts.
  Spec.it s "CR 611.2b the duration never starts, so no effect is stored" $ do
    darksteelMyr <- Registry.printing registry "Darksteel Myr"
    masterThief <- Registry.printing registry "Master Thief"
    let (thief, myr, entering) = masterThiefBoard darksteelMyr masterThief
        onStack = masterThiefSettle entering
        lethal = S.settleSba (S.markDamage thief 2 onStack)
        after = masterThiefResolveAll lethal
    Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
    Spec.assertEqWith s "Master Thief died before it resolved" (Game.lookupObject thief after) Nothing
    Spec.assertEqWith s "nothing was stored" (GameState.continuousEffects after) []
    Spec.assertEqWith s "control never changed" (Projection.controllerOf myr after) (Just S.bob)
    -- CR 302.6: a control-change stored by GainControl re-Sicks the
    -- target; the duration never starting must leave that untouched.
    -- This is the discriminator settleForPriority's sweepConditional
    -- can't launder away: it proves nothing was EVER stored, not
    -- merely that nothing survived the sweep.
    Spec.assertEqWith s "and was never re-Sicked" (fmap Object.sickness (Game.lookupObject myr after)) (Just (Sickness.Settled S.bob))
  -- Ruling: "If Master Thief ceases to be under your control before its
  -- ability resolves, you won't gain control of the targeted artifact at
  -- all." Falsifies the CONTROL half of S.youControlSource's
  -- ControlledBy conjunct (CR 611.2b/613.1b/400.7): Master Thief stays on the
  -- battlefield the whole time -- only its controller changes -- so this
  -- case cannot pass for the "left the battlefield" reason the sibling
  -- case above covers. End to end through the real pipeline (CR 113.8:
  -- the ability's controller is alice, frozen at trigger time, and
  -- Resolve.resolveEffects must read that frozen value rather than bob's
  -- live control of the thief).
  Spec.it s "CR 611.2b ceasing to be under your control (not leaving the battlefield) also stops it" $ do
    darksteelMyr <- Registry.printing registry "Darksteel Myr"
    masterThief <- Registry.printing registry "Master Thief"
    let (thief, myr, entering) = masterThiefBoard darksteelMyr masterThief
        onStack = masterThiefSettle entering
        taken = S.giveControl thief S.bob onStack
        after = masterThiefResolveAll taken
    Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
    Spec.assertEqWith s "Master Thief is still on the battlefield, under bob" (Projection.controllerOf thief taken) (Just S.bob)
    Spec.assertBool s (Maybe.isJust (Game.lookupObject thief after)) "Master Thief is still on the battlefield after resolution"
    Spec.assertEqWith s "control never changed" (Projection.controllerOf myr after) (Just S.bob)
    -- Filtered to the ARTIFACT: `taken`'s own S.giveControl fixture
    -- already stored an unrelated AtCleanup SetController effect on
    -- the thief itself, so a blanket `[] == continuousEffects` would
    -- fail for a reason that has nothing to do with this bug.
    Spec.assertEqWith s "nothing was stored for the artifact" (filter (S.continuousEffectAffects myr) (GameState.continuousEffects after)) []
    -- CR 302.6: a control-change stored by GainControl re-Sicks the
    -- target; the duration never starting must leave that untouched.
    Spec.assertEqWith s "and was never re-Sicked" (fmap Object.sickness (Game.lookupObject myr after)) (Just (Sickness.Settled S.bob))
  -- Ruling: "If another player gains control of Master Thief, its
  -- control-change effect ends. Regaining control of Master Thief won't
  -- cause you to regain control of the artifact." THE FALSIFIER: an
  -- implementation that filters the effect out of the projection while
  -- the condition is false, instead of deleting it, fails exactly here.
  Spec.it s "CR 611.2b the latch: regaining the source does not regain the artifact" $ do
    darksteelMyr <- Registry.printing registry "Darksteel Myr"
    masterThief <- Registry.printing registry "Master Thief"
    let (thief, myr, entering) = masterThiefBoard darksteelMyr masterThief
        stolen = masterThiefResolveAll (masterThiefSettle entering)
        taken = S.giveControl thief S.bob stolen
        swept = masterThiefSettle taken
        returned = Expiry.dropAtCleanup swept
        relatched = masterThiefSettle returned
    Spec.assertEqWith s "bob has Master Thief" (Projection.controllerOf thief taken) (Just S.bob)
    Spec.assertEqWith s "so the artifact goes back to its owner" (Projection.controllerOf myr swept) (Just S.bob)
    Spec.assertEqWith s "at cleanup Master Thief comes home" (Projection.controllerOf thief returned) (Just S.alice)
    Spec.assertEqWith s "and the artifact does NOT" (Projection.controllerOf myr relatched) (Just S.bob)
  -- CR 800.4a, third example: "If Bianca leaves the game, Serra Angel also
  -- leaves the game." The stolen object is owned by the departing player, so
  -- it goes with them -- the thief keeps nothing.
  Spec.it s "CR 800.4a the stolen artifact's OWNER departs: the artifact leaves the game and Master Thief stays" $ do
    darksteelMyr <- Registry.printing registry "Darksteel Myr"
    masterThief <- Registry.printing registry "Master Thief"
    let (thief, myr, stolen) = masterThiefThreeWay darksteelMyr masterThief
        gone = Departure.depart Departure.Type.Conceded S.bob stolen
    Spec.assertEqWith s "alice really had it before bob left" (Projection.controllerOf myr stolen) (Just S.alice)
    Spec.assertEqWith s "the Myr is gone from the game" (Game.lookupObject myr gone) Nothing
    Spec.assertEqWith s "so it has no controller" (Projection.controllerOf myr gone) Nothing
    Spec.assertBool s (Maybe.isJust (Game.lookupObject thief gone)) "Master Thief is still on the battlefield"
    Spec.assertEqWith s "under alice" (Projection.controllerOf thief gone) (Just S.alice)
    -- The stored SetController effect stays, with an `affected` set naming an
    -- object that no longer exists. It is inert: Projection.controllerOf
    -- returns Nothing for an unknown id, and GameState.nextObjectId is
    -- monotone so the id is never reused. Pinned rather than tidied, because
    -- CR 800.4a ends only the effects that GIVE the departing player control
    -- and this one gives control to alice, who is still here.
    Spec.assertEqWith s "the effect that named it is still stored, and inert" (length (GameState.continuousEffects gone)) 1
    Spec.assertEqWith s "CR 104.2a: two survivors, so the game continues" (GameState.result gone) Nothing
  -- CR 800.4a, first example: "If Alex leaves the game, so does Mind Control,
  -- and Assault Griffin reverts to Bianca's control." CR 800.4a's second
  -- example says the same for Act of Treason's change-of-control effect.
  -- Master Thief is a creature rather than an Aura, so the thief simply
  -- leaves; what matters is that the effect ends AT THE DEPARTURE and not at
  -- some later sweep.
  Spec.it s "CR 800.4a the THIEF departs: the control effect ends immediately and the artifact reverts" $ do
    darksteelMyr <- Registry.printing registry "Darksteel Myr"
    masterThief <- Registry.printing registry "Master Thief"
    let (thief, myr, stolen) = masterThiefThreeWay darksteelMyr masterThief
        gone = Departure.depart Departure.Type.Conceded S.alice stolen
    Spec.assertEqWith s "alice really had it before she left" (Projection.controllerOf myr stolen) (Just S.alice)
    Spec.assertEqWith s "Master Thief left the game with its owner" (Game.lookupObject thief gone) Nothing
    Spec.assertEqWith s "the Myr is still in the game" (fmap Object.owner (Game.lookupObject myr gone)) (Just S.bob)
    Spec.assertBool s (Set.member myr (GameState.battlefield gone)) "and still on the battlefield"
    Spec.assertEqWith s "under bob again" (Projection.controllerOf myr gone) (Just S.bob)
    -- The discriminator against "a sweep would have got there eventually":
    -- CR 800.4a says "It happens as soon as the player leaves the game", and
    -- Expiry.sweepConditional runs at the next settle, not now.
    Spec.assertEqWith s "no stored effect survives the departure itself" (GameState.continuousEffects gone) []
    Spec.assertEqWith s "CR 104.2a: two survivors, so the game continues" (GameState.result gone) Nothing

-- CR 725.2: the monarch's inherent end-step draw -- a triggered ability that
-- belongs to NO object. The falsifier: no permanents on the battlefield at all,
-- so the draw cannot hang on a bearer.
monarchSettle :: GameState.GameState -> GameState.GameState
monarchSettle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

monarchResolveAll :: GameState.GameState -> GameState.GameState
monarchResolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop

monarchSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
monarchSpec s registry = Spec.describe s "Monarch" $ do
  Spec.it s "CR 725.2 the monarch draws at the beginning of their own end step" $ do
    piker <- Registry.printing registry "Goblin Piker"
    let (_, gs0) = S.addLibraryCard piker S.alice (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
        began = S.withEvents [GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice] gs0
        after = monarchResolveAll (monarchSettle began)
    Spec.assertEqWith s "alice drew (one card now in hand)" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
  Spec.it s "CR 725.2 the end-step draw fires only on the monarch's own end step" $ do
    piker <- Registry.printing registry "Goblin Piker"
    let (_, gs0) = S.addLibraryCard piker S.bob (S.withMonarch S.bob (Setup.emptyGame S.bothPlayers))
        -- alice is the active player; her end step is not bob's (the monarch).
        began = S.withEvents [GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice] gs0
        after = monarchResolveAll (monarchSettle began)
    Spec.assertEqWith s "bob did not draw on alice's end step" (length (Game.zoneMembers Zone.Hand S.bob after)) 0
  Spec.it s "CR 725.2 combat damage to the monarch hands the crown to the damager's controller" $ do
    piker <- Registry.printing registry "Goblin Piker"
    let (bobCreature, gs0) = S.addCreature piker S.bob (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
        dmg = DamageEvent.MkDamageEvent bobCreature (Recipient.ToPlayer S.alice) 2 False False 0 DamageKind.Combat
        began = S.withEvents [GameEvent.DamageDealt dmg] gs0
        after = monarchResolveAll (monarchSettle began)
    Spec.assertEqWith s "bob took the crown" (GameState.monarch after) (Just S.bob)
  Spec.it s "CR 725.2 noncombat damage to the monarch does not hand over the crown" $ do
    piker <- Registry.printing registry "Goblin Piker"
    let (bobCreature, gs0) = S.addCreature piker S.bob (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
        dmg = DamageEvent.MkDamageEvent bobCreature (Recipient.ToPlayer S.alice) 2 False False 0 DamageKind.Noncombat
        began = S.withEvents [GameEvent.DamageDealt dmg] gs0
        after = monarchResolveAll (monarchSettle began)
    Spec.assertEqWith s "alice keeps the crown" (GameState.monarch after) (Just S.alice)
  Spec.it s "CR 725 Palace Jailer: ETB makes the caster monarch and exiles an opponent's creature until an opponent takes the crown" $ do
    piker <- Registry.printing registry "Goblin Piker"
    palaceJailer <- Registry.printing registry "Palace Jailer"
    let gs0 = Setup.emptyGame S.bothPlayers
        (victim, gs1) = S.addCreature piker S.bob gs0
        (jailer, gs2) = S.addCreature palaceJailer S.alice gs1
        entered = ZoneChange.MkZoneChange jailer jailer Zone.Stack Zone.Battlefield
        gs3 = S.withEvents [GameEvent.Moved entered (Projection.project jailer gs2)] gs2
        afterEtb = monarchResolveAll (monarchSettle gs3)
        -- caster stays monarch across a turn boundary: exile holds.
        heldExiled = monarchSettle afterEtb
        -- an opponent (bob) takes the crown: the creature returns.
        afterSteal = monarchResolveAll (monarchSettle (S.withMonarch S.bob heldExiled))
    Spec.assertEqWith s "alice is monarch on ETB" (GameState.monarch afterEtb) (Just S.alice)
    Spec.assertEqWith s "victim is exiled" (length (filter (== victim) (Set.toList (GameState.battlefield afterEtb)))) 0
    Spec.assertBool s (not (Map.null (GameState.exiledUntilMonarch afterEtb))) "victim registered for return"
    Spec.assertBool s (not (Map.null (GameState.exiledUntilMonarch heldExiled))) "still exiled while alice stays monarch"
    Spec.assertEqWith s "a creature is back on the battlefield once bob is monarch" (length (Game.zoneMembers Zone.Battlefield S.bob afterSteal)) 1
    Spec.assertEqWith s "return cleared the exile register" (Map.null (GameState.exiledUntilMonarch afterSteal)) True
  Spec.it s "M5.6c gate: the monarch holding a Palace Jailer exile leaves, CR 725.4 crowns the active player, and the prisoner comes home" $ do
    -- Alice, bob, carol. Alice's Palace Jailer has entered: alice is the
    -- monarch and bob's Piker is exiled under the CR 725 watch. Carol is the
    -- active player, so CR 725.4's FIRST sentence applies when alice leaves.
    --
    -- Three rules meet here:
    --   * CR 800.4a removes alice's Palace Jailer (she owns it) but NOT bob's
    --     exiled card (he does), and it ends no effect here, because an exile
    --     grants nobody control. The card's own ruling agrees the watch is not
    --     tied to the source object: "Palace Jailer leaving the battlefield
    --     won't cause the exiled creature to return. The game will continue to
    --     watch for the next time an opponent becomes the monarch."
    --     (2021-03-19)
    --   * CR 725.4 crowns carol "at the same time as that player leaves the
    --     game."
    --   * CR 800.4i freezes what "an opponent" of departed alice means to the
    --     last known information -- in a free-for-all with no teams, every
    --     other player who was in the game. Carol is necessarily in that set,
    --     so the prisoner returns as part of the same departure.
    piker <- Registry.printing registry "Goblin Piker"
    palaceJailer <- Registry.printing registry "Palace Jailer"
    let (victim, gs1) = S.addCreature piker S.bob S.threePlayerGame
        (jailer, gs2) = S.addCreature palaceJailer S.alice gs1
        entered = ZoneChange.MkZoneChange jailer jailer Zone.Stack Zone.Battlefield
        gs3 = S.withEvents [GameEvent.Moved entered (Projection.project jailer gs2)] gs2
        afterEtb = monarchResolveAll (monarchSettle (gs3 {GameState.activePlayer = S.carol}))
        gone = Departure.depart Departure.Type.Conceded S.alice afterEtb
        settled = monarchSettle gone
    Spec.assertEqWith s "alice is the monarch on ETB" (GameState.monarch afterEtb) (Just S.alice)
    Spec.assertEqWith s "bob's creature is exiled under the watch, keyed to alice, baselined on her crown" (Map.elems (GameState.exiledUntilMonarch afterEtb)) [MonarchWatch.MkMonarchWatch {MonarchWatch.controller = S.alice, MonarchWatch.lastMonarch = Just S.alice}]
    Spec.assertEqWith s "the original is off the battlefield" (length (filter (== victim) (Set.toList (GameState.battlefield afterEtb)))) 0
    -- CR 800.4a: alice's own object leaves; bob's exiled card does not.
    Spec.assertEqWith s "Palace Jailer left the game with alice" (Game.lookupObject jailer gone) Nothing
    Spec.assertEqWith s "but the watch survived her departure, still keyed to her and still baselined on her crown" (Map.elems (GameState.exiledUntilMonarch gone)) [MonarchWatch.MkMonarchWatch {MonarchWatch.controller = S.alice, MonarchWatch.lastMonarch = Just S.alice}]
    -- CR 725.4, first sentence.
    Spec.assertEqWith s "carol, the active player, is the monarch" (GameState.monarch gone) (Just S.carol)
    -- CR 800.4i: carol is in departed alice's frozen opponent set, so the
    -- watch fires at the next settle (CR 704.3 fixes that as the coarsest
    -- moment anything can observe it).
    Spec.assertEqWith s "the prisoner is back on bob's side of the table" (length (Game.zoneMembers Zone.Battlefield S.bob settled)) 1
    Spec.assertEqWith s "and the watch is cleared" (Map.null (GameState.exiledUntilMonarch settled)) True
    Spec.assertEqWith s "CR 104.2a: two survivors, so the game continues" (GameState.result settled) Nothing

hagUpkeep :: Phase.Phase
hagUpkeep = Phase.Beginning BeginningStep.Upkeep

hagBeginUpkeep :: GameState.GameState -> GameState.GameState
hagBeginUpkeep gs = Event.recordEvent (GameEvent.StepBegan hagUpkeep S.alice) (gs {GameState.phase = hagUpkeep, GameState.activePlayer = S.alice})

hagSettle :: GameState.GameState -> GameState.GameState
hagSettle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

hagResolveAll :: GameState.GameState -> GameState.GameState
hagResolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop

hagHandoff :: GameState.GameState -> GameState.GameState
hagHandoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn

-- alice's Hag, and exactly one creature bob controls, so the CR 603.3d target
-- choice is forced.
hagBoardWith :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
hagBoardWith hag printing =
  let gs0 = Setup.emptyGame S.bothPlayers
      (_, gs1) = S.addCreature hag S.alice gs0
      (victimId, gs2) = S.addCreature printing S.bob gs1
   in (victimId, hagResolveAll (hagSettle (hagBeginUpkeep gs2)))

-- Hag of Inner Weakness {2}{B} Creature -- Hag Warlock 2/2: "At the beginning of
-- your upkeep, target creature an opponent controls gets -2/-1 until your next
-- turn." No Gatherer rulings exist, so these derive from CR 611.2a and CR 514.2.
hagSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
hagSpec s registry = Spec.describe s "HagOfInnerWeakness" $ do
  Spec.it s "CR 613.4c it works: the opponent's 3/3 becomes 1/2" $ do
    hag <- Registry.printing registry "Hag of Inner Weakness"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (mammoth, afterTrigger) = hagBoardWith hag warMammoth
    Spec.assertEqWith s "power" (Projection.powerOf mammoth afterTrigger) (Just 1)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf mammoth afterTrigger) (Just 2)
  -- THE FALSIFIER for both "treat it as until end of turn" and any
  -- implementation that expires the effect by scanning the event log for
  -- a matching StepBegan: the effect was CREATED on a turn whose untap
  -- step has already happened, so a log scan kills it the turn it is born.
  Spec.it s "CR 514.2 it survives cleanup and the whole of the opponent's turn" $ do
    hag <- Registry.printing registry "Hag of Inner Weakness"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (mammoth, afterTrigger) = hagBoardWith hag warMammoth
        ended = Expiry.dropAtCleanup afterTrigger
        bobsTurn = hagHandoff ended
        bobsTurnSwept = Expiry.dropAtCleanup bobsTurn
    Spec.assertEqWith s "survives its own turn's CR 514.2 cleanup sweep" (Projection.powerOf mammoth ended) (Just 1)
    Spec.assertEqWith s "bob is active" (GameState.activePlayer bobsTurn) S.bob
    Spec.assertEqWith s "power still 1 at the start of bob's turn" (Projection.powerOf mammoth bobsTurn) (Just 1)
    Spec.assertEqWith s "toughness still 2 at the start of bob's turn" (Projection.toughnessOf mammoth bobsTurn) (Just 2)
    Spec.assertEqWith s "power survives bob's own CR 514.2 cleanup sweep too" (Projection.powerOf mammoth bobsTurnSwept) (Just 1)
    Spec.assertEqWith s "toughness survives bob's own CR 514.2 cleanup sweep too" (Projection.toughnessOf mammoth bobsTurnSwept) (Just 2)
  Spec.it s "CR 611.2a it expires as the controller's next turn begins" $ do
    hag <- Registry.printing registry "Hag of Inner Weakness"
    warMammoth <- Registry.printing registry "War Mammoth"
    let (mammoth, afterTrigger) = hagBoardWith hag warMammoth
        alicesTurn = hagHandoff (hagHandoff (Expiry.dropAtCleanup afterTrigger))
    Spec.assertEqWith s "alice is active again" (GameState.activePlayer alicesTurn) S.alice
    -- Asserted BEFORE the upkeep trigger fires a second time, so
    -- the two effects can never be confused.
    Spec.assertEqWith s "back to 3/3" (Projection.powerOf mammoth alicesTurn) (Just 3)
    Spec.assertEqWith s "back to 3/3" (Projection.toughnessOf mammoth alicesTurn) (Just 3)
    Spec.assertEqWith s "nothing stored" (GameState.continuousEffects alicesTurn) []
  Spec.it s "CR 704.5f the modification really applies: a 2/1 becomes 0/0 and dies" $ do
    hag <- Registry.printing registry "Hag of Inner Weakness"
    piker <- Registry.printing registry "Goblin Piker"
    let (_, afterPiker) = hagBoardWith hag piker
    Spec.assertEqWith s "bob's Piker is gone" (S.creaturesInPlay S.bob afterPiker) 0
  -- CR 608.2b: "If the source of an ability has left the zone it was in, its
  -- last known information is used during this process."
  --
  -- The Hag's slot says "target creature an OPPONENT controls" -- a filter
  -- that reads a perspective. Take the perspective from the SOURCE PERMANENT
  -- and killing the Hag in response empties the legal set, so CR 608.2b's
  -- re-check finds the target illegal and the trigger wrongly fizzles. The
  -- ability's own controller is still known, and that is the perspective the
  -- rule means.
  Spec.it s "CR 608.2b killing the Hag in response does not fizzle its trigger" $ do
    hag <- Registry.printing registry "Hag of Inner Weakness"
    warMammoth <- Registry.printing registry "War Mammoth"
    let gs0 = Setup.emptyGame S.bothPlayers
        (hagId, gs1) = S.addCreature hag S.alice gs0
        (mammoth, gs2) = S.addCreature warMammoth S.bob gs1
        -- The trigger is on the stack with its target already chosen.
        staged = hagSettle (hagBeginUpkeep gs2)
        -- Bob answers it by killing the Hag before it resolves.
        hagGone = S.runPure S.identityAnswer staged (Event.destroy Regenerability.Regenerable [hagId])
        resolved = hagResolveAll hagGone
    Spec.assertBool s (S.creaturesInPlay S.alice hagGone == 0) "the Hag really did leave"
    Spec.assertEqWith s "the trigger still resolved: power" (Projection.powerOf mammoth resolved) (Just 1)
    Spec.assertEqWith s "and toughness" (Projection.toughnessOf mammoth resolved) (Just 2)
  -- The discriminating twin: CR 608.2b still FIZZLES when the thing that
  -- actually became illegal is the target. This fails if the fix were "stop
  -- re-checking targets" rather than "read the right perspective".
  Spec.it s "CR 608.2b killing the TARGET in response does fizzle the trigger" $ do
    hag <- Registry.printing registry "Hag of Inner Weakness"
    warMammoth <- Registry.printing registry "War Mammoth"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, gs1) = S.addCreature hag S.alice gs0
        (mammoth, gs2) = S.addCreature warMammoth S.bob gs1
        staged = hagSettle (hagBeginUpkeep gs2)
        targetGone = S.runPure S.identityAnswer staged (Event.destroy Regenerability.Regenerable [mammoth])
        resolved = hagResolveAll targetGone
    Spec.assertEqWith s "nothing was stored, because the trigger fizzled" (GameState.continuousEffects resolved) []
  -- CR 603.3a: a triggered ability's controller is whoever controlled its
  -- source WHEN IT TRIGGERED. CR 608.2b then re-checks against that player.
  --
  -- DISCRIMINATING against the tempting simplification -- reading the
  -- perspective off the source's LIVE controller. Stealing the Hag after its
  -- trigger is on the stack would flip "an opponent controls" to bob's point
  -- of view, making bob's own Mammoth illegal and the trigger fizzle. Alice
  -- keeps a creature of her own precisely so that flip would be visible.
  Spec.it s "CR 603.3a stealing the Hag mid-trigger does not flip its perspective" $ do
    hag <- Registry.printing registry "Hag of Inner Weakness"
    warMammoth <- Registry.printing registry "War Mammoth"
    piker <- Registry.printing registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (hagId, gs1) = S.addCreature hag S.alice gs0
        (_, gs2) = S.addCreature piker S.alice gs1
        (mammoth, gs3) = S.addCreature warMammoth S.bob gs2
        -- Only bob's Mammoth is "a creature an opponent controls" for alice,
        -- so the CR 603.3d target choice is forced.
        staged = hagSettle (hagBeginUpkeep gs3)
        stolen = S.giveControl hagId S.bob staged
        resolved = hagResolveAll stolen
    Spec.assertEqWith s "the Hag is bob's now" (Projection.controllerOf hagId stolen) (Just S.bob)
    Spec.assertEqWith s "but the trigger still resolved against bob's Mammoth" (Projection.powerOf mammoth resolved) (Just 1)
    Spec.assertEqWith s "and its toughness" (Projection.toughnessOf mammoth resolved) (Just 2)

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Expiry" $ do
  armSpec s
  cleanupSpec s registry
  handoffSpec s
  conditionalSpec s registry
  masterThiefSpec s registry
  monarchSpec s registry
  hagSpec s registry
