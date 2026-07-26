-- Covers Pawl.Expiry and Pawl.Type.Expiry: the printed Duration -> stored Expiry
-- arming (CR 611.2), the sweeps that end a duration (CR 514.2, 611.2a, 611.2b),
-- and the two gate cards (Master Thief, Hag of Inner Weakness).
module Pawl.ExpirySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Condition as Condition
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Departure as Departure.Type
import qualified Pawl.Type.DestructionRewrite as DestructionRewrite
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Expiry as Expiry.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.MonarchWatch as MonarchWatch
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Uses as Uses
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

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

armTests :: Tasty.TestTree
armTests =
  Tasty.testGroup
    "Arm"
    [ HU.testCase "CR 514.2 an until-end-of-turn duration arms to AtCleanup" $
        HU.assertEqual "armed" (Just Expiry.Type.AtCleanup) (Expiry.arm S.alice S.noSource Duration.UntilEndOfTurn armGs),
      HU.testCase "CR 611.2a an indefinite duration arms to Never" $
        HU.assertEqual "armed" (Just Expiry.Type.Never) (Expiry.arm S.alice S.noSource Duration.Indefinite armGs),
      HU.testCase "CR 611.2a / 109.5 'until your next turn' bakes the controller" $
        HU.assertEqual "armed" (Just (Expiry.Type.AtTurnOf S.alice)) (Expiry.arm S.alice S.noSource Duration.UntilYourNextTurn armGs)
    ]

handoffTests :: Tasty.TestTree
handoffTests =
  Tasty.testGroup
    "DropAtTurnOf"
    [ HU.testCase "CR 611.2a an AtTurnOf effect ends as that player's turn begins, not before" $
        let gs0 = Setup.emptyGame S.bothPlayers
            -- alice is the active player; the effect ends at ALICE's next turn.
            armed = effectWith (Expiry.Type.AtTurnOf S.alice) gs0
            bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
            alicesTurn = S.runPure S.identityAnswer bobsTurn Engine.handoffTurn
         in do
              HU.assertEqual "alice is active when it is created" S.alice (GameState.activePlayer armed)
              HU.assertEqual "it survives the creating turn's handoff" 1 (length (GameState.continuousEffects bobsTurn))
              HU.assertEqual "bob is active" S.bob (GameState.activePlayer bobsTurn)
              HU.assertEqual "it ends as alice's next turn begins" [] (GameState.continuousEffects alicesTurn),
      HU.testCase "CR 514.2 does not touch an AtTurnOf effect" $
        let gs0 = Setup.emptyGame S.bothPlayers
            armed = effectWith (Expiry.Type.AtTurnOf S.alice) gs0
         in HU.assertEqual "survives cleanup" 1 (length (GameState.continuousEffects (Expiry.dropAtCleanup armed))),
      HU.testCase "CR 611.2a the sweep is scoped to the player whose turn began" $
        let gs0 = Setup.emptyGame S.bothPlayers
            armed = effectWith (Expiry.Type.AtTurnOf S.bob) gs0
            bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
         in HU.assertEqual "bob's turn ends bob's effect" [] (GameState.continuousEffects bobsTurn),
      HU.testCase "CR 611.2a dropAtTurnOf ends the NAMED player's AtTurnOf effects, whoever is active" $
        -- alice is the active player throughout; the sweep is told to fire for
        -- BOB, and does. That divorce from GameState.activePlayer is the whole
        -- generalization -- it is what lets CR 800.4m fire at a seat whose turn
        -- never begins.
        let gs0 = S.threePlayerGame
            armed = effectWith (Expiry.Type.AtTurnOf S.alice) (effectWith (Expiry.Type.AtTurnOf S.bob) gs0)
            after = Expiry.dropAtTurnOf S.bob armed
         in do
              HU.assertEqual "alice is still the active player" S.alice (GameState.activePlayer after)
              HU.assertEqual "only alice's effect survives" [Expiry.Type.AtTurnOf S.alice] (fmap ContinuousEffect.expiry (GameState.continuousEffects after)),
      HU.testCase "CR 611.2a dropAtTurnOf touches no other expiry" $
        let gs0 = S.threePlayerGame
            armed = effectWith Expiry.Type.Never (effectWith Expiry.Type.AtCleanup (effectWith (Expiry.Type.AtTurnOf S.bob) gs0))
            after = Expiry.dropAtTurnOf S.bob armed
         in HU.assertEqual "Never and AtCleanup both survive" 2 (length (GameState.continuousEffects after)),
      HU.testCase "CR 800.4k/800.4m a departed seat is skipped, and its durations end there anyway" $
        -- alice, bob, carol. Bob departs, then alice's turn ends. CR 800.4k: bob's
        -- turn doesn't begin, so carol becomes active. CR 800.4m: bob's "until
        -- your next turn" effect ends AT BOB'S SEAT -- not immediately when he
        -- left, and not never.
        let gone = Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame
            armed = effectWith (Expiry.Type.AtTurnOf S.bob) gone
            after = S.runPure S.identityAnswer armed Engine.handoffTurn
         in do
              HU.assertEqual "it survived bob's departure itself" 1 (length (GameState.continuousEffects armed))
              HU.assertEqual "carol takes the turn, not bob" S.carol (GameState.activePlayer after)
              HU.assertEqual "bob's effect ended at bob's seat" [] (GameState.continuousEffects after),
      HU.testCase "CR 800.4m the walk sweeps only the seats it passes" $
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
         in do
              HU.assertEqual "carol takes the turn, not bob" S.carol (GameState.activePlayer after)
              HU.assertEqual "alice's survives, bob's does not" [Expiry.Type.AtTurnOf S.alice] (fmap ContinuousEffect.expiry (GameState.continuousEffects after)),
      HU.testCase "CR 800.4m the sweep reaches a seat the walk only passes through, not just the seat it lands next to" $
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
         in do
              HU.assertEqual "alice takes the turn (wrapping past both departed seats)" S.alice (GameState.activePlayer after)
              HU.assertEqual "carol's effect ended at carol's seat, two hops past alice" [] (GameState.continuousEffects after)
    ]

cleanupTests :: Registry.Type.Registry -> Tasty.TestTree
cleanupTests registry =
  Tasty.testGroup
    "DropAtCleanup"
    [ HU.testCase "CR 514.2 cleanup drops an AtCleanup continuous effect and keeps a Never one" $
        let gs0 = Setup.emptyGame S.bothPlayers
            gs1 = effectWith Expiry.Type.Never (effectWith Expiry.Type.AtCleanup gs0)
            after = Expiry.dropAtCleanup gs1
         in do
              HU.assertEqual "two stored before" 2 (length (GameState.continuousEffects gs1))
              HU.assertEqual "one survives" [Expiry.Type.Never] (fmap ContinuousEffect.expiry (GameState.continuousEffects after)),
      HU.testCase "CR 514.2 the same sweep drops an AtCleanup floating replacement" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (oid, gs1) = S.addCreature piker S.alice gs0
            shielded = S.addRegenShield oid gs1
            after = Expiry.dropAtCleanup shielded
        HU.assertEqual "one shield before" 1 (length (GameState.replacements shielded))
        HU.assertEqual "none after" [] (fmap ActiveReplacement.expiry (GameState.replacements after))
    ]

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
-- projection (outside the layer fold, per Pawl.Condition's spec) and the one
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

conditionalTests :: Registry.Type.Registry -> Tasty.TestTree
conditionalTests registry =
  Tasty.testGroup
    "Conditional"
    [ HU.testCase "CR 611.2b YouControlSource holds while the source is controlled" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (srcId, _, gs) = board piker warMammoth
        HU.assertBool "holds" (holdsYouControlSource S.alice srcId gs),
      HU.testCase "CR 613.1b it stops holding when another player gains control of the source" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (srcId, _, gs) = board piker warMammoth
            stolen = S.giveControl srcId S.bob gs
        HU.assertBool "no longer holds" (not (holdsYouControlSource S.alice srcId stolen)),
      HU.testCase "CR 400.7 it stops holding when the source leaves the battlefield" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (srcId, _, gs) = board piker warMammoth
            gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
        HU.assertBool "no longer holds" (not (holdsYouControlSource S.alice srcId gone)),
      HU.testCase "CR 611.2b arm returns Nothing when the condition is already false" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (srcId, _, gs) = board piker warMammoth
            gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
        HU.assertEqual
          "never starts"
          Nothing
          (Expiry.arm S.alice srcId (Duration.ForAsLongAs S.youControlSource) gone),
      HU.testCase "CR 611.2b arm returns a While when the condition holds now" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (srcId, _, gs) = board piker warMammoth
        HU.assertEqual
          "starts"
          (Just (Expiry.Type.While S.alice S.youControlSource))
          (Expiry.arm S.alice srcId (Duration.ForAsLongAs S.youControlSource) gs),
      HU.testCase "CR 611.2b the sweep DELETES the effect once the condition fails" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (srcId, targetId, gs) = board piker warMammoth
            gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
            (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
        HU.assertEqual "alice held it while the source stood" (Just S.alice) (Projection.controllerOf targetId gs)
        HU.assertBool "the sweep reports a change" changed
        HU.assertEqual "the effect is gone, not masked" [] (GameState.continuousEffects swept)
        HU.assertEqual "control reverted" (Just S.bob) (Projection.controllerOf targetId swept),
      HU.testCase "CR 611.2b a sweep that changes nothing reports False" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (_, _, gs) = board piker warMammoth
            (changed, _) = Engine.runGamePure S.identityAnswer gs Expiry.sweepConditional
        HU.assertBool "no change" (not changed),
      HU.testCase "CR 704.3 settleForPriority runs the sweep" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (srcId, targetId, gs) = board piker warMammoth
            gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
            settled = S.runPure S.identityAnswer gone Engine.settleForPriority
        HU.assertEqual "control reverted at the settle" (Just S.bob) (Projection.controllerOf targetId settled),
      HU.testCase "CR 611.2b the sweep's replacements half survives while the source stands, then deletes once it doesn't" $ do
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
        HU.assertBool "no change while the source stands" (not unchanged)
        HU.assertEqual "the replacement survives" 1 (length (GameState.replacements stillUp))
        HU.assertBool "the sweep reports a change once the source is gone" changed
        HU.assertEqual "the replacement is gone" [] (GameState.replacements swept)
    ]

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
      entered = ZoneChange.MkZoneChange thiefId Zone.Stack Zone.Battlefield
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
      entered = ZoneChange.MkZoneChange thiefId Zone.Stack Zone.Battlefield
      gs3 = S.withEvents [GameEvent.Moved entered (Projection.project thiefId gs2)] gs2
   in (thiefId, myrId, masterThiefResolveAll (masterThiefSettle gs3))

-- Master Thief {2}{U}{U} Creature -- Human Rogue 2/2: "When this creature
-- enters, gain control of target artifact for as long as you control this
-- creature." CR 611.2b's own printed example; the three assertions below in
-- tests 2-4 are its three Gatherer rulings, verbatim.
masterThiefTests :: Registry.Type.Registry -> Tasty.TestTree
masterThiefTests registry =
  Tasty.testGroup
    "MasterThief"
    [ HU.testCase "CR 611.2b it works: the ETB resolves and control of the artifact changes" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        masterThief <- Registry.printing registry "Master Thief"
        let (_, myr, entering) = masterThiefBoard darksteelMyr masterThief
            stolen = masterThiefResolveAll (masterThiefSettle entering)
        HU.assertEqual "alice controls the Myr" (Just S.alice) (Projection.controllerOf myr stolen)
        -- CR 302.6: the new controller has not controlled it continuously.
        HU.assertEqual "and it is re-Sicked" (Just Sickness.Sick) (fmap Object.sickness (Game.lookupObject myr stolen)),
      -- Ruling: "If Master Thief leaves the battlefield, you no longer
      -- control it, and its control-change effect ends."
      HU.testCase "CR 611.2b leaving the battlefield ends it" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        masterThief <- Registry.printing registry "Master Thief"
        let (thief, myr, entering) = masterThiefBoard darksteelMyr masterThief
            stolen = masterThiefResolveAll (masterThiefSettle entering)
            dead = S.runPure S.identityAnswer stolen (Event.destroy thief)
            swept = masterThiefSettle dead
        HU.assertEqual "control reverts at the next settle" (Just S.bob) (Projection.controllerOf myr swept)
        HU.assertEqual "and stays reverted" (Just S.bob) (Projection.controllerOf myr (masterThiefSettle swept)),
      -- Ruling: "If Master Thief ceases to be under your control before its
      -- ability resolves, you won't gain control of the targeted artifact at
      -- all." CR 704.5g destroys it for lethal damage while the trigger is
      -- on the stack; the trigger still RESOLVES (its target is legal, CR
      -- 608.2b), but the duration never starts.
      HU.testCase "CR 611.2b the duration never starts, so no effect is stored" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        masterThief <- Registry.printing registry "Master Thief"
        let (thief, myr, entering) = masterThiefBoard darksteelMyr masterThief
            onStack = masterThiefSettle entering
            lethal = S.settleSba (S.markDamage thief 2 onStack)
            after = masterThiefResolveAll lethal
        HU.assertBool "the trigger really was on the stack" (not (null (GameState.stack onStack)))
        HU.assertEqual "Master Thief died before it resolved" Nothing (Game.lookupObject thief after)
        HU.assertEqual "nothing was stored" [] (GameState.continuousEffects after)
        HU.assertEqual "control never changed" (Just S.bob) (Projection.controllerOf myr after)
        -- CR 302.6: a control-change stored by GainControl re-Sicks the
        -- target; the duration never starting must leave that untouched.
        -- This is the discriminator settleForPriority's sweepConditional
        -- can't launder away: it proves nothing was EVER stored, not
        -- merely that nothing survived the sweep.
        HU.assertEqual "and was never re-Sicked" (Just Sickness.Settled) (fmap Object.sickness (Game.lookupObject myr after)),
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
      HU.testCase "CR 611.2b ceasing to be under your control (not leaving the battlefield) also stops it" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        masterThief <- Registry.printing registry "Master Thief"
        let (thief, myr, entering) = masterThiefBoard darksteelMyr masterThief
            onStack = masterThiefSettle entering
            taken = S.giveControl thief S.bob onStack
            after = masterThiefResolveAll taken
        HU.assertBool "the trigger really was on the stack" (not (null (GameState.stack onStack)))
        HU.assertEqual "Master Thief is still on the battlefield, under bob" (Just S.bob) (Projection.controllerOf thief taken)
        HU.assertBool "Master Thief is still on the battlefield after resolution" (Maybe.isJust (Game.lookupObject thief after))
        HU.assertEqual "control never changed" (Just S.bob) (Projection.controllerOf myr after)
        -- Filtered to the ARTIFACT: `taken`'s own S.giveControl fixture
        -- already stored an unrelated AtCleanup SetController effect on
        -- the thief itself, so a blanket `[] == continuousEffects` would
        -- fail for a reason that has nothing to do with this bug.
        HU.assertEqual "nothing was stored for the artifact" [] (filter (S.continuousEffectAffects myr) (GameState.continuousEffects after))
        -- CR 302.6: a control-change stored by GainControl re-Sicks the
        -- target; the duration never starting must leave that untouched.
        HU.assertEqual "and was never re-Sicked" (Just Sickness.Settled) (fmap Object.sickness (Game.lookupObject myr after)),
      -- Ruling: "If another player gains control of Master Thief, its
      -- control-change effect ends. Regaining control of Master Thief won't
      -- cause you to regain control of the artifact." THE FALSIFIER: an
      -- implementation that filters the effect out of the projection while
      -- the condition is false, instead of deleting it, fails exactly here.
      HU.testCase "CR 611.2b the latch: regaining the source does not regain the artifact" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        masterThief <- Registry.printing registry "Master Thief"
        let (thief, myr, entering) = masterThiefBoard darksteelMyr masterThief
            stolen = masterThiefResolveAll (masterThiefSettle entering)
            taken = S.giveControl thief S.bob stolen
            swept = masterThiefSettle taken
            returned = Expiry.dropAtCleanup swept
            relatched = masterThiefSettle returned
        HU.assertEqual "bob has Master Thief" (Just S.bob) (Projection.controllerOf thief taken)
        HU.assertEqual "so the artifact goes back to its owner" (Just S.bob) (Projection.controllerOf myr swept)
        HU.assertEqual "at cleanup Master Thief comes home" (Just S.alice) (Projection.controllerOf thief returned)
        HU.assertEqual "and the artifact does NOT" (Just S.bob) (Projection.controllerOf myr relatched),
      -- CR 800.4a, third example: "If Bianca leaves the game, Serra Angel also
      -- leaves the game." The stolen object is owned by the departing player, so
      -- it goes with them -- the thief keeps nothing.
      HU.testCase "CR 800.4a the stolen artifact's OWNER departs: the artifact leaves the game and Master Thief stays" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        masterThief <- Registry.printing registry "Master Thief"
        let (thief, myr, stolen) = masterThiefThreeWay darksteelMyr masterThief
            gone = Departure.depart Departure.Type.Conceded S.bob stolen
        HU.assertEqual "alice really had it before bob left" (Just S.alice) (Projection.controllerOf myr stolen)
        HU.assertEqual "the Myr is gone from the game" Nothing (Game.lookupObject myr gone)
        HU.assertEqual "so it has no controller" Nothing (Projection.controllerOf myr gone)
        HU.assertBool "Master Thief is still on the battlefield" (Maybe.isJust (Game.lookupObject thief gone))
        HU.assertEqual "under alice" (Just S.alice) (Projection.controllerOf thief gone)
        -- The stored SetController effect stays, with an `affected` set naming an
        -- object that no longer exists. It is inert: Projection.controllerOf
        -- returns Nothing for an unknown id, and GameState.nextObjectId is
        -- monotone so the id is never reused. Pinned rather than tidied, because
        -- CR 800.4a ends only the effects that GIVE the departing player control
        -- and this one gives control to alice, who is still here.
        HU.assertEqual "the effect that named it is still stored, and inert" 1 (length (GameState.continuousEffects gone))
        HU.assertEqual "CR 104.2a: two survivors, so the game continues" Nothing (GameState.result gone),
      -- CR 800.4a, first example: "If Alex leaves the game, so does Mind Control,
      -- and Assault Griffin reverts to Bianca's control." CR 800.4a's second
      -- example says the same for Act of Treason's change-of-control effect.
      -- Master Thief is a creature rather than an Aura, so the thief simply
      -- leaves; what matters is that the effect ends AT THE DEPARTURE and not at
      -- some later sweep.
      HU.testCase "CR 800.4a the THIEF departs: the control effect ends immediately and the artifact reverts" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        masterThief <- Registry.printing registry "Master Thief"
        let (thief, myr, stolen) = masterThiefThreeWay darksteelMyr masterThief
            gone = Departure.depart Departure.Type.Conceded S.alice stolen
        HU.assertEqual "alice really had it before she left" (Just S.alice) (Projection.controllerOf myr stolen)
        HU.assertEqual "Master Thief left the game with its owner" Nothing (Game.lookupObject thief gone)
        HU.assertEqual "the Myr is still in the game" (Just S.bob) (fmap Object.owner (Game.lookupObject myr gone))
        HU.assertBool "and still on the battlefield" (Set.member myr (GameState.battlefield gone))
        HU.assertEqual "under bob again" (Just S.bob) (Projection.controllerOf myr gone)
        -- The discriminator against "a sweep would have got there eventually":
        -- CR 800.4a says "It happens as soon as the player leaves the game", and
        -- Expiry.sweepConditional runs at the next settle, not now.
        HU.assertEqual "no stored effect survives the departure itself" [] (GameState.continuousEffects gone)
        HU.assertEqual "CR 104.2a: two survivors, so the game continues" Nothing (GameState.result gone)
    ]

-- CR 725.2: the monarch's inherent end-step draw -- a triggered ability that
-- belongs to NO object. The falsifier: no permanents on the battlefield at all,
-- so the draw cannot hang on a bearer.
monarchSettle :: GameState.GameState -> GameState.GameState
monarchSettle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

monarchResolveAll :: GameState.GameState -> GameState.GameState
monarchResolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop

monarchTests :: Registry.Type.Registry -> Tasty.TestTree
monarchTests registry =
  Tasty.testGroup
    "Monarch"
    [ HU.testCase "CR 725.2 the monarch draws at the beginning of their own end step" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (_, gs0) = S.addLibraryCard piker S.alice (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
            began = S.withEvents [GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice] gs0
            after = monarchResolveAll (monarchSettle began)
        HU.assertEqual "alice drew (one card now in hand)" 1 (length (Game.zoneMembers Zone.Hand S.alice after)),
      HU.testCase "CR 725.2 the end-step draw fires only on the monarch's own end step" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (_, gs0) = S.addLibraryCard piker S.bob (S.withMonarch S.bob (Setup.emptyGame S.bothPlayers))
            -- alice is the active player; her end step is not bob's (the monarch).
            began = S.withEvents [GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice] gs0
            after = monarchResolveAll (monarchSettle began)
        HU.assertEqual "bob did not draw on alice's end step" 0 (length (Game.zoneMembers Zone.Hand S.bob after)),
      HU.testCase "CR 725.2 combat damage to the monarch hands the crown to the damager's controller" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (bobCreature, gs0) = S.addCreature piker S.bob (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
            dmg = DamageEvent.MkDamageEvent bobCreature (Recipient.ToPlayer S.alice) 2 False False DamageKind.Combat
            began = S.withEvents [GameEvent.DamageDealt dmg] gs0
            after = monarchResolveAll (monarchSettle began)
        HU.assertEqual "bob took the crown" (Just S.bob) (GameState.monarch after),
      HU.testCase "CR 725.2 noncombat damage to the monarch does not hand over the crown" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (bobCreature, gs0) = S.addCreature piker S.bob (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
            dmg = DamageEvent.MkDamageEvent bobCreature (Recipient.ToPlayer S.alice) 2 False False DamageKind.Noncombat
            began = S.withEvents [GameEvent.DamageDealt dmg] gs0
            after = monarchResolveAll (monarchSettle began)
        HU.assertEqual "alice keeps the crown" (Just S.alice) (GameState.monarch after),
      HU.testCase "CR 725 Palace Jailer: ETB makes the caster monarch and exiles an opponent's creature until an opponent takes the crown" $ do
        piker <- Registry.printing registry "Goblin Piker"
        palaceJailer <- Registry.printing registry "Palace Jailer"
        let gs0 = Setup.emptyGame S.bothPlayers
            (victim, gs1) = S.addCreature piker S.bob gs0
            (jailer, gs2) = S.addCreature palaceJailer S.alice gs1
            entered = ZoneChange.MkZoneChange jailer Zone.Stack Zone.Battlefield
            gs3 = S.withEvents [GameEvent.Moved entered (Projection.project jailer gs2)] gs2
            afterEtb = monarchResolveAll (monarchSettle gs3)
            -- caster stays monarch across a turn boundary: exile holds.
            heldExiled = monarchSettle afterEtb
            -- an opponent (bob) takes the crown: the creature returns.
            afterSteal = monarchResolveAll (monarchSettle (S.withMonarch S.bob heldExiled))
        HU.assertEqual "alice is monarch on ETB" (Just S.alice) (GameState.monarch afterEtb)
        HU.assertEqual "victim is exiled" 0 (length (filter (== victim) (Set.toList (GameState.battlefield afterEtb))))
        HU.assertBool "victim registered for return" (not (Map.null (GameState.exiledUntilMonarch afterEtb)))
        HU.assertBool "still exiled while alice stays monarch" (not (Map.null (GameState.exiledUntilMonarch heldExiled)))
        HU.assertEqual "a creature is back on the battlefield once bob is monarch" 1 (length (Game.zoneMembers Zone.Battlefield S.bob afterSteal))
        HU.assertEqual "return cleared the exile register" True (Map.null (GameState.exiledUntilMonarch afterSteal)),
      HU.testCase "M5.6c gate: the monarch holding a Palace Jailer exile leaves, CR 725.4 crowns the active player, and the prisoner comes home" $ do
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
            entered = ZoneChange.MkZoneChange jailer Zone.Stack Zone.Battlefield
            gs3 = S.withEvents [GameEvent.Moved entered (Projection.project jailer gs2)] gs2
            afterEtb = monarchResolveAll (monarchSettle (gs3 {GameState.activePlayer = S.carol}))
            gone = Departure.depart Departure.Type.Conceded S.alice afterEtb
            settled = monarchSettle gone
        HU.assertEqual "alice is the monarch on ETB" (Just S.alice) (GameState.monarch afterEtb)
        HU.assertEqual "bob's creature is exiled under the watch, keyed to alice" [S.alice] (fmap MonarchWatch.controller (Map.elems (GameState.exiledUntilMonarch afterEtb)))
        HU.assertEqual "the original is off the battlefield" 0 (length (filter (== victim) (Set.toList (GameState.battlefield afterEtb))))
        -- CR 800.4a: alice's own object leaves; bob's exiled card does not.
        HU.assertEqual "Palace Jailer left the game with alice" Nothing (Game.lookupObject jailer gone)
        HU.assertEqual "but the watch survived her departure, still keyed to her" [S.alice] (fmap MonarchWatch.controller (Map.elems (GameState.exiledUntilMonarch gone)))
        -- CR 725.4, first sentence.
        HU.assertEqual "carol, the active player, is the monarch" (Just S.carol) (GameState.monarch gone)
        -- CR 800.4i: carol is in departed alice's frozen opponent set, so the
        -- watch fires at the next settle (CR 704.3 fixes that as the coarsest
        -- moment anything can observe it).
        HU.assertEqual "the prisoner is back on bob's side of the table" 1 (length (Game.zoneMembers Zone.Battlefield S.bob settled))
        HU.assertEqual "and the watch is cleared" True (Map.null (GameState.exiledUntilMonarch settled))
        HU.assertEqual "CR 104.2a: two survivors, so the game continues" Nothing (GameState.result settled)
    ]

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
hagTests :: Registry.Type.Registry -> Tasty.TestTree
hagTests registry =
  Tasty.testGroup
    "HagOfInnerWeakness"
    [ HU.testCase "CR 613.4c it works: the opponent's 3/3 becomes 1/2" $ do
        hag <- Registry.printing registry "Hag of Inner Weakness"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (mammoth, afterTrigger) = hagBoardWith hag warMammoth
        HU.assertEqual "power" (Just 1) (Projection.powerOf mammoth afterTrigger)
        HU.assertEqual "toughness" (Just 2) (Projection.toughnessOf mammoth afterTrigger),
      -- THE FALSIFIER for both "treat it as until end of turn" and any
      -- implementation that expires the effect by scanning the event log for
      -- a matching StepBegan: the effect was CREATED on a turn whose untap
      -- step has already happened, so a log scan kills it the turn it is born.
      HU.testCase "CR 514.2 it survives cleanup and the whole of the opponent's turn" $ do
        hag <- Registry.printing registry "Hag of Inner Weakness"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (mammoth, afterTrigger) = hagBoardWith hag warMammoth
            ended = Expiry.dropAtCleanup afterTrigger
            bobsTurn = hagHandoff ended
            bobsTurnSwept = Expiry.dropAtCleanup bobsTurn
        HU.assertEqual "survives its own turn's CR 514.2 cleanup sweep" (Just 1) (Projection.powerOf mammoth ended)
        HU.assertEqual "bob is active" S.bob (GameState.activePlayer bobsTurn)
        HU.assertEqual "power still 1 at the start of bob's turn" (Just 1) (Projection.powerOf mammoth bobsTurn)
        HU.assertEqual "toughness still 2 at the start of bob's turn" (Just 2) (Projection.toughnessOf mammoth bobsTurn)
        HU.assertEqual "power survives bob's own CR 514.2 cleanup sweep too" (Just 1) (Projection.powerOf mammoth bobsTurnSwept)
        HU.assertEqual "toughness survives bob's own CR 514.2 cleanup sweep too" (Just 2) (Projection.toughnessOf mammoth bobsTurnSwept),
      HU.testCase "CR 611.2a it expires as the controller's next turn begins" $ do
        hag <- Registry.printing registry "Hag of Inner Weakness"
        warMammoth <- Registry.printing registry "War Mammoth"
        let (mammoth, afterTrigger) = hagBoardWith hag warMammoth
            alicesTurn = hagHandoff (hagHandoff (Expiry.dropAtCleanup afterTrigger))
        HU.assertEqual "alice is active again" S.alice (GameState.activePlayer alicesTurn)
        -- Asserted BEFORE the upkeep trigger fires a second time, so
        -- the two effects can never be confused.
        HU.assertEqual "back to 3/3" (Just 3) (Projection.powerOf mammoth alicesTurn)
        HU.assertEqual "back to 3/3" (Just 3) (Projection.toughnessOf mammoth alicesTurn)
        HU.assertEqual "nothing stored" [] (GameState.continuousEffects alicesTurn),
      HU.testCase "CR 704.5f the modification really applies: a 2/1 becomes 0/0 and dies" $ do
        hag <- Registry.printing registry "Hag of Inner Weakness"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, afterPiker) = hagBoardWith hag piker
        HU.assertEqual "bob's Piker is gone" 0 (S.creaturesInPlay S.bob afterPiker)
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Pawl.ExpirySpec" [armTests, cleanupTests registry, handoffTests, conditionalTests registry, masterThiefTests registry, monarchTests registry, hagTests registry]
