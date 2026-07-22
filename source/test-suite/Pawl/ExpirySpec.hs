-- Covers Pawl.Expiry and Pawl.Type.Expiry: the printed Duration -> stored Expiry
-- arming (CR 611.2), the sweeps that end a duration (CR 514.2, 611.2a, 611.2b),
-- and the two gate cards (Master Thief, Hag of Inner Weakness).
module Pawl.ExpirySpec where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.DestructionRewrite as DestructionRewrite
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Expiry as Expiry.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.StateCondition as StateCondition
import qualified Pawl.Type.Uses as Uses
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- A stored continuous effect with a chosen expiry, over a stand-in target.
-- Object id 998 is the stand-in source (Support.withEffectAt's posture);
-- nothing here reads the source's characteristics.
effectWith :: Expiry.Type.Expiry -> GameState.GameState -> GameState.GameState
effectWith expiry gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 998,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = expiry,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Flying,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton (ObjectId.MkObjectId 999))
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- A stand-in source and state for the three arms that don't consult either --
-- only Duration.ForAsLongAs reads them.
armGs :: GameState.GameState
armGs = Setup.emptyGame S.bothPlayers

armSource :: ObjectId.ObjectId
armSource = ObjectId.MkObjectId 999

armTests :: Tasty.TestTree
armTests =
  Tasty.testGroup
    "Arm"
    [ HU.testCase "CR 514.2 an until-end-of-turn duration arms to AtCleanup" $
        HU.assertEqual "armed" (Just Expiry.Type.AtCleanup) (Expiry.arm S.alice armSource Duration.UntilEndOfTurn armGs),
      HU.testCase "CR 611.2a an indefinite duration arms to Never" $
        HU.assertEqual "armed" (Just Expiry.Type.Never) (Expiry.arm S.alice armSource Duration.Indefinite armGs),
      HU.testCase "CR 611.2a / 109.5 'until your next turn' bakes the controller" $
        HU.assertEqual "armed" (Just (Expiry.Type.AtTurnOf S.alice)) (Expiry.arm S.alice armSource Duration.UntilYourNextTurn armGs)
    ]

handoffTests :: Tasty.TestTree
handoffTests =
  Tasty.testGroup
    "DropAtHandoff"
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
         in HU.assertEqual "bob's turn ends bob's effect" [] (GameState.continuousEffects bobsTurn)
    ]

cleanupTests :: Cards.Cards -> Tasty.TestTree
cleanupTests cards =
  Tasty.testGroup
    "DropAtCleanup"
    [ HU.testCase "CR 514.2 cleanup drops an AtCleanup continuous effect and keeps a Never one" $
        let gs0 = Setup.emptyGame S.bothPlayers
            gs1 = effectWith Expiry.Type.Never (effectWith Expiry.Type.AtCleanup gs0)
            after = Expiry.dropAtCleanup gs1
         in do
              HU.assertEqual "two stored before" 2 (length (GameState.continuousEffects gs1))
              HU.assertEqual "one survives" [Expiry.Type.Never] (map ContinuousEffect.expiry (GameState.continuousEffects after)),
      HU.testCase "CR 514.2 the same sweep drops an AtCleanup floating replacement" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (oid, gs1) = S.addPiker cards S.alice gs0
            shielded = S.addRegenShield oid gs1
            after = Expiry.dropAtCleanup shielded
         in do
              HU.assertEqual "one shield before" 1 (length (GameState.replacements shielded))
              HU.assertEqual "none after" [] (map ActiveReplacement.expiry (GameState.replacements after))
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
            ContinuousEffect.expiry = Expiry.Type.While you StateCondition.YouControlSource,
            ContinuousEffect.modification = Modification.SetController you,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

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
            ActiveReplacement.expiry = Expiry.Type.While you StateCondition.YouControlSource,
            ActiveReplacement.uses = Uses.Unlimited
          }
   in S.addReplacement active gs1

conditionalTests :: Cards.Cards -> Tasty.TestTree
conditionalTests cards =
  let board =
        let gs0 = Setup.emptyGame S.bothPlayers
            (srcId, gs1) = S.addPiker cards S.alice gs0
            (targetId, gs2) = S.addCreature (Cards.warMammothPrinting cards) S.bob gs1
         in (srcId, targetId, whileEffect srcId targetId S.alice gs2)
   in Tasty.testGroup
        "Conditional"
        [ HU.testCase "CR 611.2b YouControlSource holds while the source is controlled" $
            let (srcId, _, gs) = board
             in HU.assertBool "holds" (Event.stateHolds S.alice srcId StateCondition.YouControlSource gs),
          HU.testCase "CR 613.1b it stops holding when another player gains control of the source" $
            let (srcId, _, gs) = board
                stolen = S.giveControl srcId S.bob gs
             in HU.assertBool "no longer holds" (not (Event.stateHolds S.alice srcId StateCondition.YouControlSource stolen)),
          HU.testCase "CR 400.7 it stops holding when the source leaves the battlefield" $
            let (srcId, _, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
             in HU.assertBool "no longer holds" (not (Event.stateHolds S.alice srcId StateCondition.YouControlSource gone)),
          HU.testCase "CR 611.2b arm returns Nothing when the condition is already false" $
            let (srcId, _, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
             in HU.assertEqual
                  "never starts"
                  Nothing
                  (Expiry.arm S.alice srcId (Duration.ForAsLongAs StateCondition.YouControlSource) gone),
          HU.testCase "CR 611.2b arm returns a While when the condition holds now" $
            let (srcId, _, gs) = board
             in HU.assertEqual
                  "starts"
                  (Just (Expiry.Type.While S.alice StateCondition.YouControlSource))
                  (Expiry.arm S.alice srcId (Duration.ForAsLongAs StateCondition.YouControlSource) gs),
          HU.testCase "CR 611.2b the sweep DELETES the effect once the condition fails" $
            let (srcId, targetId, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
                (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
             in do
                  HU.assertEqual "alice held it while the source stood" (Just S.alice) (Projection.controllerOf targetId gs)
                  HU.assertBool "the sweep reports a change" changed
                  HU.assertEqual "the effect is gone, not masked" [] (GameState.continuousEffects swept)
                  HU.assertEqual "control reverted" (Just S.bob) (Projection.controllerOf targetId swept),
          HU.testCase "CR 611.2b a sweep that changes nothing reports False" $
            let (_, _, gs) = board
                (changed, _) = Engine.runGamePure S.identityAnswer gs Expiry.sweepConditional
             in HU.assertBool "no change" (not changed),
          HU.testCase "CR 704.3 settleForPriority runs the sweep" $
            let (srcId, targetId, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
                settled = S.runPure S.identityAnswer gone Engine.settleForPriority
             in HU.assertEqual "control reverted at the settle" (Just S.bob) (Projection.controllerOf targetId settled),
          HU.testCase "CR 611.2b the sweep's replacements half survives while the source stands, then deletes once it doesn't" $
            let (srcId, _, gs0) = board
                gs = whileReplacement srcId S.alice gs0
                (unchanged, stillUp) = Engine.runGamePure S.identityAnswer gs Expiry.sweepConditional
                -- A direct zone change, NOT Event.destroy: the fixture's own
                -- payload is a DestructionR (Regenerate), so routing the removal
                -- through the destruction funnel would let it replace/regenerate
                -- itself instead of leaving the battlefield.
                gone = S.runPure S.identityAnswer gs (Event.changeZone srcId Zone.Graveyard)
                (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
             in do
                  HU.assertBool "no change while the source stands" (not unchanged)
                  HU.assertEqual "the replacement survives" 1 (length (GameState.replacements stillUp))
                  HU.assertBool "the sweep reports a change once the source is gone" changed
                  HU.assertEqual "the replacement is gone" [] (GameState.replacements swept)
        ]

-- Does any stored continuous effect name `target` in its affected set? Used to
-- tell "nothing was stored FOR THIS OBJECT" apart from an unrelated entry
-- already in play (S.giveControl's own AtCleanup SetController on the object
-- whose control moved, which is not what CR 611.2b's "never starts" is about).
affects :: ObjectId.ObjectId -> ContinuousEffect.ContinuousEffect -> Bool
affects target eff = case ContinuousEffect.affected eff of
  Affected.TheseObjects ids -> Set.member target ids
  _ -> False

-- Master Thief {2}{U}{U} Creature -- Human Rogue 2/2: "When this creature
-- enters, gain control of target artifact for as long as you control this
-- creature." CR 611.2b's own printed example; the three assertions below in
-- tests 2-4 are its three Gatherer rulings, verbatim.
masterThiefTests :: Cards.Cards -> Tasty.TestTree
masterThiefTests cards =
  let settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      -- bob's Darksteel Myr (Artifact Creature -- Myr, 0/1) is the only artifact
      -- on the board, so the CR 603.3d target choice is forced.
      board =
        let gs0 = Setup.emptyGame S.bothPlayers
            (myrId, gs1) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob gs0
            (thiefId, gs2) = S.addCreature (Cards.masterThiefPrinting cards) S.alice gs1
            entered = ZoneChange.MkZoneChange thiefId Zone.Stack Zone.Battlefield
            gs3 = S.withEvent (GameEvent.Moved entered (Projection.project thiefId gs2)) gs2
         in (thiefId, myrId, gs3)
      (thief, myr, entering) = board
      stolen = resolveAll (settle entering)
   in Tasty.testGroup
        "MasterThief"
        [ HU.testCase "CR 611.2b it works: the ETB resolves and control of the artifact changes" $
            do
              HU.assertEqual "alice controls the Myr" (Just S.alice) (Projection.controllerOf myr stolen)
              -- CR 302.6: the new controller has not controlled it continuously.
              HU.assertEqual "and it is re-Sicked" (Just Sickness.Sick) (fmap Object.sickness (Game.lookupObject myr stolen)),
          -- Ruling: "If Master Thief leaves the battlefield, you no longer
          -- control it, and its control-change effect ends."
          HU.testCase "CR 611.2b leaving the battlefield ends it" $
            let dead = S.runPure S.identityAnswer stolen (Event.destroy thief)
                swept = settle dead
             in do
                  HU.assertEqual "control reverts at the next settle" (Just S.bob) (Projection.controllerOf myr swept)
                  HU.assertEqual "and stays reverted" (Just S.bob) (Projection.controllerOf myr (settle swept)),
          -- Ruling: "If Master Thief ceases to be under your control before its
          -- ability resolves, you won't gain control of the targeted artifact at
          -- all." CR 704.5g destroys it for lethal damage while the trigger is
          -- on the stack; the trigger still RESOLVES (its target is legal, CR
          -- 608.2b), but the duration never starts.
          HU.testCase "CR 611.2b the duration never starts, so no effect is stored" $
            let onStack = settle entering
                lethal = S.settleSba (S.markDamage thief 2 onStack)
                after = resolveAll lethal
             in do
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
          -- all." Falsifies the CONTROL half of Event.stateHolds's YouControlSource
          -- conjunct (CR 611.2b/613.1b/400.7): Master Thief stays on the
          -- battlefield the whole time -- only its controller changes -- so this
          -- case cannot pass for the "left the battlefield" reason the sibling
          -- case above covers. End to end through the real pipeline (CR 113.7a/
          -- 603.3a: the ability's controller is alice, frozen at trigger time, and
          -- Resolve.resolveEffects must read that frozen value rather than bob's
          -- live control of the thief).
          HU.testCase "CR 611.2b ceasing to be under your control (not leaving the battlefield) also stops it" $
            let onStack = settle entering
                taken = S.giveControl thief S.bob onStack
                after = resolveAll taken
             in do
                  HU.assertBool "the trigger really was on the stack" (not (null (GameState.stack onStack)))
                  HU.assertEqual "Master Thief is still on the battlefield, under bob" (Just S.bob) (Projection.controllerOf thief taken)
                  HU.assertBool "Master Thief is still on the battlefield after resolution" (Maybe.isJust (Game.lookupObject thief after))
                  HU.assertEqual "control never changed" (Just S.bob) (Projection.controllerOf myr after)
                  -- Filtered to the ARTIFACT: `taken`'s own S.giveControl fixture
                  -- already stored an unrelated AtCleanup SetController effect on
                  -- the thief itself, so a blanket `[] == continuousEffects` would
                  -- fail for a reason that has nothing to do with this bug.
                  HU.assertEqual "nothing was stored for the artifact" [] (filter (affects myr) (GameState.continuousEffects after))
                  -- CR 302.6: a control-change stored by GainControl re-Sicks the
                  -- target; the duration never starting must leave that untouched.
                  HU.assertEqual "and was never re-Sicked" (Just Sickness.Settled) (fmap Object.sickness (Game.lookupObject myr after)),
          -- Ruling: "If another player gains control of Master Thief, its
          -- control-change effect ends. Regaining control of Master Thief won't
          -- cause you to regain control of the artifact." THE FALSIFIER: an
          -- implementation that filters the effect out of the projection while
          -- the condition is false, instead of deleting it, fails exactly here.
          HU.testCase "CR 611.2b the latch: regaining the source does not regain the artifact" $
            let taken = S.giveControl thief S.bob stolen
                swept = settle taken
                returned = Expiry.dropAtCleanup swept
                relatched = settle returned
             in do
                  HU.assertEqual "bob has Master Thief" (Just S.bob) (Projection.controllerOf thief taken)
                  HU.assertEqual "so the artifact goes back to its owner" (Just S.bob) (Projection.controllerOf myr swept)
                  HU.assertEqual "at cleanup Master Thief comes home" (Just S.alice) (Projection.controllerOf thief returned)
                  HU.assertEqual "and the artifact does NOT" (Just S.bob) (Projection.controllerOf myr relatched)
        ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.ExpirySpec" [armTests, cleanupTests cards, handoffTests, conditionalTests cards, masterThiefTests cards]
