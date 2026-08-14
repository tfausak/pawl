{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Expiry and Pawl.Types.Expiry: the printed Duration -> stored Expiry
-- arming (CR 611.2), the sweeps that end a duration (CR 514.2, 500.5, 611.2a,
-- 611.2b), and the three gate cards (Master Thief, Hag of Inner Weakness, Jade
-- Statue).
module Pawl.ExpirySpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.AfterTurn as AfterTurn
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.While as While
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
    Spec.assertEqWith s "armed" (Expiry.arm Map.empty S.alice S.noSource Duration.UntilEndOfTurn armGs) (Just Expiry.Type.AtCleanup)
  Spec.it s "CR 611.2a an indefinite duration arms to Never" $
    Spec.assertEqWith s "armed" (Expiry.arm Map.empty S.alice S.noSource Duration.Indefinite armGs) (Just Expiry.Type.Never)
  Spec.it s "CR 611.2a / 109.5 'until your next turn' bakes the controller" $
    Spec.assertEqWith s "armed" (Expiry.arm Map.empty S.alice S.noSource Duration.UntilYourNextTurn armGs) (Just (Expiry.Type.AtTurnOf S.alice))
  -- The turn number is sampled as well as the seat, and armGs is turn 1. Without
  -- it the sweep could not tell the controller's CURRENT turn from their next
  -- one, which is exactly the case the two phrasings disagree about.
  Spec.it s "CR 611.2a / 109.5 'until the end of your next turn' bakes the controller AND the turn" $
    Spec.assertEqWith s "armed" (Expiry.arm Map.empty S.alice S.noSource Duration.UntilEndOfYourNextTurn armGs) (Just (Expiry.Type.AtEndOfTurnOf (AfterTurn.MkAfterTurn S.alice 1)))

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

-- One turn handoff, through the real Engine.handoffTurn -- the same idiom
-- handoffSpec above uses.
handoff :: GameState.GameState -> GameState.GameState
handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn

-- CR 611.2a's OTHER phrasing, swept at the cleanup step rather than at the
-- handoff. Stated against the AtTurnOf cases above, which are the reading this
-- one is not: every case here names the turn AtTurnOf would have ended on.
endOfNextTurnSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
endOfNextTurnSpec s = Spec.describe s "DropAtEndOfTurnOf" $ do
  -- THE case that separates the two readings. An AtTurnOf effect created on
  -- alice's own turn also survives here, so the assertion below is what makes
  -- this more than a repeat: it must still be alive at alice's NEXT turn.
  Spec.it s "CR 611.2a created on the controller's own turn, it survives that turn's cleanup and the opponent's" $ do
    let gs0 = Setup.emptyGame S.bothPlayers
        armed = effectWith (Expiry.Type.AtEndOfTurnOf (AfterTurn.MkAfterTurn S.alice 1)) gs0
        ownCleanup = Expiry.dropAtCleanup armed
        bobsTurn = Expiry.dropAtCleanup (handoff ownCleanup)
    Spec.assertEqWith s "alice is active on turn 1, the turn it was created on" (GameState.activePlayer armed, GameState.turnNumber armed) (S.alice, 1)
    Spec.assertEqWith s "its own turn's cleanup leaves it" (length (GameState.continuousEffects ownCleanup)) 1
    Spec.assertEqWith s "bob is active on turn 2" (GameState.activePlayer bobsTurn, GameState.turnNumber bobsTurn) (S.bob, 2)
    Spec.assertEqWith s "and bob's cleanup leaves it too" (length (GameState.continuousEffects bobsTurn)) 1
  Spec.it s "CR 611.2a it ends at the cleanup of the controller's next turn, not as that turn begins" $ do
    let gs0 = Setup.emptyGame S.bothPlayers
        armed = effectWith (Expiry.Type.AtEndOfTurnOf (AfterTurn.MkAfterTurn S.alice 1)) gs0
        alicesNext = handoff (handoff (Expiry.dropAtCleanup armed))
        ended = Expiry.dropAtCleanup alicesNext
    Spec.assertEqWith s "alice is active again on turn 3" (GameState.activePlayer alicesNext, GameState.turnNumber alicesNext) (S.alice, 3)
    -- Where an AtTurnOf effect is already gone (handoffSpec above).
    Spec.assertEqWith s "it is still there as that turn begins" (length (GameState.continuousEffects alicesNext)) 1
    Spec.assertEqWith s "and gone once that turn's cleanup runs" (GameState.continuousEffects ended) []
  -- The mirror board: created on an OPPONENT's turn, so the controller's next
  -- turn is the very next one and the effect ends one cleanup sooner. Same
  -- expiry, same sweeps, different arming turn -- so a sweep that ignored the
  -- turn number would answer the same for both and this pair pins it.
  Spec.it s "CR 611.2a created on an opponent's turn, it ends at the controller's next turn's cleanup" $ do
    let gs0 = handoff (Setup.emptyGame S.bothPlayers)
        armed = effectWith (Expiry.Type.AtEndOfTurnOf (AfterTurn.MkAfterTurn S.alice 2)) gs0
        alicesNext = handoff (Expiry.dropAtCleanup armed)
        ended = Expiry.dropAtCleanup alicesNext
    Spec.assertEqWith s "bob is active on turn 2, the turn it was created on" (GameState.activePlayer armed, GameState.turnNumber armed) (S.bob, 2)
    Spec.assertEqWith s "bob's cleanup leaves it" (length (GameState.continuousEffects (Expiry.dropAtCleanup armed))) 1
    Spec.assertEqWith s "alice is active on turn 3" (GameState.activePlayer alicesNext, GameState.turnNumber alicesNext) (S.alice, 3)
    Spec.assertEqWith s "and it ends at that turn's cleanup" (GameState.continuousEffects ended) []
  Spec.it s "CR 611.2a another player's cleanup never ends it, whatever the turn number" $ do
    let gs0 = S.threePlayerGame
        armed = effectWith (Expiry.Type.AtEndOfTurnOf (AfterTurn.MkAfterTurn S.bob 1)) gs0
        carolsTurn = handoff (handoff armed)
    Spec.assertEqWith s "carol is active on turn 3, past bob's own turn 2" (GameState.activePlayer carolsTurn, GameState.turnNumber carolsTurn) (S.carol, 3)
    Spec.assertEqWith s "carol's cleanup is not bob's" (length (GameState.continuousEffects (Expiry.dropAtCleanup carolsTurn))) 1
  Spec.it s "CR 611.2a the turn handoff leaves it alone: it ends at that turn's END" $ do
    let gs0 = Setup.emptyGame S.bothPlayers
        armed = effectWith (Expiry.Type.AtEndOfTurnOf (AfterTurn.MkAfterTurn S.alice 1)) gs0
        after = Expiry.dropAtTurnOf S.alice armed
    Spec.assertEqWith s "the sweep that ends an AtTurnOf effect keeps this one" (length (GameState.continuousEffects after)) 1
  -- CR 800.4m, the exception to the case above: a departed player's turn never
  -- begins, so the cleanup that would end this never comes and the rule ends it
  -- at the point that turn would have begun instead.
  Spec.it s "CR 800.4m a departed controller's effect ends at the seat their turn would have begun at" $ do
    let gone = Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame
        armed = effectWith (Expiry.Type.AtEndOfTurnOf (AfterTurn.MkAfterTurn S.bob 1)) gone
        after = handoff armed
    Spec.assertEqWith s "it survived bob's departure itself" (length (GameState.continuousEffects armed)) 1
    Spec.assertEqWith s "carol takes the turn, not bob" (GameState.activePlayer after) S.carol
    Spec.assertEqWith s "bob's effect ended at bob's seat" (GameState.continuousEffects after) []

cleanupSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cleanupSpec s registry = Spec.describe s "DropAtCleanup" $ do
  Spec.it s "CR 514.2 cleanup drops an AtCleanup continuous effect and keeps a Never one" $ do
    let gs0 = Setup.emptyGame S.bothPlayers
        gs1 = effectWith Expiry.Type.Never (effectWith Expiry.Type.AtCleanup gs0)
        after = Expiry.dropAtCleanup gs1
    Spec.assertEqWith s "two stored before" (length (GameState.continuousEffects gs1)) 2
    Spec.assertEqWith s "one survives" (fmap ContinuousEffect.expiry (GameState.continuousEffects after)) [Expiry.Type.Never]
  Spec.it s "CR 514.2 the same sweep drops an AtCleanup floating replacement" $ do
    piker <- S.printingOf s registry "Goblin Piker"
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
            ContinuousEffect.expiry = Expiry.Type.While (While.MkWhile you S.youControlSource),
            ContinuousEffect.modification = Modification.SetController you,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- Event.stateHolds's retired (you, source, cond, gs) shape, over the FULL
-- projection (outside the layer fold, per Pawl.Engine.Condition's spec) and the one
-- Condition S.youControlSource replaces StateCondition.YouControlSource with.
holdsYouControlSource :: PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> Bool
holdsYouControlSource you source gs =
  Condition.holds (Projection.fullView gs) (Filter.contextFor (Just you) (Just source)) gs source S.youControlSource

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
            ActiveReplacement.controller = you,
            ActiveReplacement.timestamp = ts,
            ActiveReplacement.expiry = Expiry.Type.While (While.MkWhile you S.youControlSource),
            ActiveReplacement.uses = Uses.Unlimited,
            ActiveReplacement.origin = ReplacementOrigin.Other,
            ActiveReplacement.rider = Nothing
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

conditionalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
conditionalSpec s registry = Spec.describe s "Conditional" $ do
  Spec.it s "CR 611.2b YouControlSource holds while the source is controlled" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
    Spec.assertBool s (holdsYouControlSource S.alice srcId gs) "holds"
  Spec.it s "CR 613.1b it stops holding when another player gains control of the source" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
        stolen = S.giveControl srcId S.bob gs
    Spec.assertBool s (not (holdsYouControlSource S.alice srcId stolen)) "no longer holds"
  Spec.it s "CR 400.7 it stops holding when the source leaves the battlefield" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
        gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [srcId])
    Spec.assertBool s (not (holdsYouControlSource S.alice srcId gone)) "no longer holds"
  Spec.it s "CR 611.2b arm returns Nothing when the condition is already false" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
        gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [srcId])
    Spec.assertEqWith
      s
      "never starts"
      (Expiry.arm Map.empty S.alice srcId (Duration.ForAsLongAs S.youControlSource) gone)
      Nothing
  Spec.it s "CR 611.2b arm returns a While when the condition holds now" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (srcId, _, gs) = board piker warMammoth
    Spec.assertEqWith
      s
      "starts"
      (Expiry.arm Map.empty S.alice srcId (Duration.ForAsLongAs S.youControlSource) gs)
      (Just (Expiry.Type.While (While.MkWhile S.alice S.youControlSource)))
  Spec.it s "CR 611.2b the sweep DELETES the effect once the condition fails" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (srcId, targetId, gs) = board piker warMammoth
        gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [srcId])
        (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
    Spec.assertEqWith s "alice held it while the source stood" (Projection.controllerOf targetId gs) (Just S.alice)
    Spec.assertBool s changed "the sweep reports a change"
    Spec.assertEqWith s "the effect is gone, not masked" (GameState.continuousEffects swept) []
    Spec.assertEqWith s "control reverted" (Projection.controllerOf targetId swept) (Just S.bob)
  Spec.it s "CR 611.2b a sweep that changes nothing reports False" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (_, _, gs) = board piker warMammoth
        (changed, _) = Engine.runGamePure S.identityAnswer gs Expiry.sweepConditional
    Spec.assertBool s (not changed) "no change"
  Spec.it s "CR 704.3 settleForPriority runs the sweep" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (srcId, targetId, gs) = board piker warMammoth
        gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [srcId])
        settled = S.runPure S.identityAnswer gone Engine.settleForPriority
    Spec.assertEqWith s "control reverted at the settle" (Projection.controllerOf targetId settled) (Just S.bob)
  Spec.it s "CR 611.2b the sweep's replacements half survives while the source stands, then deletes once it doesn't" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
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
      gs3 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project thiefId gs2))] gs2
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
      gs3 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project thiefId gs2))] gs2
   in (thiefId, myrId, masterThiefResolveAll (masterThiefSettle gs3))

-- Master Thief {2}{U}{U} Creature -- Human Rogue 2/2: "When this creature
-- enters, gain control of target artifact for as long as you control this
-- creature." CR 611.2b's own printed example; the three assertions below in
-- tests 2-4 are its three Gatherer rulings, verbatim.
masterThiefSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
masterThiefSpec s registry = Spec.describe s "MasterThief" $ do
  Spec.it s "CR 611.2b it works: the ETB resolves and control of the artifact changes" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    masterThief <- S.printingOf s registry "Master Thief"
    let (_, myr, entering) = masterThiefBoard darksteelMyr masterThief
        stolen = masterThiefResolveAll (masterThiefSettle entering)
    Spec.assertEqWith s "alice controls the Myr" (Projection.controllerOf myr stolen) (Just S.alice)
    -- CR 302.6: the new controller has not controlled it continuously.
    Spec.assertEqWith s "and it is re-Sicked" (fmap Object.sickness (Game.lookupObject myr stolen)) (Just Sickness.Sick)
  -- Ruling: "If Master Thief leaves the battlefield, you no longer
  -- control it, and its control-change effect ends."
  Spec.it s "CR 611.2b leaving the battlefield ends it" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    masterThief <- S.printingOf s registry "Master Thief"
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
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    masterThief <- S.printingOf s registry "Master Thief"
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
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    masterThief <- S.printingOf s registry "Master Thief"
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
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    masterThief <- S.printingOf s registry "Master Thief"
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
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    masterThief <- S.printingOf s registry "Master Thief"
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
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    masterThief <- S.printingOf s registry "Master Thief"
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

monarchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monarchSpec s registry = Spec.describe s "Monarch" $ do
  Spec.it s "CR 725.2 the monarch draws at the beginning of their own end step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, gs0) = S.addLibraryCard piker S.alice (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
        began = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice)] gs0
        after = monarchResolveAll (monarchSettle began)
    Spec.assertEqWith s "alice drew (one card now in hand)" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
  Spec.it s "CR 725.2 the end-step draw fires only on the monarch's own end step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, gs0) = S.addLibraryCard piker S.bob (S.withMonarch S.bob (Setup.emptyGame S.bothPlayers))
        -- alice is the active player; her end step is not bob's (the monarch).
        began = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice)] gs0
        after = monarchResolveAll (monarchSettle began)
    Spec.assertEqWith s "bob did not draw on alice's end step" (length (Game.zoneMembers Zone.Hand S.bob after)) 0
  Spec.it s "CR 725.2 combat damage to the monarch hands the crown to the damager's controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (bobCreature, gs0) = S.addCreature piker S.bob (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
        dmg = DamageEvent.MkDamageEvent bobCreature (Recipient.ToPlayer S.alice) 2 False False False 0 Nothing DamageKind.Combat
        began = S.withEvents [GameEvent.DamageDealt dmg] gs0
        after = monarchResolveAll (monarchSettle began)
    Spec.assertEqWith s "bob took the crown" (GameState.monarch after) (Just S.bob)
  Spec.it s "CR 725.2 noncombat damage to the monarch does not hand over the crown" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (bobCreature, gs0) = S.addCreature piker S.bob (S.withMonarch S.alice (Setup.emptyGame S.bothPlayers))
        dmg = DamageEvent.MkDamageEvent bobCreature (Recipient.ToPlayer S.alice) 2 False False False 0 Nothing DamageKind.Noncombat
        began = S.withEvents [GameEvent.DamageDealt dmg] gs0
        after = monarchResolveAll (monarchSettle began)
    Spec.assertEqWith s "alice keeps the crown" (GameState.monarch after) (Just S.alice)
  Spec.it s "CR 725 Palace Jailer: ETB makes the caster monarch and exiles an opponent's creature until an opponent takes the crown" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    palaceJailer <- S.printingOf s registry "Palace Jailer"
    let gs0 = Setup.emptyGame S.bothPlayers
        (victim, gs1) = S.addCreature piker S.bob gs0
        (jailer, gs2) = S.addCreature palaceJailer S.alice gs1
        entered = ZoneChange.MkZoneChange jailer jailer Zone.Stack Zone.Battlefield
        gs3 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project jailer gs2))] gs2
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
    piker <- S.printingOf s registry "Goblin Piker"
    palaceJailer <- S.printingOf s registry "Palace Jailer"
    let (victim, gs1) = S.addCreature piker S.bob S.threePlayerGame
        (jailer, gs2) = S.addCreature palaceJailer S.alice gs1
        entered = ZoneChange.MkZoneChange jailer jailer Zone.Stack Zone.Battlefield
        gs3 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project jailer gs2))] gs2
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

-- Take the LOWEST-numbered legal recipient of every slot. Garland's two
-- abilities each announce one target, and both are decided this way, so a filter
-- that admitted more than it should would take a DIFFERENT permanent rather than
-- the same one -- see garlandBoard for the ordering that makes that true.
garlandPlan :: Prompt.Prompt r -> r
garlandPlan p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (maybe Set.empty Set.singleton . Set.lookupMin . snd) asked
  _ -> S.identityAnswer p

-- alice's Garland enters; bob and carol have a creature each. CAROL'S IS ADDED
-- FIRST, so it holds the lowest ObjectId of the three creatures on the board:
-- under garlandPlan above, an ability that dropped the "that player controls"
-- conjunct would take HERS, and one that read "you control" would take Garland
-- itself. Only the printed reading takes bob's.
--
-- Returns bob's creature, carol's, and the state with Garland's entry recorded
-- but nothing settled.
garlandBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
garlandBoard piker garland =
  let (carols, gs1) = S.addCreature piker S.carol S.threePlayerGame
      (bobs, gs2) = S.addCreature piker S.bob gs1
      (entrant, gs3) = S.addCreature garland S.alice gs2
      entered = ZoneChange.MkZoneChange entrant entrant Zone.Stack Zone.Battlefield
   in (bobs, carols, S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project entrant gs3))] gs3)

-- The condition Garland's duration stores, AS STORED: CR 725.1's designation
-- read of one PARTICULAR player, baked out of the `thatPlayer` slot the crowning
-- bound (Pawl.Engine.Condition.bakeBound). Spelled with bob because bob is who
-- the fixture crowns.
garlandCondition :: Condition.Type.Condition
garlandCondition =
  Condition.Type.Compares
    ( Compares.MkCompares
        (Quantity.Type.IsMonarch (PlayerRef.Specific S.bob))
        Comparison.AtLeast
        (Quantity.Type.Literal 1)
    )

-- Garland, Royal Kidnapper {2}{U}{B} Legendary Creature -- Human Knight 3/4:
-- "Whenever an opponent becomes the monarch, gain control of target creature
-- that player controls for as long as they're the monarch." Two rules meet, and
-- the card is the only printing in the pool that exercises either:
--
--   * CR 603.2 binds the newly crowned player (Pawl.Engine.Event.eventBindings),
--     which is what "that player" and "they" both name; and
--   * CR 611.2b's duration outlives the resolution that stored it, so the
--     condition is BAKED to that seat as the duration begins -- an unbaked slot
--     read would be unresolvable at the very first sweep.
--
-- THREE SEATS throughout. On two, the crowned player, the ability's one opponent
-- and whoever wears the crown at any later moment are one seat, and no assertion
-- below could tell a right answer from three wrong ones.
garlandSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
garlandSpec s registry = Spec.describe s "GarlandRoyalKidnapper" $ do
  -- THE proving test: the crown lands on bob, the ability takes HIS creature,
  -- and the stored duration names him outright rather than the slot it was
  -- printed with.
  Spec.it s "CR 603.2 the crowned opponent's creature changes hands, and the stored duration names him" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    let (bobs, carols, entering) = garlandBoard piker garland
        stolen = S.runPure garlandPlan (monarchSettle entering) Engine.priorityLoop
    Spec.assertEqWith s "CR 725.1: Garland's own entry crowned bob" (GameState.monarch stolen) (Just S.bob)
    Spec.assertEqWith s "so alice controls bob's creature" (Projection.controllerOf bobs stolen) (Just S.alice)
    Spec.assertEqWith s "and carol's is untouched" (Projection.controllerOf carols stolen) (Just S.carol)
    Spec.assertEqWith
      s
      "CR 611.2b: the duration reads bob's crown, with no slot left to resolve"
      (fmap ContinuousEffect.expiry (filter (S.continuousEffectAffects bobs) (GameState.continuousEffects stolen)))
      [Expiry.Type.While (While.MkWhile S.alice garlandCondition)]
  -- The other half of a duration: it must not end while its condition holds. A
  -- duration that never ends passes every assertion in the case above, so this
  -- and the two below are what separate the two.
  Spec.it s "CR 611.2b it holds while bob wears the crown, cleanup included" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    let (bobs, _, entering) = garlandBoard piker garland
        stolen = S.runPure garlandPlan (monarchSettle entering) Engine.priorityLoop
        swept = monarchSettle stolen
        -- CR 514.2's sweep, which an AtCleanup duration would not survive: this
        -- is what says the stored expiry is a CR 611.2b one and not a turn's.
        pastCleanup = monarchSettle (Expiry.dropAtCleanup swept)
    Spec.assertEqWith s "still alice's at the next settle" (Projection.controllerOf bobs swept) (Just S.alice)
    Spec.assertEqWith s "and after the turn ends" (Projection.controllerOf bobs pastCleanup) (Just S.alice)
  -- CR 725.3: as carol becomes the monarch, bob ceases to be, so the condition
  -- the duration named stops holding. THE case a two-player board cannot state:
  -- there is still a monarch and it is still not alice, so only a duration that
  -- names BOB ends here.
  Spec.it s "CR 611.2b it ends when the crown moves to the third player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    let (bobs, _, entering) = garlandBoard piker garland
        stolen = S.runPure garlandPlan (monarchSettle entering) Engine.priorityLoop
        -- S.withMonarch rather than a second crowning through the rules: it
        -- records no GameEvent, so Garland's trigger does not fire again and
        -- this case is about the sweep alone.
        crowned = monarchSettle (S.withMonarch S.carol stolen)
    Spec.assertEqWith s "control goes back to bob" (Projection.controllerOf bobs crowned) (Just S.bob)
    -- CR 611.2b's one continuous period: the effect is deleted, not masked.
    Spec.assertEqWith s "and the stored effect is gone" (filter (S.continuousEffectAffects bobs) (GameState.continuousEffects crowned)) []
  -- The same ending with the crown going to Garland's OWN controller, which is
  -- the reading that would survive if the condition had been baked to CR 109.5's
  -- "you" instead of to the slot.
  Spec.it s "CR 611.2b it ends when the crown moves to the ability's controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    let (bobs, _, entering) = garlandBoard piker garland
        stolen = S.runPure garlandPlan (monarchSettle entering) Engine.priorityLoop
        crowned = monarchSettle (S.withMonarch S.alice stolen)
    Spec.assertEqWith s "control goes back to bob" (Projection.controllerOf bobs crowned) (Just S.bob)

hagUpkeep :: Phase.Phase
hagUpkeep = Phase.Beginning BeginningStep.Upkeep

hagBeginUpkeep :: GameState.GameState -> GameState.GameState
hagBeginUpkeep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan hagUpkeep S.alice)) (gs {GameState.phase = hagUpkeep, GameState.activePlayer = S.alice})

hagSettle :: GameState.GameState -> GameState.GameState
hagSettle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

hagResolveAll :: GameState.GameState -> GameState.GameState
hagResolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop

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
hagSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hagSpec s registry = Spec.describe s "HagOfInnerWeakness" $ do
  Spec.it s "CR 613.4c it works: the opponent's 3/3 becomes 1/2" $ do
    hag <- S.printingOf s registry "Hag of Inner Weakness"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (mammoth, afterTrigger) = hagBoardWith hag warMammoth
    Spec.assertEqWith s "power" (Projection.powerOf mammoth afterTrigger) (Just 1)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf mammoth afterTrigger) (Just 2)
  -- THE FALSIFIER for both "treat it as until end of turn" and any
  -- implementation that expires the effect by scanning the event log for
  -- a matching StepBegan: the effect was CREATED on a turn whose untap
  -- step has already happened, so a log scan kills it the turn it is born.
  Spec.it s "CR 514.2 it survives cleanup and the whole of the opponent's turn" $ do
    hag <- S.printingOf s registry "Hag of Inner Weakness"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (mammoth, afterTrigger) = hagBoardWith hag warMammoth
        ended = Expiry.dropAtCleanup afterTrigger
        bobsTurn = handoff ended
        bobsTurnSwept = Expiry.dropAtCleanup bobsTurn
    Spec.assertEqWith s "survives its own turn's CR 514.2 cleanup sweep" (Projection.powerOf mammoth ended) (Just 1)
    Spec.assertEqWith s "bob is active" (GameState.activePlayer bobsTurn) S.bob
    Spec.assertEqWith s "power still 1 at the start of bob's turn" (Projection.powerOf mammoth bobsTurn) (Just 1)
    Spec.assertEqWith s "toughness still 2 at the start of bob's turn" (Projection.toughnessOf mammoth bobsTurn) (Just 2)
    Spec.assertEqWith s "power survives bob's own CR 514.2 cleanup sweep too" (Projection.powerOf mammoth bobsTurnSwept) (Just 1)
    Spec.assertEqWith s "toughness survives bob's own CR 514.2 cleanup sweep too" (Projection.toughnessOf mammoth bobsTurnSwept) (Just 2)
  Spec.it s "CR 611.2a it expires as the controller's next turn begins" $ do
    hag <- S.printingOf s registry "Hag of Inner Weakness"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (mammoth, afterTrigger) = hagBoardWith hag warMammoth
        alicesTurn = handoff (handoff (Expiry.dropAtCleanup afterTrigger))
    Spec.assertEqWith s "alice is active again" (GameState.activePlayer alicesTurn) S.alice
    -- Asserted BEFORE the upkeep trigger fires a second time, so
    -- the two effects can never be confused.
    Spec.assertEqWith s "back to 3/3" (Projection.powerOf mammoth alicesTurn) (Just 3)
    Spec.assertEqWith s "back to 3/3" (Projection.toughnessOf mammoth alicesTurn) (Just 3)
    Spec.assertEqWith s "nothing stored" (GameState.continuousEffects alicesTurn) []
  Spec.it s "CR 704.5f the modification really applies: a 2/1 becomes 0/0 and dies" $ do
    hag <- S.printingOf s registry "Hag of Inner Weakness"
    piker <- S.printingOf s registry "Goblin Piker"
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
    hag <- S.printingOf s registry "Hag of Inner Weakness"
    warMammoth <- S.printingOf s registry "War Mammoth"
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
    hag <- S.printingOf s registry "Hag of Inner Weakness"
    warMammoth <- S.printingOf s registry "War Mammoth"
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
    hag <- S.printingOf s registry "Hag of Inner Weakness"
    warMammoth <- S.printingOf s registry "War Mammoth"
    piker <- S.printingOf s registry "Goblin Piker"
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

-- The steps and phases left of alice's turn once the combat phase is under way,
-- so a runStep-driven test can play out of combat and into the main phase after
-- it (CR 511.3: "After the end of combat step ends, the combat phase is over and
-- the postcombat main phase begins").
restOfTurnFromDeclareAttackers :: Seq.Seq Phase.Phase
restOfTurnFromDeclareAttackers =
  Seq.fromList
    [ Phase.Combat CombatStep.DeclareAttackers,
      Phase.Combat CombatStep.DeclareBlockers,
      Phase.Combat CombatStep.CombatDamage,
      Phase.Combat CombatStep.EndOfCombat,
      Phase.PostcombatMain,
      Phase.Ending EndingStep.EndStep,
      Phase.Ending EndingStep.Cleanup
    ]

-- alice controls a Jade Statue and two Mountains, bob controls each printing in
-- `theirs`, and the game sits at the BEGINNING OF COMBAT step (CR 506.1's first)
-- with the rest of the turn scheduled.
--
-- Beginning of combat and not declare attackers, because CR 500.5a is exactly
-- the claim that the animation outlives the step it was made in: it has to be
-- created in one combat step and still be there in a later one. The two
-- Mountains are the {2}, and there are exactly two so the ability can be
-- activated ONCE -- which is what keeps `animateAnswer` below from re-animating
-- in every step it is offered.
jadeBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
jadeBoard statue mountain theirs =
  let gs0 = Setup.emptyGame S.bothPlayers
      (statueId, gs1) = S.addCreature statue S.alice gs0
      (_, gs2) = S.addCreature mountain S.alice gs1
      (_, gs3) = S.addCreature mountain S.alice gs2
      addAll (ids, g) p = let (oid, g1) = S.addCreature p S.bob g in (ids <> [oid], g1)
      (theirIds, gs4) = List.foldl' addAll ([], gs3) theirs
   in ( statueId,
        theirIds,
        gs4
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.remaining = restOfTurnFromDeclareAttackers
          }
      )

isActivate :: A.Action -> Bool
isActivate a = case a of
  A.Activate _ _ -> True
  _ -> False

-- Takes any activation the engine offers, and otherwise attacks and blocks with
-- everything. On the boards below the only activations that ever reach
-- Action.legalActions are alice's Jade Statue animation and bob's Desert ping --
-- a Mountain's and a Desert's mana abilities are excluded by CR 605.1a -- and
-- each belongs to a different player, so "the first one offered" is never a
-- choice between two different abilities.
animateAnswer :: Prompt.Prompt r -> r
animateAnswer p = case p of
  Prompt.ChooseAction _ _ options -> case filter isActivate options of
    a : _ -> a
    [] -> A.Pass
  _ -> S.aggressiveAnswer p

-- animateAnswer for alice only: bob passes on every action. The control for the
-- Desert test below, where what has to be isolated is bob's ping and not alice's
-- animation.
aliceOnlyAnswer :: Prompt.Prompt r -> r
aliceOnlyAnswer p = case p of
  Prompt.ChooseAction _ who _ | who /= S.alice -> A.Pass
  _ -> animateAnswer p

-- One whole step through the engine, under `animateAnswer`.
animateStep :: GameState.GameState -> GameState.GameState
animateStep gs = S.runPure animateAnswer gs Engine.runStep

-- CR 500.5's whole moment, and the duration that fills its first clause.
--
-- Jade Statue ({4} Artifact, Arabian Nights): "{2}: This artifact becomes a 3/6
-- Golem artifact creature until end of combat. Activate only during combat."
-- The card is the producer for BOTH halves -- Duration.UntilEndOfCombat and
-- ActivationRestriction.DuringPhase's whole-phase window -- which is why the two
-- landed together.
untilEndOfCombatSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
untilEndOfCombatSpec s registry = Spec.describe s "UntilEndOfCombat" $ do
  -- CR 500.5a: "Effects that last 'until end of combat' expire at the end of the
  -- combat PHASE, not at the beginning of the end of combat step." So the
  -- printed duration arms to the PHASE window and never to the step, and the two
  -- are different values of the same type -- which is what makes the sweep's
  -- equality test able to tell them apart at all.
  Spec.it s "CR 500.5a an until-end-of-combat duration arms to the end of the combat PHASE" $
    Spec.assertEqWith
      s
      "armed"
      (Expiry.arm Map.empty S.alice S.noSource Duration.UntilEndOfCombat armGs)
      (Just (Expiry.Type.AtEndOf PhaseSelector.CombatPhase))
  -- The unit-level half of CR 500.5a, and the sharpest statement of it: the end
  -- of combat STEP ending does not reach the effect, and the combat PHASE ending
  -- does. A containment test in place of the equality would fail this, and so
  -- would arming to PhaseSelector.Step.
  Spec.it s "CR 500.5a the end of combat STEP ending does not expire it; the PHASE ending does" $ do
    let armed = effectWith (Expiry.Type.AtEndOf PhaseSelector.CombatPhase) (Setup.emptyGame S.bothPlayers)
        stepEnded = Expiry.dropAtEndOf (PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat)) armed
        phaseEnded = Expiry.dropAtEndOf PhaseSelector.CombatPhase armed
    Spec.assertEqWith s "the end of combat step's own end leaves it alone" (length (GameState.continuousEffects stepEnded)) 1
    Spec.assertEqWith s "an earlier combat step's end leaves it alone too" (length (GameState.continuousEffects (Expiry.dropAtEndOf (PhaseSelector.Step (Phase.Combat CombatStep.BeginningOfCombat)) armed))) 1
    Spec.assertEqWith s "and the combat phase's end takes it" (GameState.continuousEffects phaseEnded) []
  -- The window is compared by equality, so a DIFFERENT phase's end is not this
  -- one's -- CR 500.1 fixes the set of phases and the beginning phase's end is
  -- not the combat phase's.
  Spec.it s "CR 500.1 dropAtEndOf is scoped to the window that ended" $ do
    let armed = effectWith (Expiry.Type.AtEndOf PhaseSelector.CombatPhase) (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "the ending phase's end is not the combat phase's" (length (GameState.continuousEffects (Expiry.dropAtEndOf PhaseSelector.EndingPhase armed))) 1
  -- The other three sweeps own other rules, and none of them owns this one: CR
  -- 514.2's cleanup, CR 611.2a's turn handoff and CR 611.2b's condition check
  -- all leave an AtEndOf entry alone. The mirror of "dropAtTurnOf touches no
  -- other expiry" above.
  Spec.it s "CR 514.2 / 611.2a no other sweep touches an AtEndOf effect" $ do
    let armed = effectWith (Expiry.Type.AtEndOf PhaseSelector.CombatPhase) (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "cleanup keeps it" (length (GameState.continuousEffects (Expiry.dropAtCleanup armed))) 1
    Spec.assertEqWith s "the turn handoff keeps it" (length (GameState.continuousEffects (Expiry.dropAtTurnOf S.alice armed))) 1
    Spec.assertEqWith s "and so does the CR 611.2b sweep" (length (GameState.continuousEffects (S.runPure S.identityAnswer armed Expiry.sweepConditional))) 1
  -- CR 500.5's ORDER, which no other test can reach: "those effects expire. THEN
  -- any unspent mana left in a player's mana pool empties."
  --
  -- PlayerEffect.DontLoseUnspentMana (Upwelling's, CR 106.4) is read LIVE by
  -- Mana.emptyManaPools, so an entry that expires as the combat phase ends keeps
  -- nothing, while the same entry swept AFTERWARDS would keep the pool for a
  -- phase it no longer covers. That is the whole of the ordering, made visible.
  --
  -- The stored effect is installed directly rather than printed by a card: no
  -- card in the pool joins an end-of-combat duration to mana retention (CR
  -- 702.189a firebending -- "Until end of combat, you don't lose this mana as
  -- steps and phases end" -- is the shape that would). The Never-scoped control
  -- in the same test is what keeps this from passing for the wrong reason.
  Spec.it s "CR 500.5 an end-of-combat retention effect expires BEFORE the pool empties" $ do
    mountain <- S.printingOf s registry "Mountain"
    let staged expiry =
          let gs0 = Setup.emptyGame S.bothPlayers
              (mtn, gs1) = S.addCreature mountain S.alice gs0
              floated = S.runPure S.identityAnswer gs1 (Cost.tapForMana mtn)
           in S.addPlayerEffect
                expiry
                (AffectedPlayers.Scoped PlayerScope.EachPlayer)
                (PlayerEffect.DontLoseUnspentMana ManaFilter.Any)
                S.alice
                floated
                  { GameState.activePlayer = S.alice,
                    GameState.phase = Phase.Combat CombatStep.EndOfCombat,
                    GameState.remaining = Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
                  }
        expiring = staged (Expiry.Type.AtEndOf PhaseSelector.CombatPhase)
        permanent = staged Expiry.Type.Never
    Spec.assertEqWith s "alice floated one mana" (poolSize S.alice expiring) 1
    Spec.assertEqWith
      s
      "the retention expired with the phase, so the pool emptied"
      (poolSize S.alice (S.runPure S.identityAnswer expiring Engine.runStep))
      0
    Spec.assertEqWith
      s
      "and a retention that does NOT expire keeps it across the same boundary"
      (poolSize S.alice (S.runPure S.identityAnswer permanent Engine.runStep))
      1
  -- The gameplay-level proof (design.md section 4), driven through
  -- Engine.runStep: alice animates her Jade Statue in the BEGINNING OF COMBAT
  -- step, and it is still a 3/6 creature once that step has ended.
  --
  -- This is the case a step-scoped duration fails. An implementation that ended
  -- the effect at the end of the step it was created in would leave a plain
  -- artifact here, and it would still pass a test that only asked "is it a
  -- creature during combat and not next turn".
  Spec.it s "CR 500.5a whole card: the animation outlives the step it was made in" $ do
    statue <- S.printingOf s registry "Jade Statue"
    mountain <- S.printingOf s registry "Mountain"
    let (statueId, _, jade) = jadeBoard statue mountain []
        afterBeginningOfCombat = animateStep jade
    Spec.assertBool s (not (Projection.isCreatureOf statueId jade)) "a Jade Statue is a noncreature artifact to begin with"
    Spec.assertEqWith s "the beginning of combat step is over" (GameState.phase afterBeginningOfCombat) (Phase.Combat CombatStep.DeclareAttackers)
    Spec.assertBool s (Projection.isCreatureOf statueId afterBeginningOfCombat) "and it is a creature in the NEXT step, not just the one it was animated in"
    Spec.assertEqWith s "a 3/6" (S.powerToughnessOf statueId afterBeginningOfCombat) (Just (3, 6))
    Spec.assertEqWith
      s
      "CR 500.5a stored against the combat PHASE's end, not the step's"
      (fmap ContinuousEffect.expiry (GameState.continuousEffects afterBeginningOfCombat))
      [ Expiry.Type.AtEndOf PhaseSelector.CombatPhase,
        Expiry.Type.AtEndOf PhaseSelector.CombatPhase,
        Expiry.Type.AtEndOf PhaseSelector.CombatPhase
      ]
  -- CR 205.1b's last sentence, in the words Jade Statue prints: "Some effects
  -- state that an object becomes a '[creature type or types] artifact
  -- creature'; these effects also allow the object to retain all of its prior
  -- card types and subtypes other than creature types, but replace any existing
  -- creature types." The Statue is the DEGENERATE case of the layer-4
  -- SetCreatureSubtype arm -- there is nothing for the set to replace, since its
  -- printed type line is a bare Artifact -- where Turn to Frog (Pawl.ProjectionSpec)
  -- is the case with a creature type standing. What the rule still has to say
  -- here is the RETENTION: Artifact is a prior CARD type, so it survives, and
  -- the projection is an Artifact Creature -- Golem rather than a
  -- Creature -- Golem.
  --
  -- CR 205.3m lists Golem among the creature types, which is what makes
  -- Pawl.Engine.Subtype.isCreatureType's filter the right one to run here.
  Spec.it s "CR 205.1b whole card: the animated Statue is an Artifact Creature -- Golem" $ do
    statue <- S.printingOf s registry "Jade Statue"
    mountain <- S.printingOf s registry "Mountain"
    let (statueId, _, jade) = jadeBoard statue mountain []
        afterBeginningOfCombat = animateStep jade
    Spec.assertEqWith s "before: an Artifact with no subtype at all" (Projection.cardTypesOf statueId jade, Projection.subtypesOf statueId jade) (Set.singleton CardType.Artifact, Set.empty)
    Spec.assertEqWith
      s
      "after: CR 205.1b the Artifact card type is retained and Creature is added"
      (Projection.cardTypesOf statueId afterBeginningOfCombat)
      (Set.fromList [CardType.Artifact, CardType.Creature])
    Spec.assertEqWith
      s
      "and the creature type is Golem"
      (Projection.subtypesOf statueId afterBeginningOfCombat)
      (Set.singleton Subtype.Golem)
  -- The other half of CR 500.5a, step by step through the whole combat phase:
  -- the animation is live at the START of the end of combat step -- "not at the
  -- beginning of the end of combat step" is the rule's own wording -- and gone
  -- once the phase is over.
  --
  -- Each step is taken separately so the state can be read BETWEEN them, which
  -- is the only place the difference shows: S.runCombat would run past the whole
  -- phase and could not tell an effect that ended at the step's beginning from
  -- one that ended at the phase's end.
  Spec.it s "CR 500.5a whole card: live throughout the end of combat step, gone once the phase ends" $ do
    statue <- S.printingOf s registry "Jade Statue"
    mountain <- S.printingOf s registry "Mountain"
    let (statueId, _, jade) = jadeBoard statue mountain []
        afterDeclareAttackers = animateStep (animateStep jade)
        afterDeclareBlockers = animateStep afterDeclareAttackers
        afterCombatDamage = animateStep afterDeclareBlockers
        afterEndOfCombat = animateStep afterCombatDamage
    Spec.assertEqWith s "the end of combat step is the one about to run" (GameState.phase afterCombatDamage) (Phase.Combat CombatStep.EndOfCombat)
    Spec.assertBool s (Projection.isCreatureOf statueId afterCombatDamage) "CR 500.5a it is STILL a creature as the end of combat step begins"
    Spec.assertEqWith s "still a 3/6" (S.powerToughnessOf statueId afterCombatDamage) (Just (3, 6))
    Spec.assertEqWith s "CR 511.3 the combat phase is over" (GameState.phase afterEndOfCombat) Phase.PostcombatMain
    Spec.assertBool s (not (Projection.isCreatureOf statueId afterEndOfCombat)) "and the animation expired with the phase"
    Spec.assertEqWith s "nothing is left stored" (GameState.continuousEffects afterEndOfCombat) []
    Spec.assertEqWith s "it kept its printed 3 damage to bob on the way through" (S.lifeOf S.bob afterEndOfCombat) (Just 17)
  -- The same claim made by a CARD rather than by a projection query, which is
  -- what makes it a gameplay-level observation of the end of combat step itself.
  --
  -- Desert (Arabian Nights): "{T}: This land deals 1 damage to target attacking
  -- creature. Activate only during the end of combat step." bob can only ping
  -- something that is an attacking CREATURE while that step is running. If the
  -- animation expired at the beginning of that step -- the reading CR 500.5a
  -- exists to forbid -- the Jade Statue would be a noncreature artifact, CR
  -- 506.4 would have taken it out of combat, the ping would have no legal target
  -- and no damage would be marked.
  Spec.it s "CR 500.5a whole card: Desert can still ping the animated Statue in the end of combat step" $ do
    statue <- S.printingOf s registry "Jade Statue"
    mountain <- S.printingOf s registry "Mountain"
    desert <- S.printingOf s registry "Desert"
    let (statueId, deserts, jade) = jadeBoard statue mountain [desert]
        after = S.runCombat animateAnswer jade
    Spec.assertEqWith s "the whole combat phase ran" (GameState.phase after) Phase.PostcombatMain
    Spec.assertEqWith s "bob took 3 from a 3/6 attacker" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "and pinged it for 1 while it was still an attacking creature" (S.damageOf statueId after) (Just 1)
    Spec.assertEqWith s "the Desert paid its {T}" (fmap Object.tapped (Maybe.listToMaybe deserts >>= \d -> Game.lookupObject d after)) (Just TapState.Tapped)
    Spec.assertBool s (not (Projection.isCreatureOf statueId after)) "the animation still expired with the phase"
    -- The control, isolating the ping: the same board and the same animation,
    -- with an interpreter under which bob never activates anything. The Statue
    -- takes no damage, so the 1 above came from the Desert and not from combat.
    let unpinged = S.runCombat aliceOnlyAnswer jade
    Spec.assertEqWith s "bob still took 3 from the same attacker" (S.lifeOf S.bob unpinged) (Just 17)
    Spec.assertEqWith s "and the Statue took nothing when bob declined" (S.damageOf statueId unpinged) (Just 0)
  -- The control for the two whole-card tests above: the same board, an
  -- interpreter that never activates. The Statue stays a noncreature artifact,
  -- so what animated it was the ability and nothing about the fixture.
  Spec.it s "CR 500.5a whole card: an unactivated Jade Statue never becomes a creature" $ do
    statue <- S.printingOf s registry "Jade Statue"
    mountain <- S.printingOf s registry "Mountain"
    let (statueId, _, jade) = jadeBoard statue mountain []
        after = S.runCombat S.aggressiveAnswer jade
    Spec.assertEqWith s "the whole combat phase ran" (GameState.phase after) Phase.PostcombatMain
    Spec.assertBool s (not (Projection.isCreatureOf statueId after)) "never a creature"
    Spec.assertEqWith s "so it never attacked" (S.lifeOf S.bob after) (Just 20)

-- Aims every target slot at one object, so a spell with two legal targets on
-- the board is pointed at the one the test means. S.identityAnswer would take
-- the engine's first offer, and below that offer is a choice between Titania's
-- Song and the Bonesplitter the Song has itself turned into a creature.
aimAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- The duration that begins when a permanent LEAVES the battlefield rather than
-- when a spell resolves.
--
-- Titania's Song ({3}{G} Enchantment, Antiquities / Masters Edition IV): "Each
-- noncreature artifact loses all abilities and becomes an artifact creature
-- with power and toughness each equal to its mana value. If this enchantment
-- leaves the battlefield, this effect continues until end of turn."
--
-- CR 604.2 gives a static ability's continuous effect exactly the life of its
-- permanent on the battlefield, so the second sentence is the card overriding
-- that: the effect has to survive the ability that made it, which means being
-- handed over to GameState.continuousEffects as the Song goes.
lingeringSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lingeringSpec s registry = Spec.describe s "TitaniasSong" $ do
  -- The gameplay-level proof (design.md section 4), all five moments in one
  -- test because they are one board: only a run that ANIMATED the Bonesplitter,
  -- then really removed the Song, can say anything about what happens after.
  --
  -- Bonesplitter ({1} Artifact -- Equipment) is the animated permanent because
  -- it makes all three of the Song's parts observable at once: it is a
  -- noncreature artifact (so the layer-4 animation reaches it), its mana value
  -- is 1 (so CR 613.4b's base P/T is a 1/1 rather than a 0/0 CR 704.5f would
  -- bury), and it prints a real Equip ability (so the layer-6 strip is
  -- observable at all -- without one, steps 1 and 4 would pass vacuously).
  --
  -- Angelic Edict ({4}{W} Sorcery, "Exile target creature or enchantment") is
  -- how the Song leaves: exiling is leaving the battlefield, and no card in the
  -- pool destroys an enchantment. Five Plains pay for it.
  Spec.it s "CR 611.2a whole card: the animation outlives Titania's Song and ends at cleanup" $ do
    plains <- S.printingOf s registry "Plains"
    titaniasSong <- S.printingOf s registry "Titania's Song"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    jadeStatue <- S.printingOf s registry "Jade Statue"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let base = S.landsInPlay plains 5
        (songId, g1) = S.addCreature titaniasSong S.alice base
        (axeId, g2) = S.addCreature bonesplitter S.alice g1
        (staged, spellId) = S.handOne angelicEdict g2
        cast = S.runPure (aimAtObject songId) staged (S.cast S.alice spellId)
        exiled = S.settleSba (S.runPure (aimAtObject songId) cast Stack.resolveTop)
        -- CR 611.2c's anti-over-reach probe: a noncreature artifact that
        -- arrives AFTER the handover. Jade Statue is a {4} artifact with an
        -- activated ability, so an implementation that carried the Song's live
        -- "each noncreature artifact" filter forward would show up here twice
        -- over -- as a 4/4 creature, and as one with no abilities.
        (statueId, withStatue) = S.addCreature jadeStatue S.alice exiled
        toCleanup =
          withStatue
            { GameState.remaining = Seq.fromList [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
            }
        afterMain = S.runPure S.identityAnswer toCleanup Engine.runStep
        afterEnd = S.runPure S.identityAnswer afterMain Engine.runStep
        afterCleanup = S.runPure S.identityAnswer afterEnd Engine.runStep
    -- 1. Before: the Song is doing all three of its jobs.
    Spec.assertBool s (Projection.isCreatureOf axeId staged) "CR 613.1d the Song animates the Bonesplitter"
    Spec.assertEqWith s "CR 613.4b a 1/1, its mana value" (S.powerToughnessOf axeId staged) (Just (1, 1))
    Spec.assertEqWith s "CR 613.1f and its Equip ability is stripped" (Projection.abilitiesOf axeId staged) []
    -- 2. The Song really left. Without this the rest passes trivially.
    Spec.assertEqWith s "Angelic Edict exiled Titania's Song" (Game.lookupObject songId exiled) Nothing
    Spec.assertBool s (not (S.onBattlefield songId exiled)) "so it is off the battlefield"
    -- 3. CR 611.2c: the set was fixed when the effect began.
    Spec.assertBool s (not (Projection.isCreatureOf statueId withStatue)) "CR 611.2c an artifact entering afterwards is NOT animated"
    Spec.assertEqWith s "and keeps its own ability" (length (Projection.abilitiesOf statueId withStatue)) 1
    -- 4. Same turn, Song gone: the effect continues.
    Spec.assertBool s (Projection.isCreatureOf axeId exiled) "CR 611.2a the animation continues without the Song"
    Spec.assertEqWith s "still a 1/1" (S.powerToughnessOf axeId exiled) (Just (1, 1))
    Spec.assertEqWith s "still no abilities" (Projection.abilitiesOf axeId exiled) []
    -- 5. CR 514.2: and the whole thing ends in the cleanup step.
    Spec.assertEqWith s "the cleanup step ran" (GameState.phase afterEnd) (Phase.Ending EndingStep.Cleanup)
    Spec.assertBool s (not (Projection.isCreatureOf axeId afterCleanup)) "CR 514.2 no longer a creature after cleanup"
    Spec.assertEqWith s "and its Equip ability is back" (length (Projection.abilitiesOf axeId afterCleanup)) 1
  -- The control, and CR 604.2 read straight: a static ability whose card says
  -- nothing about leaving the battlefield ends with its permanent, THIS
  -- INSTANT. Humility is the same shape as Titania's Song one card over -- a
  -- layer-6 strip beside a layer-7b P/T set -- so an implementation that handed
  -- every departing permanent's static abilities over would keep Bird Maiden a
  -- 1/1 with no flying here, and the test above could not tell it apart.
  Spec.it s "CR 604.2 an ordinary static ability does NOT linger past its permanent" $ do
    plains <- S.printingOf s registry "Plains"
    humility <- S.printingOf s registry "Humility"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let base = S.landsInPlay plains 5
        (humilityId, g1) = S.addCreature humility S.alice base
        (birdId, g2) = S.addCreature birdMaiden S.alice g1
        (staged, spellId) = S.handOne angelicEdict g2
        cast = S.runPure (aimAtObject humilityId) staged (S.cast S.alice spellId)
        exiled = S.settleSba (S.runPure (aimAtObject humilityId) cast Stack.resolveTop)
    Spec.assertEqWith s "CR 613 Humility makes it a 1/1" (S.powerToughnessOf birdId staged) (Just (1, 1))
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying birdId staged)) "with no flying"
    Spec.assertEqWith s "Angelic Edict exiled Humility" (Game.lookupObject humilityId exiled) Nothing
    Spec.assertEqWith s "CR 604.2 and the Maiden is a 1/2 again at once" (S.powerToughnessOf birdId exiled) (Just (1, 2))
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying birdId exiled) "with its flying back"
    Spec.assertEqWith s "nothing was handed over" (GameState.continuousEffects exiled) []

-- CR 601.2c: Soulfire Eruption's target slot allows none, so a fixture that
-- wants a card exiled has to say how many. One target, aimed by S.identityAnswer.
soulfireCast :: Prompt.Prompt r -> r
soulfireCast p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const 1) offers
  _ -> S.identityAnswer p

-- Casts THAT object whenever the engine offers it, and passes otherwise. Pinned
-- to one id rather than "the first legal cast": an answerer that searched for
-- something castable would repair the assertion by playing a land or a card from
-- hand instead, and the test would stay green with the permission broken.
castingFromExile :: ObjectId.ObjectId -> Prompt.Prompt r -> r
castingFromExile oid p = case p of
  Prompt.ChooseAction _ _ actions -> Maybe.fromMaybe A.Pass (List.find (S.isCastOf oid) actions)
  _ -> S.identityAnswer p

-- Whole steps, through the real turn machinery, until that turn number begins or
-- the game ends. Bounded so a rules bug cannot hang the suite rather than fail.
runToTurn :: (forall r. Prompt.Prompt r -> r) -> Natural -> GameState.GameState -> GameState.GameState
runToTurn answer turn = go (128 :: Int)
  where
    go budget gs =
      if budget <= 0 || GameState.turnNumber gs >= turn || Maybe.isJust (GameState.result gs)
        then gs
        else go (budget - 1) (S.runPure answer gs Engine.runStep)

-- CR 601.3's permission, as the player it names.
permissionOn :: ObjectId.ObjectId -> GameState.GameState -> Maybe PlayerId.PlayerId
permissionOn oid gs = fmap ExilePlayPermission.player (Game.lookupObject oid gs >>= Object.playableFromExile)

-- alice, on turn 1, casts Soulfire Eruption off nine Mountains at one target.
-- That exiles the top card of her library -- a Goblin Piker, the one card here
-- she can afford off Mountains later -- and grants CR 601.3's play permission for
-- the printed duration. Returns the exiled card and the board the spell left.
--
-- BOTH libraries are stocked underneath, because the cases below advance five
-- turns and a fixture player who runs out of cards loses to CR 104.3c before the
-- assertion runs.
soulfireBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, GameState.GameState)
soulfireBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  soulfire <- S.printingOf s registry "Soulfire Eruption"
  piker <- S.printingOf s registry "Goblin Piker"
  let stockedWith printing pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 :: Int .. 6]
      g1 = S.landsFor mountain S.alice 9 (Setup.emptyGame S.bothPlayers)
      g2 = stockedWith mountain S.bob (stockedWith mountain S.alice g1)
      -- Last in is on top, so this is the card the spell exiles.
      g3 = snd (S.addLibraryCard piker S.alice g2)
      (withSpell, spell) = S.handOne soulfire g3
      cast = S.runPure soulfireCast withSpell (S.cast S.alice spell)
      resolved = S.runPure soulfireCast cast Engine.priorityLoop
  pure
    ( case Game.zoneMembers Zone.Exile S.alice resolved of
        [oid] -> oid
        oids -> error ("Pawl.ExpirySpec: expected one exiled card, got " <> show (length oids)),
      resolved
    )

-- Soulfire Eruption {6}{R}{R}{R} Sorcery (data/cards/soulfire-eruption.json) --
-- "... You may play the exiled cards until the end of your next turn." (name,
-- cost, type line and Oracle text checked against api.scryfall.com.) The pool's
-- producer of CR 611.2a's second phrasing, and the Hag above is its exact
-- contrast: the Hag's effect is gone as alice's next turn BEGINS, this one lasts
-- through the whole of it.
--
-- alice takes turns 1, 3 and 5, so the two readings disagree about turn 3 and
-- agree about turns 1, 2 and 4 onwards -- which is why every assertion here is
-- stated at turn 3 or at turn 4. Both are driven through Engine.runStep, so the
-- permission is observed the way a player would: by the engine offering the cast
-- as a legal action, or not.
soulfireSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulfireSpec s registry = Spec.describe s "SoulfireEruption" $ do
  Spec.it s "CR 611.2a the permission lasts through the controller's next turn, and the card is played on it" $ do
    (pikerId, resolved) <- soulfireBoard s registry
    -- Turns 1 and 2 are played out with nothing cast, so the card is played on
    -- alice's NEXT turn rather than on the turn the spell resolved.
    let alicesNext = runToTurn S.identityAnswer 3 resolved
        played = runToTurn (castingFromExile pikerId) 4 alicesNext
    Spec.assertEqWith s "alice's next turn began" (GameState.activePlayer alicesNext, GameState.turnNumber alicesNext) (S.alice, 3)
    -- Where an "until your next turn" duration is already over: this is the
    -- assertion the printed card and pawl's old, stricter reading disagree on.
    Spec.assertEqWith s "the permission survived the handoff into it" (permissionOn pikerId alicesNext) (Just S.alice)
    Spec.assertEqWith s "alice played the exiled card during that turn" (S.creaturesInPlay S.alice played) 1
    Spec.assertEqWith s "so it is no longer in exile" (Game.zoneMembers Zone.Exile S.alice played) []
  Spec.it s "CR 611.2a / 514.2 it ends as that turn ends, and no later turn of theirs can play the card" $ do
    (pikerId, resolved) <- soulfireBoard s registry
    -- The same board, run one turn further with the same answerer: the only
    -- difference from the case above is which turn the cast is attempted on.
    let afterwards = runToTurn S.identityAnswer 4 resolved
        later = runToTurn (castingFromExile pikerId) 6 afterwards
    Spec.assertEqWith s "turn 4 began, so alice's next turn is over" (GameState.activePlayer afterwards, GameState.turnNumber afterwards) (S.bob, 4)
    Spec.assertEqWith s "the permission is gone" (permissionOn pikerId afterwards) Nothing
    Spec.assertEqWith s "the card itself is untouched, still in exile" (Game.zoneMembers Zone.Exile S.alice afterwards) [pikerId]
    Spec.assertEqWith s "and alice cannot play it on turn 5 either" (S.creaturesInPlay S.alice later, Game.zoneMembers Zone.Exile S.alice later) (0, [pikerId])

poolSize :: PlayerId.PlayerId -> GameState.GameState -> Int
poolSize pid gs = case Game.poolOf pid gs of
  Mana.Type.MkMana units -> length units

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Expiry" $ do
  armSpec s
  cleanupSpec s registry
  handoffSpec s
  conditionalSpec s registry
  untilEndOfCombatSpec s registry
  masterThiefSpec s registry
  monarchSpec s registry
  garlandSpec s registry
  hagSpec s registry
  endOfNextTurnSpec s
  soulfireSpec s registry
  lingeringSpec s registry
