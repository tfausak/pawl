{-# LANGUAGE GADTs #-}

-- Covers: CR 502.3 / CR 101.2's UNTAP PROHIBITION in its printed and one-shot
-- carriers -- Pawl.Types.UntapRestriction, the set Pawl.Engine.UntapRestriction
-- answers, and Object.doesNotUntapNext, the one-shot Effect.DoesNotUntapNext
-- stores (CR 508.1g's exert stores Object.exertedBy beside it, and
-- Pawl.CombatSpec's Exert group proves that carrier) -- plus
-- the one place all are subtracted (Pawl.Engine.Engine.untapAll); CR 602.1 /
-- 605.1a read as
-- Pawl.Types.Filter's HasNonManaActivatedAbility atom off
-- Pawl.Engine.Filter.View's `nonManaActivatedAbility` field; and CR 702.77b's
-- claim that a reinforce ability "continues to exist while the object is on the
-- battlefield and in all other zones".
--
-- Tsabo's Web and Rustic Clachan are the PRINTED carrier's fixtures, and the
-- pairing is the whole
-- point: rule 702.77b is unobservable without an effect that depends on an object
-- having an activated ability, and Tsabo's Web is that effect. Rustic Clachan is
-- the one printing that is a LAND with reinforce, which is what puts the two
-- cards on the same board at all.
--
-- THE BOARD SHAPE that makes those cases discriminating: two of alice's
-- lands, both tapped, differing in exactly one thing. Seat of the Synod's only
-- activated ability is "{T}: Add {U}", which CR 605.1a excludes; Rustic Clachan
-- has that same shape of mana ability PLUS reinforce. So the Clachan staying
-- tapped while the Seat untaps is rule 702.77b and rule 605.1a and nothing else
-- -- not the card type, not the tap state, not who controls them.
--
-- Prodigal Sorcerer is the third permanent, tapped beside them: a NON-LAND with
-- an activated ability that is not a mana ability, so it untaps and the "each
-- land" conjunct is proved rather than assumed.
--
-- Rustic Clachan's CR 614.1c "you may reveal a Kithkin card from your hand" plays
-- no part here: every board in that group starts the land already on the
-- battlefield, so no entry replacement runs. Pawl.ReplacementSpec is where that
-- sentence is proved.
module Pawl.UntapRestrictionSpec where

import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

-- The untap step's turn-based actions, run for whoever the game state says is
-- active (CR 502.3). The one door both untap prohibitions are read through --
-- Pawl.Engine.UntapRestriction's printed set and Object.doesNotUntapNext.
untapStep :: GameState.GameState -> GameState.GameState
untapStep gs = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))

-- Alice's board: a tapped Rustic Clachan, a tapped Seat of the Synod and a tapped
-- Prodigal Sorcerer, with `web` deciding whether Tsabo's Web is on the battlefield
-- beside them. Nothing else differs between the two boards this builds.
webBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
webBoard s registry web = do
  clachan <- S.printingOf s registry "Rustic Clachan"
  seat <- S.printingOf s registry "Seat of the Synod"
  sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
  tsabosWeb <- S.printingOf s registry "Tsabo's Web"
  let (clachanId, g0) = S.addCreature clachan S.alice (Setup.emptyGame S.bothPlayers)
      (seatId, g1) = S.addCreature seat S.alice g0
      (sorcererId, g2) = S.addCreature sorcerer S.alice g1
      g3 = if web then snd (S.addCreature tsabosWeb S.alice g2) else g2
      tapped = S.tapObject sorcererId (S.tapObject seatId (S.tapObject clachanId g3))
  pure (clachanId, seatId, sorcererId, tapped)

-- Alice's board for the ONE-SHOT prohibition: an Elvish Hunter, a tapped Goblin
-- Piker and a tapped Hill Giant, with two Forests to pay the Hunter's {1}{G}.
-- `atGiant` decides which of the two tapped creatures the ability aims at, and
-- decides NOTHING else -- the two boards this builds are the same five
-- permanents, the same mana spent and the same tap states, so a creature that
-- stays tapped on one board and untaps on the other did so because of the target.
--
-- Alice targets her OWN creature, which "target creature" allows and which keeps
-- the victim under the seat whose untap step runs below.
hunterBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
hunterBoard s registry atGiant = do
  forest <- S.printingOf s registry "Forest"
  hunter <- S.printingOf s registry "Elvish Hunter"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  let (hunterId, g1) = S.addCreature hunter S.alice (S.landsInPlay forest 2)
      (pikerId, g2) = S.addCreature piker S.alice g1
      (giantId, g3) = S.addCreature giant S.alice g2
      board = S.tapObject giantId (S.tapObject pikerId g3)
      victim = if atGiant then giantId else pikerId
      -- The card's FIRST activated ability, off the JSON, and folded rather than
      -- indexed so nothing here is partial. An Elvish Hunter that printed no
      -- ability would activate nothing and let the victim untap, which is the
      -- assertion below failing rather than a fixture passing quietly.
      activate g ab = S.runPure (aimAt victim) g (Activate.activateAbility S.alice hunterId ab >> Stack.resolveTop)
      activated = foldr (flip activate) board (take 1 (Face.activatedAbilities (S.combinedFace hunter)))
  pure (hunterId, pikerId, giantId, activated)

-- Aim every target slot at one object, and leave the mana payment to
-- S.identityAnswer. ColorSpec.aimAtObject's shape, group-local per this suite's
-- convention.
aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "UntapRestriction" $ do
  prohibitionSpec s registry
  oneShotSpec s registry
  existenceSpec s registry

-- CR 502.3 / CR 611.2's ONE-SHOT prohibition: Effect.DoesNotUntapNext, the flag
-- it writes, and CR 611.2a's expiry at the one untap step its sentence names. CR
-- 508.1g's exert writes a SEPARATE flag
-- (Object.exertedBy), keyed to the exerting player rather than to the victim's
-- controller; Pawl.CombatSpec's Exert group is where that path is proved.
oneShotSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
oneShotSpec s registry = Spec.describe s "OneShot" $ do
  -- The unit's central claim. Every assertion is made on one board, so none of
  -- them can pass because nothing untapped at all: the Hill Giant was tapped
  -- exactly as the Piker was and untaps, and the Elvish Hunter tapped ITSELF to
  -- pay the ability's cost and untaps too.
  Spec.it s "CR 502.3 whole card: Elvish Hunter's target does not untap, and everything else on the board does" $ do
    (hunterId, pikerId, giantId, gs) <- hunterBoard s registry False
    Spec.assertBool s (Game.isTapped pikerId gs) "the Piker the ability aimed at is tapped before the step"
    let untapped = untapStep gs
    Spec.assertBool s (Game.isTapped pikerId untapped) "and it is still tapped after CR 502.3's untap"
    Spec.assertBool s (not (Game.isTapped giantId untapped)) "the Hill Giant, tapped the same way and not aimed at, untapped"
    Spec.assertBool s (not (Game.isTapped hunterId untapped)) "and so did the Elvish Hunter, which tapped for the cost"
  -- The same board with the ability aimed at the OTHER tapped creature and
  -- nothing else changed. Both assertions flip together, so neither board can be
  -- passing because a Goblin Piker never untaps or a Hill Giant always does.
  Spec.it s "CR 502.3 aimed at the Hill Giant instead, the Piker is the one that untaps" $ do
    (_, pikerId, giantId, gs) <- hunterBoard s registry True
    let untapped = untapStep gs
    Spec.assertBool s (not (Game.isTapped pikerId untapped)) "the Piker untapped"
    Spec.assertBool s (Game.isTapped giantId untapped) "and the Hill Giant did not"
  -- CR 701.43b: the prohibition expires during the very untap step it applies in.
  -- The Piker is still tapped going into the second step -- nothing re-tapped it
  -- -- so a second step untapping it is the flag having been cleared and not a
  -- board that had changed underneath.
  Spec.it s "CR 701.43b the prohibition is spent at that untap step and the next one untaps it" $ do
    (_, pikerId, _, gs) <- hunterBoard s registry False
    let once = untapStep gs
        twice = untapStep once
    Spec.assertBool s (Game.isTapped pikerId once) "still tapped after the first untap step"
    Spec.assertBool s (not (Game.isTapped pikerId twice)) "and untapped by the second"

-- CR 502.3 / CR 101.2 through the untap step itself.
prohibitionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
prohibitionSpec s registry = Spec.describe s "Prohibition" $ do
  -- The unit's central claim, and all three permanents are asserted on one board
  -- so no assertion can pass on a board the others did not see.
  Spec.it s "CR 502.3/702.77b whole cards: under Tsabo's Web the Rustic Clachan does not untap, and the Seat of the Synod does" $ do
    (clachanId, seatId, sorcererId, gs) <- webBoard s registry True
    let untapped = untapStep gs
    Spec.assertBool s (Game.isTapped clachanId untapped) "the land with reinforce is still tapped"
    Spec.assertBool s (not (Game.isTapped seatId untapped)) "CR 605.1a: the land whose only ability adds mana untapped"
    Spec.assertBool s (not (Game.isTapped sorcererId untapped)) "and 'each land' left the Prodigal Sorcerer alone"
  -- The same board with Tsabo's Web taken away and nothing else changed, so the
  -- assertion above cannot be passing because a Rustic Clachan never untaps.
  Spec.it s "CR 502.3 without Tsabo's Web every one of them untaps" $ do
    (clachanId, seatId, sorcererId, gs) <- webBoard s registry False
    let untapped = untapStep gs
    Spec.assertBool s (not (Game.isTapped clachanId untapped)) "the Clachan untapped"
    Spec.assertBool s (not (Game.isTapped seatId untapped)) "the Seat untapped"
    Spec.assertBool s (not (Game.isTapped sorcererId untapped)) "the Sorcerer untapped"

-- CR 702.77b's two halves, which pull in opposite directions: the ability EXISTS
-- on the battlefield, and it can be ACTIVATED only from a hand.
existenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
existenceSpec s registry = Spec.describe s "Existence" $ do
  -- The projection's list, which is what the Filter atom above measures: the
  -- printed mana ability and rule 702.77a's minted one, on a permanent.
  Spec.it s "CR 702.77b the reinforce ability exists on the battlefield" $ do
    (clachanId, seatId, _, gs) <- webBoard s registry False
    Spec.assertEqWith s "the Clachan has two activated abilities there" (length (Projection.abilitiesOf clachanId gs)) 2
    Spec.assertEqWith s "and the Seat of the Synod has only its mana ability" (length (Projection.abilitiesOf seatId gs)) 1
  -- CR 702.77a's other half, as a pair of boards differing in the Clachan's ZONE
  -- and in nothing else: the same two Plains pay the same {1}{W}, and the same
  -- Goblin Piker is the same legal target. So the offer flipping is CR 113.6m
  -- withholding an ability whose cost discards the card from a hand, and not the
  -- mana, the timing or an empty target pool.
  --
  -- The two OFFER assertions are a fence rather than a proof, and the ABILITY
  -- LIST below is what discriminates: deleting Activate.abilitiesForGiven's
  -- functionsIn filter leaves the offers exactly as they are, because CR 118.3
  -- then refuses the activation anyway -- a DiscardThis cost is unpayable by a
  -- permanent (Cost.canPayComponent asks the zone). Two rules answer here, and
  -- only the list can say which.
  Spec.it s "CR 702.77a reinforce is offered from a hand and not from the battlefield" $ do
    clachan <- S.printingOf s registry "Rustic Clachan"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay plains 2
        (_, withPiker) = S.addCreature piker S.alice base
        (fromHand, handId) = S.handOne clachan withPiker
        (fieldId, played) = S.addCreature clachan S.alice withPiker
        inHand = fromHand {GameState.priority = Just S.alice}
        onField = played {GameState.priority = Just S.alice}
    Spec.assertBool s (any (activateOf handId) (Action.legalActions S.alice inHand)) "the hand's Clachan offers its reinforce ability"
    Spec.assertBool s (not (any (activateOf fieldId) (Action.legalActions S.alice onField))) "the battlefield's Clachan offers no activation"
    -- CR 113.6m, said of the list rather than of the offer: the mana ability is
    -- the only one that functions on the battlefield, so the ability the
    -- projection just reported as EXISTING there is not on it. CR 605.3b is why
    -- the mana ability is no Action.Activate either.
    Spec.assertEqWith s "one ability functions on the battlefield" (length (Activate.abilitiesFor fieldId onField)) 1
    Spec.assertBool
      s
      (not (any (elem CostComponent.DiscardThis . Cost.components . ActivatedAbility.cost) (Activate.abilitiesFor fieldId onField)))
      "and it is not the one whose cost discards the card"

-- Was an activation of THIS source offered? `any isActivate` cannot say, and both
-- boards above carry other permanents.
activateOf :: ObjectId.ObjectId -> A.Action -> Bool
activateOf oid action = case action of
  A.Activate src _ -> src == oid
  _ -> False
