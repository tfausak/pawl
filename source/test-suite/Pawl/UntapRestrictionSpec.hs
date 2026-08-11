-- Covers: CR 502.1 / CR 101.2's UNTAP PROHIBITION -- Pawl.Types.UntapRestriction,
-- the set Pawl.Engine.UntapRestriction answers, and the one place it is
-- subtracted (Pawl.Engine.Engine.untapAll); CR 602.1 / 605.1a read as
-- Pawl.Types.Filter's HasNonManaActivatedAbility atom off
-- Pawl.Engine.Filter.View's `nonManaActivatedAbility` field; and CR 702.77b's
-- claim that a reinforce ability "continues to exist while the object is on the
-- battlefield and in all other zones".
--
-- Tsabo's Web and Rustic Clachan are the fixtures, and the pairing is the whole
-- point: rule 702.77b is unobservable without an effect that depends on an object
-- having an activated ability, and Tsabo's Web is that effect. Rustic Clachan is
-- the one printing that is a LAND with reinforce, which is what puts the two
-- cards on the same board at all.
--
-- THE BOARD SHAPE that makes every case here discriminating: two of alice's
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
-- Not implemented: Rustic Clachan's "As this land enters, you may reveal a
-- Kithkin card from your hand. If you don't, this land enters tapped" -- pawl's
-- card enters tapped unconditionally, which is STRICTER than printed (#1282).
module Pawl.UntapRestrictionSpec where

import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase

-- The untap step's turn-based actions, run for whoever the game state says is
-- active (CR 502.1). The one door Pawl.Engine.UntapRestriction is read through.
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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "UntapRestriction" $ do
  prohibitionSpec s registry
  existenceSpec s registry

-- CR 502.1 / CR 101.2 through the untap step itself.
prohibitionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
prohibitionSpec s registry = Spec.describe s "Prohibition" $ do
  -- The unit's central claim, and all three permanents are asserted on one board
  -- so no assertion can pass on a board the others did not see.
  Spec.it s "CR 502.1/702.77b whole cards: under Tsabo's Web the Rustic Clachan does not untap, and the Seat of the Synod does" $ do
    (clachanId, seatId, sorcererId, gs) <- webBoard s registry True
    let untapped = untapStep gs
    Spec.assertBool s (Game.isTapped clachanId untapped) "the land with reinforce is still tapped"
    Spec.assertBool s (not (Game.isTapped seatId untapped)) "CR 605.1a: the land whose only ability adds mana untapped"
    Spec.assertBool s (not (Game.isTapped sorcererId untapped)) "and 'each land' left the Prodigal Sorcerer alone"
  -- The same board with Tsabo's Web taken away and nothing else changed, so the
  -- assertion above cannot be passing because a Rustic Clachan never untaps.
  Spec.it s "CR 502.1 without Tsabo's Web every one of them untaps" $ do
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
