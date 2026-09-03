{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 702.184 station -- Pawl.Types.Keyword's Station arm and the ability
-- Pawl.Engine.Keyword.station mints from it, offered through the same
-- Pawl.Engine.Projection.abilitiesGiven route crew takes; and CR 721, the station
-- card, whose striations are ordinary conditional static abilities on the card
-- (CR 721.2a-b) and whose power and toughness live nowhere else (CR 721.2c).
--
-- Gameplay-level throughout. Lumen-Class Frigate is the fixture: a {1}{W} Artifact
-- -- Spacecraft whose whole printed text is station, a 2+ anthem and a 12+
-- striation carrying flying, lifelink and a 3/5 box.
--
-- The arithmetic is deliberately non-degenerate. Blind-Spot Giant is 4/1 and Hill
-- Giant is 3/3, so a single station off the Blind-Spot loads 4 counters -- which is
-- not 1 (how many creatures were tapped), not 2 (the anthem's threshold), not 3 or
-- 5 (the Frigate's own numbers) and not 12. The two striation thresholds are read
-- as PAIRS of boards differing in exactly one thing: one charge counter against
-- two, and eleven against twelve.
--
-- THREE SEATS, not two. CR 721.2a's "other creatures you control" is a real
-- narrowing, and on a two-seat board "an opponent" and "the other player"
-- coincide.
--
-- Not covered here: CR 702.184c's static abilities that make a station ability
-- read a characteristic other than the tapped creature's power (#3127).
module Pawl.StationSpec where

import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TapState as TapState

-- CR 122.1's charge counter, taken from the ENGINE rather than respelled here:
-- CR 122.1 makes counters of the same name interchangeable, so the string
-- Pawl.Engine.Keyword.station mints and the one
-- data/cards/lumen-class-frigate.json's striations count have to be one key, and a
-- test that respelled it would pass on a board the card cannot see.
charge :: CounterKind.CounterKind Keyword.Type.Keyword
charge = CounterKind.Named Keyword.chargeCounter

-- The station ability, taken from the PROJECTION rather than from the card's face:
-- rule 702.184a's ability is minted by Pawl.Engine.Keyword and appended by
-- Pawl.Engine.Projection.abilitiesGiven, so a card file that declares no
-- activatedAbilities still offers one.
stationAbility :: ObjectId.ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
stationAbility oid gs = case Projection.abilitiesOf oid gs of
  ability : _ -> Just ability
  [] -> Nothing

-- Alice's board: one Lumen-Class Frigate and one creature per printing, all
-- Settled and untapped, with alice holding priority in her precombat main phase.
-- Three seats, for the reason the module header gives.
board :: Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
board frigate crew =
  let (frigateId, gs0) = S.addCreature frigate S.alice S.threePlayerGame
      add (ids, g) p = let (oid, g1) = S.addCreature p S.alice g in (ids <> [oid], g1)
      (crewIds, gs1) = foldl add ([], gs0) crew
   in ( frigateId,
        crewIds,
        gs1
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Put this many charge counters on the Frigate, without stationing for them. The
-- striation cases want a PAIR of boards either side of a threshold, and reaching
-- eleven and twelve by activation would make the two differ in what was tapped as
-- well as in the tally.
withCharge :: Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withCharge = S.addCounter charge

-- Activate the Frigate's station ability and resolve it. Returns the state
-- unchanged if the permanent offers no ability at all, so a case that expects
-- stationing to have happened asserts on the board and not on this returning Just.
stationWith :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
stationWith answer frigateId gs = case stationAbility frigateId gs of
  Nothing -> gs
  Just ability ->
    let activated = S.runPure answer gs (Activate.activateAbility S.alice frigateId ability)
     in S.runPure answer activated Stack.resolveTop

-- Can alice activate the Frigate's station ability on this board?
stationable :: ObjectId.ObjectId -> GameState.GameState -> Bool
stationable frigateId gs = case stationAbility frigateId gs of
  Nothing -> False
  Just ability -> Activate.activatable S.alice frigateId ability gs

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

isCreature :: ObjectId.ObjectId -> GameState.GameState -> Bool
isCreature oid gs = Set.member CardType.Creature (Projection.cardTypesOf oid gs)

isArtifact :: ObjectId.ObjectId -> GameState.GameState -> Bool
isArtifact oid gs = Set.member CardType.Artifact (Projection.cardTypesOf oid gs)

sizeOf :: ObjectId.ObjectId -> GameState.GameState -> (Maybe Integer, Maybe Integer)
sizeOf oid gs = (Projection.powerOf oid gs, Projection.toughnessOf oid gs)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Station" $ do
  stationAbilitySpec s registry
  striationSpec s registry

-- CR 702.184a's ability: its cost, its effect, and CR 721.4's "at all times".
stationAbilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stationAbilitySpec s registry = Spec.describe s "StationAbility" $ do
  -- The engine never makes this choice: two untapped creatures against a cost that
  -- taps exactly one, so the prompt is a real one and the answer decides how many
  -- counters land. Pinning it to the power-4 creature is what makes 4 the reading
  -- rather than 3.
  Spec.it s "CR 702.184a stationing taps the chosen creature and loads its power in charge counters" $ do
    frigate <- S.printingOf s registry "Lumen-Class Frigate"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (frigateId, crewIds, gs) = board frigate [blindSpot, hillGiant]
    case crewIds of
      [bigId, smallId] ->
        let pinned :: Prompt.Prompt r -> r
            pinned p = case p of
              Prompt.ChooseTaps {} -> Set.singleton bigId
              _ -> S.identityAnswer p
            after = stationWith pinned frigateId gs
         in do
              Spec.assertEqWith s "power 4 loads four charge counters" (S.counterOf charge frigateId after) 4
              Spec.assertEqWith s "the creature named was tapped" (tapStateOf bigId after) (Just TapState.Tapped)
              Spec.assertEqWith s "the one not named was not" (tapStateOf smallId after) (Just TapState.Untapped)
              -- Rule 702.184a's cost has no tap symbol, so the Spacecraft itself
              -- stays untapped -- crew's posture one rule over.
              Spec.assertEqWith s "and the Spacecraft is not tapped" (tapStateOf frigateId after) (Just TapState.Untapped)
      _ -> Spec.assertFailure s "fixture should have exactly two creatures to tap"
  -- CR 702.184a's "another". At twelve counters the Frigate is an untapped creature
  -- alice controls -- every word of the criterion but that one -- so if "another"
  -- were dropped it could station off its own power.
  Spec.it s "CR 702.184a a Spacecraft that has become a creature cannot station itself" $ do
    frigate <- S.printingOf s registry "Lumen-Class Frigate"
    let (frigateId, _, gs) = board frigate []
        loaded = withCharge 12 frigateId gs
    Spec.assertBool s (isCreature frigateId loaded) "it is a creature now"
    Spec.assertEqWith s "and untapped" (tapStateOf frigateId loaded) (Just TapState.Untapped)
    Spec.assertBool s (not (stationable frigateId loaded)) "but it is not a candidate for its own station cost"
  -- CR 702.184a's "you control". Three seats, so the creature that would pay the
  -- cost belongs to a player who is not "the other player" either.
  Spec.it s "CR 702.184a a creature an opponent controls cannot be tapped to station" $ do
    frigate <- S.printingOf s registry "Lumen-Class Frigate"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (frigateId, gs0) = S.addCreature frigate S.alice S.threePlayerGame
        (_, gs1) = S.addCreature blindSpot S.carol gs0
        gs = gs1 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
        (mineId, _, mine) = board frigate [blindSpot]
    Spec.assertBool s (not (stationable frigateId gs)) "carol's Giant is not alice's to tap"
    -- The same printing under alice's control on an otherwise identical board, so
    -- the negative above is CR 702.184a's control clause and not a missing card.
    Spec.assertBool s (stationable mineId mine) "and alice's own is"
  -- Two boards differing in exactly one thing: whose turn it is. The same untapped
  -- creature stands on each, so CR 307.5's "during the main phase of their turn" is
  -- all that moves.
  Spec.it s "CR 702.184a station only as a sorcery" $ do
    frigate <- S.printingOf s registry "Lumen-Class Frigate"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (frigateId, _, gs) = board frigate [blindSpot]
        bobsTurn = gs {GameState.activePlayer = S.bob}
    Spec.assertBool s (stationable frigateId gs) "offered on alice's own main phase"
    Spec.assertBool s (not (stationable frigateId bobsTurn)) "not offered on bob's turn"
  -- CR 721.4: the station ability is not one of the striations, so no threshold
  -- gates it. Eleven is below the 12+ symbol and twelve is above it, and it is
  -- offered on both.
  Spec.it s "CR 721.4 the station ability is offered at any number of charge counters" $ do
    frigate <- S.printingOf s registry "Lumen-Class Frigate"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (frigateId, _, gs) = board frigate [blindSpot]
    Spec.assertEqWith
      s
      "offered at zero, eleven and twelve charge counters"
      (fmap (\n -> stationable frigateId (withCharge n frigateId gs)) [0, 11, 12])
      [True, True, True]

-- CR 721.2's striations, and CR 721.2c's zones.
striationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
striationSpec s registry = Spec.describe s "Striation" $ do
  -- CR 721.2c. The Frigate prints its 3/5 inside the 12+ striation, so the box is
  -- that static ability's and not the card's: in a hand there is nothing to read.
  Spec.it s "CR 721.2c a station card in a hand has no power or toughness" $ do
    frigate <- S.printingOf s registry "Lumen-Class Frigate"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (handGs, cardId) = S.handOne frigate (Setup.emptyGame S.bothPlayers)
        (giantId, gs) = S.addCreature hillGiant S.alice handGs
    Spec.assertEqWith s "no power or toughness in a hand" (sizeOf cardId gs) (Nothing, Nothing)
    -- The positive control: the SAME read point answers for an ordinary creature
    -- on the SAME board, so a Nothing above is rule 721.2c and not a broken
    -- fixture.
    Spec.assertEqWith s "Hill Giant still reports 3/3" (sizeOf giantId gs) (Just 3, Just 3)
    Spec.assertBool s (isArtifact cardId gs) "and it is still an artifact card"
  -- CR 721.2a. Two boards differing in exactly one charge counter, and a third seat
  -- holding the same printing, so "other creatures YOU control" is narrowed by the
  -- board rather than by the assertion.
  Spec.it s "CR 721.2a the 2+ striation pumps other creatures you control and nobody else's" $ do
    frigate <- S.printingOf s registry "Lumen-Class Frigate"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (frigateId, gs0) = S.addCreature frigate S.alice S.threePlayerGame
        (mineId, gs1) = S.addCreature hillGiant S.alice gs0
        (bobsId, gs2) = S.addCreature hillGiant S.bob gs1
        gs = gs2 {GameState.priority = Just S.alice}
        below = withCharge 1 frigateId gs
        above = withCharge 2 frigateId gs
    Spec.assertEqWith s "alice's Hill Giant is 3/3 at one charge counter" (sizeOf mineId below) (Just 3, Just 3)
    Spec.assertEqWith s "and 4/4 at two" (sizeOf mineId above) (Just 4, Just 4)
    Spec.assertEqWith s "bob's stays 3/3 on the same board" (sizeOf bobsId above) (Just 3, Just 3)
    -- "Other": the Frigate is not a creature at two counters, so the anthem has
    -- nothing on it to pump either way, and rule 721.2b's threshold is what the
    -- next case reads.
    Spec.assertBool s (not (isCreature frigateId above)) "and the Spacecraft is not yet a creature"
  -- CR 721.2b. Eleven against twelve, one counter apart on otherwise identical
  -- boards: below the symbol it is a noncreature artifact with no size, above it a
  -- 3/5 artifact creature with flying and lifelink. Its 3 is its OWN base power and
  -- not the anthem's doing -- rule 721.2a says "other".
  Spec.it s "CR 721.2b the 12+ striation makes it an attacking 3/5 artifact creature" $ do
    frigate <- S.printingOf s registry "Lumen-Class Frigate"
    let (frigateId, _, gs) = board frigate []
        eleven = withCharge 11 frigateId gs
        twelve = withCharge 12 frigateId gs
    Spec.assertBool s (not (Combat.canAttack S.alice frigateId eleven)) "eleven charge counters: cannot attack"
    Spec.assertBool s (Combat.canAttack S.alice frigateId twelve) "twelve: can attack"
    Spec.assertBool s (elem frigateId (Combat.legalAttackers S.alice twelve)) "and is offered as an attacker"
    Spec.assertBool s (not (isCreature frigateId eleven)) "eleven: not a creature"
    Spec.assertBool s (isCreature frigateId twelve) "twelve: a creature"
    Spec.assertBool s (isArtifact frigateId twelve) "and still an artifact"
    Spec.assertEqWith s "no size at eleven" (sizeOf frigateId eleven) (Nothing, Nothing)
    Spec.assertEqWith s "3/5 at twelve" (sizeOf frigateId twelve) (Just 3, Just 5)
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Type.Flying frigateId eleven)) "no flying at eleven"
    Spec.assertBool s (Projection.hasKeyword Keyword.Type.Flying frigateId twelve) "flying at twelve"
    Spec.assertBool s (Projection.hasKeyword Keyword.Type.Lifelink frigateId twelve) "and lifelink"
