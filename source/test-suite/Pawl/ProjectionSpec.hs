-- Pattern matching on Pawl.Types.Prompt, a GADT, in aimAtObject below.
{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Projection: the layer fold -- CR 613 layer order, CR 613.7
-- within-layer timestamp order, and the CR 613.8 dependency reorder that
-- overrides it. Mostly directly-constructed continuous effects, so the engine is
-- proven independently of any card wiring; the card-level proofs live alongside.
-- Also Pawl.Engine.Subtype, the CR 205.3i land-type and CR 205.3m creature-type
-- classifications the layer-4 SetLandSubtype, SetCreatureSubtype,
-- AddCreatureSubtype and AddEveryCreatureSubtype arms fold with.
module Pawl.ProjectionSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.CardSpec as CardSpec
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
-- Pawl.Types.Filter aliased Filter.Type: the evaluator Pawl.Engine.Filter already claims
-- the alias Filter above (documented phase exception). Pawl.Types.Subtype is
-- aliased Subtype.Type below for the same reason, against Pawl.Engine.Subtype.

import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Subtype as Subtype
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layer as Layer
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype.Type
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- alice has a Forest for mana, a Piker on the battlefield, and Giant Growth in
-- hand, in her main phase. Cast Giant Growth (identityAnswer targets the only
-- creature), then resolve it.
giantGrowthOnPiker :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
giantGrowthOnPiker forest piker giantGrowth =
  let base = S.landsInPlay forest 1
      (pikerId, withPiker) = S.addCreature piker S.alice base
      (gs, ggId) = S.handOne giantGrowth withPiker
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice ggId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (pikerId, resolved)

-- Chooses X = 4; every other prompt takes the identity fallback, which targets
-- the only creature on the board. The liar pattern CastSpec's answerX3 uses.
answerX4 :: Prompt.Prompt r -> r
answerX4 p = case p of
  Prompt.ChooseX {} -> 4
  _ -> S.identityAnswer p

-- alice has five Forests, a Goblin Piker on the battlefield, and Untamed Might
-- ({X}{G}, "target creature gets +X/+X until end of turn") in hand. Cast it at
-- the Piker for X = 4 -- five Forests is exactly {4}{G} -- and resolve it.
untamedMightOnPiker :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
untamedMightOnPiker forest piker untamedMight =
  let base = S.landsInPlay forest 5
      (pikerId, withPiker) = S.addCreature piker S.alice base
      (gs, umId) = S.handOne untamedMight withPiker
      cast = snd (Engine.runGamePure answerX4 gs (S.cast S.alice umId))
      resolved = snd (Engine.runGamePure answerX4 cast Stack.resolveTop)
   in (pikerId, resolved)

-- Append a stored continuous effect over a dynamic set, at timestamp `ts`.
withDynamicEffect :: Affected.Affected -> Timestamp.Timestamp -> Modification.Modification -> GameState.GameState -> GameState.GameState
withDynamicEffect aff ts m gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 997,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = aff
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

-- Aims every target slot at one object, deferring the rest to S.identityAnswer
-- (ModalSpec.chooseModeAt's shape). Liquimetal Coating's "target permanent" admits
-- every permanent on the board, so the choice has to be answered rather than
-- forced by construction.
aimAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- Records every library-search candidate list and every shuffled library, and
-- finds `wanted`. The recording is the point: what a search FINDS cannot tell a
-- candidate set that was computed correctly from one that admitted everything,
-- so the set itself has to be observed.
searchRecordingAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State ([[ObjectId.ObjectId]], [[ObjectId.ObjectId]]) r
searchRecordingAnswer wanted p = case p of
  Prompt.SearchLibrary _ _ matches _ -> do
    State.modify' (\(searches, shuffles) -> (searches <> [matches], shuffles))
    pure [wanted]
  Prompt.Shuffle library -> do
    State.modify' (\(searches, shuffles) -> (searches, shuffles <> [library]))
    -- An identity permutation: what was shuffled is the assertion, not the order.
    pure library
  _ -> pure (S.identityAnswer p)

-- The card names behind a set of object ids, as a Set so an assertion reads as
-- "exactly these cards" without depending on library order.
namesOf :: GameState.GameState -> [ObjectId.ObjectId] -> Set.Set Text.Text
namesOf gs = Set.fromList . fmap CardName.unwrap . Maybe.mapMaybe (fmap Face.name . flip Game.faceOf gs)

-- Imperial Recruiter's search candidates, by card name, over a board whose
-- graveyards hold `buried`. Alice's library is fixed: the Tarmogoyf CR 208.2a is
-- about, a Goblin Piker (printed 2, a candidate on every board, so the prompt is
-- never short-circuited down to the one card it had to offer), a Hill Giant
-- (printed 3, out on every board) and a Mountain (out on the creature clause).
-- The Piker is what the answerer takes, so no board's search fails for want of a
-- legal pick and the candidate SET is the only thing that moves.
recruiterCandidates :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [String] -> m [Set.Set Text.Text]
recruiterCandidates s registry buried = do
  mountain <- S.printingOf s registry "Mountain"
  recruiter <- S.printingOf s registry "Imperial Recruiter"
  goyf <- S.printingOf s registry "Tarmogoyf"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  graveyard <- traverse (S.printingOf s registry) buried
  let bury board printing = snd (S.addGraveyardCard printing S.alice board)
      base0 = Foldable.foldl' bury (S.landsInPlay mountain 3) graveyard
      (_, base1) = S.addLibraryCard mountain S.alice base0
      (_, base2) = S.addLibraryCard giant S.alice base1
      (_, base3) = S.addLibraryCard goyf S.alice base2
      (pikerId, base4) = S.addLibraryCard piker S.alice base3
      (gs, spellId) = S.handOne recruiter base4
      (_, (searches, _)) =
        State.runState
          (Engine.runGame (searchRecordingAnswer pikerId) gs (do S.cast S.alice spellId; Engine.priorityLoop))
          ([], [])
  pure (fmap (namesOf gs) searches)

-- Goblin Matron's search candidates, by card name, over a board that either has
-- Maskwood Nexus on the battlefield or does not. Alice's library is fixed: a
-- Goblin Piker (printed a Goblin, so both boards have a candidate and the prompt
-- is never short-circuited down to the one card it had to offer), a Hill Giant
-- (a creature card printed a Giant) and a Mountain (a land, outside the Nexus's
-- creature-card set on either board). The Piker is what the answerer takes, so
-- neither search fails for want of a legal pick and the candidate SET is the
-- only thing that moves.
--
-- The library is stocked BEFORE the Nexus reaches the battlefield, which is what
-- separates "the effect applied to those cards as they arrived" from "the effect
-- applies to cards sitting in a library": the former reads the same set on both
-- boards.
matronCandidates :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m [Set.Set Text.Text]
matronCandidates s registry withNexus = do
  mountain <- S.printingOf s registry "Mountain"
  matron <- S.printingOf s registry "Goblin Matron"
  nexus <- S.printingOf s registry "Maskwood Nexus"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  let base0 = S.landsInPlay mountain 3
      (_, base1) = S.addLibraryCard mountain S.alice base0
      (_, base2) = S.addLibraryCard giant S.alice base1
      (pikerId, base3) = S.addLibraryCard piker S.alice base2
      base4 = if withNexus then snd (S.addCreature nexus S.alice base3) else base3
      (gs, spellId) = S.handOne matron base4
      (_, (searches, _)) =
        State.runState
          (Engine.runGame (matronAnswer pikerId) gs (do S.cast S.alice spellId; Engine.priorityLoop))
          ([], [])
  pure (fmap (namesOf gs) searches)

-- searchRecordingAnswer with CR 603.5's "may" exercised, which Goblin Matron's
-- trigger asks and Imperial Recruiter's mandatory one does not.
matronAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State ([[ObjectId.ObjectId]], [[ObjectId.ObjectId]]) r
matronAnswer wanted p = case p of
  Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
  _ -> searchRecordingAnswer wanted p

-- aimAtObject for a Pool.Creatures slot, whose recipients are ToCreature.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- Put a one-target creature spell into alice's hand, cast it AT `victimId`, and
-- resolve it. `board` must already hold enough untapped lands for the cost. The
-- target is answered rather than forced by construction, because the boards below
-- hold more than one creature.
castAtCreature :: ObjectId.ObjectId -> Printing.Printing -> GameState.GameState -> GameState.GameState
castAtCreature victimId printing board =
  let (gs, spellId) = S.handOne printing board
      cast = snd (Engine.runGamePure (aimAtCreature victimId) gs (S.cast S.alice spellId))
   in snd (Engine.runGamePure (aimAtCreature victimId) cast Stack.resolveTop)

-- The board CR 604.3a's two routes for changeling are told apart on. A Goblin
-- Piker and a Woodland Changeling, each Turn-to-Frogged, and then -- only when
-- `granted` -- Synthetic Borrowed Shape enchanting the Piker. The Aura enters
-- AFTER both spells resolve, so CR 613.7a stamps its effects later than theirs;
-- the pair of boards differs in the Aura and in nothing else.
borrowedShapeBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
borrowedShapeBoard s registry granted = do
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  changeling <- S.printingOf s registry "Woodland Changeling"
  turnToFrog <- S.printingOf s registry "Turn to Frog"
  aura <- S.printingOf s registry "Synthetic Borrowed Shape"
  let (changelingId, g1) = S.addCreature changeling S.alice (S.landsInPlay island 4)
      (pikerId, g2) = S.addCreature piker S.alice g1
      frogged = castAtCreature pikerId turnToFrog (castAtCreature changelingId turnToFrog g2)
      gs =
        if not granted
          then frogged
          else
            let (auraId, g3) = S.addCreature aura S.alice frogged
             in S.attach auraId pikerId g3
  pure (pikerId, changelingId, gs)

-- The object timestamp of the (single) Humility on the battlefield.
humilityTimestamp :: Printing.Printing -> GameState.GameState -> Timestamp.Timestamp
humilityTimestamp humility gs =
  let isHum oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard p -> Printing.card p == Printing.card humility
          Source.OfToken _ -> False
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
      hums = filter isHum (Set.toList (GameState.battlefield gs))
      stampOf oid = fmap Object.timestamp (Game.lookupObject oid gs)
   in case Maybe.mapMaybe stampOf hums of
        t : _ -> t
        [] -> Timestamp.MkTimestamp 0

-- Two synthetic nonbasic lands, each printing "OTHER nonbasic lands are [type]", so each
-- sits inside the other's affected set and the two form a CR 613.8a dependency
-- loop. "Other" (a Not IsSource conjunct) keeps each out of its OWN affected set,
-- which isolates rule 613.8b: a land that stripped itself would raise the separate
-- and unsettled question of whether an effect that removes the ability generating
-- it keeps applying (#945), and this pair asks only about the loop. `lunarFirst` controls which is older -- fresh timestamps ascend with
-- placement -- and rule 613.8b makes the older one the winner, so unlike
-- bloodMoonUrborg below the answer is deliberately order-DEPENDENT.
wasteLoop :: Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
wasteLoop lunar tidal lunarFirst =
  let base = Setup.emptyGame S.bothPlayers
   in if lunarFirst
        then
          let (l, g1) = S.addCreature lunar S.alice base
              (t, g2) = S.addCreature tidal S.alice g1
           in (l, t, g2)
        else
          let (t, g1) = S.addCreature tidal S.alice base
              (l, g2) = S.addCreature lunar S.alice g1
           in (l, t, g2)

-- Blood Moon, Urborg, and a Forest on the battlefield. `urborgFirst` controls
-- the timestamp order (fresh timestamps ascend with placement), to prove the
-- outcome is order-INDEPENDENT (CR 613.8 dependency overrides CR 613.7).
bloodMoonUrborg :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
bloodMoonUrborg forest urborg bloodMoon urborgFirst =
  let base = Setup.emptyGame S.bothPlayers
      (forestId, g1) = S.addCreature forest S.alice base
      place g =
        if urborgFirst
          then
            let (u, g') = S.addCreature urborg S.alice g
                (_, g'') = S.addCreature bloodMoon S.alice g'
             in (u, g'')
          else
            let (_, g') = S.addCreature bloodMoon S.alice g
                (u, g'') = S.addCreature urborg S.alice g'
             in (u, g'')
      (urborgId, gs) = place g1
   in (forestId, urborgId, gs)

-- Ashaya, Blood Moon, a Goblin Piker and a Goblin Piker TOKEN, all under alice,
-- plus a basic Forest for the CDA to count. `ashayaFirst` controls the timestamp
-- order (fresh timestamps ascend with placement).
--
-- The mirror image of bloodMoonUrborg. There, the LATER-applying effect (Urborg's)
-- is the one that gets stripped, so it never applies. Here it is the other way
-- round: Ashaya's layer-4 type change is what puts her (and every other nontoken
-- creature you control) INTO Blood Moon's affected set, so Blood Moon depends on
-- Ashaya under CR 613.8a and must apply second -- in either timestamp order.
ashayaBloodMoon :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
ashayaBloodMoon forest piker ashaya bloodMoon ashayaFirst =
  let base = Setup.emptyGame S.bothPlayers
      (forestId, g1) = S.addCreature forest S.alice base
      (pikerId, g2) = S.addCreature piker S.alice g1
      (tokenId, g3) = S.addToken (Printing.card piker) S.alice g2
      place g =
        if ashayaFirst
          then
            let (a, g') = S.addCreature ashaya S.alice g
                (_, g'') = S.addCreature bloodMoon S.alice g'
             in (a, g'')
          else
            let (_, g') = S.addCreature bloodMoon S.alice g
                (a, g'') = S.addCreature ashaya S.alice g'
             in (a, g'')
      (ashayaId, gs) = place g3
   in (forestId, pikerId, tokenId, ashayaId, gs)

-- Life and Limb, Blood Moon, a Bayou and a Shroofus Sproutsire, all under alice.
-- `limbFirst` controls the timestamp order (fresh timestamps ascend with
-- placement).
--
-- CR 613.8b's dependency LOOP, and it takes both permanents to see it. Life and
-- Limb depends on Blood Moon at the Bayou: CR 305.7's set deletes the Forest
-- subtype, so the Bayou leaves "all Forests". Blood Moon depends on Life and Limb
-- at Shroofus: the Saproling becomes a nonbasic land, so it enters "nonbasic
-- lands". Neither edge is visible at the other permanent, so only a relation
-- decided over the whole board sees the loop at all: asked one permanent at a
-- time, each reports a different one-way dependency, and the pair of answers is
-- one no single order of the two effects produces.
--
-- Shroofus is the pool's only printed nontoken Saproling. His combat-damage token
-- trigger never fires here: these cases read a projection off a board that runs
-- no combat.
-- Synthetic Ferocious Chorus, Bad Moon, a Bog Wraith and a Child of Night, all
-- alice's. `chorusFirst` controls the timestamp order (fresh timestamps ascend
-- with placement).
--
-- CR 613.8a clause (b)'s "what it does to" limb, the one no affected set moves:
-- both effects are CR 613.4c layer 7c, Bad Moon's set is the black creatures and
-- the Chorus's is alice's creatures, and neither writes anything the other's
-- filter reads. What Bad Moon moves is HOW MUCH the Chorus does -- its "for each
-- creature you control with power 4 or greater" counts a Bog Wraith only Bad
-- Moon lifts to 4 -- so the Chorus depends on it and CR 613.8b makes it wait, in
-- either timestamp order.
--
-- The Child of Night is what stops the count reading as "every creature": Bad
-- Moon leaves it 3/2, under the threshold, so the count is 1 and not 2. It also
-- rules out a fixpoint reading, since the FINISHED board holds two creatures with
-- power 4 or greater and the count was still 1 -- CR 613.8b's "just after all of
-- those effects have been applied", asked once, not re-asked.
--
-- Synthetic (#157): the effect DSL puts a quantity in the two layer-7 arms alone,
-- so the only count that can read its own layer is a P/T effect counting power,
-- and every printed one is a resolution's frozen number (CR 608.2h), a cost
-- modification (CR 613.11), or Sword of the Squeak's BASE power -- which reads
-- layer 7b from 7c and is exact already.
chorusBadMoon :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
chorusBadMoon chorus badMoon bogWraith childOfNight chorusFirst =
  let place g =
        if chorusFirst
          then snd (S.addCreature badMoon S.alice (snd (S.addCreature chorus S.alice g)))
          else snd (S.addCreature chorus S.alice (snd (S.addCreature badMoon S.alice g)))
      (wraithId, g2) = S.addCreature bogWraith S.alice (place (Setup.emptyGame S.bothPlayers))
      (childId, gs) = S.addCreature childOfNight S.alice g2
   in (wraithId, childId, gs)

limbBloodMoon :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
limbBloodMoon bayou shroofus limb bloodMoon limbFirst =
  let base = Setup.emptyGame S.bothPlayers
      (bayouId, g1) = S.addCreature bayou S.alice base
      (shroofusId, g2) = S.addCreature shroofus S.alice g1
      place g =
        if limbFirst
          then snd (S.addCreature bloodMoon S.alice (snd (S.addCreature limb S.alice g)))
          else snd (S.addCreature limb S.alice (snd (S.addCreature bloodMoon S.alice g)))
   in (bayouId, shroofusId, place g2)

-- alice has four Forests, an Abomination of Llanowar on the battlefield, two
-- cards of `stocked` already in her graveyard and Maskwood Nexus in hand. Cast
-- the Nexus, and read the Abomination's power BEFORE and AFTER it resolves.
--
-- The pair is the two readings of one board: the graveyard is stocked before
-- the Nexus is cast and nothing moves between them, so the only difference is
-- that the Nexus's continuous effect exists in the second.
abominationAcrossNexus ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (Maybe Integer, Maybe Integer)
abominationAcrossNexus forest abomination nexus stocked =
  let base = S.landsInPlay forest 4
      (_, g1) = S.addGraveyardCard stocked S.alice base
      (_, g2) = S.addGraveyardCard stocked S.alice g1
      (abominationId, g3) = S.addCreature abomination S.alice g2
      (g4, nexusId) = S.handOne nexus g3
      cast = snd (Engine.runGamePure S.identityAnswer g4 (S.cast S.alice nexusId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (Projection.powerOf abominationId cast, Projection.powerOf abominationId resolved)

-- Elspeth, Sun's Champion {4}{W}{W} Legendary Planeswalker -- Elspeth, loyalty
-- 4. "-7: You get an emblem with \"Creatures you control get +2/+2 and have
-- flying.\"" (Name, cost, type line, loyalty and oracle text checked against
-- Scryfall.) The pool's first emblem with a STATIC ability, and what proves
-- CR 114.4 -- "abilities of emblems function in the command zone" -- for the
-- layer fold: the modifications are layer 7c and layer 6, and their only bearer
-- is an object CR 114.1 keeps in the command zone.
--
-- One board, read twice; the Bool is the single difference, whether the
-- ultimate was activated. Everything else -- seats, creatures, Elspeth's
-- loyalty -- is held equal, so a reading that came from the planeswalker's own
-- presence rather than from the emblem it minted would move both halves.
--
-- Three seats, and three creatures of two printed sizes: alice's Goblin Piker
-- (2/1), bob's Hill Giant (3/3) and carol's Piker. "Creatures you control" is
-- CR 114.2's controller, so a fold that took the active player, the emblem's
-- owner-as-everyone, or the whole battlefield would buff one of the other two.
-- The sizes keep the numbers apart: the buffed Piker is 4/3, which no unbuffed
-- creature on the board reads as.
--
-- The loyalty is a fixture rather than seven turns of +1: CR 306.5b's counters
-- are what the ability's cost pays, and how they got there is no part of this.
elspethEmblemBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Bool ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
elspethEmblemBoard s registry ultimate = do
  elspeth <- S.printingOf s registry "Elspeth, Sun's Champion"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  let (elspethId, g1) = S.addCreature elspeth S.alice S.threePlayerGame
      (mine, g2) = S.addCreature piker S.alice g1
      (theirs, g3) = S.addCreature giant S.bob g2
      (carols, g4) = S.addCreature piker S.carol g3
      armed = S.addCounter CounterKind.Loyalty 7 elspethId g4
      -- The third loyalty ability, in printed order: +1, -3, -7.
      used = case (ultimate, drop 2 (Face.activatedAbilities (S.combinedFace elspeth))) of
        (True, ability : _) -> S.runPure S.identityAnswer armed (do Activate.activateAbility S.alice elspethId ability; Stack.resolveTop)
        _ -> armed
  pure (mine, theirs, carols, used)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Projection" $ do
  Spec.it s "layer classification matches CR 613.1" $ do
    Spec.assertEqWith s "grant is layer 6" (Projection.layer (Modification.GainKeyword Keyword.Deathtouch)) Layer.Ability
    Spec.assertEqWith s "lose-all is layer 6" (Projection.layer Modification.LoseAllAbilities) Layer.Ability
    Spec.assertEqWith s "set base is 7b" (Projection.layer (Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)))) Layer.SetPT
    Spec.assertEqWith s "modify is 7c" (Projection.layer (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)))) Layer.ModifyPT

  -- CR 613.1b: layer 2 is where control-changing effects apply, whether the new
  -- controller was baked at resolution (SetController) or is derived from the
  -- effect's source (SetControllerToSource).
  Spec.it s "CR 613.1b: SetControllerToSource is a layer-2 modification" $
    Spec.assertEqWith s "layer 2" (Projection.layer Modification.SetControllerToSource) Layer.Control

  Spec.it s "no effects: the projection is the base printing (Piker is 2/1)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
    Spec.assertEqWith s "power" (Projection.powerOf oid gs) (Just 2)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf oid gs) (Just 1)
    Spec.assertBool s (Map.null (Projection.keywordsOf oid gs)) "no keywords"

  Spec.it s "CR 613.3 layer 7c +3/+3 raises a Piker to 5/4" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))) gs0
    Spec.assertEqWith s "power" (Projection.powerOf oid gs) (Just 5)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf oid gs) (Just 4)

  Spec.it s "CR 613 layer 6 GainKeyword adds deathtouch" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.GainKeyword Keyword.Deathtouch) gs0
    Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch oid gs) "has deathtouch"

  -- CR 702.164b's own example: "If a creature with toxic 2 gains toxic 1 due
  -- to another effect, its total toxic value is 3." The two abilities are
  -- distinct, so they sum rather than shadow each other.
  Spec.it s "CR 702.164b total toxic value is the SUM of a creature's toxic abilities" $ do
    stalker <- S.printingOf s registry "Branchblight Stalker"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs0) = S.addCreature stalker S.bob (S.landsInPlay mountain 1)
        (plain, gs1) = S.addCreature piker S.bob gs0
        gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.GainKeyword (Keyword.Toxic 1)) gs1
    Spec.assertEqWith s "printed toxic 2 alone" (Projection.totalToxic oid gs1) 2
    Spec.assertEqWith s "toxic 2 plus a granted toxic 1" (Projection.totalToxic oid gs) 3
    Spec.assertEqWith s "a creature without toxic has a total toxic value of zero" (Projection.totalToxic plain gs) 0

  -- Rule 702.164 has no redundancy clause -- contrast CR 702.3c and 702.9c,
  -- which say in so many words that multiple instances of defender and of
  -- flying ARE redundant. So two toxic 1 abilities are two abilities, and
  -- CR 702.164b sums both. The falsifier is a projection that keeps keywords
  -- in a Set, where the second toxic 1 collapses into the first.
  --
  -- The flying half of the same test is the control: multiplicity is
  -- tracked for every keyword, and CR 702.9c redundancy is a fact about what
  -- READERS ask (hasKeyword), not about what the projection stores.
  Spec.it s "CR 702.164b two toxic abilities with the SAME N both count" $ do
    stalker <- S.printingOf s registry "Branchblight Stalker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs0) = S.addCreature stalker S.bob (S.landsInPlay mountain 1)
        grant ts = S.withEffectAt oid (Timestamp.MkTimestamp ts)
        gs =
          grant 101 (Modification.GainKeyword (Keyword.Toxic 1))
            . grant 100 (Modification.GainKeyword (Keyword.Toxic 1))
            $ gs0
    Spec.assertEqWith s "toxic 2 plus TWO granted toxic 1s" (Projection.totalToxic oid gs) 4
    let flown =
          grant 103 (Modification.GainKeyword Keyword.Flying)
            . grant 102 (Modification.GainKeyword Keyword.Flying)
            $ gs
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying oid flown) "CR 702.9c: two flying grants still just fly"
    Spec.assertEqWith s "and do not disturb the total toxic value" (Projection.totalToxic oid flown) 4

  Spec.it s "CR 613 layer 7b SetBasePowerToughness makes a Piker 1/1" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1))) gs0
    Spec.assertEqWith s "power" (Projection.powerOf oid gs) (Just 1)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf oid gs) (Just 1)

  Spec.it s "CR 613 sublayer order: 7b then 7c, a set-1/1 Piker with +3/+3 is 4/4" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        -- Deliberately give 7c the EARLIER timestamp to prove layer beats
        -- timestamp: 7b still applies first.
        gs1 = S.withEffectAt oid (Timestamp.MkTimestamp 50) (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))) gs0
        gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1))) gs1
    Spec.assertEqWith s "power 1 then +3" (Projection.powerOf oid gs) (Just 4)
    Spec.assertEqWith s "toughness 1 then +3" (Projection.toughnessOf oid gs) (Just 4)

  Spec.it s "CR 613.7 within layer 6, timestamp order: later grant survives an earlier lose-all" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        gs1 = S.withEffectAt oid (Timestamp.MkTimestamp 10) Modification.LoseAllAbilities gs0
        gs = S.withEffectAt oid (Timestamp.MkTimestamp 20) (Modification.GainKeyword Keyword.Deathtouch) gs1
    Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch oid gs) "grant wins"

  Spec.it s "CR 613.7 within layer 6, timestamp order: earlier grant is erased by a later lose-all" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        gs1 = S.withEffectAt oid (Timestamp.MkTimestamp 10) (Modification.GainKeyword Keyword.Deathtouch) gs0
        gs = S.withEffectAt oid (Timestamp.MkTimestamp 20) Modification.LoseAllAbilities gs1
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Deathtouch oid gs)) "lose-all wins"

  Spec.it s "a P/T modification never gives P/T to a land" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs0 = S.landsInPlay mountain 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))) gs0
    Spec.assertEqWith s "still no power" (Projection.powerOf landId gs) Nothing

  Spec.it s "CR 611 Giant Growth stores a +3/+3 effect; the Piker is 5/4" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    giantGrowth <- S.printingOf s registry "Giant Growth"
    let (pikerId, gs) = giantGrowthOnPiker forest piker giantGrowth
    Spec.assertEqWith s "one stored effect" (length (GameState.continuousEffects gs)) 1
    Spec.assertEqWith s "power" (Projection.powerOf pikerId gs) (Just 5)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf pikerId gs) (Just 4)

  -- The freeze's proving card. CR 608.2h: "the answer is determined only
  -- once, when the effect is applied", and CR 611.2d says the same of a
  -- variable such as X. X is bound on the SPELL, which by the time these
  -- assertions run is in its owner's graveyard: CR 608.2n puts it there "as
  -- the final part of an instant or sorcery spell's resolution". A stored
  -- `Quantity.X` would be re-read against the PIKER all the same --
  -- applyModification evaluates against the AFFECTED object wherever the
  -- source sits -- and the Piker carries no such binding, so the delta is
  -- unevaluable and addPT drops it, leaving the printed 2/1. Frozen, the pump
  -- is a pair of Literals and survives for the rest of the turn.
  Spec.it s "CR 608.2h/611.2d Untamed Might's X is frozen at resolution, not re-read" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    untamedMight <- S.printingOf s registry "Untamed Might"
    let (pikerId, gs) = untamedMightOnPiker forest piker untamedMight
    Spec.assertEqWith s "the spell has left the stack" (GameState.stack gs) []
    Spec.assertEqWith s "power 2 + 4" (Projection.powerOf pikerId gs) (Just 6)
    Spec.assertEqWith s "toughness 1 + 4" (Projection.toughnessOf pikerId gs) (Just 5)
    -- What was stored, not just what it projects to: the X is gone, replaced
    -- by the value chosen when the effect was applied.
    Spec.assertEqWith
      s
      "the stored modification is a pair of Literals"
      (fmap ContinuousEffect.modification (GameState.continuousEffects gs))
      [Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 4) (Quantity.Literal 4))]
    -- And it stays frozen across later passes: a state-based-action pass
    -- reprojects everything, and CR 514.2 is what finally ends it.
    let afterSba = S.settleSba gs
    Spec.assertEqWith s "still 6/5 after an SBA pass" (Projection.powerOf pikerId afterSba, Projection.toughnessOf pikerId afterSba) (Just 6, Just 5)
    let afterCleanup = snd (Engine.runGamePure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
    Spec.assertEqWith s "CR 514.2 back to 2/1 at cleanup" (Projection.powerOf pikerId afterCleanup, Projection.toughnessOf pikerId afterCleanup) (Just 2, Just 1)

  -- The freeze's own contract, read off the function rather than the board.
  -- CR 608.2h gives the effect ONE moment to determine its answer, so a
  -- quantity with no answer at that moment has none to defer to: the whole
  -- modification is refused, and Resolve stores nothing (ResolveSpec's "a
  -- modification that cannot be frozen is not stored at all").
  Spec.it s "CR 608.2h freezeQuantities locks an answerable quantity and refuses an unanswerable one" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        freeze = Projection.freezeQuantities gs pikerId (Just S.alice)
    Spec.assertEqWith
      s
      "read against the SOURCE, the Piker's power locks in at 2"
      (freeze (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness Quantity.Power Quantity.Power)))
      (Just (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 2) (Quantity.Literal 2))))
    -- CR 208.2: a bare star has no value of its own -- the projection
    -- substitutes the object's characteristic-defining quantity for it at the
    -- seed, so one reaching here was never resolved and has no answer.
    Spec.assertEqWith
      s
      "one unanswerable half refuses the whole modification"
      (freeze (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) Quantity.Star)))
      Nothing
    Spec.assertEqWith
      s
      "a modification carrying no quantity always freezes"
      (freeze (Modification.GainKeyword Keyword.Flying))
      (Just (Modification.GainKeyword Keyword.Flying))

  Spec.it s "CR 601.2c Giant Growth is uncastable with no creature to target" $ do
    forest <- S.printingOf s registry "Forest"
    giantGrowth <- S.printingOf s registry "Giant Growth"
    let (gs, ggId) = S.handOne giantGrowth (S.landsInPlay forest 1)
    Spec.assertBool s (not (S.castable S.alice ggId gs)) "no legal target, not castable"

  Spec.it s "CR 514.2 an until-end-of-turn effect wears off at cleanup" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    giantGrowth <- S.printingOf s registry "Giant Growth"
    let (pikerId, cast) = giantGrowthOnPiker forest piker giantGrowth
        -- Run the cleanup step's turn-based actions; the +3/+3 must be gone.
        afterCleanup = snd (Engine.runGamePure S.identityAnswer cast (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
    Spec.assertEqWith s "effect dropped" (GameState.continuousEffects afterCleanup) []
    Spec.assertEqWith s "Piker back to base power" (Projection.powerOf pikerId afterCleanup) (Just 2)
    Spec.assertEqWith s "Piker back to base toughness" (Projection.toughnessOf pikerId afterCleanup) (Just 1)

  Spec.it s "CR 613 Humility makes every creature 1/1 with no abilities" $ do
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    mountain <- S.printingOf s registry "Mountain"
    humility <- S.printingOf s registry "Humility"
    let (flyerId, gs0) = S.addCreature birdMaiden S.bob (S.landsInPlay mountain 1)
        gs = S.withHumility humility gs0
    Spec.assertEqWith s "power 1" (Projection.powerOf flyerId gs) (Just 1)
    Spec.assertEqWith s "toughness 1" (Projection.toughnessOf flyerId gs) (Just 1)
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying flyerId gs)) "no flying"

  Spec.it s "CR 613 layer 6: Humility strips a creature's activated abilities" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    humility <- S.printingOf s registry "Humility"
    let (sorcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        gs = S.withHumility humility g0
    Spec.assertEqWith s "no abilities under Humility" (Projection.abilitiesOf sorcId gs) []

  Spec.it s "without Humility the ability is present" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (sorcId, gs) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "one ability" (length (Projection.abilitiesOf sorcId gs)) 1

  -- The same layer-6 removal from the OTHER kind of ability, and the card whose
  -- ability does not sit at layer 6 alone: Titania's Song removes at 6, animates
  -- at 4 and sets base P/T at 7b, all as ONE effect. CR 613.6 fixes its "each
  -- noncreature artifact" set at layer 4 -- the Coating is still noncreature
  -- there -- and every later part of the Song reuses that answer, which is why
  -- the removal lands on a permanent the Song has itself just made a creature.
  Spec.it s "CR 613.6 Titania's Song strips the artifact its own layer-4 part animates" $ do
    liquimetalCoating <- S.printingOf s registry "Liquimetal Coating"
    titaniasSong <- S.printingOf s registry "Titania's Song"
    let (coatingId, g0) = S.addCreature liquimetalCoating S.alice (Setup.emptyGame S.bothPlayers)
        gs = snd (S.addCreature titaniasSong S.bob g0)
    Spec.assertEqWith s "control: the Coating's {T} ability is there to begin with" (length (Projection.abilitiesOf coatingId g0)) 1
    Spec.assertEqWith s "CR 613.1f: and gone under the Song" (Projection.abilitiesOf coatingId gs) []
    Spec.assertEqWith s "CR 613.4b: base power is its mana value" (Projection.powerOf coatingId gs) (Just 2)
    Spec.assertEqWith s "CR 613.4b: base toughness too" (Projection.toughnessOf coatingId gs) (Just 2)

  -- The same card one layer deeper: CR 613.6 fixes Titania's Song's set at layer
  -- 4, and CR 613.7/613.8 say that layer's EARLIER effects have already applied
  -- when the question is asked. Liquimetal Coating's AddCardType is one of them,
  -- so a permanent it turned into an artifact is inside "each noncreature
  -- artifact" -- and the removal half has to agree with the animation half.
  --
  -- Bad Moon is the coated permanent because it satisfies three things at once:
  -- it is a noncreature, nonartifact permanent, its mana value is 2 rather than
  -- 0 (a land would be a 0/0 and CR 704.5f would bury it before anything could
  -- be read), and its ability is an ANTHEM, so whether the Song silenced it is
  -- observable on a bystander rather than on Bad Moon alone.
  --
  -- ORDER IS LOAD-BEARING: the Coating's ability resolves BEFORE the Song
  -- arrives. With the Song out first the Coating is itself a noncreature
  -- artifact, loses its abilities (the test above proves exactly that), and
  -- could not be activated at all.
  Spec.it s "CR 613.6 Titania's Song strips a permanent Liquimetal Coating made an artifact in the same layer" $ do
    liquimetalCoating <- S.printingOf s registry "Liquimetal Coating"
    titaniasSong <- S.printingOf s registry "Titania's Song"
    badMoon <- S.printingOf s registry "Bad Moon"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    let (moonId, g0) = S.addCreature badMoon S.alice (Setup.emptyGame S.bothPlayers)
        (wraithId, g1) = S.addCreature bogWraith S.alice g0
        (coatingId, g2) = S.addCreature liquimetalCoating S.alice g1
        ability = case Face.activatedAbilities (S.combinedFace liquimetalCoating) of
          ab : _ -> Just ab
          [] -> Nothing
    case ability of
      Nothing -> Spec.assertFailure s "Liquimetal Coating should print one activated ability"
      Just coat -> do
        let ready = g2 {GameState.priority = Just S.alice}
            activated = snd (Engine.runGamePure (aimAtObject moonId) ready (Activate.activateAbility S.alice coatingId coat))
            coated = snd (Engine.runGamePure (aimAtObject moonId) activated Stack.resolveTop)
            sung = snd (S.addCreature titaniasSong S.bob coated)
            -- The control: the same three permanents plus the Song, with the
            -- Coating's ability never activated. Without it the whole test would
            -- pass for the wrong reason if the activation silently no-opped.
            unsung = snd (S.addCreature titaniasSong S.bob g2)
        Spec.assertBool s (Set.member CardType.Artifact (Projection.cardTypesOf moonId coated)) "the Coating made Bad Moon an artifact"
        -- Assertion 1: the animation half of the Song reached it.
        Spec.assertBool s (Projection.isCreatureOf moonId sung) "CR 613.1d: the Song animates it at layer 4"
        -- Assertion 2: base 2/2 from CR 613.4b, and NOT 3/3 -- Bad Moon's own
        -- anthem would pump a black creature, itself included, if the Song had
        -- not taken the ability away at layer 6.
        Spec.assertEqWith s "CR 613.1f: base 2/2, unpumped, because its own anthem is gone" (Projection.powerOf moonId sung) (Just 2)
        -- Assertion 3: the same removal seen from outside. This is the one that
        -- fails when the removal gate reads a pre-layer-4 board.
        Spec.assertEqWith s "CR 613.1f: and gone for bystanders too, so the Wraith is 3/3" (Projection.powerOf wraithId sung) (Just 3)
        -- Assertion 4, the control: no Coating activation, so Bad Moon is not a
        -- noncreature ARTIFACT, the Song passes it over, and the anthem lives.
        Spec.assertBool s (not (Projection.isCreatureOf moonId unsung)) "control: uncoated, the Song does not animate Bad Moon"
        Spec.assertEqWith s "control: so the anthem still pumps the Wraith to 4/4" (Projection.powerOf wraithId unsung) (Just 4)

  Spec.it s "CR 704.5g Humility's toughness drop makes an already-damaged creature die" $ do
    warMammoth <- S.printingOf s registry "War Mammoth"
    mountain <- S.printingOf s registry "Mountain"
    humility <- S.printingOf s registry "Humility"
    let (mammothId, gs0) = S.addCreature warMammoth S.bob (S.landsInPlay mountain 1)
        damaged = S.markDamage mammothId 2 gs0
        underHumility = S.withHumility humility damaged
        afterSba = S.settleSba underHumility
    Spec.assertEqWith s "survives at 3/3 with 2 marked" (Projection.toughnessOf mammothId damaged) (Just 3)
    Spec.assertEqWith s "no creature survives once toughness is 1" (S.creaturesInPlay S.bob afterSba) 0

  Spec.it s "CR 613 layer order: Giant Growth on a Humility'd Piker is 4/4" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    giantGrowth <- S.printingOf s registry "Giant Growth"
    let base = S.landsInPlay forest 1
        (pikerId, withPiker) = S.addCreature piker S.alice base
        withHum = S.withHumility humility withPiker
        (gs, ggId) = S.handOne giantGrowth withHum
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice ggId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    -- Layer 7b (set 1/1) before 7c (+3/+3): 1 then +3 = 4.
    Spec.assertEqWith s "power" (Projection.powerOf pikerId resolved) (Just 4)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf pikerId resolved) (Just 4)

  Spec.it s "CR 611 Serpent's Gift grants deathtouch to its target" $ do
    -- {2}{G} needs 3 total mana; 3 Forests, not 2 (a brief fixture bug --
    -- 2 Forests only pay {1}{G}, leaving the spell uncast and the assertion
    -- vacuously true off the base card's native trample).
    forest <- S.printingOf s registry "Forest"
    warMammoth <- S.printingOf s registry "War Mammoth"
    serpentsGift <- S.printingOf s registry "Serpent's Gift"
    let base = S.landsInPlay forest 3
        (mammothId, withMammoth) = S.addCreature warMammoth S.alice base
        (gs, sgId) = S.handOne serpentsGift withMammoth
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice sgId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertBool s (Projection.hasKeyword Keyword.Trample mammothId resolved) "keeps trample"
    Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch mammothId resolved) "gains deathtouch"

  Spec.it s "CR 613.7 layer 6: a grant older than Humility is erased; newer survives" $ do
    -- War Mammoth and Humility on the battlefield; a directly-built
    -- Serpent's-Gift effect (GainKeyword Deathtouch, the same value the card
    -- creates) whose timestamp straddles Humility's object timestamp, to
    -- witness BOTH orders of CR 613.7 in layer 6. h-1 and h+1 make the
    -- relative order exact, not a guess.
    warMammoth <- S.printingOf s registry "War Mammoth"
    mountain <- S.printingOf s registry "Mountain"
    humility <- S.printingOf s registry "Humility"
    let (mammothId, gs0) = S.addCreature warMammoth S.bob (S.landsInPlay mountain 1)
        withHum = S.withHumility humility gs0
        Timestamp.MkTimestamp h = humilityTimestamp humility withHum
        olderGrant = S.withEffectAt mammothId (Timestamp.MkTimestamp (h - 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
        newerGrant = S.withEffectAt mammothId (Timestamp.MkTimestamp (h + 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Deathtouch mammothId olderGrant)) "grant before Humility: erased"
    Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch mammothId newerGrant) "grant after Humility: survives"

  Spec.it s "projected type line: a Piker is a Creature - Goblin Warrior" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
    Spec.assertBool s (Projection.isCreatureOf oid gs) "is a creature"
    Spec.assertEqWith s "card types" (Projection.cardTypesOf oid gs) (Set.singleton CardType.Creature)
    Spec.assertEqWith s "subtypes" (Projection.subtypesOf oid gs) (Set.fromList [Subtype.Type.Goblin, Subtype.Type.Warrior])

  Spec.it s "projected type line: a Mountain is a Land - Mountain, not a creature" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
    Spec.assertBool s (not (Projection.isCreatureOf landId gs)) "not a creature"
    Spec.assertEqWith s "subtypes" (Projection.subtypesOf landId gs) (Set.singleton Subtype.Type.Mountain)

  Spec.it s "CR 613.1d layer 4: the four type-changing modifications are Type" $ do
    Spec.assertEqWith s "set land subtype" (Projection.layer (Modification.SetLandSubtype Subtype.Type.Mountain)) Layer.Type
    Spec.assertEqWith s "add land subtype" (Projection.layer (Modification.AddLandSubtype Subtype.Type.Swamp)) Layer.Type
    Spec.assertEqWith s "set creature subtype" (Projection.layer (Modification.SetCreatureSubtype Subtype.Type.Frog)) Layer.Type
    Spec.assertEqWith s "add card type" (Projection.layer (Modification.AddCardType CardType.Creature)) Layer.Type

  Spec.it s "CR 613.1c layer 3: ChangeSubtypeWord is Text" $
    Spec.assertEqWith s "text layer" (Projection.layer (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island))) Layer.Text

  Spec.it s "CR 612.1 ChangeSubtypeWord rewrites a Forest's subtype to Island" $ do
    forest <- S.printingOf s registry "Forest"
    let gs0 = S.landsInPlay forest 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Forest Subtype.Type.Island)) gs0
    Spec.assertEqWith s "only Island" (Projection.subtypesOf landId gs) (Set.singleton Subtype.Type.Island)

  Spec.it s "CR 612.2 ChangeSubtypeWord for an absent type is a no-op" $ do
    forest <- S.printingOf s registry "Forest"
    let gs0 = S.landsInPlay forest 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island)) gs0
    Spec.assertEqWith s "still Forest" (Projection.subtypesOf landId gs) (Set.singleton Subtype.Type.Forest)

  -- CR 612.1 colliding two KEYS of the projection's keyword map. Stalker Hag
  -- {B/G}{B/G}{B/G} Creature -- Hag 3/2, "Swampwalk, forestwalk" (checked
  -- against Scryfall, 2026-08-05) is the pool's only creature printing two
  -- landwalks, and a printing is the only way to reach this branch: both
  -- keywords must be on the map at CR 613.1c layer 3, and a GRANTED one arrives
  -- at layer 6, after the swap.
  --
  -- Forest -> Swamp maps both keys onto Landwalk (HasSubtype Swamp), which is
  -- why applyModification uses Map.mapKeysWith (+) and not Map.mapKeys: the Hag
  -- still has TWO landwalk abilities, redundant under CR 702.14e rather than
  -- merged into one. Nothing in the rules core can tell 2 from 1 here --
  -- Pawl.Engine.Combat.landwalkAllowsGiven reads Map.keys and never a count --
  -- so the map itself is the only place the choice between summing and
  -- discarding is visible, and the assertion is on the map for that reason.
  Spec.it s "CR 612.1 a swap that collides two landwalk keys keeps both abilities" $ do
    stalkerHag <- S.printingOf s registry "Stalker Hag"
    let (hagId, gs0) = S.addCreature stalkerHag S.alice (Setup.emptyGame S.bothPlayers)
        gs = S.withEffectAt hagId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Forest Subtype.Type.Swamp)) gs0
        walk t = Keyword.Landwalk (Filter.Type.HasSubtype t)
    Spec.assertEqWith
      s
      "before: one swampwalk and one forestwalk"
      (Projection.keywordsOf hagId gs0)
      (Map.fromList [(walk Subtype.Type.Swamp, 1), (walk Subtype.Type.Forest, 1)])
    Spec.assertEqWith
      s
      "after: one key, and both abilities still counted"
      (Projection.keywordsOf hagId gs)
      (Map.singleton (walk Subtype.Type.Swamp) 2)

  -- CR 612.1: a text-changing effect "can apply to any words or symbols printed
  -- on that object, but generally affects only that object's rules text ...
  -- and/or the text that appears in its type line". A static ability's AFFECTED
  -- clause is rules text like any other, so hacking the ability's source moves
  -- the SET it applies to and not only its modifications (#402).
  --
  -- Kormus Bell {4} Artifact -- "All Swamps are 1/1 black creatures that are
  -- still lands" (checked against Scryfall) -- is the card that shows it: its
  -- affected set is Matching (HasSubtype Swamp), the shape whose word the swap
  -- has to reach. Blood Moon and Humility cannot, since they select by supertype
  -- and card type; Urborg carries its land type in the MODIFICATION, which was
  -- already rewritten.
  Spec.it s "CR 612.1 hacking Kormus Bell moves which lands it animates" $ do
    kormusBell <- S.printingOf s registry "Kormus Bell"
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    let base = Setup.emptyGame S.bothPlayers
        (bellId, g1) = S.addCreature kormusBell S.alice base
        (swampId, g2) = S.addCreature swamp S.alice g1
        (islandId, plain) = S.addCreature island S.alice g2
        -- The swap is on the BELL, which is where the words are printed.
        hacked = S.withEffectAt bellId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Swamp Subtype.Type.Island)) plain
    -- Unhacked, the Bell animates the Swamp and nothing else.
    Spec.assertBool s (Projection.isCreatureOf swampId plain) "the Swamp is a creature"
    Spec.assertEqWith s "a 1/1" (Projection.powerOf swampId plain) (Just 1)
    Spec.assertBool s (not (Projection.isCreatureOf islandId plain)) "and the Island is not"
    -- Hacked, the affected set moves with the word: the Island animates and the
    -- Swamp stops. An implementation that rewrote only the modifications would
    -- leave BOTH of these the way they are above.
    Spec.assertBool s (Projection.isCreatureOf islandId hacked) "hacked, the Island is a creature"
    Spec.assertEqWith s "a 1/1 too" (Projection.powerOf islandId hacked) (Just 1)
    Spec.assertBool s (not (Projection.isCreatureOf swampId hacked)) "and the Swamp is not animated any more"

  -- CR 612.1 again, through the fourth carrier of an object's rules text: an
  -- ACTIVATED ability printed on the permanent. "Any words or symbols printed on
  -- that object ... generally affects only that object's rules text (which
  -- appears in its text box)" -- and an activated ability is printed in that box,
  -- so its land-type word swaps with everything else.
  --
  -- Tidal Warrior {U} Creature -- Merfolk Warrior, "{T}: Target land becomes an
  -- Island until end of turn." (checked against Scryfall) is the card: a vanilla
  -- 1/1 body carrying exactly one activated ability whose effect is
  -- ModifyTarget UntilEndOfTurn (SetLandSubtype Island).
  --
  -- Read off the PROJECTION rather than at resolution, because CR 113.7a makes
  -- an activated ability on the stack an object independent of its source -- its
  -- text is fixed when it is put on the stack, so the rewrite has to happen
  -- where the ability is enumerated. Pawl.ActivateSpec's whole-card case is the
  -- end-to-end proof.
  Spec.it s "CR 612.1 hacking Tidal Warrior swaps the land type inside its activated ability" $ do
    tidalWarrior <- S.printingOf s registry "Tidal Warrior"
    let base = Setup.emptyGame S.bothPlayers
        (warriorId, plain) = S.addCreature tidalWarrior S.alice base
        hacked = S.withEffectAt warriorId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Island Subtype.Type.Swamp)) plain
        setsTo gs = case Projection.abilitiesOf warriorId gs of
          ability : _ -> concatMap (Maybe.mapMaybe landTypeSet . Foldable.toList . Mode.allEffects) (Modal.modes (ActivatedAbility.modal ability))
          [] -> []
        landTypeSet effect = case effect of
          Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ (Modification.SetLandSubtype st) _) -> Just st
          _ -> Nothing
    -- The control: unhacked, the printed word stands.
    Spec.assertEqWith s "unhacked, the ability sets Island" (setsTo plain) [Subtype.Type.Island]
    -- And hacked, the ability the projection hands out carries the new word.
    Spec.assertEqWith s "hacked, the ability sets Swamp" (setsTo hacked) [Subtype.Type.Swamp]

  -- CR 612.1 through the next carrier of that same text box: a TRIGGERED
  -- ability. The rule draws no distinction between the kinds of ability printed
  -- there, so the word swaps in a trigger exactly as it does in the activated
  -- ability just above.
  --
  -- Barbarian Outcast {1}{R} Creature -- Human Barbarian Beast 2/2, "When you
  -- control no Swamps, sacrifice this creature." (checked against Scryfall) is
  -- the card, and the word is in the CONDITION rather than in the payload: its
  -- CR 603.8 state trigger counts permanents matching `HasSubtype Swamp`. A
  -- rewrite that reached only Mode.effects would leave this assertion at Swamp.
  --
  -- Pawl.TriggerSpec's whole-card case is the end-to-end proof, through
  -- Pawl.Engine.Event.stateTriggers.
  Spec.it s "CR 612.1 hacking Barbarian Outcast swaps the land type inside its triggered ability" $ do
    barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
    let base = Setup.emptyGame S.bothPlayers
        (outcastId, plain) = S.addCreature barbarianOutcast S.alice base
        hacked = S.withEffectAt outcastId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Swamp Subtype.Type.Island)) plain
        asksAbout gs = case Projection.triggeredAbilitiesOf outcastId gs of
          ability : _ -> case TriggeredAbility.condition ability of
            TriggerCondition.StateIs (Condition.Type.Compares (Compares.MkCompares measured _ _)) -> countedSubtypes measured
            _ -> []
          [] -> []
        countedSubtypes quantity = case quantity of
          Quantity.Count count -> filterSubtypes (Count.Type.filter count)
          _ -> []
        filterSubtypes predicate = case predicate of
          Filter.Type.HasSubtype st -> [st]
          Filter.Type.And fs -> concatMap filterSubtypes fs
          _ -> []
    -- The control: unhacked, the printed word stands.
    Spec.assertEqWith s "unhacked, the trigger counts Swamps" (asksAbout plain) [Subtype.Type.Swamp]
    -- And hacked, the trigger the projection hands out asks the new question.
    Spec.assertEqWith s "hacked, the trigger counts Islands" (asksAbout hacked) [Subtype.Type.Island]

  -- A CREATURE type word in a type line, swapped -- the half CR 612.2 names
  -- second, and the half no pair could reach before Artificial Evolution. Bog
  -- Wraith's printed Wraith moves to Elf.
  --
  -- The board is the Ashaya one, because it also pins what a text change may NOT
  -- reach: Ashaya, Soul of the Wild makes alice's nontoken creatures Forest lands
  -- in addition to their other types, so the Wraith projects Land Creature --
  -- Forest Wraith, and a land pair naming that Forest changes nothing at all. CR
  -- 612.1 is why -- a text-changing effect applies to "words or symbols PRINTED
  -- on that object", and the Forest is printed nowhere near Bog Wraith -- and CR
  -- 613.1c/613.1d are how: the swap is layer 3 and Ashaya's subtype grant is
  -- layer 4, so the word the swap looks for does not exist yet when it looks.
  -- Wizards' own Artificial Evolution ruling agrees: it "only changes what is
  -- printed on the card ... It will not change any effects that are on the
  -- permanent."
  Spec.it s "CR 612.1/612.2 a printed creature type word is swapped, and a granted land type is not" $ do
    island <- S.printingOf s registry "Island"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    let (_, withAshaya) = S.addCreature ashaya S.alice (S.landsInPlay island 3)
        (wraithId, plain) = S.addCreature bogWraith S.alice withAshaya
        swapped from to = S.withEffectAt wraithId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord from to)) plain
    Spec.assertEqWith
      s
      "unswapped: Ashaya's Forest beside the printed Wraith"
      (Projection.subtypesOf wraithId plain)
      (Set.fromList [Subtype.Type.Forest, Subtype.Type.Wraith])
    Spec.assertEqWith
      s
      "a creature pair (Wraith -> Elf) moves the printed word, and only it"
      (Projection.subtypesOf wraithId (swapped Subtype.Type.Wraith Subtype.Type.Elf))
      (Set.fromList [Subtype.Type.Forest, Subtype.Type.Elf])
    Spec.assertEqWith
      s
      "a land pair (Forest -> Island) reaches nothing: that Forest is Ashaya's, not Bog Wraith's"
      (Projection.subtypesOf wraithId (swapped Subtype.Type.Forest Subtype.Type.Island))
      (Set.fromList [Subtype.Type.Forest, Subtype.Type.Wraith])
    Spec.assertEqWith
      s
      "and a pair naming neither word changes nothing"
      (Projection.subtypesOf wraithId (swapped Subtype.Type.Swamp Subtype.Type.Mountain))
      (Set.fromList [Subtype.Type.Forest, Subtype.Type.Wraith])

  -- CR 612.2 at the OTHER site a pair meets a word: a Modification's own subtype
  -- word, where the family is fixed by the CONSTRUCTOR rather than by the word.
  -- Magical Hack rewrites a SetLandSubtype (Tidal Warrior's Island, proved end
  -- to end by Pawl.ActivateSpec); Artificial Evolution rewrites a
  -- SetCreatureSubtype (Turn to Frog's Frog, proved end to end by
  -- Pawl.ResolveSpec's ArtificialEvolution group); and a pair of the WRONG
  -- family reaches neither.
  --
  -- The cross-family pairs here are hand-built, and deliberately so: pawl's land
  -- types and creature types share no word, so no card in the pool can produce
  -- such a pair and no board can make the gate's answer visible. That is why the
  -- gate is written out rather than left to the exact-match test -- what stops a
  -- creature-type pair rewriting a land-type word is CR 612.2, not a coincidence
  -- of which words the two lists happen to hold.
  Spec.it s "CR 612.2 a modification's subtype word is rewritten only by a pair of its own family" $ do
    let rewrite from to = Projection.rewriteModification [(from, to)]
    Spec.assertEqWith
      s
      "a creature pair rewrites a creature-type word"
      (rewrite Subtype.Type.Frog Subtype.Type.Elf (Modification.SetCreatureSubtype Subtype.Type.Frog))
      (Modification.SetCreatureSubtype Subtype.Type.Elf)
    Spec.assertEqWith
      s
      "a land pair rewrites a land-type word"
      (rewrite Subtype.Type.Island Subtype.Type.Swamp (Modification.SetLandSubtype Subtype.Type.Island))
      (Modification.SetLandSubtype Subtype.Type.Swamp)
    Spec.assertEqWith
      s
      "and the added land type too"
      (rewrite Subtype.Type.Island Subtype.Type.Swamp (Modification.AddLandSubtype Subtype.Type.Island))
      (Modification.AddLandSubtype Subtype.Type.Swamp)
    Spec.assertEqWith
      s
      "a creature pair leaves a land-type position alone"
      (rewrite Subtype.Type.Frog Subtype.Type.Elf (Modification.SetLandSubtype Subtype.Type.Frog))
      (Modification.SetLandSubtype Subtype.Type.Frog)
    Spec.assertEqWith
      s
      "and a land pair leaves a creature-type position alone"
      (rewrite Subtype.Type.Island Subtype.Type.Swamp (Modification.SetCreatureSubtype Subtype.Type.Island))
      (Modification.SetCreatureSubtype Subtype.Type.Island)

  -- CR 613.8a: Kormus Bell's affected set READS subtypes at layer 4, and
  -- Urborg's AddLandSubtype WRITES them at layer 4 -- so the two are dependent
  -- and Urborg must apply first, which is the classic pairing. It is what makes
  -- every land a 1/1: Urborg makes them all Swamps, and the Bell then animates
  -- what Urborg produced rather than what the printed type lines said.
  --
  -- Not a text-change case, but it is the same reader (`affected` at layer 4)
  -- that #402 changed, and nothing else in the suite pairs an affected-set read
  -- with a same-layer write.
  Spec.it s "CR 613.8a Urborg makes every land a Swamp, so Kormus Bell animates all of them" $ do
    kormusBell <- S.printingOf s registry "Kormus Bell"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    forest <- S.printingOf s registry "Forest"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addCreature kormusBell S.alice base
        (forestId, withBell) = S.addCreature forest S.alice g1
        (_, withUrborg) = S.addCreature urborg S.alice withBell
    -- Without Urborg the Forest is no Swamp, so the Bell leaves it alone.
    Spec.assertBool s (not (Projection.isCreatureOf forestId withBell)) "the Forest is not animated on its own"
    -- With Urborg it is a Swamp (layer 4, written) and therefore animated (layer
    -- 4, read) -- and a 1/1 at 7b.
    Spec.assertBool s (Set.member Subtype.Type.Swamp (Projection.subtypesOf forestId withUrborg)) "Urborg makes it a Swamp"
    Spec.assertBool s (Projection.isCreatureOf forestId withUrborg) "so the Bell animates it"
    Spec.assertEqWith s "as a 1/1" (Projection.powerOf forestId withUrborg) (Just 1)

  Spec.it s "CR 613.1d AddLandSubtype gives a Forest the Swamp subtype" $ do
    forest <- S.printingOf s registry "Forest"
    let gs0 = S.landsInPlay forest 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.AddLandSubtype Subtype.Type.Swamp) gs0
    Spec.assertEqWith s "Forest and Swamp" (Projection.subtypesOf landId gs) (Set.fromList [Subtype.Type.Forest, Subtype.Type.Swamp])

  Spec.it s "CR 305.7 SetLandSubtype sets a Forest to only Mountain" $ do
    forest <- S.printingOf s registry "Forest"
    let gs0 = S.landsInPlay forest 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.SetLandSubtype Subtype.Type.Mountain) gs0
    Spec.assertEqWith s "only Mountain" (Projection.subtypesOf landId gs) (Set.singleton Subtype.Type.Mountain)

  -- CR 305.7's set, with the type read off the effect's SOURCE rather than
  -- carried by the modification. THE FALSIFIER for a subtype baked into card
  -- data: this modification has no payload at all, so the Island can only have
  -- come from Object.chosenSubtype on the source -- which is where CR 614.1c's
  -- as-enters choice is written. Two sources with different choices, on one
  -- board, so a constructor that ignored the source and conjured one type could
  -- not pass both halves.
  Spec.it s "CR 305.7 SetLandSubtypeToChosen reads the SOURCE's entry choice" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = S.landsInPlay forest 2
        (landA, landB) = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : j : _ -> (i, j)
          _ -> (ObjectId.MkObjectId 998, ObjectId.MkObjectId 999)
        -- Two ordinary permanents standing in for two Aura sources; all this
        -- arm reads off a source is Object.chosenSubtype.
        (srcA, g1) = S.addCreature piker S.alice gs0
        (srcB, g2) = S.addCreature piker S.alice g1
        withChoices = S.withChosenSubtype Subtype.Type.Swamp srcB (S.withChosenSubtype Subtype.Type.Island srcA g2)
        gs =
          S.withEffectFromAt srcB landB (Timestamp.MkTimestamp 101) Modification.SetLandSubtypeToChosen $
            S.withEffectFromAt srcA landA (Timestamp.MkTimestamp 100) Modification.SetLandSubtypeToChosen withChoices
    Spec.assertEqWith s "the Island source's land is only an Island" (Projection.subtypesOf landA gs) (Set.singleton Subtype.Type.Island)
    Spec.assertEqWith s "the Swamp source's land is only a Swamp" (Projection.subtypesOf landB gs) (Set.singleton Subtype.Type.Swamp)

  -- Turn to Frog {1}{U}: "Until end of turn, target creature loses all
  -- abilities and becomes a blue Frog with base power and toughness 1/1."
  -- Four layers at once -- 4 (Frog), 5 (blue), 6 (loses all abilities) and 7b
  -- (base 1/1) -- and the pool's first producer of the layer-4 arm that SETS a
  -- creature type.
  --
  -- Jade Statue is the other, and the degenerate one: "becomes a 3/6 Golem
  -- artifact creature" sets a creature type over a permanent that prints none,
  -- so the arm's filter runs over an empty set and the only thing left for CR
  -- 205.1b to say is that the ARTIFACT card type is retained. Pawl.ExpirySpec
  -- proves that one end to end; the case with a creature type standing is here.
  Spec.it s "CR 205.1b Turn to Frog replaces Bog Wraith's creature type: a Frog, and no longer a Wraith" $ do
    island <- S.printingOf s registry "Island"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    turnToFrog <- S.printingOf s registry "Turn to Frog"
    let (wraithId, board) = S.addCreature bogWraith S.alice (S.landsInPlay island 3)
        after = castAtCreature wraithId turnToFrog board
    Spec.assertEqWith s "before: Creature -- Wraith" (Projection.subtypesOf wraithId board) (Set.singleton Subtype.Type.Wraith)
    -- CR 205.1b's last sentence: an effect making an object a "[creature type]
    -- artifact creature" lets it keep every prior subtype "other than creature
    -- types, but replace any existing creature types". So this is a SET over
    -- the creature types, and an ADD would leave Wraith standing.
    Spec.assertEqWith s "after: Creature -- Frog, the Wraith replaced" (Projection.subtypesOf wraithId after) (Set.singleton Subtype.Type.Frog)

  -- THE REPLACE-ONLY-CREATURE-TYPES FALSIFIER. Ashaya, Soul of the Wild makes
  -- each nontoken creature alice controls a Forest land in addition to its other
  -- types, so the Bog Wraith carries a LAND type and a CREATURE type at once.
  -- CR 205.1a: "when an effect sets one or more of an object's subtypes, the new
  -- subtype(s) replaces any existing subtypes from the appropriate set (creature
  -- types, land types, artifact types, enchantment types, planeswalker types, or
  -- spell types)" -- the appropriate set here is the creature types alone, so
  -- Forest has to survive. Replacing ALL subtypes leaves {Frog}; adding leaves
  -- {Forest, Wraith, Frog}; only the rule's answer is {Forest, Frog}.
  --
  -- Neither effect depends on the other under CR 613.8a -- Turn to Frog's set is
  -- a CR 611.2c TheseObjects, and Ashaya's reads card types and controller,
  -- which no layer-4 subtype arm writes -- so this holds in timestamp order, and
  -- both orders give the same answer anyway.
  Spec.it s "CR 205.1a Turn to Frog replaces only the CREATURE types: Ashaya's Forest survives" $ do
    island <- S.printingOf s registry "Island"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    turnToFrog <- S.printingOf s registry "Turn to Frog"
    let (_, withAshaya) = S.addCreature ashaya S.alice (S.landsInPlay island 3)
        (wraithId, board) = S.addCreature bogWraith S.alice withAshaya
        after = castAtCreature wraithId turnToFrog board
    Spec.assertEqWith
      s
      "before: animated into a Forest land, still a Wraith"
      (Projection.subtypesOf wraithId board)
      (Set.fromList [Subtype.Type.Forest, Subtype.Type.Wraith])
    Spec.assertEqWith
      s
      "after: the creature type moved and the land type did not"
      (Projection.subtypesOf wraithId after)
      (Set.fromList [Subtype.Type.Forest, Subtype.Type.Frog])
    -- CR 205.1a's last sentence -- "Removing an object's subtype doesn't affect
    -- its card types at all" -- and CR 305.7's fourth, which the land-subtype
    -- arm cites: setting a subtype moves no card type either way.
    Spec.assertBool s (Projection.isCreatureOf wraithId after) "still a creature"
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf wraithId after)) "still a land"

  -- The other three of Turn to Frog's four layers, on the same board: CR 613.1e
  -- (blue), CR 613.1f (loses all abilities) and CR 613.4b (base 1/1). Bog Wraith
  -- is printed black, 3/3 and with swampwalk, so every one of them moves.
  Spec.it s "CR 613.1e/613.1f/613.4b Turn to Frog also makes the Wraith blue, ability-less and 1/1" $ do
    island <- S.printingOf s registry "Island"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    turnToFrog <- S.printingOf s registry "Turn to Frog"
    let (wraithId, board) = S.addCreature bogWraith S.alice (S.landsInPlay island 3)
        after = castAtCreature wraithId turnToFrog board
    Spec.assertEqWith s "before: black" (Projection.colorsOf wraithId board) (Set.singleton Color.Black)
    Spec.assertBool s (Projection.hasKeyword (Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Type.Swamp)) wraithId board) "before: swampwalk"
    Spec.assertEqWith s "before: 3/3" (Projection.powerOf wraithId board, Projection.toughnessOf wraithId board) (Just 3, Just 3)
    Spec.assertEqWith s "after: blue only (CR 105.3, a set)" (Projection.colorsOf wraithId after) (Set.singleton Color.Blue)
    Spec.assertBool s (not (Projection.hasKeyword (Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Type.Swamp)) wraithId after)) "after: no swampwalk"
    Spec.assertEqWith s "after: base 1/1" (Projection.powerOf wraithId after, Projection.toughnessOf wraithId after) (Just 1, Just 1)

  Spec.it s "CR 514.2 Turn to Frog wears off at cleanup and the Wraith is a Wraith again" $ do
    island <- S.printingOf s registry "Island"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    turnToFrog <- S.printingOf s registry "Turn to Frog"
    let (wraithId, board) = S.addCreature bogWraith S.alice (S.landsInPlay island 3)
        after = castAtCreature wraithId turnToFrog board
        afterCleanup = snd (Engine.runGamePure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
    Spec.assertEqWith s "all four effects dropped" (GameState.continuousEffects afterCleanup) []
    Spec.assertEqWith s "Creature -- Wraith again" (Projection.subtypesOf wraithId afterCleanup) (Set.singleton Subtype.Type.Wraith)
    Spec.assertEqWith s "black again" (Projection.colorsOf wraithId afterCleanup) (Set.singleton Color.Black)
    Spec.assertBool s (Projection.hasKeyword (Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Type.Swamp)) wraithId afterCleanup) "swampwalk again"
    Spec.assertEqWith s "3/3 again" (Projection.powerOf wraithId afterCleanup, Projection.toughnessOf wraithId afterCleanup) (Just 3, Just 3)

  Spec.it s "CR 613.1d AddCardType makes a land a creature" $ do
    forest <- S.printingOf s registry "Forest"
    let gs0 = S.landsInPlay forest 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.AddCardType CardType.Creature) gs0
    Spec.assertBool s (Projection.isCreatureOf landId gs) "now a creature"

  Spec.it s "CR 202.3 SetBasePowerToughness ManaValue sets a Piker to its mana value ({1}{R} = 2)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness Quantity.ManaValue Quantity.ManaValue)) gs0
    Spec.assertEqWith s "power = mana value" (Projection.powerOf oid gs) (Just 2)
    Spec.assertEqWith s "toughness = mana value" (Projection.toughnessOf oid gs) (Just 2)

  Spec.it s "CR 613 affected-set reads the partial: a layer-4 creature-add is seen by a layer-6 Matching (HasCardType Creature) grant" $ do
    forest <- S.printingOf s registry "Forest"
    let gs0 = S.landsInPlay forest 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        -- Layer 4 makes the land a creature; layer 6 grants flying to all
        -- creatures. The grant reaches the land ONLY because the affected set
        -- is evaluated after layer 4.
        gs1 = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.AddCardType CardType.Creature) gs0
        gs = withDynamicEffect (Affected.Matching (Filter.Type.HasCardType CardType.Creature)) (Timestamp.MkTimestamp 200) (Modification.GainKeyword Keyword.Flying) gs1
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying landId gs) "land gained flying because it became a creature"

  Spec.it s "CR 305.7/613.8 Blood Moon strips Urborg: Urborg is only a Mountain (Blood Moon older)" $ do
    forest <- S.printingOf s registry "Forest"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, urborgId, gs) = bloodMoonUrborg forest urborg bloodMoon False
    Spec.assertEqWith s "Urborg subtypes" (Projection.subtypesOf urborgId gs) (Set.singleton Subtype.Type.Mountain)

  Spec.it s "CR 305.7/613.8 Blood Moon strips Urborg: Urborg is only a Mountain (Urborg older)" $ do
    forest <- S.printingOf s registry "Forest"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, urborgId, gs) = bloodMoonUrborg forest urborg bloodMoon True
    Spec.assertEqWith s "Urborg subtypes, order-independent" (Projection.subtypesOf urborgId gs) (Set.singleton Subtype.Type.Mountain)

  -- CR 613.8b's last sentence: "if several
  -- dependent effects form a dependency loop, then this rule is ignored and the
  -- effects in the dependency loop are applied in timestamp order."
  --
  -- Two nonbasic lands that each set every nonbasic land's subtype form the loop:
  -- each is inside the other's affected set, so each would strip the other's rules
  -- text, and by CR 613.8a each therefore depends on the other. Dependency order
  -- cannot break the tie, so rule 613.8b hands it to the timestamps -- the EARLIER
  -- effect applies and the later one, its ability now gone, never does.
  --
  -- Both cards are synthetic, and they have to be: every printed effect that SETS
  -- a land subtype lives on a non-land (Blood Moon is an enchantment, Magus of the
  -- Moon and Harbinger of the Seas are creatures), and every printed one whose
  -- source IS a land only ADDS a type (Urborg, Yavimaya), which strips nothing. So
  -- no printed pair can sit inside each other's affected sets, in either direction.
  -- Nothing in the CR forbids the card -- it is Blood Moon's text on a land.
  Spec.it s "CR 613.8b a dependency loop applies in timestamp order: the older land wins" $ do
    lunar <- S.printingOf s registry "Synthetic Lunar Waste"
    tidal <- S.printingOf s registry "Synthetic Tidal Waste"
    let (lunarId, tidalId, gs) = wasteLoop lunar tidal True
    Spec.assertEqWith s "the older Lunar Waste applies: Tidal is a Mountain" (Projection.subtypesOf tidalId gs) (Set.singleton Subtype.Type.Mountain)
    -- And the loser's effect never applied, so Lunar keeps the subtype it was
    -- printed with -- none. The falsifier for both effects applying.
    Spec.assertEqWith s "and Lunar is untouched" (Projection.subtypesOf lunarId gs) Set.empty

  -- The same board with the timestamps swapped, which is the whole point: under
  -- the old visited-set escape BOTH effects were treated as live and the answer
  -- did not move. Rule 613.8b says it must.
  Spec.it s "CR 613.8b the same loop with the timestamps swapped gives the other answer" $ do
    lunar <- S.printingOf s registry "Synthetic Lunar Waste"
    tidal <- S.printingOf s registry "Synthetic Tidal Waste"
    let (lunarId, tidalId, gs) = wasteLoop lunar tidal False
    Spec.assertEqWith s "the older Tidal Waste applies: Lunar is an Island" (Projection.subtypesOf lunarId gs) (Set.singleton Subtype.Type.Island)
    Spec.assertEqWith s "and Tidal is untouched" (Projection.subtypesOf tidalId gs) Set.empty

  -- CR 305.7's GATE half, which applyModification structurally cannot do:
  -- Urborg's ability lands on OTHER objects, so a stripped Urborg has to be kept
  -- out of the gather's candidate list (setLandSubtypeEffects -> liveGiven)
  -- rather than erased from its own projection.
  --
  -- Convincing Mirage is the first CHOSEN-subtype effect to reach that gate, and
  -- the pool's second static producer after Blood Moon. The gate classifies by
  -- CONSTRUCTOR, behind a wildcard the compiler cannot police -- so a
  -- subtype-setting modification missing from it leaves the two halves of one
  -- rule disagreeing, with the enchanted land's own projection stripped and its
  -- static ability still firing at everything else. The Forest is what shows it.
  Spec.it s "CR 305.7 Convincing Mirage strips Urborg's static ability, so no land is a Swamp" $ do
    forest <- S.printingOf s registry "Forest"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    convincingMirage <- S.printingOf s registry "Convincing Mirage"
    let gs0 = S.landsInPlay forest 1
        forestId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        (urborgId, g1) = S.addCreature urborg S.alice gs0
        (mirageId, g2) = S.addCreature convincingMirage S.alice g1
        gs = S.withChosenSubtype Subtype.Type.Island mirageId (S.attach mirageId urborgId g2)
    Spec.assertEqWith s "before: Urborg makes the Forest a Swamp too" (Projection.subtypesOf forestId g2) (Set.fromList [Subtype.Type.Forest, Subtype.Type.Swamp])
    Spec.assertEqWith s "Urborg itself is only an Island now" (Projection.subtypesOf urborgId gs) (Set.singleton Subtype.Type.Island)
    Spec.assertEqWith s "and the Forest is a plain Forest again" (Projection.subtypesOf forestId gs) (Set.singleton Subtype.Type.Forest)

  -- Not implemented, so the card file omits them: Celestial Dawn's colour clause
  -- for spells you control and nonland cards you own off the battlefield (#160),
  -- and its mana clause -- "You may spend white mana as though it were mana of any
  -- color. You may spend other mana only as though it were colorless mana" (#1579).
  -- Pawl.Types.ManaSpending is CR 609.4b's axis, but it rides one cast permission
  -- (CR 118.14) where this clause is a continuous effect on a player, and it only
  -- widens where this one also narrows.
  -- That clause's permission and restriction go together, so pawl's card is more
  -- permissive than printed about spending non-white mana; nothing below reads
  -- mana. Its first two clauses are printed in full.
  --
  -- CR 305.7's gate reached by an affected set that asks who CONTROLS the
  -- candidate, which is the shape that used to make Projection.controllerOf
  -- diverge: the control fold gated itself on CR 305.7, the gate evaluated
  -- "lands you control", and CR 109.5's "you" asked the control fold again.
  -- Celestial Dawn is the pool's first card of that shape, so it is what proves
  -- the fold no longer consults the gate.
  --
  -- Bob's Forest is the falsifier for an affected-set test that answers by being
  -- eager: "every land is a Plains" and "no land is a Plains" both terminate, and
  -- only a board with a land on each side tells either from the right answer.
  Spec.it s "CR 305.7/109.5 Celestial Dawn sets only the lands its own controller controls" $ do
    forest <- S.printingOf s registry "Forest"
    dawn <- S.printingOf s registry "Celestial Dawn"
    let base = Setup.emptyGame S.bothPlayers
        (aliceLand, g1) = S.addCreature forest S.alice base
        (bobLand, g2) = S.addCreature forest S.bob g1
        gs = snd (S.addCreature dawn S.alice g2)
    Spec.assertEqWith s "alice's Forest is a Plains" (Projection.subtypesOf aliceLand gs) (Set.singleton Subtype.Type.Plains)
    Spec.assertEqWith s "bob's Forest is untouched" (Projection.subtypesOf bobLand gs) (Set.singleton Subtype.Type.Forest)
    Spec.assertEqWith s "and asking who controls one answers rather than diverging" (Projection.controllerOf bobLand gs) (Just S.bob)

  -- The other half: the perspective Celestial Dawn's filter reads is the CR
  -- 613.1b LAYER-2 controller, not the owner. Control Magic can only enchant a
  -- creature (CR 303.4), so Living Plane animates the land first.
  --
  -- The pair differs in one thing -- whether the Aura is attached -- so a
  -- control fold that skipped the grant, or one that read owners, fails one leg
  -- while the other still passes. Alice's own Forest is asserted in both legs, so
  -- the unattached leg cannot pass by Celestial Dawn reaching nothing at all.
  Spec.it s "CR 613.1b Celestial Dawn reads the layer-2 controller, so a stolen land is a Plains" $ do
    forest <- S.printingOf s registry "Forest"
    dawn <- S.printingOf s registry "Celestial Dawn"
    livingPlane <- S.printingOf s registry "Living Plane"
    controlMagic <- S.printingOf s registry "Control Magic"
    let base = Setup.emptyGame S.bothPlayers
        (bobLand, g1) = S.addCreature forest S.bob base
        (aliceLand, g2) = S.addCreature forest S.alice g1
        (_, g3) = S.addCreature livingPlane S.bob g2
        (_, g4) = S.addCreature dawn S.alice g3
        (aura, unattached) = S.addCreature controlMagic S.alice g4
        gs = S.attach aura bobLand unattached
    Spec.assertBool s (Projection.isCreatureOf bobLand unattached) "Living Plane animates the land, so the Aura may enchant it"
    Spec.assertEqWith s "Celestial Dawn is live: alice's own Forest is a Plains" (Projection.subtypesOf aliceLand unattached) (Set.singleton Subtype.Type.Plains)
    Spec.assertEqWith s "unattached: bob controls his Forest" (Projection.controllerOf bobLand unattached) (Just S.bob)
    Spec.assertEqWith s "so Celestial Dawn's set does not reach it" (Projection.subtypesOf bobLand unattached) (Set.singleton Subtype.Type.Forest)
    Spec.assertEqWith s "attached: alice controls it" (Projection.controllerOf bobLand gs) (Just S.alice)
    Spec.assertEqWith s "so now Celestial Dawn's set does reach it" (Projection.subtypesOf bobLand gs) (Set.singleton Subtype.Type.Plains)

  Spec.it s "CR 613.8 Urborg's stripped ability adds no Swamp to a Forest (Blood Moon older)" $ do
    forest <- S.printingOf s registry "Forest"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (forestId, _, gs) = bloodMoonUrborg forest urborg bloodMoon False
    Spec.assertEqWith s "Forest stays a Forest" (Projection.subtypesOf forestId gs) (Set.singleton Subtype.Type.Forest)

  Spec.it s "CR 613.8 Urborg's stripped ability adds no Swamp to a Forest (Urborg older)" $ do
    forest <- S.printingOf s registry "Forest"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (forestId, _, gs) = bloodMoonUrborg forest urborg bloodMoon True
    Spec.assertEqWith s "Forest stays a Forest, order-independent" (Projection.subtypesOf forestId gs) (Set.singleton Subtype.Type.Forest)

  -- Ashaya + Blood Moon. Both effects are layer 4 (CR 613.1d) and neither is
  -- a characteristic-defining ability (CR 604.3a(3): both directly affect
  -- OTHER objects), so CR 613.8a clauses (a) and (c) are satisfied and the
  -- pair is eligible to depend.
  --
  -- CR 613.8a clause (b), evaluated where the rule evaluates it -- against the
  -- state as the layer begins, with CR 613.8c re-asking after each application:
  --
  --   * Blood Moon DEPENDS on Ashaya. Applying Ashaya adds the card type Land
  --     to alice's nontoken creatures, which changes "what it applies to" for
  --     an effect whose set is "nonbasic lands".
  --   * Ashaya does NOT depend on Blood Moon. Applying Blood Moon rewrites land
  --     subtypes and (CR 305.7) strips rules text from NONBASIC LANDS; at that
  --     moment Ashaya is a Legendary Creature -- Elemental and no creature of
  --     alice's is a land, so nothing about Ashaya's text, existence, set or
  --     result changes.
  --
  -- So this is a one-way dependency, NOT the dependency LOOP of CR 613.8b's
  -- last sentence, and the answer the CR gives is Ashaya-then-Blood-Moon in
  -- BOTH timestamp orders. That is what the paired order tests below pin: were
  -- the engine to fall back to CR 613.7 timestamp order (which is what the
  -- loop clause prescribes), the Blood-Moon-older board would leave the Piker
  -- a Forest instead of a Mountain and the two would disagree.
  Spec.it s "CR 613.8a Ashaya animates a nontoken creature into a land (Ashaya older)" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, pikerId, _, _, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf pikerId gs)) "the Piker is a land"
    Spec.assertBool s (Projection.isCreatureOf pikerId gs) "and still a creature (CR 205.1b: adding a type keeps the others)"

  Spec.it s "CR 613.8b Blood Moon depends on Ashaya, so it applies second (Ashaya older)" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, pikerId, _, _, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
    Spec.assertBool s (Set.member Subtype.Type.Mountain (Projection.subtypesOf pikerId gs)) "the animated Piker is a Mountain"
    Spec.assertBool s (not (Set.member Subtype.Type.Forest (Projection.subtypesOf pikerId gs))) "and not the Forest Ashaya made it"

  -- The proving test for Projection.effectUnits, and it fails without it:
  -- Ashaya's one ability has two layer-4 parts, Blood Moon depends only on the
  -- first (the card-type add), and ordered per MODIFICATION an older Blood Moon
  -- applies between them -- leaving the Piker a Mountain AND the Forest the
  -- second part then re-adds. CR 613.8 orders effects, not modifications.
  Spec.it s "CR 613.8b the dependency overrides timestamp order (Blood Moon older)" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, pikerId, _, _, gs) = ashayaBloodMoon forest piker ashaya bloodMoon False
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf pikerId gs)) "still a land"
    Spec.assertBool s (Set.member Subtype.Type.Mountain (Projection.subtypesOf pikerId gs)) "still a Mountain, order-independent"
    Spec.assertBool s (not (Set.member Subtype.Type.Forest (Projection.subtypesOf pikerId gs))) "still not a Forest, order-independent"

  Spec.it s "CR 305.7 Ashaya's own type change reaches herself, and Blood Moon then reaches her" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, _, _, ashayaId, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf ashayaId gs)) "Ashaya is a land"
    Spec.assertBool s (Set.member Subtype.Type.Mountain (Projection.subtypesOf ashayaId gs)) "Ashaya is a Mountain"

  -- CR 613.8a clause (b)'s last limb -- "what it does to any of the things it
  -- applies to" -- reached without moving any affected set. See chorusBadMoon for
  -- the board and why the card is synthetic.
  --
  -- Both halves of the fix are load-bearing here, and the older-Chorus board is
  -- what needs both: reading the running board without the dependency edge leaves
  -- the Chorus counting first and finding nothing, and adding the edge without
  -- the running board leaves it counting against a view that stops below its own
  -- layer. Either alone gives the Wraith 4/4.
  Spec.it s "CR 613.8a a count depends on a same-layer effect that changes what it counts (Chorus older)" $ do
    chorus <- S.printingOf s registry "Synthetic Ferocious Chorus"
    badMoon <- S.printingOf s registry "Bad Moon"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    childOfNight <- S.printingOf s registry "Child of Night"
    let (wraithId, childId, gs) = chorusBadMoon chorus badMoon bogWraith childOfNight True
    Spec.assertEqWith s "CR 613.8b: Bad Moon first (3+1), then a Chorus counting the 4-power Wraith (+1)" (Projection.powerOf wraithId gs) (Just 5)
    Spec.assertEqWith s "and its toughness with it" (Projection.toughnessOf wraithId gs) (Just 5)
    Spec.assertEqWith s "the Child gets the same +1/+1 without ever being counted" (Projection.powerOf childId gs) (Just 4)
    Spec.assertEqWith s "so its toughness is 1+1+1" (Projection.toughnessOf childId gs) (Just 3)

  -- The dependency overrides CR 613.7, so the answer is the same with the
  -- timestamps swapped. This board is the one that passes on the running-board
  -- half alone, which is why it is not the proving test.
  Spec.it s "CR 613.8b the count's dependency overrides timestamp order (Bad Moon older)" $ do
    chorus <- S.printingOf s registry "Synthetic Ferocious Chorus"
    badMoon <- S.printingOf s registry "Bad Moon"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    childOfNight <- S.printingOf s registry "Child of Night"
    let (wraithId, childId, gs) = chorusBadMoon chorus badMoon bogWraith childOfNight False
    Spec.assertEqWith s "order-independent: still 5/5" (Projection.powerOf wraithId gs) (Just 5)
    Spec.assertEqWith s "order-independent: still 5/5" (Projection.toughnessOf wraithId gs) (Just 5)
    Spec.assertEqWith s "order-independent: still 4/3" (Projection.powerOf childId gs) (Just 4)
    Spec.assertEqWith s "order-independent: still 4/3" (Projection.toughnessOf childId gs) (Just 3)

  -- The negative, the same board minus the one thing under test: with no Bad Moon
  -- nothing reaches power 4, the Chorus counts nothing, and the +1/+1 it would
  -- otherwise hand out is absent. Without this a Chorus that counted every
  -- creature, or one that counted itself into a fixpoint, would pass above.
  Spec.it s "CR 613.8a control: no Bad Moon, so nothing reaches power 4 and the count is 0" $ do
    chorus <- S.printingOf s registry "Synthetic Ferocious Chorus"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    childOfNight <- S.printingOf s registry "Child of Night"
    let (_, g1) = S.addCreature chorus S.alice (Setup.emptyGame S.bothPlayers)
        (wraithId, g2) = S.addCreature bogWraith S.alice g1
        (childId, gs) = S.addCreature childOfNight S.alice g2
    Spec.assertEqWith s "the Wraith is its printed 3/3" (Projection.powerOf wraithId gs) (Just 3)
    Spec.assertEqWith s "the Wraith is its printed 3/3" (Projection.toughnessOf wraithId gs) (Just 3)
    Spec.assertEqWith s "the Child is its printed 2/1" (Projection.powerOf childId gs) (Just 2)
    Spec.assertEqWith s "the Child is its printed 2/1" (Projection.toughnessOf childId gs) (Just 1)

  -- The card-level proof of the layer-4 AddCreatureSubtype arm: Life and Limb
  -- makes a basic Forest a 1/1 green Saproling creature land, and the Saproling
  -- is the type this arm adds. CR 205.1b's add keeps the types already there,
  -- which the Forest land type surviving alongside it shows.
  Spec.it s "CR 205.1b Life and Limb adds a creature type without replacing the land's own" $ do
    forest <- S.printingOf s registry "Forest"
    limb <- S.printingOf s registry "Life and Limb"
    let base = Setup.emptyGame S.bothPlayers
        (forestId, g1) = S.addCreature forest S.alice base
        (_, gs) = S.addCreature limb S.alice g1
    Spec.assertBool s (Set.member Subtype.Type.Saproling (Projection.subtypesOf forestId gs)) "the Forest is a Saproling"
    Spec.assertBool s (Set.member Subtype.Type.Forest (Projection.subtypesOf forestId gs)) "and still a Forest (CR 205.1b: the add keeps the rest)"
    Spec.assertBool s (Projection.isCreatureOf forestId gs) "and a creature"
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf forestId gs)) "and still a land"

  -- CR 702.73a's changeling, the subtype-defining ability -- CR 604.3 makes it a
  -- CDA, so CR 613.3 applies it at the start of its layer (4, CR 613.1d) rather
  -- than in timestamp order, and Pawl.Engine.Projection.applySubtypeDefining is
  -- where it lands. Woodland Changeling ({1}{G} Creature -- Shapeshifter 2/2,
  -- changeling and nothing else) is the card.
  Spec.it s "CR 702.73a a changeling is every creature type, and no other family's" $ do
    changeling <- S.printingOf s registry "Woodland Changeling"
    let (oid, gs) = S.addCreature changeling S.alice (Setup.emptyGame S.bothPlayers)
        subtypes = Projection.subtypesOf oid gs
    Spec.assertBool s (Set.member Subtype.Type.Shapeshifter subtypes) "still its printed Shapeshifter"
    Spec.assertBool s (Set.member Subtype.Type.Merfolk subtypes) "a Merfolk"
    Spec.assertBool s (Set.member Subtype.Type.Goblin subtypes) "a Goblin"
    -- CR 205.3m is the family rule 702.73a names, so the other families of CR
    -- 205.3 stay out: an "every subtype" reading would make it a land.
    Spec.assertBool s (not (Set.member Subtype.Type.Island subtypes)) "not a land type"
    Spec.assertBool s (not (Set.member Subtype.Type.Equipment subtypes)) "not an artifact type"
    Spec.assertBool s (not (Set.member Subtype.Type.Aura subtypes)) "not an enchantment type"

  -- THE GAMEPLAY PROOF, and the reader half: a lord has to SEE the type.
  -- Lord of Atlantis gives other Merfolk +1/+1 and islandwalk, and its affected
  -- set is a Filter.HasSubtype Merfolk read off the projection.
  Spec.it s "CR 702.73a Lord of Atlantis pumps a changeling and not a Goblin Piker" $ do
    lord <- S.printingOf s registry "Lord of Atlantis"
    changeling <- S.printingOf s registry "Woodland Changeling"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (_, withLord) = S.addCreature lord S.alice base
        (changelingId, withChangeling) = S.addCreature changeling S.alice withLord
        (pikerId, gs) = S.addCreature piker S.alice withChangeling
    Spec.assertEqWith s "the 2/2 changeling is a Merfolk, so 3/3" (Projection.powerOf changelingId gs) (Just 3)
    Spec.assertBool s (Projection.hasKeyword (Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Type.Island)) changelingId gs) "and has islandwalk"
    -- The control: the Piker is a Goblin Warrior and no changeling, so the same
    -- lord on the same board leaves it alone.
    Spec.assertEqWith s "the Piker is untouched" (Projection.powerOf pikerId gs) (Just 2)

  -- THE ORDERING FALSIFIER. Turn to Frog sets the creature types (CR 205.1b),
  -- and it is an ordinary timestamped layer-4 effect. CR 613.3 puts the CDA
  -- first, so the set wipes every type changeling defined and Frog alone
  -- survives; applying changeling after layer 4's effects leaves all of them.
  Spec.it s "CR 613.3 Turn to Frog beats a changeling's CDA: a Frog and nothing else" $ do
    island <- S.printingOf s registry "Island"
    changeling <- S.printingOf s registry "Woodland Changeling"
    turnToFrog <- S.printingOf s registry "Turn to Frog"
    let (oid, board) = S.addCreature changeling S.alice (S.landsInPlay island 3)
        after = castAtCreature oid turnToFrog board
    Spec.assertBool s (Set.member Subtype.Type.Merfolk (Projection.subtypesOf oid board)) "before: a Merfolk among the rest"
    Spec.assertEqWith s "after: Creature -- Frog alone" (Projection.subtypesOf oid after) (Set.singleton Subtype.Type.Frog)

  -- CR 702.73a says the ability "works everywhere", and viewOfCard is built from
  -- a face rather than folded through CR 613, so it applies the ability itself --
  -- devoid's posture at CR 702.114a.
  Spec.it s "CR 702.73a a changeling is every creature type OFF the battlefield too" $ do
    changeling <- S.printingOf s registry "Woodland Changeling"
    let view = Projection.viewOfCard (S.combinedFace changeling)
    Spec.assertBool s (Set.member Subtype.Type.Merfolk (Filter.subtypes view)) "a Merfolk in a library"
    Spec.assertBool s (not (Set.member Subtype.Type.Island (Filter.subtypes view))) "and still not a land type"

  -- CR 205.1b's add over the whole of CR 205.3m, the layer-4 arm changeling's two
  -- routes both come down to. Wings of Velis Vel is the card: "target creature has
  -- base power and toughness 4/4, gains all creature types, and gains flying".
  -- Lord of Atlantis is the reader -- its affected set is a Filter.HasSubtype
  -- Merfolk -- so the Piker ends up 4/4 base plus the lord's +1/+1, and the two
  -- numbers are distinct on purpose: a no-op arm leaves 4.
  Spec.it s "CR 205.1b Wings of Velis Vel adds every creature type without replacing the Piker's own" $ do
    island <- S.printingOf s registry "Island"
    lord <- S.printingOf s registry "Lord of Atlantis"
    piker <- S.printingOf s registry "Goblin Piker"
    wings <- S.printingOf s registry "Wings of Velis Vel"
    let (_, withLord) = S.addCreature lord S.alice (S.landsInPlay island 3)
        (pikerId, board) = S.addCreature piker S.alice withLord
        after = castAtCreature pikerId wings board
        subtypes = Projection.subtypesOf pikerId after
    Spec.assertEqWith s "base 4/4, and the lord sees a Merfolk" (Projection.powerOf pikerId after) (Just 5)
    Spec.assertBool s (Set.member Subtype.Type.Merfolk subtypes) "a Merfolk"
    Spec.assertBool s (Set.member Subtype.Type.Goblin subtypes) "and still the printed Goblin (CR 205.1b: the add keeps the rest)"
    Spec.assertBool s (not (Set.member Subtype.Type.Island subtypes)) "and not a land type"
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying pikerId after) "and flying"

  -- THE TWO ROUTES, ONE BOARD. CR 604.3a(2) gives CDA status only to an ability
  -- printed on the card it affects (or on a token's creating effect, or acquired
  -- by copy or text change), so a changeling another object's static ability
  -- GRANTS is an ordinary timestamped layer-4 effect (CR 613.1d) where a printed
  -- one is applied at the START of layer 4 (CR 613.3). An OLDER Turn to Frog is
  -- what tells them apart, and both creatures on this board have one: it beats the
  -- CDA and loses to the grant.
  --
  -- Synthetic Borrowed Shape ("enchanted creature has changeling") is the granter.
  -- No printing grants the keyword: every card that hands changeling to an object
  -- does it as a copy-effect exception (Moritte of the Frost, Omni-Changeling),
  -- which CR 604.3a(2) makes a CDA, and the rest write the subtype sentence.
  Spec.it s "CR 613.7a a granted changeling beats an OLDER Turn to Frog, where a printed one loses to it" $ do
    board <- borrowedShapeBoard s registry True
    let (pikerId, changelingId, gs) = board
    Spec.assertBool s (Set.member Subtype.Type.Merfolk (Projection.subtypesOf pikerId gs)) "the granted changeling is newer, so every creature type is back"
    Spec.assertBool s (Projection.hasKeyword Keyword.Changeling pikerId gs) "and the keyword itself is there (CR 613.1f layer 6)"
    -- The other route on the same board, under the same older Turn to Frog.
    Spec.assertEqWith s "the PRINTED changeling is a Frog and nothing else" (Projection.subtypesOf changelingId gs) (Set.singleton Subtype.Type.Frog)

  -- The negative: the same board with the Aura left off, so the only difference is
  -- the grant. Without it the Piker is what Turn to Frog made it.
  Spec.it s "CR 613.7a without the grant the same Piker is a Frog and nothing else" $ do
    board <- borrowedShapeBoard s registry False
    let (pikerId, _, gs) = board
    Spec.assertEqWith s "Creature -- Frog alone" (Projection.subtypesOf pikerId gs) (Set.singleton Subtype.Type.Frog)
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Changeling pikerId gs)) "and no changeling"

  -- CR 613.8b's loop clause reached by two PRINTED cards, and the proving pair
  -- for deciding CR 613.8a over the whole board rather than per projected object.
  -- Each permanent shows one edge only (see limbBloodMoon), so a per-object
  -- relation finds a one-way dependency at each and answers Blood-Moon-first at
  -- the Bayou while answering Life-and-Limb-first at Shroofus -- an answer no
  -- single order produces, and the same answer in both timestamp orders. The loop
  -- makes CR 613.8b hand the tie to CR 613.7, so the two orders must differ.
  Spec.it s "CR 613.8b Life and Limb and Blood Moon form a loop: the older Life and Limb applies first" $ do
    bayou <- S.printingOf s registry "Bayou"
    shroofus <- S.printingOf s registry "Shroofus Sproutsire"
    limb <- S.printingOf s registry "Life and Limb"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (bayouId, shroofusId, gs) = limbBloodMoon bayou shroofus limb bloodMoon True
    Spec.assertEqWith s "the Bayou is a Saproling, then a Mountain" (Projection.subtypesOf bayouId gs) (Set.fromList [Subtype.Type.Mountain, Subtype.Type.Saproling])
    Spec.assertBool s (Projection.isCreatureOf bayouId gs) "and a creature, which Blood-Moon-first never makes it"
    Spec.assertEqWith s "Shroofus is a Saproling Mountain" (Projection.subtypesOf shroofusId gs) (Set.fromList [Subtype.Type.Mountain, Subtype.Type.Saproling])

  Spec.it s "CR 613.8b the same loop with Blood Moon older gives the other answer" $ do
    bayou <- S.printingOf s registry "Bayou"
    shroofus <- S.printingOf s registry "Shroofus Sproutsire"
    limb <- S.printingOf s registry "Life and Limb"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (bayouId, shroofusId, gs) = limbBloodMoon bayou shroofus limb bloodMoon False
    Spec.assertEqWith s "the Bayou is only a Mountain" (Projection.subtypesOf bayouId gs) (Set.singleton Subtype.Type.Mountain)
    Spec.assertBool s (not (Projection.isCreatureOf bayouId gs)) "and not a creature: Blood Moon took the Forest type before Life and Limb was asked"
    Spec.assertEqWith s "Shroofus is a Saproling Forest, never reached by Blood Moon" (Projection.subtypesOf shroofusId gs) (Set.fromList [Subtype.Type.Forest, Subtype.Type.Saproling])

  -- Conversion is the pool's first static ability whose AFFECTED set names a
  -- basic land type, which is what makes CR 612's word swap reach an affected set
  -- at all. The pair below differs only in whether the swap is installed.
  Spec.it s "CR 305.7 Conversion turns Mountains into Plains" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    conversion <- S.printingOf s registry "Conversion"
    let base = Setup.emptyGame S.bothPlayers
        (mountainId, g1) = S.addCreature mountain S.alice base
        (islandId, g2) = S.addCreature island S.alice g1
        (_, gs) = S.addCreature conversion S.alice g2
    Spec.assertBool s (Set.member Subtype.Type.Plains (Projection.subtypesOf mountainId gs)) "the Mountain is a Plains"
    Spec.assertBool s (not (Set.member Subtype.Type.Mountain (Projection.subtypesOf mountainId gs))) "and no longer a Mountain (CR 305.7's set)"
    Spec.assertBool s (Set.member Subtype.Type.Island (Projection.subtypesOf islandId gs)) "the Island is untouched"

  Spec.it s "CR 612.1 a text change moves which lands Conversion names" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    conversion <- S.printingOf s registry "Conversion"
    let base = Setup.emptyGame S.bothPlayers
        (mountainId, g1) = S.addCreature mountain S.alice base
        (islandId, g2) = S.addCreature island S.alice g1
        (conversionId, g3) = S.addCreature conversion S.alice g2
        -- A Magical Hack on Conversion: "All Mountains are Plains" becomes "All
        -- Islands are Plains". The swap reaches the AFFECTED set, not a
        -- modification, which is the read-point this card exists to exercise.
        gs = S.withEffectAt conversionId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island)) g3
    Spec.assertBool s (Set.member Subtype.Type.Plains (Projection.subtypesOf islandId gs)) "the Island is now a Plains"
    Spec.assertBool s (not (Set.member Subtype.Type.Island (Projection.subtypesOf islandId gs))) "and no longer an Island"
    Spec.assertBool s (Set.member Subtype.Type.Mountain (Projection.subtypesOf mountainId gs)) "the Mountain is left alone"
    Spec.assertBool s (not (Set.member Subtype.Type.Plains (Projection.subtypesOf mountainId gs))) "and is not a Plains"

  -- CR 612.1 reaching the OTHER read-point of the same affected clause: CR 305.7's
  -- ability strip, which Projection.setLandSubtypeEffects gathers and
  -- Projection.liveGiven answers. The layer fold above decides what a subtype
  -- BECOMES; this gate decides whose rules-text abilities survive, and the two must
  -- agree about which permanents Conversion names.
  --
  -- SYNTHETIC CARD, and why. The gate reads BASE characteristics, so seeing it
  -- disagree needs a permanent whose PRINTED type line carries a basic land type
  -- AND which has a rules-text static ability reaching other objects. Every basic
  -- land is abilityless, and no printed nonbasic land in the pool carries a basic
  -- land type beside such an ability, so nothing could be on both sides at once.
  --
  --   Synthetic Volcanic Estuary  Land -- Mountain
  --     "All Forests are Swamps."
  --
  -- Nothing in the CR forbids it: CR 305.7's own subject is a land whose subtype is
  -- set, so a land that both HAS a basic land type and SETS one is the rule's
  -- ordinary shape rather than an exception to it.
  --
  -- Swamp and not Island on purpose. The Estuary's own effect is a layer-4
  -- subtype set, and so is Conversion's, so a shared word would put CR 613.8's
  -- dependency between them and make the two worlds differ for a second reason.
  Spec.it s "CR 305.7 Conversion strips the Estuary's ability, and CR 612.1 hands it back" $ do
    forest <- S.printingOf s registry "Forest"
    estuary <- S.printingOf s registry "Synthetic Volcanic Estuary"
    conversion <- S.printingOf s registry "Conversion"
    let base = Setup.emptyGame S.bothPlayers
        (forestId, g1) = S.addCreature forest S.alice base
        (_, g2) = S.addCreature estuary S.alice g1
        (conversionId, printed) = S.addCreature conversion S.alice g2
        -- The same Magical Hack as the case above, on the same card: "All
        -- Mountains are Plains" becomes "All Islands are Plains". The Estuary's
        -- printed type line still says Mountain, so the rewritten clause no longer
        -- names it.
        hacked = S.withEffectAt conversionId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island)) printed
        -- The anti-vacuity control: with no Conversion at all the Estuary's
        -- ability plainly works, so "the Forest is a Swamp" below is the strip
        -- lifting rather than the card doing nothing either way.
        alone = g2
    Spec.assertBool s (Set.member Subtype.Type.Swamp (Projection.subtypesOf forestId alone)) "with no Conversion the Estuary makes the Forest a Swamp"
    Spec.assertBool s (Set.member Subtype.Type.Forest (Projection.subtypesOf forestId printed)) "under the printed Conversion the Forest is untouched"
    Spec.assertBool s (not (Set.member Subtype.Type.Swamp (Projection.subtypesOf forestId printed))) "because CR 305.7 stripped the Estuary's ability"
    Spec.assertBool s (Set.member Subtype.Type.Swamp (Projection.subtypesOf forestId hacked)) "under the hacked one the ability is live again"
    Spec.assertBool s (not (Set.member Subtype.Type.Forest (Projection.subtypesOf forestId hacked))) "and CR 305.7's set replaced the old land type"

  -- CR 111.3: a token is not a card, and nothing in CR 613 changes that -- so
  -- Not IsToken reads no projected aspect and no ordering turns on it.
  Spec.it s "Ashaya's 'nontoken creatures' excludes a token creature" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, pikerId, tokenId, _, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf pikerId gs)) "the nontoken Piker was animated"
    Spec.assertBool s (not (Set.member CardType.Land (Projection.cardTypesOf tokenId gs))) "the token copy of it was not"
    Spec.assertBool s (not (Set.member Subtype.Type.Mountain (Projection.subtypesOf tokenId gs))) "so Blood Moon never reaches the token"

  -- CR 604.3 / 613.4a: Ashaya's own */* is a characteristic-defining ability
  -- counting lands you control, and it counts the lands her OTHER ability
  -- just made -- layer 4 is applied before layer 7a. Without Blood Moon that
  -- is the Forest, the animated Piker and Ashaya herself; the token is
  -- excluded from the animation and so is not a land either.
  Spec.it s "CR 613.4a Ashaya's CDA counts the lands her layer-4 ability made" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addCreature forest S.alice base
        (_, g2) = S.addCreature piker S.alice g1
        (_, g3) = S.addToken (Printing.card piker) S.alice g2
        (ashayaId, gs) = S.addCreature ashaya S.alice g3
    Spec.assertEqWith s "Forest + animated Piker + Ashaya" (Projection.powerOf ashayaId gs) (Just 3)
    Spec.assertEqWith s "toughness the same" (Projection.toughnessOf ashayaId gs) (Just 3)

  -- CR 305.7's second sentence, in full: "It loses all abilities generated
  -- from its rules text, ITS OLD LAND TYPES, and any copiable effects
  -- affecting that land". Only the LAND types go; the fourth sentence spells
  -- out what stays -- "Setting a land's subtype doesn't add or remove any
  -- card types (such as creature) or supertypes". A creature type is neither
  -- a land type nor a card type, so it survives untouched.
  -- CR 205.3i's list, directly: the classification the arm above folds with,
  -- and the boundary that makes "keeps its creature types" mean anything.
  -- Desert is in the list too, and is why this is not the same question as
  -- Pawl.Engine.Mana.subtypeMana's CR 305.6 one: "Of that list, Forest, Island,
  -- Mountain, Plains, and Swamp are the basic land types", so a Desert is a
  -- land type that grants no intrinsic mana ability. Pawl.ManaSpec pins the
  -- other half of that pair.
  Spec.it s "CR 205.3i a land type is a land type and a creature type is not" $
    Spec.assertEqWith s "Forest, Mountain and Desert in, Goblin and Wall out" (fmap Subtype.isLandType [Subtype.Type.Forest, Subtype.Type.Mountain, Subtype.Type.Desert, Subtype.Type.Goblin, Subtype.Type.Wall]) [True, True, True, False, False]

  -- CR 205.3m's list, the other half of the pair, and the classification the
  -- layer-4 SetCreatureSubtype arm folds with. The two are complements, not
  -- independent judgements, because CR 205.3c/205.3d make the families disjoint
  -- -- so the interesting arms are the ones in NEITHER: Aura is an enchantment
  -- type (CR 205.3h), Equipment an artifact type (CR 205.3g), Jace a
  -- planeswalker type (CR 205.3j) and Arcane a spell type (CR 205.3k), and a
  -- creature-type set must leave every one of them alone.
  Spec.it s "CR 205.3m a creature type is a creature type, and nothing in another family is" $ do
    Spec.assertEqWith
      s
      "Frog, Wraith and Elemental in; Forest and Desert out"
      (fmap Subtype.isCreatureType [Subtype.Type.Frog, Subtype.Type.Wraith, Subtype.Type.Elemental, Subtype.Type.Forest, Subtype.Type.Desert])
      [True, True, True, False, False]
    Spec.assertEqWith
      s
      "and the four other families are out too"
      (fmap Subtype.isCreatureType [Subtype.Type.Aura, Subtype.Type.Equipment, Subtype.Type.Jace, Subtype.Type.Arcane])
      [False, False, False, False]

  Spec.it s "CR 305.7 a Blood Moon'd creature-land keeps its creature types" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, pikerId, _, ashayaId, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
    -- Goblin Piker is a Creature - Goblin Warrior; Ashaya's Forest and the
    -- Piker's printed Goblin/Warrior are the two kinds in one set, and only
    -- the first is a land type.
    Spec.assertEqWith s "the Forest went, Goblin and Warrior stayed" (Projection.subtypesOf pikerId gs) (Set.fromList [Subtype.Type.Mountain, Subtype.Type.Goblin, Subtype.Type.Warrior])
    Spec.assertEqWith s "and Ashaya keeps Elemental" (Projection.subtypesOf ashayaId gs) (Set.fromList [Subtype.Type.Mountain, Subtype.Type.Elemental])

  -- CR 305.7's FIRST clause -- "It loses all abilities generated from its
  -- rules text" -- reaches a characteristic-defining ability like any other:
  -- CR 604.3 makes a CDA a static ability, and CR 613.6's rescue does not
  -- apply because Ashaya's */* would first apply at layer 7a, after the layer
  -- 4 that takes it away.
  --
  -- Nothing is then left to define her power, and CR 208.5 fills the hole:
  -- "If a creature somehow has no value for its power, its power is 0. The
  -- same is true for toughness." She is still a creature (CR 305.7 adds and
  -- removes no card types), so the substitution applies and she reads 0/0
  -- rather than blank. The board consequence -- CR 704.5f burying her -- is
  -- proved at gameplay level in Pawl.PowerToughnessSpec.
  Spec.it s "CR 305.7/208.5 Blood Moon takes Ashaya's CDA, and the creature with no value reads 0" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, pikerId, _, ashayaId, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
    Spec.assertEqWith s "no CDA left to define power, so CR 208.5 makes it 0" (Projection.powerOf ashayaId gs) (Just 0)
    Spec.assertEqWith s "and the same for toughness" (Projection.toughnessOf ashayaId gs) (Just 0)
    -- 0 and absent both read as "no value" to a careless helper, and the
    -- Piker pins the other end: its 2/1 is printed rather than defined by an
    -- ability, so CR 305.7 leaves it alone and CR 208.5 never reaches it.
    Spec.assertEqWith s "the Piker under the same Blood Moon is untouched" (S.powerToughnessOf pikerId gs) (Just (2, 1))

  -- The remaining two ability kinds the strip has to reach, at the projection
  -- rather than through a game: Corpsejack Menace's counter-doubling
  -- replacement effect and Goblin Piker's (empty) trigger list are read off
  -- PC.replacementEffects and PC.triggeredAbilities, and CR 305.7 empties
  -- both. The gameplay proof of the replacement half is in
  -- Pawl.ReplacementSpec.
  Spec.it s "CR 305.7 Blood Moon takes an animated creature's replacement effect" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (corpsejackId, g1) = S.addCreature corpsejackMenace S.alice base
        (_, g2) = S.addCreature piker S.alice g1
        (_, g3) = S.addCreature ashaya S.alice g2
    Spec.assertEqWith s "it has one before Blood Moon" (length (Projection.replacementsOf corpsejackId g3)) 1
    let (_, gs) = S.addCreature bloodMoon S.alice g3
    Spec.assertBool s (Set.member Subtype.Type.Mountain (Projection.subtypesOf corpsejackId gs)) "the Menace is a Mountain now"
    Spec.assertEqWith s "and its rules text is gone" (Projection.replacementsOf corpsejackId gs) []

  Spec.it s "CR 612 hacking Blood Moon Mountain->Island: nonbasic lands become Islands (hack newer)" $ do
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (nonbasicId, g1) = S.addCreature urborg S.alice base
        (bloodMoonId, g2) = S.addCreature bloodMoon S.alice g1
        gs = S.withEffectAt bloodMoonId (Timestamp.MkTimestamp 500) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island)) g2
    Spec.assertEqWith s "nonbasic land is now Island" (Projection.subtypesOf nonbasicId gs) (Set.singleton Subtype.Type.Island)

  Spec.it s "CR 612 hacking Blood Moon is order-independent (hack older)" $ do
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (nonbasicId, g1) = S.addCreature urborg S.alice base
        (bloodMoonId, g2) = S.addCreature bloodMoon S.alice g1
        -- Timestamp 1 is older than Blood Moon's own object timestamp; the
        -- outcome must not change.
        gs = S.withEffectAt bloodMoonId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island)) g2
    Spec.assertEqWith s "nonbasic land is Island, order-independent" (Projection.subtypesOf nonbasicId gs) (Set.singleton Subtype.Type.Island)

  Spec.it s "Opalescence makes Humility a creature: legal creature target and SBA-killable" $ do
    humility <- S.printingOf s registry "Humility"
    opalescence <- S.printingOf s registry "Opalescence"
    let base = Setup.emptyGame S.bothPlayers
        (humilityId, g1) = S.addCreature humility S.alice base
        -- Opalescence AFTER Humility, so Opalescence's 7b (mana value 4) wins
        -- the timestamp race: Humility is a 4/4 creature.
        (_, g2) = S.addCreature opalescence S.alice g1
    Spec.assertBool s (Projection.isCreatureOf humilityId g2) "Humility is a creature"
    Spec.assertEqWith s "base P/T = its mana value" (Projection.toughnessOf humilityId g2) (Just 4)
    let damaged = S.markDamage humilityId 4 g2
        afterSba = S.settleSba damaged
    Spec.assertBool s (not (Set.member humilityId (GameState.battlefield afterSba))) "lethal damage destroys the animated enchantment"

  -- Opalescence's card text says "each OTHER enchantment" (no rule number --
  -- CR 305.2 is the one-land-per-turn rule and is unrelated): Opalescence
  -- does not animate itself. Since #163 that is the Not IsSource conjunct in
  -- the card's own affected-set Filter, evaluated against the candidate
  -- View's identity -- so it is the data file, not an engine field, that has
  -- to carry it.
  Spec.it s "Opalescence does not animate itself" $ do
    humility <- S.printingOf s registry "Humility"
    opalescence <- S.printingOf s registry "Opalescence"
    let base = Setup.emptyGame S.bothPlayers
        (opalescenceId, g1) = S.addCreature opalescence S.alice base
        -- A second enchantment, so the effect is demonstrably live: it
        -- animates Humility in the same state where it skips itself.
        (humilityId, gs) = S.addCreature humility S.alice g1
    Spec.assertBool s (Projection.isCreatureOf humilityId gs) "the other enchantment IS animated"
    Spec.assertBool s (not (Projection.isCreatureOf opalescenceId gs)) "Opalescence is not"

  -- CR 613.6: "If an effect starts to apply in one layer and/or sublayer, it
  -- will continue to be applied to the same set of objects in each other
  -- applicable layer and/or sublayer, even if the ability generating the
  -- effect is removed during this process."
  --
  -- March of the Machines is the card that needs it, and the reason nothing
  -- before it did. Its affected set is "each NONCREATURE artifact" and its own
  -- layer-4 part makes every object in that set a creature, so a set
  -- re-derived at layer 7b would be empty: the animated artifact would never
  -- get the P/T March's own layer-7b part is there to set, and CR 208.5 would
  -- hand it 0/0 and CR 704.5f would bury it. Opalescence never noticed because
  -- its filter reads card types it does not change.
  Spec.it s "CR 613.6: March of the Machines animates an artifact AND still sets its P/T" $ do
    march <- S.printingOf s registry "March of the Machines"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let base = Setup.emptyGame S.bothPlayers
        (equip, g1) = S.addCreature bonesplitter S.alice base
        (_, gs) = S.addCreature march S.alice g1
    Spec.assertBool s (Projection.isCreatureOf equip gs) "Bonesplitter is a creature (layer 4)"
    Spec.assertEqWith s "power equal to its mana value, {1} (layer 7b)" (Projection.powerOf equip gs) (Just 1)
    Spec.assertEqWith s "and toughness the same" (Projection.toughnessOf equip gs) (Just 1)

  -- The other half of the same rule, and the half a per-layer re-derivation
  -- gets right by accident: an artifact that was ALREADY a creature is not in
  -- the set when March starts to apply, so it is in the set at NEITHER layer.
  -- Its P/T must stay printed rather than becoming its mana value.
  Spec.it s "CR 613.6: an artifact that was already a creature is in no part of March's set" $ do
    march <- S.printingOf s registry "March of the Machines"
    myr <- S.printingOf s registry "Darksteel Myr"
    let base = Setup.emptyGame S.bothPlayers
        (myrId, g1) = S.addCreature myr S.alice base
        (_, gs) = S.addCreature march S.alice g1
    Spec.assertEqWith s "still its printed 0 power, not its {3} mana value" (Projection.powerOf myrId gs) (Just 0)
    Spec.assertEqWith s "and its printed 1 toughness" (Projection.toughnessOf myrId gs) (Just 1)

  -- Living Plane is March of the Machines' green cousin: the same layer-4
  -- animation and layer-7b base P/T, but over LANDS and at a literal 1/1
  -- rather than a mana value. "That are still lands" is CR 205.1b, which
  -- AddCardType satisfies without a clause of its own -- adding a card type
  -- never removes one.
  Spec.it s "Living Plane makes a land a 1/1 creature that is still a land" $ do
    livingPlane <- S.printingOf s registry "Living Plane"
    forest <- S.printingOf s registry "Forest"
    let base = Setup.emptyGame S.bothPlayers
        (land, g1) = S.addCreature forest S.alice base
        (self, gs) = S.addCreature livingPlane S.alice g1
    Spec.assertBool s (Projection.isCreatureOf land gs) "the Forest is a creature (layer 4)"
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf land gs)) "and still a land"
    Spec.assertEqWith s "power 1 (layer 7b)" (Projection.powerOf land gs) (Just 1)
    Spec.assertEqWith s "toughness 1" (Projection.toughnessOf land gs) (Just 1)
    Spec.assertBool s (not (Projection.isCreatureOf self gs)) "the enchantment itself is no land, so it animates nothing but lands"

  -- The whole card, cast: March of the Machines' own reminder text is
  -- "(Equipment that's a creature can't equip a creature.)" -- CR 301.5c, whose
  -- state-based action is CR 704.5p. So the two halves meet here: the layer-7b
  -- part that CR 613.6 rescues gives the Equipment its P/T, and the layer-4
  -- part that gave it the creature type also knocks it off the creature it was
  -- equipping.
  Spec.it s "CR 613.6 + CR 704.5p whole card: casting March animates an equipped Bonesplitter, which falls off" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    march <- S.printingOf s registry "March of the Machines"
    let base = S.landsInPlay island 4 -- {3}{U}
        (creature, g1) = S.addCreature piker S.alice base
        (equip, g2) = S.addCreature bonesplitter S.alice g1
        attached = S.attach equip creature g2
        (withSpell, spellId) = S.handOne march attached
        cast = snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        after = snd (Engine.runGamePure S.identityAnswer resolved Engine.settleForPriority)
    Spec.assertEqWith s "equipped, the Piker was 4/1" (Projection.powerOf creature attached) (Just 4)
    Spec.assertEqWith s "the Equipment is a 1/1 creature" (Projection.powerOf equip after) (Just 1)
    Spec.assertBool s (Set.member equip (GameState.battlefield after)) "it is still on the battlefield"
    Spec.assertEqWith s "but unattached" (fmap Object.attachedTo (Game.lookupObject equip after)) (Just Nothing)
    Spec.assertEqWith s "so the Piker is back to 2 power" (Projection.powerOf creature after) (Just 2)

  Spec.it s "CR 613 Humility + Opalescence: a real creature is 1/1 with no abilities" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    opalescence <- S.printingOf s registry "Opalescence"
    let base = Setup.emptyGame S.bothPlayers
        (pikerId, g1) = S.addCreature piker S.alice base
        (_, g2) = S.addCreature humility S.alice g1
        (_, gs) = S.addCreature opalescence S.alice g2
    Spec.assertEqWith s "power 1" (Projection.powerOf pikerId gs) (Just 1)
    Spec.assertEqWith s "toughness 1" (Projection.toughnessOf pikerId gs) (Just 1)
    Spec.assertBool s (Map.null (Projection.keywordsOf pikerId gs)) "no abilities"

  Spec.it s "CR 613.7 Humility + Opalescence: Humility is 4/4 when Opalescence is newer" $ do
    humility <- S.printingOf s registry "Humility"
    opalescence <- S.printingOf s registry "Opalescence"
    let base = Setup.emptyGame S.bothPlayers
        (humilityId, g1) = S.addCreature humility S.alice base
        (_, gs) = S.addCreature opalescence S.alice g1
    Spec.assertEqWith s "Opalescence's mana-value 7b wins" (Projection.powerOf humilityId gs) (Just 4)

  Spec.it s "CR 613.7 Humility + Opalescence: Humility is 1/1 when Humility is newer" $ do
    humility <- S.printingOf s registry "Humility"
    opalescence <- S.printingOf s registry "Opalescence"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addCreature opalescence S.alice base
        (humilityId, gs) = S.addCreature humility S.alice g1
    Spec.assertEqWith s "Humility's 1/1 7b wins" (Projection.powerOf humilityId gs) (Just 1)

  -- Opalescence's own text says "each other NON-AURA enchantment". Card text, not
  -- a rule -- CR 305.2 is the one-land-per-turn rule and does not bear on this.
  Spec.it s "Opalescence does not animate an Aura" $ do
    opalescence <- S.printingOf s registry "Opalescence"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    let base = Setup.emptyGame S.bothPlayers
        (_, withOpal) = S.addCreature opalescence S.alice base
        (auraId, withAura) = S.addCreature unholyStrength S.alice withOpal
        (ripId, gs) = S.addCreature restInPeace S.alice withAura
    Spec.assertBool s (not (Projection.isCreatureOf auraId gs)) "the Aura stays a non-creature"
    Spec.assertBool s (Projection.isCreatureOf ripId gs) "a non-Aura enchantment IS animated"

  -- CR 613.1b puts control-changing effects in layer 2 and CR 613.1f puts
  -- ability-removing effects in layer 6, so layer 2 is applied FIRST: by the
  -- time Humility's LoseAllAbilities is applied, Control Magic's grant has
  -- already moved the creature. Nothing can reverse that order -- CR 613.8a
  -- scopes dependency to effects "applied in the same layer (and, if
  -- applicable, sublayer)", so a layer-6 effect is never pulled ahead of a
  -- layer-2 one -- and CR 613.6 keeps an effect applying "even if the
  -- ability generating the effect is removed during this process". An
  -- ability-stripped control grant therefore still holds what it took.
  --
  -- Humility cannot in fact reach Control Magic at all: its affected set is
  -- "each creature" and the Aura is an enchantment, which the test above is
  -- the other half of (the pool's one enchantment animator excludes Auras).
  -- So the assertion that Humility is LIVE -- the stolen Piker is 1/1 -- is
  -- what stops this passing for the trivial reason.
  Spec.it s "CR 613.1b before CR 613.1f: Humility does not hand back a Control Magic'd creature" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    humility <- S.printingOf s registry "Humility"
    let base = Setup.emptyGame S.bothPlayers
        (creature, g1) = S.addCreature piker S.bob base
        (aura, g2) = S.addCreature controlMagic S.alice g1
        stolen = S.attach aura creature g2
        gs = S.withHumility humility stolen
    Spec.assertEqWith s "before Humility, alice controls it" (Projection.controllerOf creature stolen) (Just S.alice)
    Spec.assertEqWith s "and after Humility she still does" (Projection.controllerOf creature gs) (Just S.alice)
    Spec.assertBool s (elem creature (Projection.controls S.alice gs)) "it is in alice's controls"
    Spec.assertBool s (notElem creature (Projection.controls S.bob gs)) "and not in bob's"
    Spec.assertEqWith s "Humility is live: the stolen Piker is 1/1" (Projection.powerOf creature gs) (Just 1)
    Spec.assertBool s (not (Projection.isCreatureOf aura gs)) "the Aura is no creature, so Humility's set never held it"

  -- The same board built the other way round: Humility is already out when
  -- the Aura arrives. CR 613.1 orders effects in different layers by LAYER,
  -- and CR 613.7's timestamps only order effects within one -- so which
  -- permanent entered first cannot change the answer.
  Spec.it s "CR 613.1: Humility first, then Control Magic, reaches the same controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    humility <- S.printingOf s registry "Humility"
    let base = Setup.emptyGame S.bothPlayers
        (creature, g1) = S.addCreature piker S.bob base
        underHumility = S.withHumility humility g1
        (aura, withAura) = S.addCreature controlMagic S.alice underHumility
        gs = S.attach aura creature withAura
    Spec.assertEqWith s "bob's until the Aura attaches" (Projection.controllerOf creature underHumility) (Just S.bob)
    Spec.assertEqWith s "alice's once it does" (Projection.controllerOf creature gs) (Just S.alice)
    Spec.assertEqWith s "Humility is live: the stolen Piker is 1/1" (Projection.powerOf creature gs) (Just 1)

  -- The leg that shows Humility is not moving controllers on its own.
  Spec.it s "CR 613.1f: Humility alone changes no controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    let base = Setup.emptyGame S.bothPlayers
        (creature, g1) = S.addCreature piker S.bob base
        gs = S.withHumility humility g1
    Spec.assertEqWith s "still bob's" (Projection.controllerOf creature gs) (Just S.bob)
    Spec.assertEqWith s "though Humility is live: 1/1" (Projection.powerOf creature gs) (Just 1)

  -- The layer-2-before-layer-6 claim pinned where no card can pin it.
  -- Nothing in the pool strips an AURA's abilities -- Humility's set is
  -- "each creature", and the pool's one enchantment animator excludes Auras
  -- by its own text -- so this drops a bare layer-6 LoseAllAbilities on the
  -- Aura itself, the stored shape the layer-6 tests above already use.
  --
  -- Honest about what it observes: ProjectedCharacteristics carries no
  -- static-ability field, so the strip is a no-op on this Aura today and
  -- this test cannot watch one land. What it pins is the DIRECTION -- gating
  -- the control-grant walk on layer-6 ability removal breaks it, and per CR
  -- 613.1b/613.1f that gate would be wrong.
  Spec.it s "CR 613.1b: a layer-6 strip on the Aura does not undo its layer-2 grant" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    let base = Setup.emptyGame S.bothPlayers
        (creature, g1) = S.addCreature piker S.bob base
        (aura, g2) = S.addCreature controlMagic S.alice g1
        stolen = S.attach aura creature g2
        gs = S.withEffectAt aura (Timestamp.MkTimestamp 500) Modification.LoseAllAbilities stolen
    Spec.assertEqWith s "the Aura still holds the creature" (Projection.controllerOf creature gs) (Just S.alice)

  -- The boundary of the claim above, and the direction where it flips.
  -- Layer 6 is applied BEFORE layer 7 (CR 613.1f, CR 613.1g), so an ability
  -- removed in layer 6 generates no layer-7 effect -- and CR 613.6 does not
  -- rescue it, because layer 7c is the only layer that part would ever have
  -- started to apply in.
  --
  -- Opalescence animates Bad Moon (layer 4), Humility strips every creature
  -- (layer 6), so Bad Moon's "black creatures get +1/+1" (layer 7c) must not
  -- apply and bob's black Skeletons is Humility's 1/1.
  --
  -- The same board without Humility is asserted alongside, so this cannot pass
  -- for the trivial reason that the pump never reaches the Skeletons at all.
  Spec.it s "CR 613.1f/613.1g layer 6 before layer 7c: a stripped Bad Moon pumps nothing" $ do
    skeletons <- S.printingOf s registry "Drudge Skeletons"
    badMoon <- S.printingOf s registry "Bad Moon"
    humility <- S.printingOf s registry "Humility"
    opalescence <- S.printingOf s registry "Opalescence"
    let base = Setup.emptyGame S.bothPlayers
        (skelId, g1) = S.addCreature skeletons S.bob base
        (badMoonId, g2) = S.addCreature badMoon S.alice g1
        (_, unstripped) = S.addCreature opalescence S.alice g2
        (_, g3) = S.addCreature humility S.alice g2
        (_, gs) = S.addCreature opalescence S.alice g3
    Spec.assertEqWith s "animated but unstripped, Bad Moon pumps: 2/2" (Projection.powerOf skelId unstripped) (Just 2)
    Spec.assertBool s (Projection.isCreatureOf badMoonId gs) "Bad Moon is a creature, so Humility's set holds it"
    Spec.assertEqWith s "Humility's 1/1, not 2/2" (Projection.powerOf skelId gs) (Just 1)
    Spec.assertEqWith s "and the same on toughness" (Projection.toughnessOf skelId gs) (Just 1)

  -- The narrowness of that gate, in the direction it must NOT reach: the
  -- question is whether BAD MOON's abilities were removed, not whether a
  -- LoseAllAbilities is on the battlefield. Humility's set is "each creature"
  -- and an un-animated Bad Moon is a plain enchantment, so it keeps its
  -- ability -- and CR 613.4's sublayers do the rest, 7b setting the base to
  -- 1/1 before 7c adds the +1/+1. The real ruling is that Humility plus Bad
  -- Moon makes a black creature 2/2.
  Spec.it s "CR 613.4 an un-animated Bad Moon is not stripped, so a Humility'd black creature is 2/2" $ do
    skeletons <- S.printingOf s registry "Drudge Skeletons"
    badMoon <- S.printingOf s registry "Bad Moon"
    humility <- S.printingOf s registry "Humility"
    let base = Setup.emptyGame S.bothPlayers
        (skelId, g1) = S.addCreature skeletons S.bob base
        (badMoonId, g2) = S.addCreature badMoon S.alice g1
        gs = S.withHumility humility g2
    Spec.assertBool s (not (Projection.isCreatureOf badMoonId gs)) "no animator, so Bad Moon is no creature"
    Spec.assertEqWith s "1/1 from Humility's 7b, then +1/+1 from Bad Moon's 7c" (Projection.powerOf skelId gs) (Just 2)
    Spec.assertEqWith s "and the same on toughness" (Projection.toughnessOf skelId gs) (Just 2)

  -- The third boundary, and the one CR 613.6 draws rather than CR 613.1f/g:
  -- an ability with a part BELOW layer 6 has already started to apply when
  -- layer 6 removes it, so "it will continue to be applied to the same set of
  -- objects in each other applicable layer" and its layer-7 part stands.
  --
  -- Opalescence animates March of the Machines, so Humility's "each creature"
  -- reaches March and strips it -- but March's layer-4 AddCardType already
  -- made Mindslaver a creature, so March's layer-7b "with power and toughness
  -- each equal to its mana value" still applies. March enters last, so its 7b
  -- has the later timestamp (CR 613.7) and beats Humility's 1/1: Mindslaver is
  -- 6/6, not 1/1. The contrast with Bad Moon two tests up is the whole rule --
  -- Bad Moon's ONLY part is layer 7c, so it never started to apply at all.
  Spec.it s "CR 613.6: a stripped March of the Machines still sets P/T, because its layer-4 part started" $ do
    mindslaver <- S.printingOf s registry "Mindslaver"
    humility <- S.printingOf s registry "Humility"
    opalescence <- S.printingOf s registry "Opalescence"
    march <- S.printingOf s registry "March of the Machines"
    let base = Setup.emptyGame S.bothPlayers
        (slaverId, g1) = S.addCreature mindslaver S.alice base
        (_, g2) = S.addCreature humility S.alice g1
        (_, g3) = S.addCreature opalescence S.alice g2
        (marchId, gs) = S.addCreature march S.alice g3
    Spec.assertBool s (Projection.isCreatureOf marchId gs) "Opalescence animated March, so Humility's set holds it"
    Spec.assertBool s (Projection.isCreatureOf slaverId gs) "March's layer-4 part still animates the artifact"
    Spec.assertEqWith s "and its layer-7b part still sets the mana value, 6" (Projection.powerOf slaverId gs) (Just 6)
    Spec.assertEqWith s "and the same on toughness" (Projection.toughnessOf slaverId gs) (Just 6)

  -- The last thing the gate must not reach. CR 122.1a makes a +1/+1 counter's
  -- +1/+1 a rule of the GAME rather than an ability of the permanent it sits
  -- on, so there is nothing on the creature for CR 613.1f to remove and the
  -- layer-7c effect stands after Humility's layer-7b 1/1. Projection.gather
  -- emits the counter as a synthetic candidate with the creature as its
  -- source, which is exactly the shape the layer-6 gate keys on -- so this
  -- pins that counters are excluded from it.
  Spec.it s "CR 122.1a a +1/+1 counter survives Humility: 1/1 becomes 2/2" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    let base = Setup.emptyGame S.bothPlayers
        (pikerId, g1) = S.addCreature piker S.bob base
        g2 = S.addCounter CounterKind.PlusOnePlusOne 1 pikerId g1
        gs = S.withHumility humility g2
    Spec.assertEqWith s "power 1 + 1" (Projection.powerOf pikerId gs) (Just 2)
    Spec.assertEqWith s "toughness 1 + 1" (Projection.toughnessOf pikerId gs) (Just 2)

  -- CR 613.8b: "An effect dependent on one or more other effects waits to
  -- apply until just after all of those effects have been applied." A Piker
  -- made a Land by B (layer 4, TheseObjects), and A = AddLandSubtype Swamp
  -- over Matching (HasCardType Land), also layer 4, with A OLDER than B.
  --
  -- A depends on B by CR 613.8a clause (b): applying B changes what A applies
  -- to. So B goes first despite its later timestamp, and the Piker gains the
  -- Swamp. Timestamp order alone would apply A to a Piker that is not a land
  -- yet and add nothing -- which is what this test asserted, and documented as
  -- known-incomplete, until #11 closed.
  Spec.it s "CR 613.8b within layer 4, a dependency overrides timestamp order" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (pikerId, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        gsA = withDynamicEffect (Affected.Matching (Filter.Type.HasCardType CardType.Land)) (Timestamp.MkTimestamp 10) (Modification.AddLandSubtype Subtype.Type.Swamp) gs0
        gs = S.withEffectAt pikerId (Timestamp.MkTimestamp 20) (Modification.AddCardType CardType.Land) gsA
    Spec.assertBool s (Set.member Subtype.Type.Swamp (Projection.subtypesOf pikerId gs)) "the newer land-maker applied first, so the Swamp lands"

  -- The other direction, which is CR 613.7 surviving underneath CR 613.8: with
  -- no dependency between them, two same-layer effects are still applied in
  -- timestamp order. Here B makes the Piker a land at timestamp 20 and A adds
  -- a Swamp to a FIXED set (the Piker) at timestamp 10 -- A's set names an
  -- object id, so applying B cannot change it, so A does not depend on B and
  -- nothing is reordered.
  --
  -- Each SetLandSubtype retires the LAND type its predecessor left and no
  -- more (CR 305.7), so the Piker's printed creature types ride through both
  -- and the Swamp/Forest pair alone carries the ordering claim.
  Spec.it s "CR 613.7 within layer 4, no dependency leaves timestamp order alone" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (pikerId, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        gsA = S.withEffectAt pikerId (Timestamp.MkTimestamp 10) (Modification.SetLandSubtype Subtype.Type.Swamp) gs0
        gs = S.withEffectAt pikerId (Timestamp.MkTimestamp 20) (Modification.SetLandSubtype Subtype.Type.Forest) gsA
    Spec.assertEqWith s "the later SetLandSubtype wins, and only the land types moved" (Projection.subtypesOf pikerId gs) (Set.fromList [Subtype.Type.Forest, Subtype.Type.Goblin, Subtype.Type.Warrior])

  -- CR 613.8b's last sentence: "If several dependent effects form a dependency
  -- loop, then this rule is ignored and the effects IN THE DEPENDENCY LOOP are
  -- applied in timestamp order." Only the loop's own members escape the
  -- dependency rule; an effect that merely waits on the loop keeps waiting.
  --
  -- Three effects on a Forest, all layer 4:
  --
  --   A (t=20) "each noncreature ... gains Swamp"     applies; reads types
  --   B (t=30) "each non-Swamp ... becomes a creature" applies; reads subtypes
  --   C (t=10) "each Swamp ... gains Mountain"         does NOT apply yet
  --
  -- A and B each stop the other from applying, so they are a two-effect loop.
  -- C depends on A -- A's Swamp is what would let C apply -- but nothing
  -- depends on C, so C is NOT in the loop. C also has the earliest timestamp,
  -- which is the whole point: a fallback that took the earliest of everything
  -- pending would spend C first, while it still does not apply, and the
  -- Mountain would never land. Restricted to the cycle, A goes first, and C
  -- gets its turn afterwards with the Swamp in place.
  Spec.it s "CR 613.8b a dependency loop lets only its own members ignore the rule" $ do
    forest <- S.printingOf s registry "Forest"
    let gs0 = S.landsInPlay forest 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        withC = withDynamicEffect (Affected.Matching (Filter.Type.HasSubtype Subtype.Type.Swamp)) (Timestamp.MkTimestamp 10) (Modification.AddLandSubtype Subtype.Type.Mountain) gs0
        withA = withDynamicEffect (Affected.Matching (Filter.Type.Not (Filter.Type.HasCardType CardType.Creature))) (Timestamp.MkTimestamp 20) (Modification.AddLandSubtype Subtype.Type.Swamp) withC
        gs = withDynamicEffect (Affected.Matching (Filter.Type.Not (Filter.Type.HasSubtype Subtype.Type.Swamp))) (Timestamp.MkTimestamp 30) (Modification.AddCardType CardType.Creature) withA
        subtypes = Projection.subtypesOf landId gs
    Spec.assertBool s (Set.member Subtype.Type.Swamp subtypes) "A applied: the Forest is a Swamp"
    Spec.assertBool s (Set.member Subtype.Type.Mountain subtypes) "and C, which was only waiting on the loop, still got its turn"
    Spec.assertBool s (not (Projection.isCreatureOf landId gs)) "B lost its window once A applied, so this is no creature"

  -- CR 613.8b with real cards, and the pair that retired #11's expiry trigger:
  -- Liquimetal Coating ("{T}: Target permanent becomes an artifact in addition
  -- to its other types until end of turn") and March of the Machines ("Each
  -- noncreature artifact is an artifact creature with power and toughness each
  -- equal to its mana value"). Both apply in layer 4.
  --
  -- March depends on the Coating: applying the Coating's effect makes its
  -- target an artifact, which changes whether March applies to it. The Coating
  -- does not depend on March -- its CR 611.2c set names an object id, and no
  -- type change moves an id in or out of that. So the Coating goes first even
  -- though March, already on the battlefield, is older.
  --
  -- The end state is the whole rule in one board: the Forest is an artifact
  -- (Coating), therefore a noncreature artifact when March is asked, therefore
  -- an artifact creature with base P/T equal to its mana value -- and a land
  -- has no mana cost, so that is 0/0 and CR 704.5f buries it.
  Spec.it s "CR 613.8b whole cards: Liquimetal Coating + March of the Machines kills the land it points at" $ do
    forest <- S.printingOf s registry "Forest"
    march <- S.printingOf s registry "March of the Machines"
    coating <- S.printingOf s registry "Liquimetal Coating"
    let gs0 = S.landsInPlay forest 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        (_, g1) = S.addCreature march S.alice gs0
        (coatingId, g2) = S.addCreature coating S.alice g1
        ability = case Face.activatedAbilities (S.combinedFace coating) of
          ab : _ -> Just ab
          [] -> Nothing
    case ability of
      Nothing -> Spec.assertFailure s "Liquimetal Coating should print one activated ability"
      Just coat -> do
        let ready = g2 {GameState.priority = Just S.alice}
            activated = snd (Engine.runGamePure (aimAtObject landId) ready (Activate.activateAbility S.alice coatingId coat))
            coated = snd (Engine.runGamePure (aimAtObject landId) activated Stack.resolveTop)
        Spec.assertBool s (Set.member CardType.Artifact (Projection.cardTypesOf landId coated)) "the Forest is an artifact now"
        Spec.assertBool s (Projection.isCreatureOf landId coated) "and March therefore animates it"
        Spec.assertEqWith s "at its mana value, which for a land is 0" (Projection.powerOf landId coated) (Just 0)
        let settled = snd (Engine.runGamePure (aimAtObject landId) coated Engine.settleForPriority)
        Spec.assertBool s (not (Set.member landId (GameState.battlefield settled))) "so CR 704.5f buries it"

  -- CR 613.8a through a KEYWORD, which Filter.HasKeyword made a real question:
  -- filterReads maps that atom to the Keywords aspect and modificationWrites maps
  -- GainKeyword to it, so a keyword grant can move an affected set exactly as a
  -- type change can. Two layer-6 effects on one Piker:
  --
  --   A (t=10) "each creature with flying gains deathtouch"  reads Keywords
  --   B (t=20) grant flying to THIS Piker                    writes Keywords
  --
  -- A depends on B by clause (b) -- applying B changes whether A applies to the
  -- Piker -- so B goes first despite its later timestamp, and the deathtouch
  -- lands. Timestamp order alone would ask A about a Piker that does not fly yet
  -- and grant nothing, which is exactly what this asserted before the Keywords
  -- aspect existed. B's set names an object id (CR 611.2c), so B does not depend
  -- on A and there is no loop.
  Spec.it s "CR 613.8b within layer 6, a keyword grant reorders a keyword-reading effect" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, gs0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        gsA = withDynamicEffect (Affected.Matching (Filter.Type.HasKeyword Keyword.Flying)) (Timestamp.MkTimestamp 10) (Modification.GainKeyword Keyword.Deathtouch) gs0
        gs = S.withEffectAt pikerId (Timestamp.MkTimestamp 20) (Modification.GainKeyword Keyword.Flying) gsA
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying pikerId gs) "the grant itself landed"
    Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch pikerId gs) "the newer flying-granter applied first, so the deathtouch lands"

  -- The same rule with a real ability-REMOVING card, which is the other half of
  -- what modificationWrites now declares: Humility (CR 613.1f, "all creatures
  -- lose all abilities and have base power and toughness 1/1") writes Keywords
  -- too, because applyModification's LoseAllAbilities arm empties PC.keywords.
  --
  --   A (older than Humility) "each creature WITHOUT flying gains deathtouch"
  --   Humility                 strips every creature's abilities
  --
  -- Bird Maiden prints flying, so A does not apply to it while Humility is
  -- unapplied. A depends on Humility, so Humility goes first, the flying goes,
  -- and A then applies -- and because A applied AFTER Humility, its grant is not
  -- one of the abilities Humility erased (the pair pinned by "a grant older than
  -- Humility is erased; newer survives" above). Timestamp order alone would ask A
  -- first, get "it flies, skip it", and leave the Maiden with nothing.
  --
  -- Asserted about the FLIER only, which is the object the dependency is visible
  -- at: a Goblin Piker beside it never flew, so applying Humility would not change
  -- A's answer there. The relation is decided over the whole board, so one such
  -- object settles it for every other.
  Spec.it s "CR 613.8b Humility reorders an effect whose set reads a keyword" $ do
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    humility <- S.printingOf s registry "Humility"
    let (flierId, gs0) = S.addCreature birdMaiden S.bob (Setup.emptyGame S.bothPlayers)
        withHum = S.withHumility humility gs0
        Timestamp.MkTimestamp h = humilityTimestamp humility withHum
        groundling = Affected.Matching (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not (Filter.Type.HasKeyword Keyword.Flying)])
        withA = withDynamicEffect groundling (Timestamp.MkTimestamp (h - 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
        withoutHumility = withDynamicEffect groundling (Timestamp.MkTimestamp (h - 1)) (Modification.GainKeyword Keyword.Deathtouch) gs0
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Deathtouch flierId withoutHumility)) "with no Humility the Maiden flies, so the set excludes it"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying flierId withA)) "Humility took the flying"
    Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch flierId withA) "so the older effect waited for Humility, applied after it, and its grant survives"

  Spec.it s "CR 614: Rest in Peace projects its graveyard->exile replacement" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    let (rip, gs) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "one redirect replacement"
      (Projection.replacementsOf rip gs)
      [ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Anyones (Filter.Type.And [])) Zone.Exile)]

  Spec.it s "a vanilla creature projects no replacements" $ do
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (piker, gs) = S.addCreature pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "none" (Projection.replacementsOf piker gs) []

  Spec.it s "CR 122.1a a +1/+1 counter adds +1/+1 (layer 7c)" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay forest 0
        (oid, gs0) = S.addCreature piker S.bob base
        gs = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
    Spec.assertEqWith s "power 2 + 1" (Projection.powerOf oid gs) (Just 3)
    Spec.assertEqWith s "toughness 1 + 1" (Projection.toughnessOf oid gs) (Just 2)

  Spec.it s "CR 122.1a a -1/-1 counter subtracts 1/1" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay forest 0
        (oid, gs0) = S.addCreature piker S.bob base
        gs = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs0
    Spec.assertEqWith s "power 2 - 1" (Projection.powerOf oid gs) (Just 1)
    Spec.assertEqWith s "toughness 1 - 1" (Projection.toughnessOf oid gs) (Just 0)

  Spec.it s "CR 613.4c a +1/+1 counter and Giant Growth stack in layer 7c" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay forest 0
        (oid, gs0) = S.addCreature piker S.bob base
        gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
        gs = S.withEffectAt oid (Timestamp.MkTimestamp 9) (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))) gs1
    Spec.assertEqWith s "power 2 + 1 + 3" (Projection.powerOf oid gs) (Just 6)
    Spec.assertEqWith s "toughness 1 + 1 + 3" (Projection.toughnessOf oid gs) (Just 5)

  Spec.it s "CR 108.4 a SetController effect overrides owner; last timestamp wins" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        install pid g =
          let (ts, g1) = Game.freshTimestamp g
              eff =
                ContinuousEffect.MkContinuousEffect
                  { ContinuousEffect.source = oid,
                    ContinuousEffect.timestamp = ts,
                    ContinuousEffect.expiry = Expiry.AtCleanup,
                    ContinuousEffect.modification = Modification.SetController pid,
                    ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
                  }
           in g1 {GameState.continuousEffects = eff : GameState.continuousEffects g1}
        gs = install S.alice (install S.bob base) -- bob first (earlier), then alice (later) -> alice wins
        owned = base
    Spec.assertEqWith s "owner controls with no effect" (Projection.controllerOf oid owned) (Just S.bob)
    Spec.assertEqWith s "the effect grants control" (Projection.controllerOf oid gs) (Just S.alice)
    Spec.assertEqWith s "alice controls oid" (Projection.controls S.alice gs) [oid]
    Spec.assertEqWith s "bob controls nothing" (Projection.controls S.bob gs) []

  Spec.it s "a copy binding seeds the fold with the copied object's copiable P/T" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, gs1) = S.addCreature piker S.alice gs0
        -- The Piker's copiable value (base 2/1) computed via the new function.
        snapshot = Projection.copiableCharacteristics pikerId gs1
        -- A second, unrelated creature (another Piker) we turn into a "copy":
        (cloneId, gs2) = S.addCreature piker S.alice gs1
        stamped = gs2 {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setCopy snapshot (Object.bindings o)}) cloneId (GameState.objects gs2)}
    Spec.assertEqWith s "copy projects the snapshot power" (Projection.powerOf cloneId stamped) (Just 2)
    Spec.assertEqWith s "copy projects the snapshot toughness" (Projection.toughnessOf cloneId stamped) (Just 1)
    Spec.assertBool s (Projection.isCreatureOf cloneId stamped) "copy is a creature"

  Spec.it s "an object with no copy binding projects its own base P/T" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, gs1) = S.addCreature piker S.alice gs0
    Spec.assertEqWith s "base power" (Projection.powerOf pikerId gs1) (Just 2)
    Spec.assertEqWith s "base toughness" (Projection.toughnessOf pikerId gs1) (Just 1)

  -- The unit half of #1512: one board, two filters, two answers. The filter is
  -- the card's, so the same battlefield offers the creature to Clone's "any
  -- creature" and the enchantment to Copy Enchantment's "any enchantment".
  Spec.it s "legalCopyTargets is the filter's battlefield matches excluding self" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    scales <- S.printingOf s registry "Hardened Scales"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, gs1) = S.addCreature piker S.alice gs0
        (scalesId, gs2) = S.addCreature scales S.alice gs1
        (cloneId, gs3) = S.addCreature piker S.alice gs2
        creatures = Filter.Type.HasCardType CardType.Creature
        enchantments = Filter.Type.HasCardType CardType.Enchantment
    Spec.assertEqWith s "excludes self, includes the other creature" (Replacement.legalCopyTargets Set.empty creatures cloneId gs3) [pikerId]
    Spec.assertEqWith s "the same board under an enchantment filter" (Replacement.legalCopyTargets Set.empty enchantments cloneId gs3) [scalesId]

  Spec.it s "viewOfObject reads a projected creature's characteristics" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        view = Projection.viewOfObject oid gs
    Spec.assertBool s (Set.member CardType.Creature (Filter.cardTypes view)) "is a creature"
    Spec.assertEqWith s "controller" (Filter.controller view) (Just S.alice)

  Spec.it s "viewOfCard reads a printed basic land's supertypes off the battlefield" $ do
    mountain <- S.printingOf s registry "Mountain"
    let face = S.combinedFace mountain
        view = Projection.viewOfCard face
    Spec.assertBool s (Set.member CardType.Land (Filter.cardTypes view)) "is a land"
    Spec.assertBool s (Set.member Supertype.Basic (Filter.supertypes view)) "is basic"
    Spec.assertEqWith s "no power off battlefield -- a land has no printed power box" (Filter.power view) Nothing
    Spec.assertEqWith s "no controller off battlefield" (Filter.controller view) Nothing

  -- The gameplay-level proof of Projection.printedPower. Imperial Recruiter's
  -- entry trigger searches alice's library for "a creature card with power 2 or
  -- less", and the candidates come from Projection.viewOfObject -- the card's own
  -- CR 613 projection, whose layer 7a is where CR 208.2a's number is filled in.
  --
  -- THE CANDIDATE SET IS THE ASSERTION, not what was found: with Filter.power
  -- Nothing for every card off the battlefield, CR 208.1's PowerAtMost answered
  -- False for all three creature cards and the set was EMPTY, which "the search
  -- found a card" cannot tell from a set that admitted everything. So the set has
  -- one card in for CR 208.2b's 0 (Primal Plasma, a printed */* with no CDA), one
  -- in for its printed 2 (Goblin Piker), and the Hill Giant out at 3. The Mountain
  -- is out on the creature clause rather than on power, and is in the library so
  -- the search has a non-candidate to shuffle back.
  Spec.it s "CR 208.1/208.2b Imperial Recruiter's search offers the */* card and the 2-power creature, not the 3-power one" $ do
    mountain <- S.printingOf s registry "Mountain"
    recruiter <- S.printingOf s registry "Imperial Recruiter"
    plasma <- S.printingOf s registry "Primal Plasma"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    let base0 = S.landsInPlay mountain 3
        (_, base1) = S.addLibraryCard mountain S.alice base0
        (_, base2) = S.addLibraryCard giant S.alice base1
        (_, base3) = S.addLibraryCard piker S.alice base2
        (plasmaId, base4) = S.addLibraryCard plasma S.alice base3
        (gs, spellId) = S.handOne recruiter base4
        ((_, settled), (searches, shuffles)) =
          State.runState
            (Engine.runGame (searchRecordingAnswer plasmaId) gs (do S.cast S.alice spellId; Engine.priorityLoop))
            ([], [])
    Spec.assertEqWith s "the Recruiter resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Imperial Recruiter") S.alice settled) 1
    Spec.assertEqWith
      s
      "exactly the */* card and the 2-power creature are candidates"
      -- Named against `gs`, the state the library was built in: the found card
      -- gets a FRESH object id when it moves (CR 400.7), so its library
      -- incarnation names nothing in `settled`.
      (fmap (namesOf gs) searches)
      [Set.fromList (fmap Text.pack ["Primal Plasma", "Goblin Piker"])]
    Spec.assertEqWith
      s
      "the Primal Plasma named is the one that reached alice's hand"
      (namesOf settled (Game.zoneMembers Zone.Hand S.alice settled))
      (Set.singleton (Text.pack "Primal Plasma"))
    Spec.assertEqWith
      s
      "the library was shuffled once, after the found card left it"
      (fmap (namesOf gs) shuffles)
      [Set.fromList (fmap Text.pack ["Mountain", "Hill Giant", "Goblin Piker"])]

  -- The gameplay-level proof of Projection.characteristicPowerIn. CR 604.3 and
  -- CR 208.2a make a characteristic-defining power function in every zone, so
  -- Imperial Recruiter's "creature card with power 2 or less" has to weigh a
  -- Tarmogoyf in alice's library against the card types among all graveyards.
  -- Two types and three straddle the threshold, and the candidate set is the
  -- assertion for the group above's reason.
  Spec.it s "CR 208.2a Tarmogoyf is a search candidate at power 2, off a land and an instant in a graveyard" $ do
    candidates <- recruiterCandidates s registry ["Mountain", "Lightning Bolt"]
    Spec.assertEqWith s "the Tarmogoyf and the Piker" candidates [Set.fromList (fmap Text.pack ["Tarmogoyf", "Goblin Piker"])]

  Spec.it s "CR 208.2a Tarmogoyf is no search candidate at power 3, a sorcery added to the graveyard" $ do
    candidates <- recruiterCandidates s registry ["Mountain", "Lightning Bolt", "Divination"]
    Spec.assertEqWith s "the Piker alone" candidates [Set.singleton (Text.pack "Goblin Piker")]

  -- CR 208.2a's last sentence in its benign form: an empty graveyard is a
  -- determined 0, not a number that can't be determined, so the Tarmogoyf is a
  -- 0-power candidate rather than an absent one.
  Spec.it s "CR 208.2a Tarmogoyf is a power-0 search candidate with every graveyard empty" $ do
    candidates <- recruiterCandidates s registry []
    Spec.assertEqWith s "the Tarmogoyf and the Piker" candidates [Set.fromList (fmap Text.pack ["Tarmogoyf", "Goblin Piker"])]

  -- CR 604.3's "in all zones" as an equality: one Tarmogoyf on the battlefield
  -- and one in the library over the same graveyards, read through the two
  -- different paths -- applyCharacteristicPT and characteristicPowerIn -- must
  -- agree. Catches a substituteStar the wrong way round in either.
  Spec.it s "CR 604.3 the battlefield and off-battlefield readings of a CDA power agree" $ do
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    goyf <- S.printingOf s registry "Tarmogoyf"
    let (_, gs0) = S.addGraveyardCard mountain S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs1) = S.addGraveyardCard bolt S.alice gs0
        (onBattlefield, gs2) = S.addCreature goyf S.alice gs1
        (inLibrary, gs3) = S.addLibraryCard goyf S.alice gs2
        libraryPower = Game.faceOf inLibrary gs3 >>= Filter.power . Projection.viewOfCardIn gs3 inLibrary
    Spec.assertEqWith s "on the battlefield" (Projection.powerOf onBattlefield gs3) (Just 2)
    Spec.assertEqWith s "in the library" libraryPower (Just 2)

  -- CR 613.1 over a card in a LIBRARY, read from OUTSIDE the fold: the search
  -- filter. Rule 613.1 starts from the actual object and names no zone, so
  -- Maskwood Nexus's creature-card set (CR 613.1d, layer 4) reaches a card in a
  -- library, and CR 701.23a's "given description" has to be matched against that
  -- projection rather than against the printed card.
  --
  -- Three readings the pair separates. The Hill Giant is a printed Giant, so
  -- "the card was always a Goblin" would offer it on the Nexus-less board too.
  -- It is in the library before the Nexus arrives, so "the effect applied to it
  -- as it arrived" offers it on neither. Only an effect applying to a card
  -- sitting in a library offers it on exactly one. The Piker is a candidate on
  -- both boards, so the difference is not the prompt appearing or vanishing.
  Spec.it s "CR 613.1d Maskwood Nexus makes a library's Giant a Goblin, and Goblin Matron's search offers it" $ do
    candidates <- matronCandidates s registry True
    Spec.assertEqWith s "the printed Goblin and the Giant the Nexus made one" candidates [Set.fromList (fmap Text.pack ["Goblin Piker", "Hill Giant"])]

  -- The negative half of the pair, differing in exactly one thing: whether the
  -- Nexus is on the battlefield. Same library, same mana, same answers.
  Spec.it s "CR 701.23a without the Nexus, Goblin Matron's search offers the printed Goblin alone" $ do
    candidates <- matronCandidates s registry False
    Spec.assertEqWith s "the Piker alone" candidates [Set.singleton (Text.pack "Goblin Piker")]

  -- CR 613.1 over a card in a GRAVEYARD, read from inside the fold. Maskwood
  -- Nexus's third clause ("creature cards you own that aren't on the
  -- battlefield") is an Affected.MatchingAnywhere set, and Abomination of
  -- Llanowar's CR 208.2a characteristic-defining P/T counts "Elf cards in your
  -- graveyard" -- a count evaluated while layer 7a is being applied, so it reads
  -- Projection.viewUpTo rather than fullView. That reader used to match every
  -- off-battlefield candidate as a PRINTED card (#623), which read 1 here.
  --
  -- Three readings the pair separates. The two graveyard cards are printed
  -- Goblin Warriors, so "the cards were always Elves" reads 3 before the Nexus
  -- resolves as well as after. They are in the graveyard before the Nexus is
  -- cast, so "the effect applied to them as they arrived" reads 1 in both. Only
  -- a continuous effect applying to a card sitting in a graveyard reads 1 then
  -- 3. The battlefield half of the count is the Abomination itself, a printed
  -- Elf, in both halves -- so the change is not that half moving.
  Spec.it s "CR 613.1 Maskwood Nexus makes the creature cards in a graveyard Elves, and a CDA counts them there" $ do
    forest <- S.printingOf s registry "Forest"
    abomination <- S.printingOf s registry "Abomination of Llanowar"
    nexus <- S.printingOf s registry "Maskwood Nexus"
    piker <- S.printingOf s registry "Goblin Piker"
    let (before, after) = abominationAcrossNexus forest abomination nexus piker
    Spec.assertEqWith s "the Abomination alone, while the Nexus is still a spell" before (Just 1)
    Spec.assertEqWith s "the Abomination plus the two Goblins the Nexus made Elves" after (Just 3)

  -- The negative half of the pair above, differing in exactly one thing: what is
  -- in the graveyard. Maskwood Nexus's set is CREATURE cards, so two Lightning
  -- Bolts there are outside it and the count stays at the Abomination itself --
  -- which is what rules out "the Nexus resolving adds two to the count".
  Spec.it s "CR 613.1 the Nexus leaves the instants in that graveyard alone" $ do
    forest <- S.printingOf s registry "Forest"
    abomination <- S.printingOf s registry "Abomination of Llanowar"
    nexus <- S.printingOf s registry "Maskwood Nexus"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (before, after) = abominationAcrossNexus forest abomination nexus bolt
    Spec.assertEqWith s "the Abomination alone, before" before (Just 1)
    Spec.assertEqWith s "the Abomination alone, after" after (Just 1)

  -- The premise, asserted rather than assumed: CR 114.2 put one emblem in the
  -- command zone, and only the ultimate put it there.
  Spec.it s "CR 114.2 Elspeth's ultimate puts one emblem in the command zone" $ do
    (_, _, _, gs) <- elspethEmblemBoard s registry True
    (_, _, _, without) <- elspethEmblemBoard s registry False
    Spec.assertEqWith s "one emblem" (Set.size (GameState.command gs)) 1
    Spec.assertEqWith s "and none without the ultimate" (Set.size (GameState.command without)) 0

  Spec.it s "CR 114.4 the emblem's static ability reaches the layer fold from the command zone" $ do
    (mine, _, _, gs) <- elspethEmblemBoard s registry True
    Spec.assertEqWith s "the Piker is 2/1 -> 4/3, layer 7c" (Projection.powerOf mine gs, Projection.toughnessOf mine gs) (Just 4, Just 3)
    Spec.assertEqWith s "and has flying, layer 6" (Projection.hasKeyword Keyword.Flying mine gs) True

  -- The negative half of that pair: the same board with the ultimate not
  -- activated. Elspeth is still there, still at seven loyalty, so what changes
  -- is only whether the emblem exists.
  Spec.it s "CR 114.4 without the emblem the same Piker is its printed 2/1" $ do
    (mine, _, _, gs) <- elspethEmblemBoard s registry False
    Spec.assertEqWith s "printed size" (Projection.powerOf mine gs, Projection.toughnessOf mine gs) (Just 2, Just 1)
    Spec.assertEqWith s "and no flying" (Projection.hasKeyword Keyword.Flying mine gs) False

  Spec.it s "CR 114.2 the emblem's \"you\" is its controller, not every seat" $ do
    (_, theirs, carols, gs) <- elspethEmblemBoard s registry True
    Spec.assertEqWith s "bob's Hill Giant is untouched" (Projection.powerOf theirs gs, Projection.toughnessOf theirs gs) (Just 3, Just 3)
    Spec.assertEqWith s "carol's Piker is untouched" (Projection.powerOf carols gs, Projection.toughnessOf carols gs) (Just 2, Just 1)
    Spec.assertEqWith s "neither gains flying" (fmap (\oid -> Projection.hasKeyword Keyword.Flying oid gs) [theirs, carols]) [False, False]

  -- CR 114.5 makes the emblem neither a card nor a permanent, and CR 408.1 puts
  -- it in a zone whose objects cannot be destroyed -- so nothing that clears the
  -- battlefield reaches it, and a creature arriving afterwards is still buffed.
  Spec.it s "CR 408.1 the emblem survives a battlefield wipe and buffs a fresh creature" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    (_, _, _, gs) <- elspethEmblemBoard s registry True
    let wiped = gs {GameState.battlefield = mempty, GameState.objects = Map.filterWithKey (\oid _ -> Set.member oid (GameState.command gs)) (GameState.objects gs)}
        (fresh, afterFresh) = S.addCreature piker S.alice wiped
    Spec.assertEqWith s "the emblem is still in the command zone" (Set.size (GameState.command wiped)) 1
    Spec.assertEqWith s "and buffs the new creature" (Projection.powerOf fresh afterFresh, Projection.toughnessOf fresh afterFresh) (Just 4, Just 3)

  Spec.it s "CR 613.1 projectUpTo stops before the bound layer" $ do
    -- A layer-7c modification is invisible to a projection bounded at
    -- ModifyPT, and visible to an unbounded one. The bound is the whole
    -- termination argument for a projected count, so it gets its own test
    -- independent of any count.
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, gs1) = S.addCreature piker S.alice gs0
        gs = S.withEffect pikerId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))) gs1
        cands = Projection.gather gs
    Spec.assertEqWith
      s
      "unbounded sees the pump"
      (PC.power (Projection.projectFrom cands pikerId gs))
      (Just 5)
    Spec.assertEqWith
      s
      "bounded at 7c does not"
      (PC.power (Projection.projectUpTo Layer.ModifyPT cands pikerId gs))
      (Just 2)

  -- Built by hand (Pawl.CardSpec.splitCard: two Instant halves, Wax {G} and
  -- Wane {W}) and put on the stack directly (S.spellOnStack), with Object.face
  -- set by hand: this is about how the field is READ, and Pawl.CastSpec's
  -- CR 709.3a cases are what prove a cast writes it. The printed Wax // Wane
  -- takes the same reading through a real cast in Pawl.CastSpec's WaxWane
  -- group; here the point is to reach the read with nothing else moving.
  Spec.it s "CR 709.3b a split spell on the stack has only the half being cast" $ do
    let wax = CardName.MkCardName (Text.pack "Wax")
        card = CardSpec.splitCard
        (oid, gs0) = S.spellOnStack (Printing.MkPrinting card) S.alice (Setup.emptyGame S.bothPlayers)
        showing n g = g {GameState.objects = Map.adjust (\o -> o {Object.face = Just n}) oid (GameState.objects g)}
    -- CR 709.3b: "While on the stack, only the characteristics of the half being
    -- cast exist. The other half's characteristics are treated as though they
    -- didn't exist." Green alone, NOT the green-and-white CR 709.4 gives the
    -- card in every other zone.
    Spec.assertEqWith s "green alone" (Projection.colorsOf oid (showing wax gs0)) (Set.singleton Color.Green)
    -- Falsifier: an engine that always answered with the combined view would
    -- report both colours here and pass every other case in this task.
    Spec.assertEqWith s "both colours with no face shown" (Projection.colorsOf oid gs0) (Set.fromList [Color.Green, Color.White])

  keywordCounterSpec s registry
  supertypeSpec s registry

-- CR 205.4b / 613.1d layer 4, through a whole card: Arcum's Weathervane
-- ({2} Artifact, "{2}, {T}: Target snow land is no longer snow." / "{2}, {T}:
-- Target nonsnow basic land becomes snow.", checked against Scryfall
-- 2026-08-08) is the pool's printed REMOVAL of a supertype, and the grant
-- beside it. Both abilities are Indefinite, which is CR 611.2a's own reading of
-- text that states no duration: "If no duration is stated, it lasts until the
-- end of the game."
--
-- The rule these prove is CR 205.4b's last sentence: "When an object gains or
-- loses a supertype, it retains any other supertypes it had." A Snow-Covered
-- Mountain is basic AND snow, so removing snow from it leaves basic behind, and
-- an ordinary Mountain gaining snow keeps basic too. Asserting the whole
-- supertype SET rather than one membership is what makes that observable.
--
-- Leyline of Singularity's grant is proved at gameplay level in
-- Pawl.DamageSpec's legend rule group instead, since CR 704.5j is what makes a
-- gained supertype visible on the board.
supertypeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
supertypeSpec s registry = Spec.describe s "Supertype" $ do
  Spec.it s "CR 205.4b Arcum's Weathervane takes snow off a Snow-Covered Mountain and leaves basic" $ do
    (before, after) <- weathervaneChain s registry "Snow-Covered Mountain" 0
    Spec.assertEqWith s "it starts basic and snow" before (Set.fromList [Supertype.Basic, Supertype.Snow])
    Spec.assertEqWith s "and ends basic alone" after (Set.singleton Supertype.Basic)

  Spec.it s "CR 205.4b Arcum's Weathervane makes a plain Mountain snow and leaves basic" $ do
    (before, after) <- weathervaneChain s registry "Mountain" 1
    Spec.assertEqWith s "it starts basic alone" before (Set.singleton Supertype.Basic)
    Spec.assertEqWith s "and ends basic and snow" after (Set.fromList [Supertype.Basic, Supertype.Snow])

-- Put the named land and an Arcum's Weathervane onto alice's battlefield with
-- enough Forests to pay the {2}, activate the Weathervane's ability at `which`
-- aimed at that land, resolve it, and report the land's supertypes before and
-- after. The answerer names the land outright rather than relying on which
-- object id it drew, so the two Forests standing in for mana cannot be aimed at
-- by accident.
weathervaneChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> Int -> m (Set.Set Supertype.Supertype, Set.Set Supertype.Supertype)
weathervaneChain s registry landName which = do
  land <- S.printingOf s registry landName
  forest <- S.printingOf s registry "Forest"
  weathervane <- S.printingOf s registry "Arcum's Weathervane"
  let (landId, g0) = S.addCreature land S.alice (Setup.emptyGame S.bothPlayers)
      (_, g1) = S.addCreature forest S.alice g0
      (_, g2) = S.addCreature forest S.alice g1
      (vaneId, board) = S.addCreature weathervane S.alice g2
      before = Projection.supertypesOf landId board
  case drop which (Projection.abilitiesOf vaneId board) of
    [] -> do
      Spec.assertFailure s "expected the Weathervane to project both of its activated abilities"
      pure (before, before)
    ability : _ ->
      let after = S.runPure (aimingAt landId) board (do Activate.activateAbility S.alice vaneId ability; Stack.resolveTop)
       in pure (before, Projection.supertypesOf landId after)

-- Answers every target choice with `oid`, and everything else as the identity
-- interpreter does.
aimingAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- CR 122.1b: "A keyword counter on a permanent ... causes that object to gain
-- that keyword", and CR 613.1f puts that grant in LAYER 6 -- not the layer 7c
-- where CR 122.1a's +1/+1 counters land.
keywordCounterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
keywordCounterSpec s registry = Spec.describe s "KeywordCounter" $ do
  Spec.it s "CR 122.1b a flying counter grants flying; without one there is none" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        flying = S.addCounter (CounterKind.Keyword Keyword.Flying) 1 pikerId board
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying pikerId board)) "a bare Piker does not fly"
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying pikerId flying) "with the counter it does"

  -- The grant is layer 6, so it must NOT disturb layer 7c. A keyword counter
  -- adds no power or toughness, which is what tells it apart from the +1/+1
  -- counter the same Map holds.
  Spec.it s "CR 613.1f the grant is layer 6, so it changes no power or toughness" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        flying = S.addCounter (CounterKind.Keyword Keyword.Flying) 1 pikerId board
    Spec.assertEqWith s "power unchanged" (Projection.powerOf pikerId flying) (Projection.powerOf pikerId board)
    Spec.assertEqWith s "toughness unchanged" (Projection.toughnessOf pikerId flying) (Projection.toughnessOf pikerId board)

  -- CR 702.164b's counting, reached through counters: the layer-6 arm counts
  -- INSTANCES (two grants are two abilities, not one absorbed into the other),
  -- so two counters must arrive as two grants.
  Spec.it s "CR 122.1b two counters grant two instances, not one" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        one = S.addCounter (CounterKind.Keyword Keyword.Flying) 1 pikerId board
        two = S.addCounter (CounterKind.Keyword Keyword.Flying) 2 pikerId board
    Spec.assertEqWith s "one counter, one instance" (Map.lookup Keyword.Flying (Projection.keywordsOf pikerId one)) (Just 1)
    Spec.assertEqWith s "two counters, two instances" (Map.lookup Keyword.Flying (Projection.keywordsOf pikerId two)) (Just 2)

  Spec.it s "CR 122.1b a counter of a DIFFERENT keyword grants that one, not flying" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        hasted = S.addCounter (CounterKind.Keyword Keyword.Haste) 1 pikerId board
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste pikerId hasted) "haste granted"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying pikerId hasted)) "flying is not"
