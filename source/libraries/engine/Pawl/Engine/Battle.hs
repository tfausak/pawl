-- | CR 310, battles: the defense a battle enters with (CR 310.4b), and the
-- protector designated as it enters (CR 310.8a / 310.11a) together with the
-- state-based actions that repair that designation (CR 704.5w / 704.5x).
--
-- Pawl.Engine.Saga's sibling, and kept apart from Pawl.Engine.Sba for the reason
-- that module gives: Sba owns WHEN a state-based action is checked, not what any
-- one of them means.
--
-- Casing on Subtype.Siege here is casing on the RULEBOOK, exactly as
-- Pawl.Engine.Saga cases on Subtype.Saga: rule 310.11 is as much a part of the
-- comprehensive rules as rule 704. Nothing here asks which EFFECT a battle's text
-- carries -- only which battle type its type line prints.
--
-- Imports no Pawl.Engine.Projection, deliberately, for Saga's reason: CR 310.4b's
-- and CR 310.8a's intrinsic abilities are minted by
-- Projection.intrinsicReplacementsOf from this module, so a dependency the other
-- way would be a cycle. Callers pass the projection in.
--
-- NOT here, and all of it still #302. What a protector is FOR is the larger half:
-- CR 310.8b (its protector can never attack it, and a Siege can be attacked by its
-- own controller), CR 310.8c (only its protector may block those attackers) and CR
-- 310.8d with CR 508.5 (while it is attacked, the protector SUBSTITUTES for the
-- defending player in every rule and effect) all need CR 310.5's attackable battle
-- first, and AttackTarget has no OfBattle arm. Beside them: CR 310.6's damage
-- removing defense counters, CR 310.7 / 704.5v's defense-0 state-based action, and
-- CR 310.11b's "when the last defense counter is removed, exile it, then you may
-- cast it transformed". Those last three are only jointly observable -- a battle
-- driven to defense 0 belongs in EXILE via CR 310.11b, not in the graveyard CR
-- 704.5v alone would send it to -- so none of them is built.
module Pawl.Engine.Battle where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Game (Game)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Subtype as Subtype

-- CR 310: is this object a battle? Read off the PROJECTED card types, so a
-- permanent that became one is judged as one and a paper-only one is not -- the
-- posture Projection.intrinsicReplacementsOf takes for CR 306.5b.
isBattle :: PC.ProjectedCharacteristics -> Bool
isBattle = Set.member CardType.Battle . PC.cardTypes

-- CR 205.3q: the battle types this battle has, which is not the same question as
-- which subtypes it has -- a permanent that is a battle and a creature has
-- creature types too, and CR 310.8a asks only about the battle ones.
--
-- Siege is the whole list because CR 205.3q's list of battle types is ("that
-- battle type is Siege"); CR 310.11 says the different and weaker thing that every
-- battle currently existing HAS that subtype. A second battle type is a rulebook
-- change to 205.3q, and lands here.
battleTypes :: PC.ProjectedCharacteristics -> Set.Set Subtype.Subtype
battleTypes = Set.intersection (Set.singleton Subtype.Siege) . PC.subtypes

-- CR 310.8a: which players may be chosen as this battle's protector, "determined
-- by its battle type (see rule 310.11)".
--
-- Two clauses, and the fallback is the rule's own: a battle with NO battle types
-- has its controller become its protector (CR 310.8a's last sentence), and a Siege
-- must take an opponent of its controller (CR 310.11a, "only an opponent of a
-- Siege's controller can be its protector").
--
-- `playing` is the players still in the game, which the caller supplies as
-- Game.stillPlaying -- CR 704.5w asks for "no player IN THE GAME designated as its
-- protector", so a departed protector is not a candidate and not a legal one to
-- keep. The controller is filtered against it too, so a battle whose controller
-- has left offers nothing rather than offering a ghost.
--
-- Order is the caller's seating order, and the head is what an interpreter that
-- declines to answer gets (Replay.defaultAnswer). Nothing here is a choice the
-- engine makes: an empty list means the rules leave no legal protector, which CR
-- 310.10, CR 704.5w and CR 704.5x all answer by putting the battle into its
-- owner's graveyard.
protectorCandidates ::
  PC.ProjectedCharacteristics ->
  PlayerId.PlayerId ->
  [PlayerId.PlayerId] ->
  [PlayerId.PlayerId]
protectorCandidates pc controller playing
  | Set.null (battleTypes pc) = filter (== controller) playing
  | otherwise = filter (/= controller) playing

-- CR 310.10: does this battle's protector designation need repairing?
--
-- ONE predicate, and rule 310.10 is why it can be one: "if a battle that isn't
-- being attacked has no player designated as its protector, OR ITS PROTECTOR IS A
-- PLAYER WHO CAN'T BE ITS PROTECTOR BASED ON ITS BATTLE TYPE, its controller
-- chooses an appropriate player to be its protector. If no player can be chosen
-- this way, the battle is put into its owner's graveyard." That is exactly "the
-- designated player is not among protectorCandidates".
--
-- CR 704.5w and CR 704.5x are the same condition split across rule 704's list --
-- 704.5w takes the no-player-designated half and 704.5x the Siege-controller case
-- of the second half -- so they are what Pawl.Engine.Sba cites at the call, and
-- rule 310.10 is what this module states. Between them 310.10 is the wider one:
-- its second clause covers an illegal protector who is neither absent nor the
-- controller, which 704.5x does not name and which this predicate answers.
--
-- NOT IMPLEMENTED: the "isn't being attacked" rider CR 310.10 and CR 704.5w share,
-- which would suspend the re-choice for the duration of an attack. Nothing can
-- attack a battle, so the rider is vacuously true at every call (#853). CR 704.5x
-- has no such rider, so that half is exact -- but no card can reach it (#853).
needsProtector ::
  PC.ProjectedCharacteristics ->
  PlayerId.PlayerId ->
  [PlayerId.PlayerId] ->
  Maybe PlayerId.PlayerId ->
  Bool
needsProtector pc controller playing designated = case designated of
  Nothing -> True
  Just pid -> notElem pid (protectorCandidates pc controller playing)

-- CR 310.8a: ask the battle's controller who protects it, and answer with their
-- pick. Nothing means the rules offer no legal protector, which both callers
-- answer by putting the battle into its owner's graveyard (CR 704.5w, CR 704.5x).
--
-- The one place the question is asked, shared by CR 310.8a's as-enters route
-- (Pawl.Engine.Event's EntryRewrite.ChooseProtector arm) and CR 704.5w/704.5x's
-- state-based re-choice (Pawl.Engine.Sba). Sharing it is what keeps the candidate
-- rule in one place: a re-choice must offer exactly what the entry choice offered.
--
-- Elided at one candidate, and the ANSWER is still the same: one candidate is one
-- outcome, so the options are indistinguishable and the engine decides nothing by
-- not asking. See Prompt.ChooseProtector.
--
-- Filters rather than trusts the answer, the posture Combat.chooseDefender and
-- Sba.chooseLegendVictims both take: an interpreter that names a player who is not
-- a candidate gets the head of the list instead of an illegal designation.
designateProtector ::
  PC.ProjectedCharacteristics ->
  PlayerId.PlayerId ->
  ObjectId.ObjectId ->
  Game (Maybe PlayerId.PlayerId)
designateProtector pc controller oid = do
  gs <- State.get
  case NonEmpty.nonEmpty (protectorCandidates pc controller (Game.stillPlaying gs)) of
    Nothing -> pure Nothing
    Just candidates
      | null (NonEmpty.tail candidates) -> pure (Just (NonEmpty.head candidates))
      | otherwise -> do
          let decider = Decide.deciderFor controller gs
          answer <- Game.choose (Prompt.ChooseProtector decider controller oid candidates)
          pure . Just $
            if List.elem answer (NonEmpty.toList candidates)
              then answer
              else NonEmpty.head candidates
