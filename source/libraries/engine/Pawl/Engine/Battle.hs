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
-- NOT here, and all of it still #302: CR 310.5's attackable battle, CR 310.6's
-- damage removing defense counters, CR 704.5v's defense-0 state-based action, and
-- CR 310.11b's "when the last defense counter is removed, exile it, then you may
-- cast it transformed". The last three are only jointly observable -- a battle
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
-- Siege is the whole list because CR 205.3q's is: rule 310.11 says so outright
-- ("all currently existing battles have the subtype Siege"). A second battle type
-- is a rulebook change, and lands here.
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
-- `playing` is CR 102.1's players still in the game, which the caller supplies as
-- Game.stillPlaying -- CR 704.5w asks for "no player IN THE GAME designated as its
-- protector", so a departed protector is not a candidate and not a legal one to
-- keep. The controller is filtered against it too, so a battle whose controller
-- has left offers nothing rather than offering a ghost.
--
-- Order is the caller's seating order, and the head is what an interpreter that
-- declines to answer gets (Replay.defaultAnswer). Nothing here is a choice the
-- engine makes: an empty list means the rules leave no legal protector, which CR
-- 704.5w and CR 704.5x both answer by putting the battle into its owner's
-- graveyard.
protectorCandidates ::
  PC.ProjectedCharacteristics ->
  PlayerId.PlayerId ->
  [PlayerId.PlayerId] ->
  [PlayerId.PlayerId]
protectorCandidates pc controller playing
  | Set.null (battleTypes pc) = filter (== controller) playing
  | otherwise = filter (/= controller) playing

-- CR 704.5w / 704.5x: does this battle's protector designation need repairing?
--
-- ONE predicate for two state-based actions, because the two rules name the same
-- condition from opposite ends and prescribe the same remedy. CR 704.5w fires when
-- the battle has "no player in the game designated as its protector"; CR 704.5x
-- fires when "a Siege's controller is also its designated protector". Both are
-- exactly "the designated player is not among protectorCandidates", and both then
-- have the controller choose an appropriate player, or put the battle into its
-- owner's graveyard if none can be chosen.
--
-- NOT IMPLEMENTED: CR 704.5w's "and no attacking creatures are currently attacking
-- that battle" rider, which would suspend the re-choice for the duration of an
-- attack. Nothing can attack a battle, so the rider is vacuously true at every
-- call (#853). CR 704.5x has no such rider, so that half is exact -- but no card
-- can reach it either (#853).
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
