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
-- Imports no Pawl.Engine.Projection: every function here takes the projection it
-- needs as an argument, so a caller that has already projected does not pay for a
-- second one. NOT Saga's reason -- rule 714's module is imported BY Projection,
-- which calls Saga.entryReplacementsOf, so a dependency the other way really would
-- be a cycle there. Nothing in Projection calls this module: CR 310.4b's clause in
-- intrinsicReplacementsOf tests `Set.member CardType.Battle` inline, exactly as CR
-- 306.5b's planeswalker clause beside it tests its own card type.
--
-- What a protector is FOR lives in Pawl.Engine.Combat, not here: CR 310.5's
-- attackable battle (Combat.attackableBattles), CR 310.8b's "any attacking player
-- for whom its protector is a defending player", and CR 310.8d with CR 508.5
-- (Combat.defendingPlayerOf). This module owns the designation; that one owns what
-- reads it.
--
-- NOT here and not anywhere, and #897: CR 310.6's damage removing defense
-- counters, CR 310.7 / 704.5v's defense-0 state-based action, and CR 310.11b's
-- "when the last defense counter is removed, exile it, then you may cast it
-- transformed". Those three are only jointly observable -- a battle driven to
-- defense 0 belongs in EXILE via CR 310.11b, not in the graveyard CR 704.5v alone
-- would send it to -- so none of them is built.
module Pawl.Engine.Battle where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
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

-- CR 310.8 / 310.8e: the player designated as this battle's protector, or Nothing
-- when nobody is -- which CR 310.10 makes a state-based action rather than a
-- steady state, so a Nothing here is a board mid-repair or an object that is no
-- battle at all.
--
-- STORED and read live rather than derived, for the reason Object.protector gives:
-- CR 310.8a's choice is made once, as the battle enters, and CR 310.8f is what
-- moves it afterwards. Every reader asks it fresh, so a designation that moves
-- while an attack stands moves the defending player with it (CR 310.8d).
protectorOf :: ObjectId.ObjectId -> GameState -> Maybe PlayerId.PlayerId
protectorOf oid gs = Object.protector =<< Game.lookupObject oid gs

-- CR 310.5 / CR 704.5w: is any attacking creature currently attacking this battle?
--
-- The LIVE record and not CR 508.8's historical Combat.attacked, because 704.5w
-- asks "currently": a creature removed from combat (CR 506.4) is deleted from this
-- map and stops attacking the battle, while the historical set keeps the entry
-- forever. That difference is the whole content of the rider.
--
-- Reads Pawl.Types.Combat directly rather than going through Pawl.Engine.Combat,
-- which imports THIS module for CR 310.5's attackable battles.
isBeingAttacked :: ObjectId.ObjectId -> GameState -> Bool
isBeingAttacked oid gs =
  List.elem (AttackTarget.OfBattle oid) (Map.elems (Combat.attackers (GameState.combat gs)))

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

-- CR 704.5w / CR 704.5x: does this battle's protector designation need repairing?
--
-- CR 310.10 states both as one sentence -- "if a battle that isn't being attacked
-- has no player designated as its protector, OR ITS PROTECTOR IS A PLAYER WHO
-- CAN'T BE ITS PROTECTOR BASED ON ITS BATTLE TYPE, its controller chooses an
-- appropriate player to be its protector" -- and rule 704 splits them, which is
-- what forces the two arms below rather than one "the designated player is not
-- among protectorCandidates".
--
-- The split is the RIDER, and the two rules put it in different places. CR 704.5w
-- says "no player IN THE GAME designated as its protector AND no attacking
-- creatures are currently attacking that battle"; CR 704.5x, the Siege whose
-- controller is its own protector, carries no rider at all. Rule 704 governs where
-- it disagrees with 310.10's shorter statement, since 310.10's own last sentence
-- defers to it ("This is a state-based action (see rule 704)"). The disagreement
-- is unreachable besides: 704.5x needs a control-change effect that can name a
-- battle, and no card in the pool has one (#853).
--
-- `attacked` is Battle.isBeingAttacked at the call. The rider suspends the
-- re-choice rather than cancelling it: Gatherer's ruling is that the controller
-- chooses a new protector once no creatures are attacking it, and that meanwhile
-- the battle "continues to be attacked and can be dealt combat damage as normal".
-- Suspension is exactly what a state-based action re-asked every check does.
--
-- Between them CR 310.10 is still the wider condition, and that width lands in the
-- second arm: an illegal protector who is neither absent nor the controller is
-- named by neither 704 rule, and protectorCandidates answers it.
needsProtector ::
  PC.ProjectedCharacteristics ->
  PlayerId.PlayerId ->
  [PlayerId.PlayerId] ->
  Bool ->
  Maybe PlayerId.PlayerId ->
  Bool
needsProtector pc controller playing attacked designated = case designated of
  -- CR 704.5w, both conjuncts.
  Nothing -> not attacked
  Just pid
    -- CR 704.5w again: a designated player who has LEFT is no player in the game
    -- designated as its protector, so the rider covers this too.
    | notElem pid playing -> not attacked
    -- CR 704.5x, and CR 310.10's second clause. No rider.
    | otherwise -> notElem pid (protectorCandidates pc controller playing)

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
