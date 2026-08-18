-- | CR 702.179, "start your engines!": speed as a per-player resource, the
-- state-based action that starts it (CR 704.5aa) and the inherent triggered
-- ability that raises it (CR 702.179d).
--
-- Pawl.Engine.Monarch's sibling, and for the same reason: rule 702.179 gives a
-- player an ability that rides no card's text, so the rules core mints it. Casing
-- on Keyword.StartYourEngines here is casing on the RULEBOOK, which
-- Pawl.Types.Keyword's own comment licenses -- rule 702 is as much a part of the
-- comprehensive rules as rule 704 is. Nothing here asks which EFFECT anything
-- came from, which is the invariant that matters.
--
-- Where it DIVERGES from the monarch is what the designation is on. CR 725.1
-- makes the monarch "a designation a player can have", at most one per game, so
-- it is a GameState field; CR 702.179b makes speed a value EACH player has or
-- lacks, so it rides Player.speed, as Player.ringTemptations does for CR 701.54c.
--
-- Rule 702.178's max speed is NOT here. CR 702.178a is a static ability -- "as
-- long as your speed is 4, this object has '[Ability]'" -- so it is card data and
-- not rules-core machinery: it rides Pawl.Types.ActivatedAbility.condition, read
-- back by Pawl.Engine.Projection.abilitiesGiven on the battlefield and by
-- Pawl.Engine.Activate.zoneAbilitiesOf where CR 702.178b's zone clause sends
-- it. All this module owes it is the number the clause compares against, and
-- Pawl.Engine.Quantity's Speed arm.
module Pawl.Engine.Speed where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Effect as Effect
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Optionality as Optionality
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggerSource as TriggerSource
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 702.179e: "a player has max speed if their speed is 4". The one number
-- rule 702.179 names, and the one CR 702.178a's "as long as your speed is 4"
-- compares against, so both readings share it.
maxSpeed :: Natural
maxSpeed = 4

-- | CR 702.179f: a player's speed as an EFFECT sees it -- 0 for a player who has
-- none. Pawl.Engine.Quantity's Speed arm is the card-facing reader; this is the
-- same reading for the rules core.
--
-- Nothing only for a player the game does not hold, which is "which player?"
-- unanswered rather than a player with no speed.
speedOf :: PlayerId -> GameState -> Maybe Natural
speedOf pid gs = fmap (Maybe.fromMaybe 0 . Player.speed) (Map.lookup pid (GameState.players gs))

-- | CR 704.5aa: the players who control a permanent with start your engines! and
-- have no speed. The state-based action's CLASSIFIER half, kept pure and taking
-- the pre-pass projection so Pawl.Engine.Sba can judge it against the same board
-- as every other CR 704.5 clause (CR 704.3's simultaneity).
--
-- Membership, not a count: CR 704.5aa asks whether a player controls "a permanent
-- with start your engines!", so a second copy starts no second set of engines.
--
-- The PROJECTED keywords, never the printed ones, because the layer system grants
-- and removes abilities -- a Muraganda Raceway whose rules text CR 305.7 stripped
-- has no start your engines! to read, CR 613.1f says the same of a creature, and
-- an effect that GRANTED the keyword would be found here.
--
-- Ascending, so the pass's writes and any transcript are deterministic.
startingEngines :: Map ObjectId PC.ProjectedCharacteristics -> GameState -> [PlayerId]
startingEngines pcs gs =
  let revs oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc ->
          if Map.member Keyword.StartYourEngines (PC.keywords pc)
            then Projection.controllerOf oid gs
            else Nothing
      revvers = Set.fromList (Maybe.mapMaybe revs (Set.toList (GameState.battlefield gs)))
      unstarted pid = fmap Player.speed (Map.lookup pid (GameState.players gs)) == Just Nothing
   in filter unstarted (Set.toAscList revvers)

-- | CR 704.5aa's ACTION half: "that player's speed becomes 1".
--
-- A set and not an increase, so it cannot stack with CR 702.179d's rise -- and
-- startingEngines above has already established the player had none, which is
-- what makes the two agree.
startEngines :: PlayerId -> GameState -> GameState
startEngines pid gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.speed = Just 1}) pid (GameState.players gs)}

-- | CR 702.179d, in full: "Whenever one or more opponents lose life during your
-- turn, if your speed is less than 4, your speed increases by 1. This ability
-- triggers only once each turn."
--
-- Minted here rather than carried in card data, on Pawl.Engine.Monarch's terms:
-- the text is printed in the comprehensive rules, not on Muraganda Raceway.
--
-- The intervening "if" is REAL, unlike either monarch ability's: CR 603.4 checks
-- it when the trigger event occurs, which inherentPending does below, and CR
-- 608.2a checks it again on resolution, which Pawl.Engine.Stack's
-- OfInherentTrigger arm does. Both halves are needed -- an opponent losing life
-- twice in one turn cannot raise speed past 4, and neither can a trigger that
-- waited on the stack while something else did.
--
-- Single mode, no targets, forced (CR 603.3's "you" is the ability's controller
-- and nothing is chosen), which is what lets Monarch.placeInherent put it on the
-- stack unprompted.
increaseAbility :: TriggeredAbility Card
increaseAbility =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.OpponentLostLifeDuringYourTurn,
      TriggeredAbility.modal =
        Modal.MkModal
          ( Seq.singleton
              ( Mode.MkMode
                  (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))))))
                  Map.empty
              )
          )
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Just belowMaxSpeed,
      -- CR 702.179d's own "this ability triggers only once each turn", stated in
      -- the data because the rule states it. It is NOT what enforces the limit
      -- here: Engine.withinTurnLimit reads the CR 603.3b log, and the record it
      -- reads is the one a SOURCELESS trigger does not get (#1026), so
      -- `alreadyTriggered` below is still the enforcement. The two agree.
      TriggeredAbility.limit = TriggerLimit.OncePerTurn
    }

-- | CR 702.179d's "if your speed is less than 4", as the Condition both checks
-- read. AtMost (maxSpeed - 1) rather than a "less than" comparison, because
-- Pawl.Types.Comparison has no strict arm and speed is a whole number, so the two
-- state the same set -- this is the pool's first producer for AtMost (#158).
belowMaxSpeed :: Condition.Condition
belowMaxSpeed =
  Condition.Compares
    ( Compares.MkCompares
        (Quantity.Speed (PlayerRef.Relative PlayerRelation.You))
        Comparison.AtMost
        (Quantity.Literal (toInteger maxSpeed - 1))
    )

-- | CR 702.179d: the inherent trigger this batch of events fires, if any, as an
-- ordinary PendingTrigger whose source is TriggerSource.Sourceless -- what lets
-- Engine.placePendingTriggers merge it into the one batch CR 603.3b orders, and
-- Pawl.Engine.Monarch.placeInherent put it on the stack.
--
-- AT MOST ONE, for two separate reasons that must not be confused. "One or more
-- opponents lose life" makes a whole batch of simultaneous losses a SINGLE
-- occurrence, which is why this scans the batch rather than mapping over it; and
-- "this ability triggers only once each turn" is the per-turn limit, which
-- GameState.speedIncreasedThisTurn carries and Engine.placePendingTriggers marks
-- -- see `increaseAbility`'s TriggerLimit for why the generic reader does not
-- reach a sourceless trigger.
--
-- Only the ACTIVE player's ability can fire, which is CR 702.179d's "during your
-- turn" and not a shortcut. Only a player with 1 or more speed HAS the ability at
-- all -- the rule hangs it off exactly that -- so a player CR 704.5aa has not yet
-- reached is asked nothing.
inherentPending :: [GameEvent] -> GameState -> [PendingTrigger]
inherentPending events gs =
  let you = GameState.activePlayer gs
      opponents = Set.fromList (filter (/= you) (Game.stillPlaying gs))
      -- A PARTIAL case with a wildcard, Pawl.Engine.Monarch.inherentMatch's
      -- posture and not an oversight: this matcher answers about one event shape,
      -- and a new GameEvent constructor is not an event rule 702.179d names.
      lostLife event = case event of
        GameEvent.LifeLost (LifeChange.MkLifeChange pid _) -> Set.member pid opponents
        _ -> False
      hasSpeed = case Map.lookup you (GameState.players gs) of
        Just player -> Maybe.maybe False (>= 1) (Player.speed player)
        Nothing -> False
      -- CR 603.4: the intervening "if" is checked here, as the event occurs. A
      -- player already at max speed does not trigger at all, so the turn's one
      -- trigger is still theirs to spend -- which Spikeshell Harrier's reduction
      -- (Effect.DecreaseSpeed) can now make observable, a player dropped back
      -- below 4 having spent no trigger.
      below = Maybe.maybe False (< maxSpeed) (speedOf you gs)
      alreadyTriggered = Set.member you (GameState.speedIncreasedThisTurn gs)
   in [ PendingTrigger.MkPendingTrigger TriggerSource.Sourceless you increaseAbility Map.empty
      | hasSpeed,
        below,
        not alreadyTriggered,
        List.any lostLife events
      ]
