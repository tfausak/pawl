-- | CR 728, "Rad Counters": the inherent triggered ability rule 728.1 writes out
-- in full, and nothing else -- the counters themselves are
-- PlayerCounterKind.Rad on Player.counters, where every other player counter
-- lives.
--
-- Pawl.Engine.Monarch's and Pawl.Engine.Speed's sibling, and for their reason:
-- rule 728.1's ability "has no source and is controlled by the active player",
-- so it rides no card's text and the rules core mints it. Nothing here asks
-- which EFFECT anything came from; it BUILDS effects, which is the direction the
-- closed/open split allows -- Pawl.Engine.Monarch's end-step draw is the same
-- act.
--
-- Why the ability is composed of ordinary opcodes rather than being one
-- rad-shaped opcode: every clause rule 728.1 states is ordinary Magic
-- vocabulary that printed cards also ask for -- a mill that counts the nonland
-- cards it milled (The Wise Mothman), a life loss, and a removal of player
-- counters (Survivor's Med Kit's "target player loses all rad counters"). The
-- one-opcode spelling would be Effect.TemptWithTheRing's shape, which is right
-- for a KEYWORD ACTION (CR 701.54) and wrong here, rule 728.1 naming no action
-- of its own.
--
-- CR 728.1a's "life loss from radiation" -- the thing Strong, the Brutish
-- Thespian reads -- is NOT here: no card in the pool asks which life loss came
-- from this ability (#856).
module Pawl.Engine.Rad where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.Types.Card (Card)
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Optionality as Optionality
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerSource as TriggerSource
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope

-- | CR 122.1: how many rad counters a player has, as the rules core reads it.
-- Pawl.Engine.Quantity's PlayerCounters arm is the card-facing reader; this is
-- the same reading, and Player.counters' absent-means-zero convention is what
-- makes a player who has never seen a rad counter answer 0 rather than nothing.
--
-- Nothing only for a player the game does not hold.
radCountersOf :: PlayerId -> GameState -> Maybe Natural
radCountersOf pid gs =
  fmap
    (Map.findWithDefault 0 PlayerCounterKind.Rad . Player.counters)
    (Map.lookup pid (GameState.players gs))

-- | The slot rule 728.1's mill binds its count into, for the two clauses after
-- it to read back as Quantity.InSlot -- "for each nonland card milled this way".
--
-- An ordinary slot name, not a reserved one: it is written mid-resolution by
-- Resolve's Mill arm onto the effect's own source, which is exactly where Bane
-- of Progress' "destroyed this way" is written. No card can collide with it,
-- this ability being the only thing that ever reads it.
milledSlot :: SlotName.SlotName
milledSlot = SlotName.MkSlotName (Text.pack "milledThisWay")

-- | CR 728.1's "nonland card". A card type question, so the tally is judged
-- against the printed card (Resolve's Mill arm), which is what a card in a
-- library has.
nonland :: Filter.Filter Keyword.Keyword
nonland = Filter.Not (Filter.HasCardType CardType.Land)

-- | CR 728.1's intervening "if": "if that player has one or more rad counters".
-- "That player" is the one whose precombat main phase began, who is this
-- ability's controller, so PlayerRelation.You resolves it.
hasRadCounters :: Condition.Condition
hasRadCounters =
  Condition.Compares
    (Quantity.PlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Rad)
    Comparison.AtLeast
    (Quantity.Literal 1)

-- | CR 728.1, in full: "At the beginning of each player's precombat main phase,
-- if that player has one or more rad counters, that player mills a number of
-- cards equal to the number of rad counters they have. For each nonland card
-- milled this way, that player loses 1 life and removes one rad counter from
-- themselves."
--
-- Three effects in ONE mode, in the order the rule states them, because the
-- second and third read what the first counted. CR 608.2c applies the
-- instructions in order, which is what makes the binding readable at all.
--
-- The intervening "if" is REAL, as CR 702.179d's is: CR 603.4 checks it when the
-- trigger event occurs (inherentPending below) and CR 608.2a again on resolution
-- (Pawl.Engine.Stack's OfInherentTrigger arm). Both halves matter here in a way
-- they do not for speed -- this ability REMOVES the counters it fires on, so a
-- second instance that somehow waited behind the first must find none and do
-- nothing.
--
-- Single mode, no targets, mandatory: rule 728.1 fixes the whole text and
-- chooses nothing, which is what lets Monarch.placeInherent put it on the stack
-- unprompted.
ability :: TriggeredAbility Card
ability =
  TriggeredAbility.MkTriggeredAbility
    { -- CR 500.1 / 505.1a: a turn has exactly one precombat main phase and it is
      -- the active player's, so ControllersTurn plus the active player as this
      -- ability's controller IS rule 728.1's "each player's precombat main
      -- phase" -- the rule quantifies over turns, not over the players of one.
      TriggeredAbility.condition = TriggerCondition.StepBegins Phase.PrecombatMain TurnScope.ControllersTurn,
      TriggeredAbility.modal =
        Modal.MkModal
          ( Seq.singleton
              ( Mode.MkMode
                  ( Seq.singleton . Clause.MkClause Nothing Optionality.Mandatory Nothing . Seq.fromList $
                      [ -- "that player mills a number of cards equal to the
                        -- number of rad counters they have", counting the
                        -- nonland cards it milled.
                        Effect.Mill
                          (PlayerRef.Relative PlayerRelation.You)
                          (Quantity.PlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Rad)
                          (Just (MillTally.MkMillTally {MillTally.slot = milledSlot, MillTally.filter = nonland})),
                        -- "for each nonland card milled this way, that player
                        -- loses 1 life" -- one life per card, which is the count
                        -- itself.
                        Effect.LoseLife (PlayerRef.Relative PlayerRelation.You) (Quantity.InSlot milledSlot),
                        -- "and removes one rad counter from themselves",
                        -- likewise once per card.
                        Effect.RemovePlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Rad (Quantity.InSlot milledSlot)
                      ]
                  )
                  Map.empty
              )
          )
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Just hasRadCounters
    }

-- | CR 728.1: the inherent trigger this batch of events fires, if any, as an
-- ordinary PendingTrigger whose source is TriggerSource.Sourceless -- what lets
-- Engine.placePendingTriggers merge it into the one batch CR 603.3b orders, and
-- Pawl.Engine.Monarch.placeInherent put it on the stack.
--
-- AT MOST ONE. A precombat main phase begins once (CR 505.1a), and the ability
-- belongs to the player whose phase it is.
--
-- Only the ACTIVE player's, which is that same rule and not a shortcut: no other
-- player has a precombat main phase on this turn, so rule 728.1's "that player"
-- can be nobody else. An opponent's rad counters wait for their own turn.
inherentPending :: [GameEvent] -> GameState -> [PendingTrigger]
inherentPending events gs =
  let you = GameState.activePlayer gs
      -- A PARTIAL case with a wildcard, Pawl.Engine.Speed.inherentPending's
      -- posture: this matcher answers about one event shape, and a new GameEvent
      -- constructor is not an event rule 728.1 names.
      precombatMainBegan event = case event of
        GameEvent.StepBegan Phase.PrecombatMain active -> active == you
        _ -> False
      -- CR 603.4: the intervening "if" is checked here, as the event occurs. A
      -- player with no rad counters does not trigger at all, which is the
      -- difference between this and an ability that triggers and then does
      -- nothing -- observable, since a trigger going on the stack is a thing
      -- other players may respond to.
      irradiated = Maybe.maybe False (>= 1) (radCountersOf you gs)
   in [ PendingTrigger.MkPendingTrigger TriggerSource.Sourceless you ability Map.empty
      | irradiated,
        List.any precombatMainBegan events
      ]
