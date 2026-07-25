module Pawl.Type.Response where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Type.Action (Action)
import Pawl.Type.Concession (Concession)
import Pawl.Type.Cost (Cost)
import Pawl.Type.ModeIndex (ModeIndex)
import Pawl.Type.MulliganDecision (MulliganDecision)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.Subtype (Subtype)

data Response
  = ChoseAction Action
  | -- CR 104.3a: whether a player conceded when asked, serialized so a
    -- DecisionLog replays the concession deterministically.
    Conceded Concession
  | Shuffled [ObjectId]
  | -- CR 729.2: the player randomness picked to go first in a subgame,
    -- serialized so a DecisionLog replays that roll deterministically.
    DeterminedFirstPlayer PlayerId
  | ChoseDiscard [ObjectId]
  | -- CR 507.1: the opponent the active player chose to attack, serialized so a
    -- DecisionLog replays a multiplayer combat deterministically. Its own
    -- constructor rather than a reuse of DeterminedFirstPlayer: decode's job is to
    -- return Nothing for a response that does not match the prompt being asked,
    -- and two prompts sharing a constructor cannot do that.
    ChoseDefender PlayerId
  | DeclaredAttackers [ObjectId]
  | DeclaredBlockers (Map ObjectId ObjectId)
  | AssignedCombatDamage (Map Recipient Natural)
  | ChoseTargets (Map SlotName Recipient)
  | -- CR 612 / the D4 binding: the (from, to) basic land types a text-changer's
    -- caster chose, serialized so a DecisionLog replays the hack deterministically.
    ChoseBasicLandTypes (Subtype, Subtype)
  | -- CR 701.23: the library card a search found (Nothing = failed to find),
    -- serialized so a DecisionLog replays a tutor deterministically.
    Searched (Maybe ObjectId)
  | -- CR 601.3 (Panglacial): the library card cast while searching (Nothing =
    -- declined), serialized so a DecisionLog replays the re-entrant cast.
    CastWhileSearched (Maybe ObjectId)
  | -- CR 601.2b: the value of X a caster chose, serialized so a DecisionLog
    -- replays a variable-cost spell deterministically.
    ChoseX Natural
  | -- CR 601.2b: the mode(s) a caster chose for a modal spell, serialized so a
    -- DecisionLog replays a modal cast deterministically.
    ChoseModes (Set ModeIndex)
  | -- CR 707.5: the permanent a copy chose to copy (Nothing = declined),
    -- serialized so a DecisionLog replays an as-enters copy deterministically.
    ChoseCopyTarget (Maybe ObjectId)
  | -- CR 208.2b: the index of the entry shape a player chose as an object entered,
    -- serialized so a DecisionLog replays it deterministically.
    ChoseEntryOption Natural
  | -- CR 603.3b: the order a player chose for their simultaneous triggers, as a
    -- permutation of the offered indices, serialized so a DecisionLog replays it.
    OrderedTriggers [Natural]
  | -- CR 616.1: the index of the replacement effect a player chose to apply next,
    -- serialized so a DecisionLog replays a replacement race deterministically.
    ChoseReplacement Natural
  | -- CR 701.21a: the permanents a player chose to sacrifice to pay a cost,
    -- serialized so a DecisionLog replays the payment deterministically.
    ChoseSacrifices (Set ObjectId)
  | -- CR 601.2b: the cost a caster announced they would pay, serialized so a
    -- DecisionLog replays an alternative-cost cast deterministically.
    ChoseCost Cost
  | -- CR 103.5: a player's mulligan declaration, serialized so a DecisionLog
    -- replays the mulligan round deterministically.
    DeclaredMulligan MulliganDecision
  | -- CR 103.5: the cards a player put on the bottom of their library after a
    -- mulligan, in chosen order, serialized so a DecisionLog replays it.
    PutOnBottom [ObjectId]
  | -- CR 103.5b: the hand card whose mulligan-window action a player took
    -- (Nothing = declined), serialized so a DecisionLog replays it. Its own
    -- constructor rather than a reuse of Searched / CastWhileSearched, for the
    -- reason ChoseDefender records: decode's job is to return Nothing for a
    -- response that does not match the prompt being asked, and two prompts
    -- sharing a constructor cannot do that.
    TookMulliganAction (Maybe ObjectId)
  | -- CR 103.6: the hand card whose opening-hand action a player took (Nothing =
    -- declined), serialized so a DecisionLog replays it. Its own constructor
    -- rather than a reuse of TookMulliganAction, for the reason ChoseDefender
    -- records: decode must return Nothing for a response that does not match the
    -- prompt being asked, and two prompts sharing a constructor cannot do that.
    TookOpeningHandAction (Maybe ObjectId)
  deriving (Eq, Show)
