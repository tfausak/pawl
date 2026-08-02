module Pawl.Types.Response where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype

data Response
  = ChoseAction Action.Action
  | -- | CR 104.3a: whether a player conceded when asked, serialized so a
    -- DecisionLog replays the concession deterministically.
    Conceded Concession.Concession
  | Shuffled [ObjectId.ObjectId]
  | -- | CR 729.2: the player randomness picked to go first in a subgame,
    -- serialized so a DecisionLog replays that roll deterministically.
    DeterminedFirstPlayer PlayerId.PlayerId
  | ChoseDiscard [ObjectId.ObjectId]
  | -- | CR 507.1: the opponent the active player chose to attack, serialized so a
    -- DecisionLog replays a multiplayer combat deterministically. Its own
    -- constructor rather than a reuse of DeterminedFirstPlayer: decode's job is to
    -- return Nothing for a response that does not match the prompt being asked,
    -- and two prompts sharing a constructor cannot do that.
    ChoseDefender PlayerId.PlayerId
  | -- | CR 601.2g: the mana source the player chose to tap.
    ChoseManaSource ObjectId.ObjectId
  | -- | CR 605.3b / 105.4: the mana the source's controller chose it to produce --
    -- the whole yield of one activation, so a Sol Ring's is two units --
    -- serialized so a DecisionLog replays an any-colour source (or a dual land)
    -- deterministically. Its own constructor rather than a reuse of
    -- ChoseManaSource: decode must return Nothing for a response that does not
    -- match the prompt being asked, and two prompts sharing a constructor cannot.
    ChoseManaYield Mana.Mana
  | -- | CR 701.34a: the permanents and players a proliferating player chose,
    -- serialized so a DecisionLog replays a proliferate deterministically. A pair
    -- rather than two constructors, because one prompt asks one question.
    ChoseProliferation (Set.Set ObjectId.ObjectId, Set.Set PlayerId.PlayerId)
  | -- | CR 704.5j: the legendary permanent its controller kept, serialized so a
    -- DecisionLog replays the legend rule deterministically. Its own constructor
    -- rather than a reuse of ChoseManaSource: decode must return Nothing for a
    -- response that does not match the prompt asked, and two ObjectId-shaped
    -- prompts sharing a constructor cannot do that.
    ChoseLegend ObjectId.ObjectId
  | DeclaredAttackers [ObjectId.ObjectId]
  | -- | CR 508.1b / CR 508.4: what one attacking creature was announced as
    -- attacking, serialized so a DecisionLog replays an attack on a planeswalker
    -- deterministically. Its own constructor rather than a reuse of
    -- ChoseDefender: decode must return Nothing for a response that does not
    -- match the prompt being asked, and two prompts sharing a constructor cannot
    -- do that -- and the payloads differ anyway, since a defending player is
    -- always a player and this may name a permanent.
    ChoseAttackTarget AttackTarget.AttackTarget
  | DeclaredBlockers (Map.Map ObjectId.ObjectId ObjectId.ObjectId)
  | AssignedCombatDamage (Map.Map Recipient.Recipient Natural.Natural)
  | ChoseTargets (Map.Map SlotName.SlotName Recipient.Recipient)
  | -- | CR 612 / the D4 binding: the (from, to) basic land types a text-changer's
    -- caster chose, serialized so a DecisionLog replays the hack deterministically.
    ChoseBasicLandTypes (Subtype.Subtype, Subtype.Subtype)
  | -- | CR 701.23: the library card a search found (Nothing = failed to find),
    -- serialized so a DecisionLog replays a tutor deterministically.
    Searched (Maybe ObjectId.ObjectId)
  | -- | CR 601.3 (Panglacial): the library card cast while searching (Nothing =
    -- declined), serialized so a DecisionLog replays the re-entrant cast.
    CastWhileSearched (Maybe ObjectId.ObjectId)
  | -- | CR 601.2b: the value of X a caster chose, serialized so a DecisionLog
    -- replays a variable-cost spell deterministically.
    ChoseX Natural.Natural
  | -- | CR 601.2b: the mode(s) a caster chose for a modal spell, serialized so a
    -- DecisionLog replays a modal cast deterministically.
    ChoseModes (Set.Set ModeIndex.ModeIndex)
  | -- | CR 707.5: the permanent a copy chose to copy (Nothing = declined),
    -- serialized so a DecisionLog replays an as-enters copy deterministically.
    ChoseCopyTarget (Maybe ObjectId.ObjectId)
  | -- | CR 208.2b: the index of the entry shape a player chose as an object entered,
    -- serialized so a DecisionLog replays it deterministically.
    ChoseEntryOption Natural.Natural
  | -- | CR 603.3b: the order a player chose for their simultaneous triggers, as a
    -- permutation of the offered indices, serialized so a DecisionLog replays it.
    OrderedTriggers [Natural.Natural]
  | -- | CR 616.1: the index of the replacement effect a player chose to apply next,
    -- serialized so a DecisionLog replays a replacement race deterministically.
    ChoseReplacement Natural.Natural
  | -- | CR 603.7c: the minted token a Create's slot bound, once a CR 614.16
    -- replacement had made several of them, serialized so a DecisionLog replays
    -- the binding deterministically. Its own constructor rather than a reuse of
    -- ChoseLegend, for the reason ChoseDefender records: decode must return
    -- Nothing for a response that does not match the prompt being asked, and two
    -- ObjectId-shaped prompts sharing a constructor cannot do that.
    ChoseBoundToken ObjectId.ObjectId
  | -- | CR 701.21a: the permanents a player chose to sacrifice to pay a cost,
    -- serialized so a DecisionLog replays the payment deterministically.
    ChoseSacrifices (Set.Set ObjectId.ObjectId)
  | -- | CR 701.3a: the object a player chose to attach a moving permanent to,
    -- serialized so a DecisionLog replays the move deterministically. Its own
    -- constructor rather than a reuse of ChoseBoundToken, for the reason
    -- ChoseDefender records: decode must return Nothing for a response that does
    -- not match the prompt being asked, and two ObjectId-shaped prompts sharing a
    -- constructor cannot do that.
    ChoseAttachment ObjectId.ObjectId
  | -- | CR 601.2b: the cost a caster announced they would pay, serialized so a
    -- DecisionLog replays an alternative-cost cast deterministically.
    ChoseCost (Cost.Cost Keyword.Keyword)
  | -- | CR 103.5: a player's mulligan declaration, serialized so a DecisionLog
    -- replays the mulligan round deterministically.
    DeclaredMulligan MulliganDecision.MulliganDecision
  | -- | CR 103.5: the cards a player put on the bottom of their library after a
    -- mulligan, in chosen order, serialized so a DecisionLog replays it.
    PutOnBottom [ObjectId.ObjectId]
  | -- | CR 103.5b: the hand card whose mulligan-window action a player took
    -- (Nothing = declined), serialized so a DecisionLog replays it. Its own
    -- constructor rather than a reuse of Searched / CastWhileSearched, for the
    -- reason ChoseDefender records: decode's job is to return Nothing for a
    -- response that does not match the prompt being asked, and two prompts
    -- sharing a constructor cannot do that.
    TookMulliganAction (Maybe ObjectId.ObjectId)
  | -- | CR 103.6: the hand card whose opening-hand action a player took (Nothing =
    -- declined), serialized so a DecisionLog replays it. Its own constructor
    -- rather than a reuse of TookMulliganAction, for the reason ChoseDefender
    -- records: decode must return Nothing for a response that does not match the
    -- prompt being asked, and two prompts sharing a constructor cannot do that.
    TookOpeningHandAction (Maybe ObjectId.ObjectId)
  | -- | CR 603.5: whether the controller of a resolving spell or ability exercised
    -- a printed "may", serialized so a DecisionLog replays an optional effect
    -- deterministically. Its own constructor rather than a reuse of Conceded or
    -- DeclaredMulligan, for the reason ChoseDefender records: decode must return
    -- Nothing for a response that does not match the prompt being asked, and two
    -- prompts sharing a constructor cannot do that.
    ChoseOptional OptionalDecision.OptionalDecision
  | -- | CR 118.13a / 601.2b: which way a caster announced they would pay a
    -- Phyrexian mana symbol, serialized so a DecisionLog replays a Mutagenic
    -- Growth paid out of life exactly as it was cast. Its own constructor rather
    -- than a reuse of ChoseOptional, Conceded or DeclaredMulligan, for the reason
    -- ChoseDefender records: decode must return Nothing for a response that does
    -- not match the prompt being asked, and two two-valued prompts sharing a
    -- constructor cannot do that.
    AnnouncedPhyrexianPayment PhyrexianPayment.PhyrexianPayment
  | -- | CR 702.42a / 601.2b: whether a caster used a modal spell's entwine
    -- ability, serialized so a DecisionLog replays an entwined cast exactly as
    -- it was made. Its own constructor rather than a reuse of ChoseOptional,
    -- Conceded or DeclaredMulligan, for the reason ChoseDefender records: decode
    -- must return Nothing for a response that does not match the prompt being
    -- asked, and two two-valued prompts sharing a constructor cannot do that.
    AnnouncedEntwine EntwineDecision.EntwineDecision
  deriving (Eq, Show)
