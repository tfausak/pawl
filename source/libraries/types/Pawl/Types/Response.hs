module Pawl.Types.Response where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.HandActionIndex as HandActionIndex
import qualified Pawl.Types.HybridPayment as HybridPayment
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype

-- | One answer to a prompt, serialized so a DecisionLog replays the game
-- deterministically.
--
-- Every prompt gets its OWN constructor, even where two share a payload shape:
-- decode's job is to return Nothing for a response that does not match the
-- prompt being asked, and two prompts sharing a constructor cannot do that.
data Response
  = ChoseAction Action.Action
  | -- | CR 104.3a: whether a player conceded when asked.
    Conceded Concession.Concession
  | Shuffled [ObjectId.ObjectId]
  | -- | CR 729.2: the player randomness picked to go first in a subgame.
    DeterminedFirstPlayer PlayerId.PlayerId
  | ChoseDiscard [ObjectId.ObjectId]
  | -- | CR 507.1: the opponent the active player chose to attack.
    ChoseDefender PlayerId.PlayerId
  | -- | CR 601.2g: the mana source the player chose to tap.
    ChoseManaSource ObjectId.ObjectId
  | -- | CR 605.3b / 105.4: the mana the source's controller chose it to produce
    -- -- the whole yield of one activation, so a Sol Ring's is two units.
    ChoseManaYield Mana.Mana
  | -- | CR 701.34a: the permanents and players a proliferating player chose. A
    -- pair rather than two constructors, because one prompt asks one question.
    ChoseProliferation (Set.Set ObjectId.ObjectId, Set.Set PlayerId.PlayerId)
  | -- | CR 701.54a: the creature a tempted player chose as their Ring-bearer.
    ChoseRingBearer ObjectId.ObjectId
  | -- | CR 704.5j: the legendary permanent its controller kept.
    ChoseLegend ObjectId.ObjectId
  | DeclaredAttackers [ObjectId.ObjectId]
  | -- | CR 508.1b / CR 508.4: what one attacking creature was announced as
    -- attacking. Unlike ChoseDefender, this may name a permanent.
    ChoseAttackTarget AttackTarget.AttackTarget
  | DeclaredBlockers (Map.Map ObjectId.ObjectId ObjectId.ObjectId)
  | AssignedCombatDamage (Map.Map Recipient.Recipient Natural.Natural)
  | ChoseTargets (Map.Map SlotName.SlotName Recipient.Recipient)
  | -- | CR 612: the (from, to) basic land types a text-changer's caster chose.
    -- Named for the swap rather than for the pair, matching
    -- Prompt.ChooseLandTypeSwap.
    ChoseLandTypeSwap (Subtype.Subtype, Subtype.Subtype)
  | -- | CR 612 again, for the creature-type half: the (from, to) creature types
    -- Artificial Evolution's caster chose.
    ChoseCreatureTypeSwap (Subtype.Subtype, Subtype.Subtype)
  | -- | CR 614.1c: the basic land type a player chose as an object entered.
    -- Singular, and distinct from ChoseLandTypeSwap above for
    -- Prompt.ChooseBasicLandType's reason.
    ChoseBasicLandType Subtype.Subtype
  | -- | CR 701.23: the library card a search found (Nothing = failed to find).
    Searched (Maybe ObjectId.ObjectId)
  | -- | CR 601.3 (Panglacial): the library card cast while searching (Nothing =
    -- declined).
    CastWhileSearched (Maybe ObjectId.ObjectId)
  | -- | CR 601.2b: the value of X a caster chose.
    ChoseX Natural.Natural
  | -- | CR 601.2b: the mode(s) a caster chose for a modal spell.
    ChoseModes (Set.Set ModeIndex.ModeIndex)
  | -- | CR 707.5: the permanent a copy chose to copy (Nothing = declined).
    ChoseCopyTarget (Maybe ObjectId.ObjectId)
  | -- | CR 208.2b: the index of the entry shape a player chose as an object
    -- entered.
    ChoseEntryOption Natural.Natural
  | -- | CR 614.1c: the colour a player chose as an object entered.
    ChoseColor Color.Color
  | -- | CR 201.4 / 614.1c: the card name a player chose as an object entered.
    ChoseCardName CardName.CardName
  | -- | The opponent a player chose for an as-enters choice the card assigns to
    -- "an opponent" (CR 614.12a puts that choice before the permanent enters).
    -- Unlike ChoseDefender, which also carries an opponent, this answers no
    -- combat question.
    ChoseOpponent PlayerId.PlayerId
  | -- | CR 603.3b: the order a player chose for their simultaneous triggers, as
    -- a permutation of the offered indices.
    OrderedTriggers [Natural.Natural]
  | -- | CR 615.7: the order a shielded player (or a shielded permanent's
    -- controller) chose for the simultaneous damage events one prevention shield
    -- may cover, as a permutation of the offered indices.
    OrderedDamage [Natural.Natural]
  | -- | CR 616.1: the index of the replacement effect a player chose to apply
    -- next.
    ChoseReplacement Natural.Natural
  | -- | CR 603.7c: the minted token a Create's slot bound, once a CR 614.16
    -- replacement had made several of them.
    ChoseBoundToken ObjectId.ObjectId
  | -- | CR 701.21a: the permanents a player chose to sacrifice to pay a cost.
    ChoseSacrifices (Set.Set ObjectId.ObjectId)
  | -- | CR 701.3a: the object a player chose to attach a moving permanent to.
    ChoseAttachment ObjectId.ObjectId
  | -- | CR 601.2b: the cost a caster announced they would pay.
    ChoseCost (Cost.Cost Keyword.Keyword)
  | -- | CR 103.5: a player's mulligan declaration.
    DeclaredMulligan MulliganDecision.MulliganDecision
  | -- | CR 103.5: the cards a player put on the bottom of their library after a
    -- mulligan, in chosen order.
    PutOnBottom [ObjectId.ObjectId]
  | -- | CR 103.5b: the hand card whose mulligan-window action a player took, and
    -- WHICH of that card's actions it was (Nothing = declined).
    TookMulliganAction (Maybe (ObjectId.ObjectId, HandActionIndex.HandActionIndex))
  | -- | CR 103.6: the hand card whose opening-hand action a player took, and which
    -- of that card's actions it was (Nothing = declined).
    TookOpeningHandAction (Maybe (ObjectId.ObjectId, HandActionIndex.HandActionIndex))
  | -- | CR 603.5: whether the controller of a resolving spell or ability
    -- exercised a printed "may".
    ChoseOptional OptionalDecision.OptionalDecision
  | -- | CR 118.12a: whether the player a resolving spell or ability offered a
    -- cost to chose to pay it. Distinct from ChoseOptional, which records CR
    -- 603.5's "may" and is always answered by the resolving controller.
    ChoseToPay PaymentDecision.PaymentDecision
  | -- | CR 118.13a / 601.2b: which way a caster announced they would pay a
    -- Phyrexian mana symbol, so a Mutagenic Growth paid out of life replays
    -- exactly as it was cast.
    AnnouncedPhyrexianPayment PhyrexianPayment.PhyrexianPayment
  | -- | CR 118.13a / 601.2b: which way a caster announced they would pay a
    -- monocolored hybrid mana symbol, so a Flame Javelin cast for {6} replays as
    -- that and not as {R}{R}{R}.
    AnnouncedHybridPayment HybridPayment.HybridPayment
  | -- | CR 702.42a / 601.2b: whether a caster used a modal spell's entwine
    -- ability.
    AnnouncedEntwine EntwineDecision.EntwineDecision
  deriving (Eq, Show)
