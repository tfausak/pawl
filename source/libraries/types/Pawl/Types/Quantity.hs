module Pawl.Types.Quantity where

import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.CastFrom as CastFrom
import qualified Pawl.Types.CompletedDungeon as CompletedDungeon
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Devotion as Devotion
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.ManaCount as ManaCount
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.Types.PlayerDesignationTally as PlayerDesignationTally
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Times as Times

-- | A number that may not be a literal number.
--
-- Deliberately no Num instance: it would be lawless and partial once Star and
-- Infinite exist, and fromInteger would erase the distinction this type draws.
-- Combining is explicit named functions; Plus composes the printed values.
--
-- Arms that name no object (Power, ObjectCounters, HasDesignation, ...) read
-- the object the evaluation is aimed at, most of them through the injected
-- view, which is what lets CR 608.2h last known information answer for an
-- object that is gone; AgainstSlot and AgainstCardsExiledWith are how a card
-- aims them elsewhere.
data Quantity
  = Literal Integer
  | -- | CR 202.3: the mana value of the object this quantity is evaluated against.
    ManaValue
  | -- | CR 208.1: the projected power of the object this quantity is evaluated
    -- against.
    Power
  | -- | CR 208.1: the projected toughness of the object this quantity is
    -- evaluated against.
    Toughness
  | -- | A number an earlier effect of the same resolution bound at that slot;
    -- Nothing when it holds no amount. CR 601.2b's X is this arm, at
    -- Pawl.Engine.Binding.variableX.
    InSlot SlotName.SlotName
  | -- | CR 701.4b: 1 if that slot of the surrounding announcement names at least
    -- one object, else 0 -- Osseous Exhale's "if a Dragon was beheld", and CR
    -- 701.59c's "if evidence was collected" (Vitu-Ghazi Inspector).
    --
    -- Reads the BINDING and never the board, which is rule 701.4b's whole point:
    -- a Count over Scope.OverBound asks the same question through CR 400.7j and
    -- so answers no for a beheld permanent bounced in response, and for one
    -- beheld out of a hand at all (CR 400.2). The QUALITY is not re-checked
    -- either, the cost's own Filter having settled it at payment time.
    WasBound SlotName.SlotName
  | -- | CR 208.2 / 208.2a: the printed star -- notation, which the projection
    -- seed substitutes (Projection.baseCharacteristics) and evaluate answers
    -- Nothing for.
    Star
  | -- | CR 208.2: composition, so a printed 1+* needs no constructor of its own.
    Plus (Plus.Plus Quantity)
  | -- | CR 107.1a: half the inner quantity, rounded the way the card prints
    -- (Pawl.Types.Rounding).
    Halved (Halved.Halved Quantity)
  | -- | CR 107.1: the payload's factor times the inner quantity, which is the
    -- "N for each" a card prints; see Pawl.Types.Times.
    Times (Times.Times Quantity)
  | -- | The negation of the inner quantity -- the minus a card prints in front
    -- of a value, as in "-X/-X". CR 107.1b: a game value may go negative; a
    -- count reader saturates at 0.
    Negate Quantity
  | -- | A quantity that counts game state (CR 208.2a, CR 608.2h); see
    -- Pawl.Types.Count. The payload is `Count Quantity` so Aggregation.Greatest
    -- recurses through the data rather than a module cycle.
    Count (Count.Count Quantity)
  | -- | CR 106.4: a count of a mana pool; see Pawl.Types.ManaCount.
    ManaCount ManaCount.ManaCount
  | -- | CR 119.1: that player's life total.
    LifeTotal PlayerRef.PlayerRef
  | -- | CR 702.179: that player's speed; no speed reads 0 (CR 702.179f).
    Speed PlayerRef.PlayerRef
  | -- | CR 725.1: 1 if that player is the monarch, else 0; no monarch reads 0
    -- (CR 725.5).
    IsMonarch PlayerRef.PlayerRef
  | -- | CR 702.131c \/ 702.195b: 1 if that player has that rest-of-game mark, else
    -- 0; a player who has never had it reads 0.
    HasPlayerDesignation PlayerDesignationTally.PlayerDesignationTally
  | -- | CR 103.1: 1 if that player is the starting player -- the head of
    -- GameState.turnOrder -- else 0.
    --
    -- Not implemented: CR 103.1c's Power Play, which makes its controller the
    -- starting player after the determination. That card is not in
    -- @data\/cards\/@ and there is no effect that reseats a turn order, so the
    -- head of the roster is the whole answer today (#882).
    IsStartingPlayer PlayerRef.PlayerRef
  | -- | CR 102.1: 1 if that player is the active player, else 0.
    IsActivePlayer PlayerRef.PlayerRef
  | -- | CR 700.5: that player's devotion to the payload's colours -- how many
    -- mana symbols of any of them appear among the mana costs of the permanents
    -- they control.
    Devotion Devotion.Devotion
  | -- | CR 122.1: how many counters of that kind that player has; an absent
    -- kind reads 0.
    PlayerCounters PlayerCounterTally.PlayerCounterTally
  | -- | CR 122.1: how many counters of that kind are on the object this
    -- quantity is evaluated against; an absent kind reads 0.
    ObjectCounters (CounterKind.CounterKind Keyword.Keyword)
  | -- | CR 122.1: how many counters of every kind, summed, are on the object
    -- this quantity is evaluated against.
    ObjectCountersOfAnyKind
  | -- | 1 if the object this quantity is evaluated against has that
    -- designation, else 0 -- CR 702.112a's "if it isn't renowned".
    HasDesignation Designation.Designation
  | -- | CR 701.37c: the value the object this quantity is evaluated against
    -- became designated with -- "monstrosity X", read back by another ability of
    -- the same permanent. A mark set with no number reads 0.
    DesignationValue Designation.Designation
  | -- | CR 716.2b: the level of the object this quantity is evaluated against;
    -- no level reads 1 (CR 716.2d, Pawl.Types.ClassLevel.defaulted).
    ClassLevel
  | -- | CR 702.33d: 1 if the spell this quantity is evaluated against was
    -- kicked with any of its kicker costs, else 0; a permanent answers for the
    -- spell that became it (CR 400.7d).
    WasKicked
  | -- | CR 702.143c: 1 if the spell this quantity is evaluated against was a
    -- foretold card before it was cast, whatever cost it was cast for, else 0.
    WasForetold
  | -- | CR 702.104b: 1 if tribute was paid for the object this quantity is
    -- evaluated against -- the opponent its controller chose had it enter with
    -- rule 702.104a's +1/+1 counters -- else 0.
    --
    -- The arm above's shape one rule over, off Object.tributePaid rather than off
    -- the counters, for the reason that field gives.
    TributeWasPaid
  | -- | CR 601.2b / 400.7d: how many times the additional cost that keyword
    -- ability offers was declared for the spell this quantity is evaluated
    -- against, or the spell that became it -- CR 702.33c/f, 702.157a, 702.175a.
    TimesPaid Keyword.Keyword
  | -- | CR 601.2b / 400.7d: 1 if the spell this quantity is evaluated against, or
    -- the spell that became it, was cast for the cost that keyword ability
    -- offers -- CR 702.74a's "if its evoke cost was paid", CR 702.138b's
    -- "escaped" -- else 0.
    CastUsing KeywordFamily.KeywordFamily
  | -- | CR 107.4h: 1 if mana carrying that production tag was spent to pay for
    -- the object this quantity is evaluated against, else 0.
    --
    -- Not the spent mana's COLOUR; TagWasSpentOfOwnColor below is that question.
    TagWasSpent ProductionTag.ProductionTag
  | -- | CR 107.4h \/ 202.2: 1 if mana carrying that production tag AND of one of
    -- the colors of the object this quantity is evaluated against was spent to
    -- pay for it, else 0 -- Boreal Outrider's "if {S} of any of that spell's
    -- colors was spent to cast it".
    --
    -- The arm above's conjunction over ONE unit rather than two of its tests: a
    -- colorless snow mana and a green non-snow mana satisfy TagWasSpent Snow and
    -- HasColor Green between them, and this clause not at all.
    TagWasSpentOfOwnColor ProductionTag.ProductionTag
  | -- | CR 202.1a \/ 702.191a: how many mana were spent to pay for the object this
    -- quantity is evaluated against -- rule 702.191a's "the amount of mana spent
    -- to cast that spell".
    --
    -- The COUNT of units where TagWasSpent above asks after one unit's
    -- classification; both read Pawl.Types.Object.manaSpent through
    -- Pawl.Engine.Filter's view.
    ManaSpent
  | -- | CR 111.6 / 608.2h: 1 if the object this quantity is evaluated against
    -- was a token, else 0.
    WasToken
  | -- | CR 508.1k / 608.2h: 1 if the object this quantity is evaluated against
    -- was attacking, else 0.
    WasAttacking
  | -- | CR 509.1g / 608.2h: 1 if the object this quantity is evaluated against
    -- was blocking, else 0.
    WasBlocking
  | -- | CR 509.1h \/ 608.2i: 1 if the object this quantity is evaluated against
    -- became a blocked creature this turn, else 0, folded from the turn-scoped
    -- GameEvent.AttackerBlocked log.
    --
    -- Pawl.ConditionSpec's WasBlockedThisTurn group is what proves rule 509.1h's
    -- last sentence here: the blocker leaving combat before the death does not
    -- take the answer away.
    WasBlockedThisTurn
  | -- | CR 702.30a / 608.2h: 1 if the player that reference names came to
    -- control the object this quantity is evaluated against since the beginning
    -- of that player's last upkeep, else 0.
    ControlGainedSinceLastUpkeep PlayerRef.PlayerRef
  | -- | CR 305.1 / 601.2a: 1 if the player that reference names played the
    -- object this quantity is evaluated against, else 0, read off
    -- GameState.cardsPlayed.
    PlayedBy PlayerRef.PlayerRef
  | -- | CR 508.3b: how many of that player's opponents were declared attacked
    -- this combat (Combat.declaredAttacked) -- rule 702.121a's melee.
    --
    -- That player's own attacks: under the shared team turns option each player
    -- on the active team is an attacking player (CR 805.10a).
    OpponentsAttacked PlayerRef.PlayerRef
  | -- | CR 508.1a / 608.2i: how many creatures that player declared as attackers
    -- this turn, folded from the turn-scoped GameEvent.AttackerDeclared log.
    --
    -- Rule 207.2c's raid is this compared against 1, and it is neither of the two
    -- arms it sits between. OpponentsAttacked counts the OPPONENTS a declaration
    -- reached, so an attack aimed only at a planeswalker (CR 506.3) answers 0;
    -- Filter.AttackedThisTurn asks a candidate whether it attacked, so a count
    -- over the battlefield loses an attacker that has since died. Raid asks
    -- neither: it asks whether the DECLARATION happened, which only the log
    -- records.
    --
    -- Only that player's declarations, read off the event: under the shared team
    -- turns option each player on the active team is an attacking player (CR
    -- 805.10a).
    AttackersDeclaredThisTurn PlayerRef.PlayerRef
  | -- | CR 701.9a / 608.2i: how many cards that player discarded this turn,
    -- folded from the turn-scoped GameEvent.Discarded log.
    CardsDiscardedThisTurn PlayerRef.PlayerRef
  | -- | CR 603.1b / 608.2i: how many of the four bending verbs (CR 701.65b,
    -- 701.66b, 701.67c, 702.189b) that player has done this turn -- Avatar
    -- Aang's "if you've done all four this turn".
    BendingsThisTurn PlayerRef.PlayerRef
  | -- | CR 119.3 / 608.2i: how much life that player gained this turn, summed
    -- from the turn-scoped GameEvent.LifeGained log.
    LifeGainedThisTurn PlayerRef.PlayerRef
  | -- | CR 120.1 / 608.2i: how many of the players that reference names were
    -- dealt damage this turn -- players, not events (rule 702.54a).
    PlayersDealtDamageThisTurn PlayerRef.PlayerRef
  | -- | CR 120.1 / 608.2i: the total damage dealt this turn to the players that
    -- reference names (rule 702.54b).
    DamageDealtToPlayersThisTurn PlayerRef.PlayerRef
  | -- | CR 120.1 / 608.2i: the total damage dealt this turn to the object this
    -- quantity is evaluated against.
    DamageDealtToThisTurn
  | -- | CR 601.2i / 608.2i: how many spells that player cast last turn, read off
    -- GameState.castsLastTurn (not CR 502.2's active-player scalar).
    SpellsCastLastTurn PlayerRef.PlayerRef
  | -- | CR 601.2i / 608.2i: how many spells that player cast this turn.
    SpellsCastThisTurn PlayerRef.PlayerRef
  | -- | CR 608.2n \/ 608.2i: how many times the ACTIVATED ABILITY this quantity is
    -- evaluated against has resolved this turn, folded from the turn-scoped
    -- GameEvent.ActivatedAbilityResolved log and counting the resolution asking.
    --
    -- Aimed at the ability's own object on the stack, which a card reaches
    -- through AgainstSlot over CR 602.2a's reserved "thisAbility" slot -- Ashling
    -- the Pilgrim's "if this is the third time this ability has resolved this
    -- turn". Aimed at anything else it reads 0, no other object's Source being a
    -- key in that log.
    --
    -- Pawl.ConditionSpec's Ashling the Pilgrim group is what proves the count,
    -- and Pawl.CopySpec's what proves rule 707.10b's third sentence here: a copy
    -- of the ability counts toward the same total, the copy keeping the
    -- original's Source.
    TimesResolvedThisTurn
  | -- | CR 702.40a: how many spells, by any player, were cast this turn before the
    -- object this quantity is evaluated against -- storm's count.
    SpellsCastBefore
  | -- | CR 700.4 \/ 608.2i: how many permanents, of any player, were put into a
    -- graveyard from the battlefield this turn -- gravestorm's count (CR
    -- 702.69a).
    PermanentsDiedThisTurn
  | -- | CR 702.185c \/ 608.2i: how many spells, by any player, were cast this
    -- turn for the cost that keyword family offers -- "a spell was warped this
    -- turn" is this compared against 1.
    SpellsCastUsingThisTurn KeywordFamily.KeywordFamily
  | -- | CR 100.6a \/ 729.1a: how many subgames have begun this match, counting one
    -- in progress -- Shahrazad and Sindbad's "if there haven't been any subgames
    -- this match", which is this compared to 0. Read off
    -- GameState.subgamesThisMatch, the only match-scoped record pawl keeps.
    SubgamesThisMatch
  | -- | CR 309.7: how many dungeons that player has completed
    -- (Player.completedDungeons).
    DungeonsCompleted PlayerRef.PlayerRef
  | -- | CR 309.7 as a 0\/1: has that player completed a dungeon with this name --
    -- Acererak the Archlich's "if you haven't completed Tomb of Annihilation".
    CompletedDungeon CompletedDungeon.CompletedDungeon
  | -- | CR 400.7 / 608.2i: 1 if the object this quantity is evaluated against
    -- entered the battlefield this turn, else 0, read off GameState.events.
    EnteredThisTurn
  | -- | CR 400.7 / 400.3: did the object this quantity is evaluated against enter
    -- the battlefield out of that player's copy of that zone? 1 if so and 0 if not.
    EnteredFrom InZone.InZone
  | -- | CR 601.2a / 400.3: did the object this quantity is evaluated against enter
    -- the battlefield as a spell the payload's caster cast out of the payload's
    -- zone? The two references are independent; see Pawl.Types.CastFrom.
    WasCastFrom CastFrom.CastFrom
  | -- | CR 509.1h / 702.23a: how many creatures block the object this quantity
    -- is evaluated against, beyond the first; unblocked reads 0.
    BlockersBeyondFirst
  | -- | Read the inner quantity against the object that slot names rather than
    -- the effect's source (CR 113.7); Nothing when the slot names no object.
    AgainstSlot (AgainstSlot.AgainstSlot Quantity)
  | -- | CR 607.2a / 614.14: the inner quantity read against each card this
    -- quantity's source exiled, summed (CR 607.3); an empty pile reads 0.
    AgainstCardsExiledWith Quantity
  | -- | CR 702.184c: the tapped creature's power, or its toughness where that is
    -- greater and the ability's controller controls a permanent carrying
    -- Modification.GrantsStationToughness. Engine-only, a leaf like Power:
    -- Pawl.Engine.Keyword.station mints it and no card should author it.
    StationMeasure
  deriving (Eq, Ord, Show)
