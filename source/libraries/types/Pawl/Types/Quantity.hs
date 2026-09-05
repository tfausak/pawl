module Pawl.Types.Quantity where

import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.CastFrom as CastFrom
import qualified Pawl.Types.CompletedDungeon as CompletedDungeon
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCount as ManaCount
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.SlotName as SlotName

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
  | -- | CR 208.2 / 208.2a: the printed star -- notation, which the projection
    -- seed substitutes (Projection.baseCharacteristics) and evaluate answers
    -- Nothing for.
    Star
  | -- | CR 208.2: composition, so a printed 1+* needs no constructor of its own.
    Plus (Plus.Plus Quantity)
  | -- | CR 107.1a: half the inner quantity, rounded the way the card prints
    -- (Pawl.Types.Rounding).
    Halved (Halved.Halved Quantity)
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
  | -- | CR 716.2b: the level of the object this quantity is evaluated against;
    -- no level reads 1 (CR 716.2d, Pawl.Types.ClassLevel.defaulted).
    ClassLevel
  | -- | CR 702.33d: 1 if the spell this quantity is evaluated against was
    -- kicked with any of its kicker costs, else 0; a permanent answers for the
    -- spell that became it (CR 400.7d).
    WasKicked
  | -- | CR 702.33c / 702.33f: how many times that kicker cost was declared for
    -- the spell this quantity is evaluated against.
    TimesKickedWith (Cost.Cost Keyword.Keyword)
  | -- | CR 107.4h: 1 if mana carrying that production tag was spent to pay for
    -- the object this quantity is evaluated against, else 0.
    --
    -- Not the spent mana's COLOUR. Boreal Outrider's "if {S} of any of that
    -- spell's colors was spent to cast it" does ask that, which is a conjunction
    -- over one unit rather than a tag this atom could name (#2008).
    TagWasSpent ProductionTag.ProductionTag
  | -- | CR 111.6 / 608.2h: 1 if the object this quantity is evaluated against
    -- was a token, else 0.
    WasToken
  | -- | CR 509.1g / 608.2h: 1 if the object this quantity is evaluated against
    -- was blocking, else 0.
    WasBlocking
  | -- | CR 508.3b: how many of that player's opponents were declared attacked
    -- this combat (Combat.declaredAttacked) -- rule 702.121a's melee.
    --
    -- WHO attacked is not recorded and does not need to be: CR 506.2 makes the
    -- attacking player the active player, so one combat phase's record can only
    -- be that player's attacks. CR 805.10a's several attacking players are what
    -- would break that (#2848).
    OpponentsAttacked PlayerRef.PlayerRef
  | -- | CR 701.9a / 608.2i: how many cards that player discarded this turn,
    -- folded from the turn-scoped GameEvent.Discarded log.
    CardsDiscardedThisTurn PlayerRef.PlayerRef
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
    --
    -- "This turn" is a different measurement over the live log, wanted by
    -- Brightspear Zealot and Ertai's Scorn. Not implemented: no arm measures it
    -- (#2185).
    SpellsCastLastTurn PlayerRef.PlayerRef
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
  | -- | CR 607.2a: the inner quantity read against each card this quantity's
    -- source exiled, summed (CR 607.3); an empty pile reads 0.
    AgainstCardsExiledWith Quantity
  | -- | CR 702.184c: the tapped creature's power, or its toughness where that is
    -- greater and the ability's controller controls a permanent carrying
    -- Modification.GrantsStationToughness. Engine-only, a leaf like Power:
    -- Pawl.Engine.Keyword.station mints it and no card should author it.
    StationMeasure
  deriving (Eq, Ord, Show)
