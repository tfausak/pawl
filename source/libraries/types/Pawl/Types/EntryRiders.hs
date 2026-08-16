module Pawl.Types.EntryRiders where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TapState as TapState

-- | What an effect says about an object AS IT ARRIVES in a zone, beyond the
-- object's own text -- Hanweir Garrison's "that are tapped and attacking",
-- Meandering Towershell's "return it to the battlefield tapped and attacking",
-- Befriending the Moths' "return it to the battlefield transformed", and
-- Ignorant Bliss' "exile all cards from your hand face down".
--
-- Carried by the OPCODE (Create, MoveToZone) and not by the entering object,
-- because neither is one of its characteristics (CR 109.3, CR 111.3). Two tokens
-- with the same text can enter differently, and one printed card can be returned
-- tapped by one effect and untapped by another.
--
-- Each rider is meaningful only in the zone its own rule scopes it to, and every
-- other destination carries the default: `tapped`, `attacking`, `counters`,
-- `transformed` and `underOwner` are battlefield-only (CR 110.5d, CR 508.4, CR
-- 122.6a, CR 712.14a, CR 110.2a), `exiledFaceDown` is exile-only (CR 406.3),
-- and `faceDown` is battlefield-only (CR 708.3).
-- A card stating one on the wrong zone states something nothing reads, which
-- Pawl.CardSpec lints.
--
-- Independent riders, not one flag, because the rules make them independent.
-- Tapped is CR 110.5's status category, defaulted by CR 110.5b.
-- Attacking is not a status at all but combat state (CR 506.3, CR 508.4), and a
-- creature put onto the battlefield attacking is NOT tapped by that fact -- CR
-- 508.1f taps only creatures declared as attackers, and CR 508.4c exempts these
-- from declaration. Transformed is neither: CR 712.14a makes it a question of
-- WHICH FACE the card enters showing, which CR 712.14 otherwise answers with the
-- front one.
--
-- `attacking` is a Bool rather than an AttackTarget because the effect does not
-- say WHAT the creature attacks; whom it attacks is chosen as it enters, by
-- Pawl.Engine.Combat.putOntoBattlefieldAttacking. CR 508.4's parenthetical case
-- of an effect that does specify has no card in the pool.
--
-- `transformed` is a Bool rather than a face name for the reason CR 712.14a
-- states: "If a spell or ability puts a double-faced card onto the battlefield
-- 'transformed' or 'converted', it enters the battlefield with its BACK face
-- up." The effect names no face -- which face that is falls out of the card's
-- layout (Pawl.Engine.Card.backFace), and Pawl.Engine.Event.changeZoneEntering
-- is where the rule is applied. That is also what keeps this rider clear of CR
-- 712.11b's choice of face when casting a modal double-faced card: a choice is
-- offered to the player as a list of castable faces, while this is an
-- instruction the effect carries and the player is never asked about.
--
-- Meaningful only for a MoveToZone of a CARD, which is CR 712.14a's own scope --
-- a token is not a card (CR 111.1), and no rule puts a double-faced token onto
-- the battlefield transformed (CR 707.8a decides a copy token's face by copy
-- rules instead). So Pawl.Engine.Resolve's Create arm does not read this rider,
-- and Pawl.CardSpec's corpus lint holds that no Create in the pool sets it.
--
-- `counters` is CR 122.6a's "enters the battlefield with counters on it", said by
-- the EFFECT rather than by the permanent -- undying's and persist's "return it
-- to the battlefield ... with a +1/+1 counter on it" (CR 702.93a, CR 702.79a).
-- The same rider one opcode over is incubate's "create an Incubator token that
-- enters the battlefield with N +1/+1 counters on it" (CR 701.53a).
-- A rider and not an Effect.PutCounters afterwards, because the permanent must
-- never exist on the battlefield without them. So Pawl.Engine.Event places them
-- INSIDE whichever door the object arrives by: inside the CR 400.7 funnel for a
-- move, before its entry loop and before the Moved event, and for a batch of
-- created tokens after all of them are minted but before any entry loop, so CR
-- 614.12's reading of that batch sees them. Through Event.putCounters either way,
-- CR 122.6's funnel, so CR 614.16 applies and Doubling Season sees them -- the
-- posture EntryRewrite.WithCounters takes for the counters a permanent's OWN text
-- asks for.
--
-- READ BY BOTH opcodes, unlike `transformed` above: a Create hands it to
-- Event.createTokens and a MoveToZone to Event.changeZoneEntering.
-- Pawl.ReplacementSpec's Eyes of Gitaxias group proves the Create road, where a
-- token created with three +1/+1 counters on it takes six from Vorinclex.
--
-- A Map by kind, Object.counters' shape, rather than WithCounters' one kind and
-- one count: nothing in CR 122.6a limits an effect to a single kind, and empty is
-- the default every other move carries.
--
-- A literal count per kind rather than a Quantity, which is what bounds the
-- wordings this can carry: every card that prints incubate prints a number, and
-- undying and persist mint a one.
--
-- CR 122.6a's "may specify which player puts those counters on it" is not
-- carried. No effect in the pool names one, and the rule's own default -- the
-- object's controller -- is what putCounters already uses.
--
-- Not implemented: a count that is not a literal, so Printlifter Ooze's "the token
-- enters with X +1/+1 counters on it, where X is the number of other creatures you
-- control" is unsayable (#1256).
--
-- `underOwner` is CR 110.2a's "unless the effect states otherwise". Undying and
-- persist return the permanent "under its OWNER's control", where CR 110.2a
-- otherwise hands it to the player the effect instructed -- the ability's
-- controller, which for a dies trigger is whoever controlled the permanent as it
-- left (CR 603.3a) and need not be the owner. A Bool and not a PlayerId, because
-- the two readings are "the effect's controller" and "the owner", both of which
-- the funnel already knows; a card cannot write a PlayerId anyway. Inert under a
-- Create, and correctly so: CR 111.2 makes a token's owner the player who created
-- it, which is who CR 110.2a hands it to regardless.
-- `exiledFaceDown` is CR 406.3's "cards 'exiled face down'", said by the EFFECT
-- that does the exiling -- Ignorant Bliss' "exile all cards from your hand face
-- down" -- against that rule's face-up default. A rider and not a second write
-- afterwards for `tapped`'s reason: the card must never sit face up in exile for
-- an instant, where the Moved event and any CR 616.1 watcher would read it.
--
-- A rider on the MOVE and not a status on the card, because CR 110.5d denies an
-- exiled card status at all; Object.exiledFaceDown says what it is instead.
--
-- `faceDown` is CR 708.3's "objects that are put onto the battlefield face
-- down", said by the EFFECT that does the putting -- Soul Summons' "manifest the
-- top card of your library" (CR 701.40a) -- against CR 110.5b's face-up default.
-- A rider and not a write after the move for `exiledFaceDown`'s reason and one
-- of its own: CR 708.3 says the object is turned face down BEFORE it enters, so
-- the CR 614.1c entry loop, the CR 603.2g Moved event and every trigger scanning
-- it must all see a permanent that already has CR 708.2a's characteristics. A
-- permanent turned face down after arriving would have fired its own
-- enters-the-battlefield trigger on the way in, which is the one thing the rule
-- exists to forbid.
--
-- A Bool, so it always writes CR 708.2a's characteristics. CR 708.2a's "unless
-- otherwise specified by the effect that put it onto the battlefield face down"
-- -- an entry that LISTS its own, as Yedora, Grave Gardener and Missy do -- is
-- not implemented (#1668).
--
-- A SECOND rider beside `exiledFaceDown` and not the same one widened, which is
-- CR 110.5d in as many words: an exiled card's face-downness "has no correlation
-- to the face-down status of a permanent". This one writes Object.facing, which
-- CR 708.2a spends on a wholesale substitution of characteristics; that one
-- writes Object.exiledFaceDown, which is about who may look. Their zones are
-- disjoint, so no move reads both.
--
-- Applied by Pawl.Engine.Event.changeZoneEntering, which is also where the
-- battlefield gate lives -- a card stating it on a move anywhere else says
-- something no rule reads, which Pawl.CardSpec lints.
--
-- Read by MoveToZone alone, like `transformed`: a token is created face up and
-- no rule puts one onto the battlefield face down, so Pawl.Engine.Resolve's
-- Create arm does not read it and the same CardSpec lint holds that no Create in
-- the pool sets it.
--
-- Not implemented: manifested-ness as state (CR 701.40a's "that permanent is a
-- manifested permanent"), so a permanent put onto the battlefield face down by
-- this rider cannot be turned face up for its mana cost (#1540). A Bool and not
-- a choice of listed characteristics, which is what CR 701.58a's cloak would
-- need -- a 2/2 with ward {2} -- and that second face is #922.
data EntryRiders = MkEntryRiders
  { tapped :: TapState.TapState,
    attacking :: Bool,
    transformed :: Bool,
    counters :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural,
    underOwner :: Bool,
    exiledFaceDown :: Bool,
    faceDown :: Bool
  }
  deriving (Eq, Ord, Show)
