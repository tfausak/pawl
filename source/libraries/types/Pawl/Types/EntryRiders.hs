module Pawl.Types.EntryRiders where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TapState as TapState

-- | What an effect says about a permanent AS IT ENTERS the battlefield, beyond the
-- permanent's own text -- Hanweir Garrison's "that are tapped and attacking",
-- Meandering Towershell's "return it to the battlefield tapped and attacking",
-- and Befriending the Moths' "return it to the battlefield transformed".
--
-- Carried by the OPCODE (Create, MoveToZone) and not by the entering object,
-- because neither is one of its characteristics (CR 109.3, CR 111.3). Two tokens
-- with the same text can enter differently, and one printed card can be returned
-- tapped by one effect and untapped by another.
--
-- Meaningful only for a BATTLEFIELD entry (CR 110.5d, CR 712.14a); a MoveToZone
-- naming any other zone carries the default and no rider is applied to it.
--
-- Independent riders, not one flag, because the rules make them
-- independent. Tapped is CR 110.5's status category, defaulted by CR 110.5b.
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
-- A rider and not an Effect.PutCounters after the move, because the permanent
-- must never exist on the battlefield without them: Pawl.Engine.Event places them
-- inside the CR 400.7 funnel, before the entry loop and before the Moved event.
-- Through Event.putCounters there, CR 122.6's funnel, so CR 614.16 applies and
-- Doubling Season sees them -- the posture EntryRewrite.WithCounters takes for
-- the counters a permanent's OWN text asks for.
--
-- A Map by kind, Object.counters' shape, rather than WithCounters' one kind and
-- one count: nothing in CR 122.6a limits an effect to a single kind, and empty is
-- the default every other move carries.
--
-- CR 122.6a's "may specify which player puts those counters on it" is not
-- carried. No effect in the pool names one, and the rule's own default -- the
-- object's controller -- is what putCounters already uses.
--
-- Not implemented: an Effect.Create does not read this field, so a token minted
-- with counters on it would arrive bare (#1189). `underOwner` under a Create
-- needs nothing: CR 111.2 makes a token's owner the player who created it, which
-- is who CR 110.2a hands it to anyway.
--
-- `underOwner` is CR 110.2a's "unless the effect states otherwise". Undying and
-- persist return the permanent "under its OWNER's control", where CR 110.2a
-- otherwise hands it to the player the effect instructed -- the ability's
-- controller, which for a dies trigger is whoever controlled the permanent as it
-- left (CR 603.3a) and need not be the owner. A Bool and not a PlayerId, because
-- the two readings are "the effect's controller" and "the owner", both of which
-- the funnel already knows; a card cannot write a PlayerId anyway.
data EntryRiders = MkEntryRiders
  { tapped :: TapState.TapState,
    attacking :: Bool,
    transformed :: Bool,
    counters :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural,
    underOwner :: Bool
  }
  deriving (Eq, Ord, Show)
