module Pawl.Types.CardType where

-- CR 205.2a lists fifteen card types. The seven missing ones are Battle (#302)
-- and the six command-zone residents in #131 -- Dungeon, Plane, Phenomenon,
-- Vanguard, Scheme and Conspiracy.
--
-- Ord here is DECLARATION ORDER, and a card file stores its type set in that
-- order (Pawl.CardsSpec's whole-pool round trip compares the arrays), so a new
-- constructor is APPENDED at the end -- inserting one reorders the type lists of
-- every multi-type card already in the corpus (darksteel-myr.json's
-- ["Creature", "Artifact"] is the shape that would move). Pawl.Types.Subtype's
-- Equipment comment states the same rule for that type.
data CardType
  = Land
  | Creature
  | Instant
  | -- CR 110.4: one of the six permanent types. Humility is the first
    -- enchantment printing (M3b).
    Enchantment
  | -- CR 301: an artifact, a permanent type. Mindslaver is the first (M3g).
    Artifact
  | -- CR 307: a sorcery, cast only at sorcery speed (not a permanent). Blaze is
    -- the first sorcery printing (M4a).
    Sorcery
  | -- CR 308.1: a kindred card. "Each kindred card has another card type.
    -- Casting and resolving a kindred card follows the rules for casting and
    -- resolving a card of the other card type" -- so this arm adds no casting or
    -- resolution path of its own, and CR 110.4 leaves kindred off the six
    -- permanent types for the same reason ("some kindred cards can enter the
    -- battlefield and some can't, depending on their other card types").
    --
    -- The one observable thing it brings is CR 308.2: kindred subtypes ARE the
    -- creature subtypes, so a noncreature card can carry a creature type.
    -- Bitterblossom is the first printing (a Kindred Enchantment -- Faerie), and
    -- Pawl.TriggerSpec's Kindred group is where that is proved. CR 308.3's
    -- "tribal" cards need no second name here: they are errata'd to kindred in
    -- the Oracle reference, which is what the card files transcribe.
    Kindred
  | -- CR 306: a planeswalker, one of CR 110.4's six permanent types. Jace
    -- Beleren is the first printing.
    --
    -- The card type is what the rest of CR 306 hangs off, and every clause of it
    -- is a closed-half read of this constructor rather than of any effect: CR
    -- 306.5b's intrinsic "enters with loyalty counters" replacement
    -- (Pawl.Engine.Projection.intrinsicReplacementsOf), CR 606's loyalty
    -- abilities (Pawl.Engine.Cost.isLoyaltyCost) and CR 704.5i's zero-loyalty
    -- state-based action (Pawl.Engine.Sba.zeroLoyalty).
    --
    -- Two clauses of CR 306 need nothing built. CR 306.4's planeswalker
    -- uniqueness rule "has been removed and planeswalker cards printed before
    -- this change have received errata in the Oracle card reference to have the
    -- legendary supertype", so Pawl.Engine.Sba's CR 704.5j legend rule already
    -- covers it. CR 306.7's damage redirection "has been removed" outright.
    Planeswalker
  deriving (Eq, Ord, Show)
