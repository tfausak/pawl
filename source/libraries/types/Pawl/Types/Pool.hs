module Pawl.Types.Pool where

import qualified Pawl.Types.GraveyardScope as GraveyardScope

-- | CR 115: the closed set of recipient kinds a target slot may draw from, fixing
-- both WHICH objects are candidates and HOW they are referenced
-- (Recipient.ToCreature / ToPlaneswalker / ToBattle / ToPlayer / ToObject).
-- Closed-half vocabulary, like the hand-carved target enum TargetSlot retired
-- (#40): it grows when the rules admit a new kind of targetable object, or a new
-- ZONE for one -- never with a card's own restriction, which is the Filter's job.
-- A union of two admitted kinds is the one further shape a card can force, since
-- CR 601.2c fixes one count per slot and a slot has one pool; SpellsAndPermanents
-- and CreaturesAndCardsInGraveyard are those.
data Pool
  = Creatures -- CR 115.1a: creatures on the battlefield (ToCreature).
  | Players -- CR 115: players still in the game (ToPlayer).
  | -- | CR 115.4: "target creature, player, planeswalker, or battle" -- all four,
    -- and nothing else. "Other game objects, such as noncreature artifacts or
    -- spells, can't be chosen."
    AnyTarget
  | Permanents -- CR 110.1: permanents on the battlefield (ToObject).
  | Spells -- CR 112.1: spells on the stack (ToObject).
  | -- | CR 113.9: activated and triggered abilities on the stack (ToObject) --
    -- Stifle's "target activated or triggered ability". CR 115.2 clause (b) is
    -- what lets a slot name one at all.
    --
    -- A SIBLING of Spells rather than a widening of it, and rule 113.9 is why:
    -- an ability on the stack is not a spell, so a Cancel must not reach an
    -- ability and a Stifle must not reach a spell -- two pools, neither
    -- containing the other. That rule's last sentence disposes of the remaining
    -- kind, since a static ability never uses the stack.
    --
    -- A MANA ability is never a candidate, and nothing filters for one, because
    -- no mana ability is ever on the stack: CR 605.3b and CR 605.4a resolve one
    -- immediately rather than putting it there, and
    -- Pawl.Engine.Activate.activatable already excludes it (CR 605.1a).
    Abilities
  | SpellsAndPermanents -- CR 115: spells on the stack + battlefield permanents (ToObject).
  | -- | CR 404.1: the cards in a graveyard (ToObject) -- Raise Dead's "target
    -- creature card in your graveyard". CR 115.2's OTHER escape hatch, the one
    -- Spells and Abilities above are not: those are its clause (b), and Players
    -- is already clause (a)'s "or a player" half, so this is the first pool
    -- admitted by clause (a)'s OTHER-ZONE half. (Those letters are prose inside
    -- rule 115.2, not subrule numbers -- there is no CR 115.2a.)
    --
    -- ToObject, not ToCreature, because the candidates are CARDS: CR 109.2's
    -- battlefield default does not apply to text that says the word "card"
    -- outright. So "creature" here is a Filter (HasCardType Creature) over an
    -- untagged card, exactly as it is for Permanents, and the pool is DISJOINT
    -- from Creatures rather than a widening of it.
    --
    -- The GraveyardScope is the axis no battlefield pool needs and this one
    -- cannot do without: CR 400.1 gives each player their own graveyard, so
    -- "your graveyard" (Raise Dead), "an opponent's", "a graveyard" (Withered
    -- Wretch) and "their graveyard" (Dwell on the Past) are different candidate
    -- sets. It cannot be pushed down into the Filter, because CR 108.4 gives a
    -- card in a graveyard no controller at all -- Filter.ControlledBy is
    -- vacuously False for every one of them.
    --
    -- No pool reaches a hand or a library, so a graveyard and exile below are
    -- the only other zones clause (a) can name here (#559).
    CardsInGraveyard GraveyardScope.GraveyardScope
  | -- | CR 406.1: the cards in the exile zone (ToObject) -- Riftsweeper's "choose
    -- target face-up exiled card". Clause (a)'s other-zone half again. Public
    -- (CR 400.2), so every candidate is visible to the chooser -- that rule's
    -- own exception for the cards an effect exiles face down being what the
    -- last paragraph here disposes of.
    --
    -- NO PlayerScope, and the asymmetry with CardsInGraveyard above is the whole
    -- point: CR 400.1 makes exile a SHARED zone, so there is no per-player copy
    -- of it for a scope to select among. A CardsInExile PlayerScope would be
    -- filtering candidates by their OWNER while wearing a type that reads as
    -- naming a zone; owner-filtering is a Filter's job if a card ever asks.
    --
    -- DISJOINT from every battlefield pool, and from CardsInGraveyard, for the
    -- reason that pool's own note gives: the candidates are CARDS in another
    -- zone, and CR 109.2's battlefield default is switched off by the card's own
    -- word.
    --
    -- "FACE-UP" is not a Filter and does not need to be: CR 406.4 lets a player
    -- choose a specific face-down exiled card only if they are allowed to look
    -- at it, and nothing grants that permission, so
    -- Pawl.Engine.Target.exileRecipients leaves every face-down card out of the
    -- pool for EVERY card that names one. Riftsweeper's qualifier is then
    -- redundant rather than vacuous: Ignorant Bliss really does put cards into
    -- exile that Riftsweeper may not name.
    CardsInExile
  | -- | CR 115.2 clause (a) exercised TWICE in one slot -- Savior of Ollenbock's
    -- "up to one other target creature from the battlefield or creature card from
    -- a graveyard". SpellsAndPermanents' shape across two zones instead of two
    -- kinds: the union of Creatures and CardsInGraveyard above, tagged as each of
    -- those pools tags its own members (ToCreature on the battlefield, ToObject in
    -- a graveyard).
    --
    -- ONE slot and not two, because the printed count covers both halves at once:
    -- two slots would let a card exile one of each where the card allows one
    -- altogether, and CR 601.2c fixes a count per slot.
    --
    -- The Filter is asked of both halves, which is what the printed words do too
    -- -- "creature" of the battlefield half under CR 109.2, "creature card" of the
    -- graveyard half under CR 109.2a. A filter that can only hold of one zone's
    -- members simply empties the other half.
    --
    -- Carries CardsInGraveyard's GraveyardScope for that pool's reason (CR 400.1's
    -- per-player graveyards) and for no other: the battlefield half needs none.
    CreaturesAndCardsInGraveyard GraveyardScope.GraveyardScope
  deriving (Eq, Ord, Show)
