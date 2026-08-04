module Pawl.Types.Pool where

import qualified Pawl.Types.PlayerScope as PlayerScope

-- | CR 115: the closed set of recipient kinds a target slot may draw from, fixing
-- both WHICH objects are candidates and HOW they are referenced
-- (Recipient.ToCreature / ToPlaneswalker / ToPlayer / ToObject). Closed-half
-- vocabulary, like the old TargetSpec enum -- it grows only when the rules define
-- a new kind of targetable object, never per card.
data Pool
  = Creatures -- CR 115.1a: creatures on the battlefield (ToCreature).
  | Players -- CR 115: players still in the game (ToPlayer).
  | -- | CR 115.4 names creatures, players, planeswalkers AND battles; battles are
    -- the one it does not admit (#302).
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
    -- The PlayerScope is the axis no battlefield pool needs and this one cannot
    -- do without: CR 400.1 gives each player their own graveyard, so "your
    -- graveyard" (Raise Dead), "an opponent's" and "a graveyard" (Withered
    -- Wretch) are different candidate sets. It cannot be pushed down into the
    -- Filter, because CR 108.4 gives a card in a graveyard no controller at all
    -- -- Filter.ControlledBy is vacuously False for every one of them.
    --
    -- PlayerScope, and NOT Pawl.Types.PlayerRef, though that type also has an
    -- every-player arm: PlayerRef's third arm is InSlot, and a target pool is
    -- the one place it cannot be resolved -- CR 601.2c (and CR 602.2b for an
    -- ability) reads this pool while the slots are still being FILLED, so
    -- nothing is bound yet.
    --
    -- No pool reaches a hand or a library, so a graveyard and exile below are
    -- the only other zones clause (a) can name here (#559).
    CardsInGraveyard PlayerScope.PlayerScope
  | -- | CR 406.1: the cards in the exile zone (ToObject) -- Riftsweeper's "choose
    -- target face-up exiled card". Clause (a)'s other-zone half again. Public
    -- (CR 400.2), so every candidate is visible to the chooser.
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
    -- "FACE-UP" is not modelled, and is vacuous rather than elided: CR 406.3
    -- keeps exiled cards face up by default, and no card in pawl's pool exiles
    -- anything face down (#557).
    CardsInExile
  deriving (Eq, Ord, Show)
