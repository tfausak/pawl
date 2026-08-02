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
    -- Stifle's "target activated or triggered ability". CR 115.2 is what lets a
    -- slot name one at all: "only permanents are legal targets for spells and
    -- abilities, unless a spell or ability ... (b) targets an object that can't
    -- exist on the battlefield, such as a spell or ability."
    --
    -- A SIBLING of Spells rather than a widening of it, and rule 113.9 is why:
    -- "Activated and triggered abilities on the stack aren't spells, and
    -- therefore can't be countered by anything that counters only spells.
    -- Activated and triggered abilities on the stack can be countered by effects
    -- that specifically counter abilities." So a Cancel must not reach an
    -- ability and a Stifle must not reach a spell -- two pools, neither
    -- containing the other. (That rule's last sentence disposes of the remaining
    -- kind: "static abilities don't use the stack and thus can't be countered at
    -- all", so there is nothing here for CR 113.3d to admit.)
    --
    -- A MANA ability is never a candidate, and nothing filters for one, because
    -- no mana ability is ever on the stack to be a candidate: CR 605.3b says "an
    -- activated mana ability doesn't go on the stack, so it can't be targeted,
    -- countered, or otherwise responded to. Rather, it resolves immediately
    -- after it is activated", and CR 605.4a says the same of a triggered mana
    -- ability. Stifle's parenthetical "(Mana abilities can't be targeted.)" is
    -- reminder text for those two rules rather than a restriction of its own,
    -- and pawl honours them upstream: Pawl.Engine.Activate.activatable -- which
    -- is what offers an activation at all -- excludes a mana ability outright
    -- (Mana.isManaAbility, CR 605.1a), so one never reaches the stack to be
    -- enumerated here.
    Abilities
  | SpellsAndPermanents -- CR 115: spells on the stack + battlefield permanents (ToObject).
  | -- | CR 404.1: the cards in a graveyard (ToObject) -- Raise Dead's "target
    -- creature card in your graveyard". CR 115.2's OTHER escape hatch, the one
    -- Spells and Abilities above are not: "only permanents are legal targets for
    -- spells and abilities, unless a spell or ability (a) SPECIFIES THAT IT CAN
    -- TARGET AN OBJECT IN ANOTHER ZONE or a player, or (b) targets an object that
    -- can't exist on the battlefield". Those two are clause (b), and Players is
    -- already clause (a)'s "or a player" half; this is the first pool admitted by
    -- clause (a)'s OTHER-ZONE half, and so the first that reaches past the
    -- battlefield and the stack. (Those letters are prose inside rule 115.2, not
    -- subrule numbers -- there is no CR 115.2a.)
    --
    -- ToObject, not ToCreature, because the candidates are CARDS: CR 109.2's
    -- battlefield default ("doesn't refer to a specific zone or include the word
    -- 'card' ... means a permanent of that card type ... on the battlefield")
    -- does not apply to text that says the word outright. So "creature" here is a
    -- Filter (HasCardType Creature) over an untagged card, exactly as it is for
    -- Permanents, and the pool is DISJOINT from Creatures rather than a widening
    -- of it -- the same relation Abilities has to Spells.
    --
    -- The PlayerScope is the axis no battlefield pool needs and this one cannot
    -- do without: CR 400.1 says "each player has their own library, hand, and
    -- graveyard", so "your graveyard" (Raise Dead), "an opponent's" and "a
    -- graveyard" (Withered Wretch's "exile target card from a graveyard") are
    -- different candidate sets. It cannot be pushed down into the Filter, because
    -- CR 108.4 says "a card doesn't have a controller unless that card represents
    -- a permanent or spell" -- so Filter.ControlledBy is vacuously False for
    -- every card in every graveyard, and a pool that ignored whose graveyard it
    -- was would offer the whole table's.
    --
    -- PlayerScope, and NOT Pawl.Types.PlayerRef, though that type also has an
    -- every-player arm: PlayerRef's third arm is InSlot, and a target pool is the
    -- one place it cannot be resolved. CR 601.2c is where this set is enumerated
    -- -- "the player announces their choice of an appropriate object or player
    -- for each target" -- so the slots are being FILLED as the pool is read and
    -- nothing is bound yet. That is the ability's moment too, via CR 602.2b: "the
    -- remainder of the process for activating an ability is identical to the
    -- process for casting a spell listed in rules 601.2b-i". A scope is a set of
    -- players and nothing else, which is exactly what a graveyard fold needs.
    --
    -- No pool reaches a hand or a library, so a graveyard and exile below are
    -- the only other zones clause (a) can name here (#559).
    CardsInGraveyard PlayerScope.PlayerScope
  | -- | CR 406.1: the cards in the exile zone (ToObject) -- Riftsweeper's "choose
    -- target face-up exiled card". Clause (a)'s other-zone half again, and the
    -- second pool to leave the battlefield and the stack behind. Public, so every
    -- candidate is visible to the chooser: CR 400.2 lists "graveyard,
    -- battlefield, stack, exile, ante, and command" as the public zones.
    --
    -- NO PlayerScope, and the asymmetry with CardsInGraveyard above is the whole
    -- point. CR 400.1: "each player has their own library, hand, and graveyard.
    -- THE OTHER ZONES ARE SHARED BY ALL PLAYERS." Exile is one of the shared
    -- ones, so there is no per-player copy of it for a scope to select among; a
    -- CardsInExile PlayerScope would be filtering candidates by their OWNER
    -- while wearing a type that reads as naming a zone. Owner-filtering is a
    -- Filter's job if a card ever asks for it, and the pool would still be this
    -- one.
    --
    -- DISJOINT from every battlefield pool, and from CardsInGraveyard, for the
    -- reason that pool's own note gives: the candidates are CARDS in another
    -- zone, and CR 109.2's battlefield default ("doesn't refer to a specific
    -- zone or include the word 'card'") is switched off by the card's own word.
    --
    -- "FACE-UP" is not modelled, and is vacuous rather than elided: CR 406.3
    -- says "exiled cards are, BY DEFAULT, kept face up", and no card in pawl's
    -- pool exiles anything face down, so every card this arm can offer is face
    -- up already (#557).
    CardsInExile
  deriving (Eq, Ord, Show)
