module Pawl.Types.Pool where

-- CR 115: the closed set of recipient kinds a target slot may draw from, fixing
-- both WHICH objects are candidates and HOW they are referenced
-- (Recipient.ToCreature / ToPlaneswalker / ToPlayer / ToObject). Closed-half
-- vocabulary, like the old TargetSpec enum -- it grows only when the rules define
-- a new kind of targetable object, never per card.
data Pool
  = Creatures -- CR 115.1a: creatures on the battlefield (ToCreature).
  | Players -- CR 115: players still in the game (ToPlayer).
  | -- CR 115.4 names creatures, players, planeswalkers AND battles; battles are
    -- the one it does not admit (#302).
    AnyTarget
  | Permanents -- CR 110.1: permanents on the battlefield (ToObject).
  | Spells -- CR 112.1: spells on the stack (ToObject).
  | -- CR 113.9: activated and triggered abilities on the stack (ToObject) --
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
  deriving (Eq, Ord, Show)
