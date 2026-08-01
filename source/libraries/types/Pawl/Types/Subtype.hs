module Pawl.Types.Subtype where

-- Grows: other land types, other creature types, …
data Subtype
  = Mountain
  | Swamp
  | Forest
  | Island
  | Plains
  | Goblin
  | Warrior
  | Human
  | Bird
  | Ogre
  | Centaur
  | Cat
  | Dinosaur
  | Beast
  | Rat
  | Elephant
  | Myr
  | Skeleton
  | Wall -- CR 205.3m (a creature type)
  | Wizard -- CR 205.3m (a creature type)
  | Shapeshifter -- CR 205.3m (a creature type; Clone's printed type)
  | Lhurgoyf -- CR 205.3m (a creature type; Tarmogoyf's printed type)
  | Arcane -- CR 205.3k (a spell type; Inner Calm, Outer Strength's)
  | Barbarian -- CR 205.3m (a creature type; Barbarian Outcast's)
  | Zombie -- CR 205.3m (a creature type; Khabál Ghoul's and Sarcomancy's token's)
  | Fungus -- CR 205.3m (a creature type; Corpsejack Menace's)
  | Elemental -- CR 205.3m (a creature type; Primal Plasma's)
  | Rogue -- CR 205.3m (a creature type; Master Thief's)
  | Hag -- CR 205.3m (a creature type; Hag of Inner Weakness's)
  | Warlock -- CR 205.3m (a creature type; Hag of Inner Weakness's)
  | Soldier -- CR 205.3m (a creature type; Thalia, Guardian of Thraben's)
  | Phyrexian -- CR 205.3m (a creature type; Glistener Elf's)
  | Elf -- CR 205.3m (a creature type; Glistener Elf's)
  | Nightmare -- CR 205.3m (a creature type; Nightmare's own)
  | Horse -- CR 205.3m (a creature type; Nightmare's)
  | -- CR 205.3h: an ENCHANTMENT type. Appended rather than grouped with the other
    -- enchantment types, of which this pool has none, so every existing card's
    -- Ord-canonical subtype list is unchanged (card JSON stores subtypes in
    -- declaration order, and a whole-pool test compares against it).
    Aura
  | -- CR 301.5: an artifact subtype. "An Equipment can be attached to a
    -- creature. It can't legally be attached to anything that isn't a
    -- creature." Appended at the END, never inserted, because Ord here is
    -- declaration order and the corpus stores each card's subtypes in
    -- Ord-canonical order -- inserting reorders existing cards' lists and fails
    -- the whole-pool test. Every constructor after this one obeys the same rule.
    Equipment
  | -- CR 205.3m (a creature type; Branchblight Stalker's).
    Scout
  | -- CR 205.3m (a creature type; Skilled Animator's).
    Artificer
  | -- CR 205.3m (a creature type; Uthden Troll's).
    Troll
  | -- CR 205.3m (a creature type; Ghitu Fire-Eater's).
    Nomad
  | -- CR 205.3m (a creature type; Burning-Tree Emissary's).
    Shaman
  | -- CR 205.3m (a creature type; Master of the Feast's).
    Demon
  | -- CR 205.3m (a creature type; Edgewalker's own, and the tribe its cost
    -- reduction names).
    Cleric
  | -- CR 205.3m (a creature type; Narcomoeba's).
    Illusion
  | -- CR 205.3m (a creature type; the token Doomed Traveler creates).
    Spirit
  | -- CR 205.3m (a creature type; Aurelia, the Warleader's).
    Angel
  | -- CR 205.3m (a creature type; Endless Cockroaches').
    Insect
  | -- CR 205.3m (a creature type; Deathknell Berserker's own, and its token's).
    Berserker
  | -- CR 205.3m (a creature type; Spined Thopter's).
    Thopter
  | -- CR 205.3m (a creature type; Moltensteel Dragon's).
    Dragon
  | -- CR 205.3m (a creature type; Prized Unicorn's).
    Unicorn
  | -- CR 205.3h: an enchantment type, the second after Aura (Curse of Death's
    -- Hold's). Purely descriptive today -- no rule and no card in this pool asks
    -- whether a permanent is a Curse -- so it exists to make the printed type
    -- line faithful, the way Nightmare's Horse does.
    Curse
  | -- CR 205.3i: a LAND type, and the first NONBASIC one here -- Desert's own
    -- ("Land -- Desert"). The five land types above it are all basic (CR 305.6),
    -- so this is the constructor that pulls Pawl.Engine.Subtype.isLandType and
    -- Pawl.Engine.Mana.subtypeMana apart: a land type that grants no intrinsic mana
    -- ability. Appended at the END for the Ord-canonical reason Equipment states.
    Desert
  | -- CR 205.3m (a creature type; Bitterblossom's, and its token's). The first
    -- subtype in this type to sit on something that is not a creature: CR 308.2
    -- makes the kindred subtypes the same set as the creature subtypes, so
    -- Bitterblossom is a Kindred Enchantment -- Faerie.
    Faerie
  | -- CR 205.3m (a creature type; Stonehorn Dignitary's).
    Rhino
  | -- CR 205.3j: a PLANESWALKER type -- "Planeswalker subtypes are always a
    -- single word and are listed after a long dash" (CR 306.3). Jace Beleren's.
    -- The first subtype in this type that belongs to neither a creature, a land,
    -- an artifact nor an enchantment, which is why Pawl.Engine.Subtype.isLandType
    -- and Pawl.Engine.Mana.subtypeMana both answer it negatively.
    Jace
  | -- CR 205.3m (a creature type; Bog Wraith's).
    Wraith
  | -- CR 205.3m (a creature type; Icehide Golem's).
    Golem
  | -- CR 205.3m (a creature type; Meandering Towershell's).
    Turtle
  | -- CR 205.3m (a creature type; Blurred Mongoose's).
    Mongoose
  deriving (Eq, Ord, Show)
