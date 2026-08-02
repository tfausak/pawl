-- Classifications over Pawl.Types.Subtype: which of CR 205.3's disjoint subtype
-- families a subtype belongs to. The rulebook owns these lists outright (CR
-- 205.3i names the land types by name), so casing on a Subtype here is the same
-- kind of act as casing on a Phase -- it is a subtype's IDENTITY, never an
-- effect's.
module Pawl.Engine.Subtype where

import qualified Pawl.Types.Subtype as Subtype

-- CR 205.3i: "Lands have their own unique set of subtypes; these subtypes are
-- called land types. The land types are Cave, Desert, Forest, Gate, Island,
-- Lair, Locus, Mine, Mountain, Plains, Planet, Power-Plant, Sphere, Swamp,
-- Tower, Town, and Urza's."
--
-- CR 205.3c gives each subtype exactly one card type it is "correlated to", and
-- CR 205.3d forbids gaining one that does not match, so the families named by CR
-- 205.3g-205.3m are disjoint: a subtype is a land type or it is not. Every other
-- constructor is therefore a False a new subtype must decide deliberately --
-- hence total, with no wildcard.
--
-- Six of CR 205.3i's seventeen exist in this type today. Five of them are the
-- BASIC land types (CR 305.6) and Desert is not, which is what keeps this
-- function distinct from Pawl.Engine.Mana.subtypeMana: that one asks the CR 305.6
-- question ("which mana does this basic land type make?") and answers Nothing
-- for Desert, while this one answers True.
isLandType :: Subtype.Subtype -> Bool
isLandType subtype = case subtype of
  Subtype.Mountain -> True
  Subtype.Swamp -> True
  Subtype.Forest -> True
  Subtype.Island -> True
  Subtype.Plains -> True
  Subtype.Desert -> True
  -- CR 205.3m: creature types.
  Subtype.Goblin -> False
  Subtype.Warrior -> False
  Subtype.Human -> False
  Subtype.Bird -> False
  Subtype.Ogre -> False
  Subtype.Centaur -> False
  Subtype.Cat -> False
  Subtype.Dinosaur -> False
  Subtype.Beast -> False
  Subtype.Rat -> False
  Subtype.Elephant -> False
  Subtype.Myr -> False
  Subtype.Skeleton -> False
  Subtype.Wall -> False
  Subtype.Wizard -> False
  Subtype.Shapeshifter -> False
  Subtype.Lhurgoyf -> False
  -- CR 205.3k: a spell type.
  Subtype.Arcane -> False
  Subtype.Barbarian -> False
  Subtype.Zombie -> False
  Subtype.Fungus -> False
  Subtype.Elemental -> False
  Subtype.Rogue -> False
  Subtype.Hag -> False
  Subtype.Warlock -> False
  Subtype.Soldier -> False
  Subtype.Phyrexian -> False
  Subtype.Elf -> False
  Subtype.Nightmare -> False
  Subtype.Horse -> False
  -- CR 205.3h: an enchantment type.
  Subtype.Aura -> False
  -- CR 301.5: an artifact type.
  Subtype.Equipment -> False
  Subtype.Scout -> False
  Subtype.Artificer -> False
  Subtype.Troll -> False
  Subtype.Nomad -> False
  Subtype.Shaman -> False
  Subtype.Demon -> False
  Subtype.Cleric -> False
  Subtype.Illusion -> False
  Subtype.Spirit -> False
  Subtype.Angel -> False
  Subtype.Insect -> False
  Subtype.Berserker -> False
  Subtype.Thopter -> False
  Subtype.Dragon -> False
  Subtype.Unicorn -> False
  -- CR 205.3h: an enchantment type.
  Subtype.Curse -> False
  -- CR 205.3m: a creature type -- and, per CR 308.2, a kindred subtype too,
  -- since those are the same set. Still not a land type either way, which is the
  -- only question this function asks.
  Subtype.Faerie -> False
  Subtype.Rhino -> False
  -- CR 205.3j: a planeswalker type. CR 205.3c -- "each subtype is correlated to
  -- its appropriate card type" -- is why it is never a land type.
  Subtype.Jace -> False
  -- CR 205.3m: a creature type (Bog Wraith's). Not a land type -- which is worth
  -- saying out loud here, because Bog Wraith is also the pool's first card with
  -- landwalk, and the land type its keyword NAMES is Swamp rather than this.
  Subtype.Wraith -> False
  -- CR 205.3m: a creature type (Icehide Golem's), even though the card is an
  -- artifact too -- CR 205.3g's artifact types are Equipment and the rest, and
  -- Golem is not one of them. Not a land type either way.
  Subtype.Golem -> False
  -- CR 205.3m: a creature type (Meandering Towershell's). Not a land type --
  -- worth saying out loud for the same reason Wraith is: the Towershell prints
  -- ISLANDWALK, and the land type its keyword names is Island rather than this.
  Subtype.Turtle -> False
  -- CR 205.3m: a creature type (Blurred Mongoose's).
  Subtype.Mongoose -> False
  -- CR 205.3m: a creature type (the one Turn to Frog sets). Not a land type,
  -- which is exactly what isCreatureType below is for: setting a creature type
  -- must leave an animated permanent's land type standing.
  Subtype.Frog -> False
  -- CR 205.3m: a creature type (Child of Night's). Not a land type.
  Subtype.Vampire -> False
  Subtype.Dryad -> False
  -- CR 205.3m: a creature type (Hero of Bladehold's). Not a land type.
  Subtype.Knight -> False

-- CR 205.3m: "Creatures and kindreds share their lists of subtypes; these
-- subtypes are called creature types." The rulebook owns that list by name, so
-- this is the same kind of classification isLandType is, and it answers CR
-- 205.1a's question for the layer-4 arm that SETS one: "when an effect sets one
-- or more of an object's subtypes, the new subtype(s) replaces any existing
-- subtypes from the appropriate set (creature types, land types, artifact types,
-- enchantment types, planeswalker types, or spell types)". The appropriate set
-- for Modification.SetCreatureSubtype is the True arms below, and nothing else
-- may be swept away with them.
--
-- CR 205.3c/205.3d make the families disjoint, exactly as isLandType's comment
-- says, so this is the complement of the five non-creature families rather than
-- an independent judgement -- and it is total, with no wildcard, so a new
-- subtype has to decide.
--
-- CR 308.2 makes the KINDRED subtypes the same set, which is why a Kindred
-- Enchantment -- Faerie answers True here: Faerie is a creature type wherever it
-- appears.
isCreatureType :: Subtype.Subtype -> Bool
isCreatureType subtype = case subtype of
  -- CR 205.3i: land types.
  Subtype.Mountain -> False
  Subtype.Swamp -> False
  Subtype.Forest -> False
  Subtype.Island -> False
  Subtype.Plains -> False
  Subtype.Desert -> False
  -- CR 205.3k: a spell type.
  Subtype.Arcane -> False
  -- CR 205.3h: enchantment types.
  Subtype.Aura -> False
  Subtype.Curse -> False
  -- CR 205.3g: an artifact type.
  Subtype.Equipment -> False
  -- CR 205.3j: a planeswalker type.
  Subtype.Jace -> False
  -- CR 205.3m: creature types, every one of them named in that rule's list.
  Subtype.Goblin -> True
  Subtype.Warrior -> True
  Subtype.Human -> True
  Subtype.Bird -> True
  Subtype.Ogre -> True
  Subtype.Centaur -> True
  Subtype.Cat -> True
  Subtype.Dinosaur -> True
  Subtype.Beast -> True
  Subtype.Rat -> True
  Subtype.Elephant -> True
  Subtype.Myr -> True
  Subtype.Skeleton -> True
  Subtype.Wall -> True
  Subtype.Wizard -> True
  Subtype.Shapeshifter -> True
  Subtype.Lhurgoyf -> True
  Subtype.Barbarian -> True
  Subtype.Zombie -> True
  Subtype.Fungus -> True
  Subtype.Elemental -> True
  Subtype.Rogue -> True
  Subtype.Hag -> True
  Subtype.Warlock -> True
  Subtype.Soldier -> True
  Subtype.Phyrexian -> True
  Subtype.Elf -> True
  Subtype.Nightmare -> True
  Subtype.Horse -> True
  Subtype.Scout -> True
  Subtype.Artificer -> True
  Subtype.Troll -> True
  Subtype.Nomad -> True
  Subtype.Shaman -> True
  Subtype.Demon -> True
  Subtype.Cleric -> True
  Subtype.Illusion -> True
  Subtype.Spirit -> True
  Subtype.Angel -> True
  Subtype.Insect -> True
  Subtype.Berserker -> True
  Subtype.Thopter -> True
  Subtype.Dragon -> True
  Subtype.Unicorn -> True
  Subtype.Faerie -> True
  Subtype.Rhino -> True
  Subtype.Wraith -> True
  -- CR 205.3m lists Golem, and CR 205.3g's artifact types do not -- so Icehide
  -- Golem's subtype is a creature type even though the card is an artifact too.
  Subtype.Golem -> True
  Subtype.Turtle -> True
  Subtype.Mongoose -> True
  Subtype.Frog -> True
  Subtype.Vampire -> True
  Subtype.Dryad -> True
  Subtype.Knight -> True
