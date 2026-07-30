-- Classifications over Pawl.Type.Subtype: which of CR 205.3's disjoint subtype
-- families a subtype belongs to. The rulebook owns these lists outright (CR
-- 205.3i names the land types by name), so casing on a Subtype here is the same
-- kind of act as casing on a Phase -- it is a subtype's IDENTITY, never an
-- effect's.
module Pawl.Subtype where

import qualified Pawl.Type.Subtype as Subtype

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
-- Five of CR 205.3i's seventeen exist in this type today, and they happen to be
-- exactly the five BASIC land types (CR 305.6). That coincidence is not what
-- this function means: Pawl.Mana.subtypeMana is the CR 305.6 question ("which
-- mana does this basic land type make?"), and the two must come apart the moment
-- a Locus or a Gate lands.
isLandType :: Subtype.Subtype -> Bool
isLandType subtype = case subtype of
  Subtype.Mountain -> True
  Subtype.Swamp -> True
  Subtype.Forest -> True
  Subtype.Island -> True
  Subtype.Plains -> True
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
