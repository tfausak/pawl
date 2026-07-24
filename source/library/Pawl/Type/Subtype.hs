module Pawl.Type.Subtype where

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
  deriving (Eq, Ord, Show)
