module Pawl.Codec.SubtypeSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Subtype" $ do
  Spec.it s "Mountain" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Mountain
      "{\"type\":\"Mountain\"}"
  Spec.it s "Swamp" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Swamp
      "{\"type\":\"Swamp\"}"
  Spec.it s "Forest" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Forest
      "{\"type\":\"Forest\"}"
  Spec.it s "Island" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Island
      "{\"type\":\"Island\"}"
  Spec.it s "Plains" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Plains
      "{\"type\":\"Plains\"}"
  Spec.it s "Goblin" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Goblin
      "{\"type\":\"Goblin\"}"
  Spec.it s "Warrior" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Warrior
      "{\"type\":\"Warrior\"}"
  Spec.it s "Human" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Human
      "{\"type\":\"Human\"}"
  Spec.it s "Bird" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Bird
      "{\"type\":\"Bird\"}"
  Spec.it s "Ogre" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Ogre
      "{\"type\":\"Ogre\"}"
  Spec.it s "Centaur" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Centaur
      "{\"type\":\"Centaur\"}"
  Spec.it s "Cat" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Cat
      "{\"type\":\"Cat\"}"
  Spec.it s "Dinosaur" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Dinosaur
      "{\"type\":\"Dinosaur\"}"
  Spec.it s "Beast" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Beast
      "{\"type\":\"Beast\"}"
  Spec.it s "Rat" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Rat
      "{\"type\":\"Rat\"}"
  Spec.it s "Elephant" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Elephant
      "{\"type\":\"Elephant\"}"
  Spec.it s "Myr" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Myr
      "{\"type\":\"Myr\"}"
  Spec.it s "Skeleton" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Skeleton
      "{\"type\":\"Skeleton\"}"
  Spec.it s "Wall" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Wall
      "{\"type\":\"Wall\"}"
  Spec.it s "Wizard" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Wizard
      "{\"type\":\"Wizard\"}"
  Spec.it s "Shapeshifter" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Shapeshifter
      "{\"type\":\"Shapeshifter\"}"
  Spec.it s "Lhurgoyf" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Lhurgoyf
      "{\"type\":\"Lhurgoyf\"}"
  Spec.it s "Arcane" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Arcane
      "{\"type\":\"Arcane\"}"
  Spec.it s "Barbarian" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Barbarian
      "{\"type\":\"Barbarian\"}"
  Spec.it s "Zombie" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Zombie
      "{\"type\":\"Zombie\"}"
  Spec.it s "Fungus" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Fungus
      "{\"type\":\"Fungus\"}"
  Spec.it s "Elemental" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Elemental
      "{\"type\":\"Elemental\"}"
  Spec.it s "Rogue" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Rogue
      "{\"type\":\"Rogue\"}"
  Spec.it s "Hag" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Hag
      "{\"type\":\"Hag\"}"
  Spec.it s "Warlock" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Warlock
      "{\"type\":\"Warlock\"}"
  Spec.it s "Soldier" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Soldier
      "{\"type\":\"Soldier\"}"
  Spec.it s "Phyrexian" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Phyrexian
      "{\"type\":\"Phyrexian\"}"
  Spec.it s "Elf" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Elf
      "{\"type\":\"Elf\"}"
  Spec.it s "Nightmare" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Nightmare
      "{\"type\":\"Nightmare\"}"
  Spec.it s "Horse" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Horse
      "{\"type\":\"Horse\"}"
  Spec.it s "Aura" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Aura
      "{\"type\":\"Aura\"}"
  Spec.it s "Equipment" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Equipment
      "{\"type\":\"Equipment\"}"
  Spec.it s "Scout" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Scout
      "{\"type\":\"Scout\"}"
  Spec.it s "Artificer" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Artificer
      "{\"type\":\"Artificer\"}"
  Spec.it s "Troll" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Troll
      "{\"type\":\"Troll\"}"
  Spec.it s "Nomad" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Nomad
      "{\"type\":\"Nomad\"}"
  Spec.it s "Shaman" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Shaman
      "{\"type\":\"Shaman\"}"
  Spec.it s "Demon" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Demon
      "{\"type\":\"Demon\"}"
  Spec.it s "Cleric" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Cleric
      "{\"type\":\"Cleric\"}"
  Spec.it s "Illusion" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Illusion
      "{\"type\":\"Illusion\"}"
  Spec.it s "Spirit" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Spirit
      "{\"type\":\"Spirit\"}"
  Spec.it s "Angel" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Angel
      "{\"type\":\"Angel\"}"
  Spec.it s "Insect" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Insect
      "{\"type\":\"Insect\"}"
  Spec.it s "Berserker" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Berserker
      "{\"type\":\"Berserker\"}"
  Spec.it s "Thopter" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Thopter
      "{\"type\":\"Thopter\"}"
  Spec.it s "Dragon" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Dragon
      "{\"type\":\"Dragon\"}"
  Spec.it s "Unicorn" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Unicorn
      "{\"type\":\"Unicorn\"}"
  Spec.it s "Curse" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Curse
      "{\"type\":\"Curse\"}"
  Spec.it s "Desert" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Desert
      "{\"type\":\"Desert\"}"
  Spec.it s "Faerie" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Faerie
      "{\"type\":\"Faerie\"}"
  Spec.it s "Rhino" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Rhino
      "{\"type\":\"Rhino\"}"
  Spec.it s "Jace" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Jace
      "{\"type\":\"Jace\"}"
  Spec.it s "Wraith" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Wraith
      "{\"type\":\"Wraith\"}"
  Spec.it s "Golem" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Golem
      "{\"type\":\"Golem\"}"
  Spec.it s "Turtle" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Turtle
      "{\"type\":\"Turtle\"}"
  Spec.it s "Mongoose" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Mongoose
      "{\"type\":\"Mongoose\"}"
  Spec.it s "Frog" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Frog
      "{\"type\":\"Frog\"}"
  Spec.it s "Vampire" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Vampire
      "{\"type\":\"Vampire\"}"
  Spec.it s "Dryad" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Dryad
      "{\"type\":\"Dryad\"}"
  Spec.it s "Knight" $
    Common.assertJsonCodec
      s
      Subtype.toJson
      Subtype.fromJson
      Subtype.Knight
      "{\"type\":\"Knight\"}"
