module Pawl.Codec.SubtypeSpec where

import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Subtype" $ do
  Spec.it s "Mountain" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Mountain
      " {\"type\":\"Mountain\"} "
  Spec.it s "Swamp" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Swamp
      " {\"type\":\"Swamp\"} "
  Spec.it s "Forest" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Forest
      " {\"type\":\"Forest\"} "
  Spec.it s "Island" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Island
      " {\"type\":\"Island\"} "
  Spec.it s "Plains" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Plains
      " {\"type\":\"Plains\"} "
  Spec.it s "Goblin" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Goblin
      " {\"type\":\"Goblin\"} "
  Spec.it s "Warrior" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Warrior
      " {\"type\":\"Warrior\"} "
  Spec.it s "Human" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Human
      " {\"type\":\"Human\"} "
  Spec.it s "Bird" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Bird
      " {\"type\":\"Bird\"} "
  Spec.it s "Ogre" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Ogre
      " {\"type\":\"Ogre\"} "
  Spec.it s "Centaur" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Centaur
      " {\"type\":\"Centaur\"} "
  Spec.it s "Cat" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Cat
      " {\"type\":\"Cat\"} "
  Spec.it s "Dinosaur" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Dinosaur
      " {\"type\":\"Dinosaur\"} "
  Spec.it s "Beast" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Beast
      " {\"type\":\"Beast\"} "
  Spec.it s "Rat" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Rat
      " {\"type\":\"Rat\"} "
  Spec.it s "Elephant" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Elephant
      " {\"type\":\"Elephant\"} "
  Spec.it s "Myr" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Myr
      " {\"type\":\"Myr\"} "
  Spec.it s "Skeleton" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Skeleton
      " {\"type\":\"Skeleton\"} "
  Spec.it s "Wall" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Wall
      " {\"type\":\"Wall\"} "
  Spec.it s "Wizard" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Wizard
      " {\"type\":\"Wizard\"} "
  Spec.it s "Shapeshifter" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Shapeshifter
      " {\"type\":\"Shapeshifter\"} "
  Spec.it s "Lhurgoyf" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Lhurgoyf
      " {\"type\":\"Lhurgoyf\"} "
  Spec.it s "Arcane" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Arcane
      " {\"type\":\"Arcane\"} "
  Spec.it s "Barbarian" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Barbarian
      " {\"type\":\"Barbarian\"} "
  Spec.it s "Zombie" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Zombie
      " {\"type\":\"Zombie\"} "
  Spec.it s "Fungus" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Fungus
      " {\"type\":\"Fungus\"} "
  Spec.it s "Elemental" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Elemental
      " {\"type\":\"Elemental\"} "
  Spec.it s "Rogue" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Rogue
      " {\"type\":\"Rogue\"} "
  Spec.it s "Hag" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Hag
      " {\"type\":\"Hag\"} "
  Spec.it s "Warlock" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Warlock
      " {\"type\":\"Warlock\"} "
  Spec.it s "Soldier" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Soldier
      " {\"type\":\"Soldier\"} "
  Spec.it s "Phyrexian" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Phyrexian
      " {\"type\":\"Phyrexian\"} "
  Spec.it s "Elf" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Elf
      " {\"type\":\"Elf\"} "
  Spec.it s "Nightmare" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Nightmare
      " {\"type\":\"Nightmare\"} "
  Spec.it s "Horse" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Horse
      " {\"type\":\"Horse\"} "
  Spec.it s "Aura" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Aura
      " {\"type\":\"Aura\"} "
  Spec.it s "Equipment" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Equipment
      " {\"type\":\"Equipment\"} "
  Spec.it s "Scout" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Scout
      " {\"type\":\"Scout\"} "
  Spec.it s "Artificer" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Artificer
      " {\"type\":\"Artificer\"} "
  Spec.it s "Troll" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Troll
      " {\"type\":\"Troll\"} "
  Spec.it s "Nomad" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Nomad
      " {\"type\":\"Nomad\"} "
  Spec.it s "Shaman" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Shaman
      " {\"type\":\"Shaman\"} "
  Spec.it s "Demon" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Demon
      " {\"type\":\"Demon\"} "
  Spec.it s "Cleric" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Cleric
      " {\"type\":\"Cleric\"} "
  Spec.it s "Illusion" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Illusion
      " {\"type\":\"Illusion\"} "
  Spec.it s "Spirit" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Spirit
      " {\"type\":\"Spirit\"} "
  Spec.it s "Angel" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Angel
      " {\"type\":\"Angel\"} "
  Spec.it s "Insect" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Insect
      " {\"type\":\"Insect\"} "
  Spec.it s "Berserker" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Berserker
      " {\"type\":\"Berserker\"} "
  Spec.it s "Thopter" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Thopter
      " {\"type\":\"Thopter\"} "
  Spec.it s "Dragon" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Dragon
      " {\"type\":\"Dragon\"} "
  Spec.it s "Unicorn" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Unicorn
      " {\"type\":\"Unicorn\"} "
  Spec.it s "Curse" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Curse
      " {\"type\":\"Curse\"} "
  Spec.it s "Desert" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Desert
      " {\"type\":\"Desert\"} "
  Spec.it s "Faerie" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Faerie
      " {\"type\":\"Faerie\"} "
  Spec.it s "Rhino" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Rhino
      " {\"type\":\"Rhino\"} "
  Spec.it s "Jace" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Jace
      " {\"type\":\"Jace\"} "
  Spec.it s "Wraith" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Wraith
      " {\"type\":\"Wraith\"} "
  Spec.it s "Golem" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Golem
      " {\"type\":\"Golem\"} "
  Spec.it s "Turtle" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Turtle
      " {\"type\":\"Turtle\"} "
  Spec.it s "Mongoose" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Mongoose
      " {\"type\":\"Mongoose\"} "
  Spec.it s "Frog" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Frog
      " {\"type\":\"Frog\"} "
  Spec.it s "Vampire" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Vampire
      " {\"type\":\"Vampire\"} "
  Spec.it s "Dryad" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Dryad
      " {\"type\":\"Dryad\"} "
  Spec.it s "Knight" $
    Common.assertCodec
      s
      Subtype.codec
      Subtype.Knight
      " {\"type\":\"Knight\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Subtype.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Subtype.codec
