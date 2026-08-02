module Pawl.Codec.Subtype where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Subtype as Subtype

toJson :: Subtype.Subtype -> Value.Value
toJson s = Common.nullary $ case s of
  Subtype.Mountain -> "Mountain"
  Subtype.Swamp -> "Swamp"
  Subtype.Forest -> "Forest"
  Subtype.Island -> "Island"
  Subtype.Plains -> "Plains"
  Subtype.Goblin -> "Goblin"
  Subtype.Warrior -> "Warrior"
  Subtype.Human -> "Human"
  Subtype.Bird -> "Bird"
  Subtype.Ogre -> "Ogre"
  Subtype.Centaur -> "Centaur"
  Subtype.Cat -> "Cat"
  Subtype.Dinosaur -> "Dinosaur"
  Subtype.Beast -> "Beast"
  Subtype.Rat -> "Rat"
  Subtype.Elephant -> "Elephant"
  Subtype.Myr -> "Myr"
  Subtype.Skeleton -> "Skeleton"
  Subtype.Wall -> "Wall"
  Subtype.Wizard -> "Wizard"
  Subtype.Shapeshifter -> "Shapeshifter"
  Subtype.Lhurgoyf -> "Lhurgoyf"
  Subtype.Arcane -> "Arcane"
  Subtype.Barbarian -> "Barbarian"
  Subtype.Zombie -> "Zombie"
  Subtype.Fungus -> "Fungus"
  Subtype.Elemental -> "Elemental"
  Subtype.Rogue -> "Rogue"
  Subtype.Hag -> "Hag"
  Subtype.Warlock -> "Warlock"
  Subtype.Soldier -> "Soldier"
  Subtype.Phyrexian -> "Phyrexian"
  Subtype.Elf -> "Elf"
  Subtype.Nightmare -> "Nightmare"
  Subtype.Horse -> "Horse"
  Subtype.Aura -> "Aura"
  Subtype.Equipment -> "Equipment"
  Subtype.Scout -> "Scout"
  Subtype.Artificer -> "Artificer"
  Subtype.Troll -> "Troll"
  Subtype.Nomad -> "Nomad"
  Subtype.Shaman -> "Shaman"
  Subtype.Demon -> "Demon"
  Subtype.Cleric -> "Cleric"
  Subtype.Illusion -> "Illusion"
  Subtype.Spirit -> "Spirit"
  Subtype.Angel -> "Angel"
  Subtype.Insect -> "Insect"
  Subtype.Berserker -> "Berserker"
  Subtype.Thopter -> "Thopter"
  Subtype.Dragon -> "Dragon"
  Subtype.Unicorn -> "Unicorn"
  Subtype.Curse -> "Curse"
  Subtype.Desert -> "Desert"
  Subtype.Faerie -> "Faerie"
  Subtype.Rhino -> "Rhino"
  Subtype.Jace -> "Jace"
  Subtype.Wraith -> "Wraith"
  Subtype.Golem -> "Golem"
  Subtype.Turtle -> "Turtle"
  Subtype.Mongoose -> "Mongoose"
  Subtype.Frog -> "Frog"
  Subtype.Vampire -> "Vampire"

fromJson :: Value.Value -> Either Text.Text Subtype.Subtype
fromJson =
  Common.decodeNullary
    "Subtype"
    [ ("Mountain", Subtype.Mountain),
      ("Swamp", Subtype.Swamp),
      ("Forest", Subtype.Forest),
      ("Island", Subtype.Island),
      ("Plains", Subtype.Plains),
      ("Goblin", Subtype.Goblin),
      ("Warrior", Subtype.Warrior),
      ("Human", Subtype.Human),
      ("Bird", Subtype.Bird),
      ("Ogre", Subtype.Ogre),
      ("Centaur", Subtype.Centaur),
      ("Cat", Subtype.Cat),
      ("Dinosaur", Subtype.Dinosaur),
      ("Beast", Subtype.Beast),
      ("Rat", Subtype.Rat),
      ("Elephant", Subtype.Elephant),
      ("Myr", Subtype.Myr),
      ("Skeleton", Subtype.Skeleton),
      ("Wall", Subtype.Wall),
      ("Wizard", Subtype.Wizard),
      ("Shapeshifter", Subtype.Shapeshifter),
      ("Lhurgoyf", Subtype.Lhurgoyf),
      ("Arcane", Subtype.Arcane),
      ("Barbarian", Subtype.Barbarian),
      ("Zombie", Subtype.Zombie),
      ("Fungus", Subtype.Fungus),
      ("Elemental", Subtype.Elemental),
      ("Rogue", Subtype.Rogue),
      ("Hag", Subtype.Hag),
      ("Warlock", Subtype.Warlock),
      ("Soldier", Subtype.Soldier),
      ("Phyrexian", Subtype.Phyrexian),
      ("Elf", Subtype.Elf),
      ("Nightmare", Subtype.Nightmare),
      ("Horse", Subtype.Horse),
      ("Aura", Subtype.Aura),
      ("Equipment", Subtype.Equipment),
      ("Scout", Subtype.Scout),
      ("Artificer", Subtype.Artificer),
      ("Troll", Subtype.Troll),
      ("Nomad", Subtype.Nomad),
      ("Shaman", Subtype.Shaman),
      ("Demon", Subtype.Demon),
      ("Cleric", Subtype.Cleric),
      ("Illusion", Subtype.Illusion),
      ("Spirit", Subtype.Spirit),
      ("Angel", Subtype.Angel),
      ("Insect", Subtype.Insect),
      ("Berserker", Subtype.Berserker),
      ("Thopter", Subtype.Thopter),
      ("Dragon", Subtype.Dragon),
      ("Unicorn", Subtype.Unicorn),
      ("Curse", Subtype.Curse),
      ("Desert", Subtype.Desert),
      ("Faerie", Subtype.Faerie),
      ("Rhino", Subtype.Rhino),
      ("Jace", Subtype.Jace),
      ("Wraith", Subtype.Wraith),
      ("Golem", Subtype.Golem),
      ("Turtle", Subtype.Turtle),
      ("Mongoose", Subtype.Mongoose),
      ("Frog", Subtype.Frog),
      ("Vampire", Subtype.Vampire)
    ]

fromJsonPair :: Value.Value -> Either Text.Text (Subtype.Subtype, Subtype.Subtype)
fromJsonPair value = case value of
  Value.Array (Array.MkArray [f, t]) -> do
    f_ <- fromJson f
    t_ <- fromJson t
    pure (f_, t_)
  _ -> Left $ Text.pack "expected a [from, to] subtype pair"
