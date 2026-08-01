-- | The @Subtype ⇆ Json@ codec (#481).
module Pawl.Codec.Subtype where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Subtype as Subtype

subtypeToJson :: Subtype.Subtype -> Value
subtypeToJson s = Json.nullary . Text.pack $ case s of
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
  Subtype.Mongoose -> "Mongoose"

jsonToSubtype :: Value -> Either Text Subtype.Subtype
jsonToSubtype =
  Json.decodeNullary
    (Text.pack "Subtype")
    [ (Text.pack "Mountain", Subtype.Mountain),
      (Text.pack "Swamp", Subtype.Swamp),
      (Text.pack "Forest", Subtype.Forest),
      (Text.pack "Island", Subtype.Island),
      (Text.pack "Plains", Subtype.Plains),
      (Text.pack "Goblin", Subtype.Goblin),
      (Text.pack "Warrior", Subtype.Warrior),
      (Text.pack "Human", Subtype.Human),
      (Text.pack "Bird", Subtype.Bird),
      (Text.pack "Ogre", Subtype.Ogre),
      (Text.pack "Centaur", Subtype.Centaur),
      (Text.pack "Cat", Subtype.Cat),
      (Text.pack "Dinosaur", Subtype.Dinosaur),
      (Text.pack "Beast", Subtype.Beast),
      (Text.pack "Rat", Subtype.Rat),
      (Text.pack "Elephant", Subtype.Elephant),
      (Text.pack "Myr", Subtype.Myr),
      (Text.pack "Skeleton", Subtype.Skeleton),
      (Text.pack "Wall", Subtype.Wall),
      (Text.pack "Wizard", Subtype.Wizard),
      (Text.pack "Shapeshifter", Subtype.Shapeshifter),
      (Text.pack "Lhurgoyf", Subtype.Lhurgoyf),
      (Text.pack "Arcane", Subtype.Arcane),
      (Text.pack "Barbarian", Subtype.Barbarian),
      (Text.pack "Zombie", Subtype.Zombie),
      (Text.pack "Fungus", Subtype.Fungus),
      (Text.pack "Elemental", Subtype.Elemental),
      (Text.pack "Rogue", Subtype.Rogue),
      (Text.pack "Hag", Subtype.Hag),
      (Text.pack "Warlock", Subtype.Warlock),
      (Text.pack "Soldier", Subtype.Soldier),
      (Text.pack "Phyrexian", Subtype.Phyrexian),
      (Text.pack "Elf", Subtype.Elf),
      (Text.pack "Nightmare", Subtype.Nightmare),
      (Text.pack "Horse", Subtype.Horse),
      (Text.pack "Aura", Subtype.Aura),
      (Text.pack "Equipment", Subtype.Equipment),
      (Text.pack "Scout", Subtype.Scout),
      (Text.pack "Artificer", Subtype.Artificer),
      (Text.pack "Troll", Subtype.Troll),
      (Text.pack "Nomad", Subtype.Nomad),
      (Text.pack "Shaman", Subtype.Shaman),
      (Text.pack "Demon", Subtype.Demon),
      (Text.pack "Cleric", Subtype.Cleric),
      (Text.pack "Illusion", Subtype.Illusion),
      (Text.pack "Spirit", Subtype.Spirit),
      (Text.pack "Angel", Subtype.Angel),
      (Text.pack "Insect", Subtype.Insect),
      (Text.pack "Berserker", Subtype.Berserker),
      (Text.pack "Thopter", Subtype.Thopter),
      (Text.pack "Dragon", Subtype.Dragon),
      (Text.pack "Unicorn", Subtype.Unicorn),
      (Text.pack "Curse", Subtype.Curse),
      (Text.pack "Desert", Subtype.Desert),
      (Text.pack "Faerie", Subtype.Faerie),
      (Text.pack "Rhino", Subtype.Rhino),
      (Text.pack "Jace", Subtype.Jace),
      (Text.pack "Wraith", Subtype.Wraith),
      (Text.pack "Golem", Subtype.Golem),
      (Text.pack "Mongoose", Subtype.Mongoose)
    ]

jsonToSubtypePair :: Value -> Either Text (Subtype.Subtype, Subtype.Subtype)
jsonToSubtypePair value = case value of
  Array (MkArray [f, t]) -> do
    f_ <- jsonToSubtype f
    t_ <- jsonToSubtype t
    pure (f_, t_)
  _ -> Left (Text.pack "expected a [from, to] subtype pair")
