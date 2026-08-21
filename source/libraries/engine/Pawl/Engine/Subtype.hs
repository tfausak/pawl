-- CR 205.3's disjoint subtype families over Pawl.Types.Subtype: which family a
-- subtype belongs to, and -- for a creature type, the one family whose word a
-- rule asks pawl to WRITE rather than only to recognise -- what that word is.
-- The rulebook owns these lists outright (CR 205.3i and CR 205.3m name the land
-- types and the creature types by name), so casing on a Subtype here is the
-- same kind of act as casing on a Phase -- it is a subtype's IDENTITY, never an
-- effect's.
module Pawl.Engine.Subtype where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily

-- | CR 612.2's question -- is this word one of the family's? -- where the
-- family is named by a card's text rather than fixed by a constructor.
--
-- BasicLandType answers with the whole of CR 205.3i, not CR 305.6's five: the
-- narrowing to basics is on what a player may CHOOSE, while the family a swap
-- may reach is the land types. Unobservable while the only basic-land
-- text-changer offers the five, and stating the family is what CR 612.2
-- actually says.
inFamily :: SubtypeFamily.SubtypeFamily -> Subtype.Subtype -> Bool
inFamily family = case family of
  SubtypeFamily.BasicLandType -> isLandType
  SubtypeFamily.CreatureType -> isCreatureType

-- | CR 205.3i
isLandType :: Subtype.Subtype -> Bool
isLandType subtype = case subtype of
  Subtype.Cave -> True
  Subtype.Desert -> True
  Subtype.Forest -> True
  Subtype.Gate -> True
  Subtype.Island -> True
  Subtype.Lair -> True
  Subtype.Locus -> True
  Subtype.Mine -> True
  Subtype.Mountain -> True
  Subtype.Plains -> True
  Subtype.Planet -> True
  Subtype.PowerPlant -> True
  Subtype.Sphere -> True
  Subtype.Swamp -> True
  Subtype.Tower -> True
  Subtype.Town -> True
  Subtype.Urzas -> True
  _ -> False

-- | CR 205.3m's list, carrying each creature type's PRINTED word -- the
-- two-word Time Lord and the punctuated Assembly-Worker, C'tan and Shi'ar
-- included -- and Nothing for a subtype of any other family.
--
-- The word is what CR 612.2a needs and a family test cannot supply: a token's
-- NAME is text, so swapping the creature type that defines it means writing the
-- new word out (Pawl.Engine.Projection.rewriteCard). isCreatureType below reads
-- this rather than restating the list, so the two cannot drift.
creatureTypeWord :: Subtype.Subtype -> Maybe Text.Text
creatureTypeWord subtype = case subtype of
  Subtype.Advisor -> Just (Text.pack "Advisor")
  Subtype.Aetherborn -> Just (Text.pack "Aetherborn")
  Subtype.Alien -> Just (Text.pack "Alien")
  Subtype.Ally -> Just (Text.pack "Ally")
  Subtype.Angel -> Just (Text.pack "Angel")
  Subtype.Antelope -> Just (Text.pack "Antelope")
  Subtype.Ape -> Just (Text.pack "Ape")
  Subtype.Archer -> Just (Text.pack "Archer")
  Subtype.Archon -> Just (Text.pack "Archon")
  Subtype.Armadillo -> Just (Text.pack "Armadillo")
  Subtype.Army -> Just (Text.pack "Army")
  Subtype.Artificer -> Just (Text.pack "Artificer")
  Subtype.Assassin -> Just (Text.pack "Assassin")
  Subtype.AssemblyWorker -> Just (Text.pack "Assembly-Worker")
  Subtype.Astartes -> Just (Text.pack "Astartes")
  Subtype.Atog -> Just (Text.pack "Atog")
  Subtype.Aurochs -> Just (Text.pack "Aurochs")
  Subtype.Avatar -> Just (Text.pack "Avatar")
  Subtype.Azra -> Just (Text.pack "Azra")
  Subtype.Badger -> Just (Text.pack "Badger")
  Subtype.Balloon -> Just (Text.pack "Balloon")
  Subtype.Barbarian -> Just (Text.pack "Barbarian")
  Subtype.Bard -> Just (Text.pack "Bard")
  Subtype.Basilisk -> Just (Text.pack "Basilisk")
  Subtype.Bat -> Just (Text.pack "Bat")
  Subtype.Bear -> Just (Text.pack "Bear")
  Subtype.Beast -> Just (Text.pack "Beast")
  Subtype.Beaver -> Just (Text.pack "Beaver")
  Subtype.Beeble -> Just (Text.pack "Beeble")
  Subtype.Beholder -> Just (Text.pack "Beholder")
  Subtype.Berserker -> Just (Text.pack "Berserker")
  Subtype.Bird -> Just (Text.pack "Bird")
  Subtype.Bison -> Just (Text.pack "Bison")
  Subtype.Blinkmoth -> Just (Text.pack "Blinkmoth")
  Subtype.Boar -> Just (Text.pack "Boar")
  Subtype.Bringer -> Just (Text.pack "Bringer")
  Subtype.Brushwagg -> Just (Text.pack "Brushwagg")
  Subtype.Camarid -> Just (Text.pack "Camarid")
  Subtype.Camel -> Just (Text.pack "Camel")
  Subtype.Capybara -> Just (Text.pack "Capybara")
  Subtype.Caribou -> Just (Text.pack "Caribou")
  Subtype.Carrier -> Just (Text.pack "Carrier")
  Subtype.Cat -> Just (Text.pack "Cat")
  Subtype.Centaur -> Just (Text.pack "Centaur")
  Subtype.Child -> Just (Text.pack "Child")
  Subtype.Chimera -> Just (Text.pack "Chimera")
  Subtype.Citizen -> Just (Text.pack "Citizen")
  Subtype.Cleric -> Just (Text.pack "Cleric")
  Subtype.Clown -> Just (Text.pack "Clown")
  Subtype.Cockatrice -> Just (Text.pack "Cockatrice")
  Subtype.Construct -> Just (Text.pack "Construct")
  Subtype.Coward -> Just (Text.pack "Coward")
  Subtype.Coyote -> Just (Text.pack "Coyote")
  Subtype.Crab -> Just (Text.pack "Crab")
  Subtype.Crocodile -> Just (Text.pack "Crocodile")
  Subtype.Ctan -> Just (Text.pack "C'tan")
  Subtype.Custodes -> Just (Text.pack "Custodes")
  Subtype.Cyberman -> Just (Text.pack "Cyberman")
  Subtype.Cyclops -> Just (Text.pack "Cyclops")
  Subtype.Dalek -> Just (Text.pack "Dalek")
  Subtype.Dauthi -> Just (Text.pack "Dauthi")
  Subtype.Demigod -> Just (Text.pack "Demigod")
  Subtype.Demon -> Just (Text.pack "Demon")
  Subtype.Deserter -> Just (Text.pack "Deserter")
  Subtype.Detective -> Just (Text.pack "Detective")
  Subtype.Devil -> Just (Text.pack "Devil")
  Subtype.Dinosaur -> Just (Text.pack "Dinosaur")
  Subtype.Djinn -> Just (Text.pack "Djinn")
  Subtype.Doctor -> Just (Text.pack "Doctor")
  Subtype.Dog -> Just (Text.pack "Dog")
  Subtype.Dragon -> Just (Text.pack "Dragon")
  Subtype.Drake -> Just (Text.pack "Drake")
  Subtype.Dreadnought -> Just (Text.pack "Dreadnought")
  Subtype.Drix -> Just (Text.pack "Drix")
  Subtype.Drone -> Just (Text.pack "Drone")
  Subtype.Druid -> Just (Text.pack "Druid")
  Subtype.Dryad -> Just (Text.pack "Dryad")
  Subtype.Dwarf -> Just (Text.pack "Dwarf")
  Subtype.Echidna -> Just (Text.pack "Echidna")
  Subtype.Efreet -> Just (Text.pack "Efreet")
  Subtype.Egg -> Just (Text.pack "Egg")
  Subtype.Elder -> Just (Text.pack "Elder")
  Subtype.Eldrazi -> Just (Text.pack "Eldrazi")
  Subtype.Elemental -> Just (Text.pack "Elemental")
  Subtype.Elephant -> Just (Text.pack "Elephant")
  Subtype.Elf -> Just (Text.pack "Elf")
  Subtype.Elk -> Just (Text.pack "Elk")
  Subtype.Employee -> Just (Text.pack "Employee")
  Subtype.Eternal -> Just (Text.pack "Eternal")
  Subtype.Eye -> Just (Text.pack "Eye")
  Subtype.Faerie -> Just (Text.pack "Faerie")
  Subtype.Ferret -> Just (Text.pack "Ferret")
  Subtype.Fish -> Just (Text.pack "Fish")
  Subtype.Flagbearer -> Just (Text.pack "Flagbearer")
  Subtype.Fox -> Just (Text.pack "Fox")
  Subtype.Fractal -> Just (Text.pack "Fractal")
  Subtype.Frog -> Just (Text.pack "Frog")
  Subtype.Fungus -> Just (Text.pack "Fungus")
  Subtype.Gamer -> Just (Text.pack "Gamer")
  Subtype.Gamma -> Just (Text.pack "Gamma")
  Subtype.Gargoyle -> Just (Text.pack "Gargoyle")
  Subtype.Germ -> Just (Text.pack "Germ")
  Subtype.Giant -> Just (Text.pack "Giant")
  Subtype.Giraffe -> Just (Text.pack "Giraffe")
  Subtype.Gith -> Just (Text.pack "Gith")
  Subtype.Glimmer -> Just (Text.pack "Glimmer")
  Subtype.Gnoll -> Just (Text.pack "Gnoll")
  Subtype.Gnome -> Just (Text.pack "Gnome")
  Subtype.Goat -> Just (Text.pack "Goat")
  Subtype.Goblin -> Just (Text.pack "Goblin")
  Subtype.God -> Just (Text.pack "God")
  Subtype.Golem -> Just (Text.pack "Golem")
  Subtype.Gorgon -> Just (Text.pack "Gorgon")
  Subtype.Graveborn -> Just (Text.pack "Graveborn")
  Subtype.Gremlin -> Just (Text.pack "Gremlin")
  Subtype.Griffin -> Just (Text.pack "Griffin")
  Subtype.Guest -> Just (Text.pack "Guest")
  Subtype.Hag -> Just (Text.pack "Hag")
  Subtype.Halfling -> Just (Text.pack "Halfling")
  Subtype.Hamster -> Just (Text.pack "Hamster")
  Subtype.Harpy -> Just (Text.pack "Harpy")
  Subtype.Hedgehog -> Just (Text.pack "Hedgehog")
  Subtype.Hellion -> Just (Text.pack "Hellion")
  Subtype.Hero -> Just (Text.pack "Hero")
  Subtype.Hippo -> Just (Text.pack "Hippo")
  Subtype.Hippogriff -> Just (Text.pack "Hippogriff")
  Subtype.Homarid -> Just (Text.pack "Homarid")
  Subtype.Homunculus -> Just (Text.pack "Homunculus")
  Subtype.Horror -> Just (Text.pack "Horror")
  Subtype.Horse -> Just (Text.pack "Horse")
  Subtype.Human -> Just (Text.pack "Human")
  Subtype.Hydra -> Just (Text.pack "Hydra")
  Subtype.Hyena -> Just (Text.pack "Hyena")
  Subtype.Illusion -> Just (Text.pack "Illusion")
  Subtype.Imp -> Just (Text.pack "Imp")
  Subtype.Incarnation -> Just (Text.pack "Incarnation")
  Subtype.Inhuman -> Just (Text.pack "Inhuman")
  Subtype.Inkling -> Just (Text.pack "Inkling")
  Subtype.Inquisitor -> Just (Text.pack "Inquisitor")
  Subtype.Insect -> Just (Text.pack "Insect")
  Subtype.Jackal -> Just (Text.pack "Jackal")
  Subtype.Jellyfish -> Just (Text.pack "Jellyfish")
  Subtype.Juggernaut -> Just (Text.pack "Juggernaut")
  Subtype.Kangaroo -> Just (Text.pack "Kangaroo")
  Subtype.Kavu -> Just (Text.pack "Kavu")
  Subtype.Kirin -> Just (Text.pack "Kirin")
  Subtype.Kithkin -> Just (Text.pack "Kithkin")
  Subtype.Knight -> Just (Text.pack "Knight")
  Subtype.Kobold -> Just (Text.pack "Kobold")
  Subtype.Kor -> Just (Text.pack "Kor")
  Subtype.Kraken -> Just (Text.pack "Kraken")
  Subtype.Kree -> Just (Text.pack "Kree")
  Subtype.Llama -> Just (Text.pack "Llama")
  Subtype.Lamia -> Just (Text.pack "Lamia")
  Subtype.Lammasu -> Just (Text.pack "Lammasu")
  Subtype.Leech -> Just (Text.pack "Leech")
  Subtype.Lemur -> Just (Text.pack "Lemur")
  Subtype.Leviathan -> Just (Text.pack "Leviathan")
  Subtype.Lhurgoyf -> Just (Text.pack "Lhurgoyf")
  Subtype.Licid -> Just (Text.pack "Licid")
  Subtype.Lizard -> Just (Text.pack "Lizard")
  Subtype.Lobster -> Just (Text.pack "Lobster")
  Subtype.Manticore -> Just (Text.pack "Manticore")
  Subtype.Masticore -> Just (Text.pack "Masticore")
  Subtype.Mercenary -> Just (Text.pack "Mercenary")
  Subtype.Merfolk -> Just (Text.pack "Merfolk")
  Subtype.Metathran -> Just (Text.pack "Metathran")
  Subtype.Minion -> Just (Text.pack "Minion")
  Subtype.Minotaur -> Just (Text.pack "Minotaur")
  Subtype.Mite -> Just (Text.pack "Mite")
  Subtype.Mole -> Just (Text.pack "Mole")
  Subtype.Monger -> Just (Text.pack "Monger")
  Subtype.Mongoose -> Just (Text.pack "Mongoose")
  Subtype.Monk -> Just (Text.pack "Monk")
  Subtype.Monkey -> Just (Text.pack "Monkey")
  Subtype.Moogle -> Just (Text.pack "Moogle")
  Subtype.Moonfolk -> Just (Text.pack "Moonfolk")
  Subtype.Mount -> Just (Text.pack "Mount")
  Subtype.Mouse -> Just (Text.pack "Mouse")
  Subtype.Mutant -> Just (Text.pack "Mutant")
  Subtype.Myr -> Just (Text.pack "Myr")
  Subtype.Mystic -> Just (Text.pack "Mystic")
  Subtype.Nautilus -> Just (Text.pack "Nautilus")
  Subtype.Necron -> Just (Text.pack "Necron")
  Subtype.Nephilim -> Just (Text.pack "Nephilim")
  Subtype.Nightmare -> Just (Text.pack "Nightmare")
  Subtype.Nightstalker -> Just (Text.pack "Nightstalker")
  Subtype.Ninja -> Just (Text.pack "Ninja")
  Subtype.Noble -> Just (Text.pack "Noble")
  Subtype.Noggle -> Just (Text.pack "Noggle")
  Subtype.Nomad -> Just (Text.pack "Nomad")
  Subtype.Nymph -> Just (Text.pack "Nymph")
  Subtype.Octopus -> Just (Text.pack "Octopus")
  Subtype.Ogre -> Just (Text.pack "Ogre")
  Subtype.Ooze -> Just (Text.pack "Ooze")
  Subtype.Orb -> Just (Text.pack "Orb")
  Subtype.Orc -> Just (Text.pack "Orc")
  Subtype.Orgg -> Just (Text.pack "Orgg")
  Subtype.Otter -> Just (Text.pack "Otter")
  Subtype.Ouphe -> Just (Text.pack "Ouphe")
  Subtype.Ox -> Just (Text.pack "Ox")
  Subtype.Oyster -> Just (Text.pack "Oyster")
  Subtype.Pangolin -> Just (Text.pack "Pangolin")
  Subtype.Peasant -> Just (Text.pack "Peasant")
  Subtype.Pegasus -> Just (Text.pack "Pegasus")
  Subtype.Pentavite -> Just (Text.pack "Pentavite")
  Subtype.Performer -> Just (Text.pack "Performer")
  Subtype.Pest -> Just (Text.pack "Pest")
  Subtype.Phelddagrif -> Just (Text.pack "Phelddagrif")
  Subtype.Phoenix -> Just (Text.pack "Phoenix")
  Subtype.Phyrexian -> Just (Text.pack "Phyrexian")
  Subtype.Pilot -> Just (Text.pack "Pilot")
  Subtype.Pincher -> Just (Text.pack "Pincher")
  Subtype.Pirate -> Just (Text.pack "Pirate")
  Subtype.Plant -> Just (Text.pack "Plant")
  Subtype.Platypus -> Just (Text.pack "Platypus")
  Subtype.Porcupine -> Just (Text.pack "Porcupine")
  Subtype.Possum -> Just (Text.pack "Possum")
  Subtype.Praetor -> Just (Text.pack "Praetor")
  Subtype.Primarch -> Just (Text.pack "Primarch")
  Subtype.Prism -> Just (Text.pack "Prism")
  Subtype.Processor -> Just (Text.pack "Processor")
  Subtype.Qu -> Just (Text.pack "Qu")
  Subtype.Rabbit -> Just (Text.pack "Rabbit")
  Subtype.Raccoon -> Just (Text.pack "Raccoon")
  Subtype.Ranger -> Just (Text.pack "Ranger")
  Subtype.Rat -> Just (Text.pack "Rat")
  Subtype.Rebel -> Just (Text.pack "Rebel")
  Subtype.Reflection -> Just (Text.pack "Reflection")
  Subtype.Rhino -> Just (Text.pack "Rhino")
  Subtype.Rigger -> Just (Text.pack "Rigger")
  Subtype.Robot -> Just (Text.pack "Robot")
  Subtype.Rogue -> Just (Text.pack "Rogue")
  Subtype.Sable -> Just (Text.pack "Sable")
  Subtype.Salamander -> Just (Text.pack "Salamander")
  Subtype.Samurai -> Just (Text.pack "Samurai")
  Subtype.Sand -> Just (Text.pack "Sand")
  Subtype.Saproling -> Just (Text.pack "Saproling")
  Subtype.Satyr -> Just (Text.pack "Satyr")
  Subtype.Scarecrow -> Just (Text.pack "Scarecrow")
  Subtype.Scientist -> Just (Text.pack "Scientist")
  Subtype.Scion -> Just (Text.pack "Scion")
  Subtype.Scorpion -> Just (Text.pack "Scorpion")
  Subtype.Scout -> Just (Text.pack "Scout")
  Subtype.Sculpture -> Just (Text.pack "Sculpture")
  Subtype.Seal -> Just (Text.pack "Seal")
  Subtype.Serf -> Just (Text.pack "Serf")
  Subtype.Serpent -> Just (Text.pack "Serpent")
  Subtype.Servo -> Just (Text.pack "Servo")
  Subtype.Shade -> Just (Text.pack "Shade")
  Subtype.Shaman -> Just (Text.pack "Shaman")
  Subtype.Shapeshifter -> Just (Text.pack "Shapeshifter")
  Subtype.Shark -> Just (Text.pack "Shark")
  Subtype.Sheep -> Just (Text.pack "Sheep")
  Subtype.Shiar -> Just (Text.pack "Shi'ar")
  Subtype.Siren -> Just (Text.pack "Siren")
  Subtype.Skeleton -> Just (Text.pack "Skeleton")
  Subtype.Skrull -> Just (Text.pack "Skrull")
  Subtype.Skunk -> Just (Text.pack "Skunk")
  Subtype.Slith -> Just (Text.pack "Slith")
  Subtype.Sliver -> Just (Text.pack "Sliver")
  Subtype.Sloth -> Just (Text.pack "Sloth")
  Subtype.Slug -> Just (Text.pack "Slug")
  Subtype.Snail -> Just (Text.pack "Snail")
  Subtype.Snake -> Just (Text.pack "Snake")
  Subtype.Soldier -> Just (Text.pack "Soldier")
  Subtype.Soltari -> Just (Text.pack "Soltari")
  Subtype.Sorcerer -> Just (Text.pack "Sorcerer")
  Subtype.Spawn -> Just (Text.pack "Spawn")
  Subtype.Specter -> Just (Text.pack "Specter")
  Subtype.Spellshaper -> Just (Text.pack "Spellshaper")
  Subtype.Sphinx -> Just (Text.pack "Sphinx")
  Subtype.Spider -> Just (Text.pack "Spider")
  Subtype.Spike -> Just (Text.pack "Spike")
  Subtype.Spirit -> Just (Text.pack "Spirit")
  Subtype.Splinter -> Just (Text.pack "Splinter")
  Subtype.Sponge -> Just (Text.pack "Sponge")
  Subtype.Spy -> Just (Text.pack "Spy")
  Subtype.Squid -> Just (Text.pack "Squid")
  Subtype.Squirrel -> Just (Text.pack "Squirrel")
  Subtype.Starfish -> Just (Text.pack "Starfish")
  Subtype.Surrakar -> Just (Text.pack "Surrakar")
  Subtype.Survivor -> Just (Text.pack "Survivor")
  Subtype.Symbiote -> Just (Text.pack "Symbiote")
  Subtype.Synth -> Just (Text.pack "Synth")
  Subtype.Tentacle -> Just (Text.pack "Tentacle")
  Subtype.Tetravite -> Just (Text.pack "Tetravite")
  Subtype.Thalakos -> Just (Text.pack "Thalakos")
  Subtype.Thopter -> Just (Text.pack "Thopter")
  Subtype.Thrull -> Just (Text.pack "Thrull")
  Subtype.Tiefling -> Just (Text.pack "Tiefling")
  Subtype.TimeLord -> Just (Text.pack "Time Lord")
  Subtype.Toy -> Just (Text.pack "Toy")
  Subtype.Treefolk -> Just (Text.pack "Treefolk")
  Subtype.Trilobite -> Just (Text.pack "Trilobite")
  Subtype.Triskelavite -> Just (Text.pack "Triskelavite")
  Subtype.Troll -> Just (Text.pack "Troll")
  Subtype.Turtle -> Just (Text.pack "Turtle")
  Subtype.Tyranid -> Just (Text.pack "Tyranid")
  Subtype.Unicorn -> Just (Text.pack "Unicorn")
  Subtype.Utrom -> Just (Text.pack "Utrom")
  Subtype.Vampire -> Just (Text.pack "Vampire")
  Subtype.Varmint -> Just (Text.pack "Varmint")
  Subtype.Vedalken -> Just (Text.pack "Vedalken")
  Subtype.Villain -> Just (Text.pack "Villain")
  Subtype.Volver -> Just (Text.pack "Volver")
  Subtype.Wall -> Just (Text.pack "Wall")
  Subtype.Walrus -> Just (Text.pack "Walrus")
  Subtype.Warlock -> Just (Text.pack "Warlock")
  Subtype.Warrior -> Just (Text.pack "Warrior")
  Subtype.Weasel -> Just (Text.pack "Weasel")
  Subtype.Weird -> Just (Text.pack "Weird")
  Subtype.Werewolf -> Just (Text.pack "Werewolf")
  Subtype.Whale -> Just (Text.pack "Whale")
  Subtype.Wizard -> Just (Text.pack "Wizard")
  Subtype.Wolf -> Just (Text.pack "Wolf")
  Subtype.Wolverine -> Just (Text.pack "Wolverine")
  Subtype.Wombat -> Just (Text.pack "Wombat")
  Subtype.Worm -> Just (Text.pack "Worm")
  Subtype.Wraith -> Just (Text.pack "Wraith")
  Subtype.Wurm -> Just (Text.pack "Wurm")
  Subtype.Yeti -> Just (Text.pack "Yeti")
  Subtype.Zombie -> Just (Text.pack "Zombie")
  Subtype.Zubera -> Just (Text.pack "Zubera")
  _ -> Nothing

-- | CR 205.3m
isCreatureType :: Subtype.Subtype -> Bool
isCreatureType = Maybe.isJust . creatureTypeWord

-- | CR 205.3m's list as a SET -- what CR 702.73a's "every creature type" means.
--
-- Sieved out of the Subtype enumeration through isCreatureType rather than
-- written out, so it cannot disagree with creatureTypeWord above; a subtype
-- added there joins this set in the same edit. That is the opposite call from
-- Pawl.Engine.Mana's five colours, and the reason is size: CR 105.1 names five,
-- where CR 205.3m names hundreds and pawl already holds the list once.
everyCreatureType :: Set.Set Subtype.Subtype
everyCreatureType = Set.fromList (filter isCreatureType [minBound ..])

-- | CR 205.3: the card types whose subtype list this subtype belongs to. CR
-- 205.1a's removal clause is one caller -- a subtype stays with an object only
-- while the object still has one of the card types its family correlates with
-- -- and CR 205.3d's rejection clause is the other direction of the same
-- question; Pawl.Engine.Projection.correspondsTo asks both through this.
--
-- A SET rather than one card type, because two families are shared: CR 205.3m
-- gives the creature types to creatures and kindreds alike, and CR 205.3k the
-- spell types to instants and sorceries. A single answer would have to pick one
-- and would strip a Kindred permanent's creature types.
--
-- The two big families delegate rather than restate. isCreatureType and
-- isLandType already hold CR 205.3m's and CR 205.3i's lists, and everyCreatureType
-- makes the point in the other direction: a second hand-kept copy is free to
-- drift.
--
-- The fallthrough answers with the EMPTY set, which the caller reads as "no
-- family known, so no card type can strip it" -- the conservative direction, and
-- isLandType's own posture toward a subtype it does not name. Unreachable today:
-- CR 205.3g through 205.3q partition Pawl.Types.Subtype, and the arms below plus
-- the two guards cover every constructor.
correlatedCardTypes :: Subtype.Subtype -> Set.Set CardType.CardType
correlatedCardTypes subtype
  | isCreatureType subtype = Set.fromList [CardType.Creature, CardType.Kindred]
  | isLandType subtype = Set.singleton CardType.Land
  | otherwise = case subtype of
      -- CR 205.3g's artifact types.
      Subtype.Attraction -> Set.singleton CardType.Artifact
      Subtype.Blood -> Set.singleton CardType.Artifact
      Subtype.Bobblehead -> Set.singleton CardType.Artifact
      Subtype.Book -> Set.singleton CardType.Artifact
      Subtype.Clue -> Set.singleton CardType.Artifact
      Subtype.Contraption -> Set.singleton CardType.Artifact
      Subtype.Equipment -> Set.singleton CardType.Artifact
      Subtype.Food -> Set.singleton CardType.Artifact
      Subtype.Fortification -> Set.singleton CardType.Artifact
      Subtype.Gold -> Set.singleton CardType.Artifact
      Subtype.Incubator -> Set.singleton CardType.Artifact
      Subtype.Infinity -> Set.singleton CardType.Artifact
      Subtype.Junk -> Set.singleton CardType.Artifact
      Subtype.Lander -> Set.singleton CardType.Artifact
      Subtype.Map -> Set.singleton CardType.Artifact
      Subtype.Mutagen -> Set.singleton CardType.Artifact
      Subtype.Powerstone -> Set.singleton CardType.Artifact
      Subtype.Spacecraft -> Set.singleton CardType.Artifact
      Subtype.Stone -> Set.singleton CardType.Artifact
      Subtype.Treasure -> Set.singleton CardType.Artifact
      Subtype.Vehicle -> Set.singleton CardType.Artifact
      Subtype.Vibranium -> Set.singleton CardType.Artifact
      -- CR 205.3h's enchantment types.
      Subtype.Aura -> Set.singleton CardType.Enchantment
      Subtype.Background -> Set.singleton CardType.Enchantment
      Subtype.Cartouche -> Set.singleton CardType.Enchantment
      Subtype.Case -> Set.singleton CardType.Enchantment
      Subtype.Class -> Set.singleton CardType.Enchantment
      Subtype.Curse -> Set.singleton CardType.Enchantment
      Subtype.Plan -> Set.singleton CardType.Enchantment
      Subtype.Role -> Set.singleton CardType.Enchantment
      Subtype.Room -> Set.singleton CardType.Enchantment
      Subtype.Rune -> Set.singleton CardType.Enchantment
      Subtype.Saga -> Set.singleton CardType.Enchantment
      Subtype.Shard -> Set.singleton CardType.Enchantment
      Subtype.Shrine -> Set.singleton CardType.Enchantment
      -- CR 205.3j's planeswalker types.
      Subtype.Ajani -> Set.singleton CardType.Planeswalker
      Subtype.Aminatou -> Set.singleton CardType.Planeswalker
      Subtype.Angrath -> Set.singleton CardType.Planeswalker
      Subtype.Arlinn -> Set.singleton CardType.Planeswalker
      Subtype.Ashiok -> Set.singleton CardType.Planeswalker
      Subtype.Bahamut -> Set.singleton CardType.Planeswalker
      Subtype.Basri -> Set.singleton CardType.Planeswalker
      Subtype.Bolas -> Set.singleton CardType.Planeswalker
      Subtype.Calix -> Set.singleton CardType.Planeswalker
      Subtype.Chandra -> Set.singleton CardType.Planeswalker
      Subtype.Comet -> Set.singleton CardType.Planeswalker
      Subtype.Dack -> Set.singleton CardType.Planeswalker
      Subtype.Dakkon -> Set.singleton CardType.Planeswalker
      Subtype.Daretti -> Set.singleton CardType.Planeswalker
      Subtype.Davriel -> Set.singleton CardType.Planeswalker
      Subtype.Dellian -> Set.singleton CardType.Planeswalker
      Subtype.Dihada -> Set.singleton CardType.Planeswalker
      Subtype.Domri -> Set.singleton CardType.Planeswalker
      Subtype.Dovin -> Set.singleton CardType.Planeswalker
      Subtype.Ellywick -> Set.singleton CardType.Planeswalker
      Subtype.Elminster -> Set.singleton CardType.Planeswalker
      Subtype.Elspeth -> Set.singleton CardType.Planeswalker
      Subtype.Estrid -> Set.singleton CardType.Planeswalker
      Subtype.Freyalise -> Set.singleton CardType.Planeswalker
      Subtype.Garruk -> Set.singleton CardType.Planeswalker
      Subtype.Gideon -> Set.singleton CardType.Planeswalker
      Subtype.Grist -> Set.singleton CardType.Planeswalker
      Subtype.Guff -> Set.singleton CardType.Planeswalker
      Subtype.Huatli -> Set.singleton CardType.Planeswalker
      Subtype.Jace -> Set.singleton CardType.Planeswalker
      Subtype.Jared -> Set.singleton CardType.Planeswalker
      Subtype.Jaya -> Set.singleton CardType.Planeswalker
      Subtype.Jeska -> Set.singleton CardType.Planeswalker
      Subtype.Kaito -> Set.singleton CardType.Planeswalker
      Subtype.Karn -> Set.singleton CardType.Planeswalker
      Subtype.Kasmina -> Set.singleton CardType.Planeswalker
      Subtype.Kaya -> Set.singleton CardType.Planeswalker
      Subtype.Kiora -> Set.singleton CardType.Planeswalker
      Subtype.Koth -> Set.singleton CardType.Planeswalker
      Subtype.Liliana -> Set.singleton CardType.Planeswalker
      Subtype.Lolth -> Set.singleton CardType.Planeswalker
      Subtype.Lukka -> Set.singleton CardType.Planeswalker
      Subtype.Minsc -> Set.singleton CardType.Planeswalker
      Subtype.Mordenkainen -> Set.singleton CardType.Planeswalker
      Subtype.Nahiri -> Set.singleton CardType.Planeswalker
      Subtype.Narset -> Set.singleton CardType.Planeswalker
      Subtype.Niko -> Set.singleton CardType.Planeswalker
      Subtype.Nissa -> Set.singleton CardType.Planeswalker
      Subtype.Nixilis -> Set.singleton CardType.Planeswalker
      Subtype.Oko -> Set.singleton CardType.Planeswalker
      Subtype.Quintorius -> Set.singleton CardType.Planeswalker
      Subtype.Ral -> Set.singleton CardType.Planeswalker
      Subtype.Rowan -> Set.singleton CardType.Planeswalker
      Subtype.Saheeli -> Set.singleton CardType.Planeswalker
      Subtype.Samut -> Set.singleton CardType.Planeswalker
      Subtype.Sarkhan -> Set.singleton CardType.Planeswalker
      Subtype.Serra -> Set.singleton CardType.Planeswalker
      Subtype.Sivitri -> Set.singleton CardType.Planeswalker
      Subtype.Sorin -> Set.singleton CardType.Planeswalker
      Subtype.Szat -> Set.singleton CardType.Planeswalker
      Subtype.Tamiyo -> Set.singleton CardType.Planeswalker
      Subtype.Tasha -> Set.singleton CardType.Planeswalker
      Subtype.Teferi -> Set.singleton CardType.Planeswalker
      Subtype.Teyo -> Set.singleton CardType.Planeswalker
      Subtype.Tezzeret -> Set.singleton CardType.Planeswalker
      Subtype.Tibalt -> Set.singleton CardType.Planeswalker
      Subtype.Tyvar -> Set.singleton CardType.Planeswalker
      Subtype.Ugin -> Set.singleton CardType.Planeswalker
      Subtype.Urza -> Set.singleton CardType.Planeswalker
      Subtype.Venser -> Set.singleton CardType.Planeswalker
      Subtype.Vivien -> Set.singleton CardType.Planeswalker
      Subtype.Vraska -> Set.singleton CardType.Planeswalker
      Subtype.Vronos -> Set.singleton CardType.Planeswalker
      Subtype.Will -> Set.singleton CardType.Planeswalker
      Subtype.Windgrace -> Set.singleton CardType.Planeswalker
      Subtype.Wrenn -> Set.singleton CardType.Planeswalker
      Subtype.Xenagos -> Set.singleton CardType.Planeswalker
      Subtype.Yanggu -> Set.singleton CardType.Planeswalker
      Subtype.Yanling -> Set.singleton CardType.Planeswalker
      Subtype.Zariel -> Set.singleton CardType.Planeswalker
      -- CR 205.3n's planar types.
      Subtype.TheAbyss -> Set.singleton CardType.Plane
      Subtype.Alara -> Set.singleton CardType.Plane
      Subtype.AlfavaMetraxis -> Set.singleton CardType.Plane
      Subtype.Amonkhet -> Set.singleton CardType.Plane
      Subtype.AndrozaniMinor -> Set.singleton CardType.Plane
      Subtype.Antausia -> Set.singleton CardType.Plane
      Subtype.Apalapucia -> Set.singleton CardType.Plane
      Subtype.Arcavios -> Set.singleton CardType.Plane
      Subtype.Arkhos -> Set.singleton CardType.Plane
      Subtype.Avishkar -> Set.singleton CardType.Plane
      Subtype.Azgol -> Set.singleton CardType.Plane
      Subtype.Belenon -> Set.singleton CardType.Plane
      Subtype.BolassMeditationRealm -> Set.singleton CardType.Plane
      Subtype.Capenna -> Set.singleton CardType.Plane
      Subtype.Cridhe -> Set.singleton CardType.Plane
      Subtype.TheDalekAsylum -> Set.singleton CardType.Plane
      Subtype.Darillium -> Set.singleton CardType.Plane
      Subtype.Dominaria -> Set.singleton CardType.Plane
      Subtype.Earth -> Set.singleton CardType.Plane
      Subtype.Echoir -> Set.singleton CardType.Plane
      Subtype.Eldraine -> Set.singleton CardType.Plane
      Subtype.Equilor -> Set.singleton CardType.Plane
      Subtype.Ergamon -> Set.singleton CardType.Plane
      Subtype.Fabacin -> Set.singleton CardType.Plane
      Subtype.Fiora -> Set.singleton CardType.Plane
      Subtype.Gallifrey -> Set.singleton CardType.Plane
      Subtype.Gargantikar -> Set.singleton CardType.Plane
      Subtype.Gobakhan -> Set.singleton CardType.Plane
      Subtype.HorseheadNebula -> Set.singleton CardType.Plane
      Subtype.Ikoria -> Set.singleton CardType.Plane
      Subtype.Innistrad -> Set.singleton CardType.Plane
      Subtype.Iquatana -> Set.singleton CardType.Plane
      Subtype.Ir -> Set.singleton CardType.Plane
      Subtype.Ixalan -> Set.singleton CardType.Plane
      Subtype.Kaldheim -> Set.singleton CardType.Plane
      Subtype.Kamigawa -> Set.singleton CardType.Plane
      Subtype.Kandoka -> Set.singleton CardType.Plane
      Subtype.Karsus -> Set.singleton CardType.Plane
      Subtype.Kephalai -> Set.singleton CardType.Plane
      Subtype.Kinshala -> Set.singleton CardType.Plane
      Subtype.Kolbahan -> Set.singleton CardType.Plane
      Subtype.Kylem -> Set.singleton CardType.Plane
      Subtype.Kyneth -> Set.singleton CardType.Plane
      Subtype.TheLibrary -> Set.singleton CardType.Plane
      Subtype.Lorwyn -> Set.singleton CardType.Plane
      Subtype.Luvion -> Set.singleton CardType.Plane
      Subtype.Mars -> Set.singleton CardType.Plane
      Subtype.Mercadia -> Set.singleton CardType.Plane
      Subtype.Mirrodin -> Set.singleton CardType.Plane
      Subtype.Moag -> Set.singleton CardType.Plane
      Subtype.Mongseng -> Set.singleton CardType.Plane
      Subtype.Moon -> Set.singleton CardType.Plane
      Subtype.Muraganda -> Set.singleton CardType.Plane
      Subtype.Necros -> Set.singleton CardType.Plane
      Subtype.NewEarth -> Set.singleton CardType.Plane
      Subtype.NewPhyrexia -> Set.singleton CardType.Plane
      Subtype.OutsideMuttersSpiral -> Set.singleton CardType.Plane
      Subtype.Phyrexia -> Set.singleton CardType.Plane
      Subtype.Pyrulea -> Set.singleton CardType.Plane
      Subtype.Rabiah -> Set.singleton CardType.Plane
      Subtype.Rath -> Set.singleton CardType.Plane
      Subtype.Ravnica -> Set.singleton CardType.Plane
      Subtype.Regatha -> Set.singleton CardType.Plane
      Subtype.Segovia -> Set.singleton CardType.Plane
      Subtype.SerrasRealm -> Set.singleton CardType.Plane
      Subtype.Shadowmoor -> Set.singleton CardType.Plane
      Subtype.Shandalar -> Set.singleton CardType.Plane
      Subtype.Shenmeng -> Set.singleton CardType.Plane
      Subtype.Skaro -> Set.singleton CardType.Plane
      Subtype.Tarkir -> Set.singleton CardType.Plane
      Subtype.Theros -> Set.singleton CardType.Plane
      Subtype.Time -> Set.singleton CardType.Plane
      Subtype.Trenzalore -> Set.singleton CardType.Plane
      Subtype.Ulgrotha -> Set.singleton CardType.Plane
      Subtype.UnknownPlanet -> Set.singleton CardType.Plane
      Subtype.Valla -> Set.singleton CardType.Plane
      Subtype.Vryn -> Set.singleton CardType.Plane
      Subtype.Wildfire -> Set.singleton CardType.Plane
      Subtype.Xerex -> Set.singleton CardType.Plane
      Subtype.Zendikar -> Set.singleton CardType.Plane
      Subtype.Zhalfir -> Set.singleton CardType.Plane
      -- CR 205.3p's one dungeon type.
      Subtype.Undercity -> Set.singleton CardType.Dungeon
      -- CR 205.3q's one battle type.
      Subtype.Siege -> Set.singleton CardType.Battle
      -- CR 205.3k: instants and sorceries SHARE the spell types, so a spell
      -- type answers with both.
      Subtype.Adventure -> Set.fromList [CardType.Instant, CardType.Sorcery]
      Subtype.Arcane -> Set.fromList [CardType.Instant, CardType.Sorcery]
      Subtype.Lesson -> Set.fromList [CardType.Instant, CardType.Sorcery]
      Subtype.Omen -> Set.fromList [CardType.Instant, CardType.Sorcery]
      Subtype.Trap -> Set.fromList [CardType.Instant, CardType.Sorcery]
      _ -> Set.empty
