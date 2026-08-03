module Pawl.Codec.Subtype where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Subtype as Subtype

toJson :: Subtype.Subtype -> Value.Value
toJson s = Common.nullary $ case s of
  Subtype.Adventure -> "Adventure"
  Subtype.Advisor -> "Advisor"
  Subtype.Aetherborn -> "Aetherborn"
  Subtype.Ajani -> "Ajani"
  Subtype.Alara -> "Alara"
  Subtype.AlfavaMetraxis -> "AlfavaMetraxis"
  Subtype.Alien -> "Alien"
  Subtype.Ally -> "Ally"
  Subtype.Aminatou -> "Aminatou"
  Subtype.Amonkhet -> "Amonkhet"
  Subtype.AndrozaniMinor -> "AndrozaniMinor"
  Subtype.Angel -> "Angel"
  Subtype.Angrath -> "Angrath"
  Subtype.Antausia -> "Antausia"
  Subtype.Antelope -> "Antelope"
  Subtype.Apalapucia -> "Apalapucia"
  Subtype.Ape -> "Ape"
  Subtype.Arcane -> "Arcane"
  Subtype.Arcavios -> "Arcavios"
  Subtype.Archer -> "Archer"
  Subtype.Archon -> "Archon"
  Subtype.Arkhos -> "Arkhos"
  Subtype.Arlinn -> "Arlinn"
  Subtype.Armadillo -> "Armadillo"
  Subtype.Army -> "Army"
  Subtype.Artificer -> "Artificer"
  Subtype.Ashiok -> "Ashiok"
  Subtype.Assassin -> "Assassin"
  Subtype.AssemblyWorker -> "AssemblyWorker"
  Subtype.Astartes -> "Astartes"
  Subtype.Atog -> "Atog"
  Subtype.Attraction -> "Attraction"
  Subtype.Aura -> "Aura"
  Subtype.Aurochs -> "Aurochs"
  Subtype.Avatar -> "Avatar"
  Subtype.Avishkar -> "Avishkar"
  Subtype.Azgol -> "Azgol"
  Subtype.Azra -> "Azra"
  Subtype.Background -> "Background"
  Subtype.Badger -> "Badger"
  Subtype.Bahamut -> "Bahamut"
  Subtype.Balloon -> "Balloon"
  Subtype.Barbarian -> "Barbarian"
  Subtype.Bard -> "Bard"
  Subtype.Basilisk -> "Basilisk"
  Subtype.Basri -> "Basri"
  Subtype.Bat -> "Bat"
  Subtype.Bear -> "Bear"
  Subtype.Beast -> "Beast"
  Subtype.Beaver -> "Beaver"
  Subtype.Beeble -> "Beeble"
  Subtype.Beholder -> "Beholder"
  Subtype.Belenon -> "Belenon"
  Subtype.Berserker -> "Berserker"
  Subtype.Bird -> "Bird"
  Subtype.Bison -> "Bison"
  Subtype.Blinkmoth -> "Blinkmoth"
  Subtype.Blood -> "Blood"
  Subtype.Boar -> "Boar"
  Subtype.Bobblehead -> "Bobblehead"
  Subtype.Bolas -> "Bolas"
  Subtype.BolassMeditationRealm -> "BolassMeditationRealm"
  Subtype.Book -> "Book"
  Subtype.Bringer -> "Bringer"
  Subtype.Brushwagg -> "Brushwagg"
  Subtype.Calix -> "Calix"
  Subtype.Camarid -> "Camarid"
  Subtype.Camel -> "Camel"
  Subtype.Capenna -> "Capenna"
  Subtype.Capybara -> "Capybara"
  Subtype.Caribou -> "Caribou"
  Subtype.Carrier -> "Carrier"
  Subtype.Cartouche -> "Cartouche"
  Subtype.Case -> "Case"
  Subtype.Cat -> "Cat"
  Subtype.Cave -> "Cave"
  Subtype.Centaur -> "Centaur"
  Subtype.Chandra -> "Chandra"
  Subtype.Child -> "Child"
  Subtype.Chimera -> "Chimera"
  Subtype.Citizen -> "Citizen"
  Subtype.Class -> "Class"
  Subtype.Cleric -> "Cleric"
  Subtype.Clown -> "Clown"
  Subtype.Clue -> "Clue"
  Subtype.Cockatrice -> "Cockatrice"
  Subtype.Comet -> "Comet"
  Subtype.Construct -> "Construct"
  Subtype.Contraption -> "Contraption"
  Subtype.Coward -> "Coward"
  Subtype.Coyote -> "Coyote"
  Subtype.Crab -> "Crab"
  Subtype.Cridhe -> "Cridhe"
  Subtype.Crocodile -> "Crocodile"
  Subtype.Ctan -> "Ctan"
  Subtype.Curse -> "Curse"
  Subtype.Custodes -> "Custodes"
  Subtype.Cyberman -> "Cyberman"
  Subtype.Cyclops -> "Cyclops"
  Subtype.Dack -> "Dack"
  Subtype.Dakkon -> "Dakkon"
  Subtype.Dalek -> "Dalek"
  Subtype.Daretti -> "Daretti"
  Subtype.Darillium -> "Darillium"
  Subtype.Dauthi -> "Dauthi"
  Subtype.Davriel -> "Davriel"
  Subtype.Dellian -> "Dellian"
  Subtype.Demigod -> "Demigod"
  Subtype.Demon -> "Demon"
  Subtype.Desert -> "Desert"
  Subtype.Deserter -> "Deserter"
  Subtype.Detective -> "Detective"
  Subtype.Devil -> "Devil"
  Subtype.Dihada -> "Dihada"
  Subtype.Dinosaur -> "Dinosaur"
  Subtype.Djinn -> "Djinn"
  Subtype.Doctor -> "Doctor"
  Subtype.Dog -> "Dog"
  Subtype.Dominaria -> "Dominaria"
  Subtype.Domri -> "Domri"
  Subtype.Dovin -> "Dovin"
  Subtype.Dragon -> "Dragon"
  Subtype.Drake -> "Drake"
  Subtype.Dreadnought -> "Dreadnought"
  Subtype.Drix -> "Drix"
  Subtype.Drone -> "Drone"
  Subtype.Druid -> "Druid"
  Subtype.Dryad -> "Dryad"
  Subtype.Dwarf -> "Dwarf"
  Subtype.Earth -> "Earth"
  Subtype.Echidna -> "Echidna"
  Subtype.Echoir -> "Echoir"
  Subtype.Efreet -> "Efreet"
  Subtype.Egg -> "Egg"
  Subtype.Elder -> "Elder"
  Subtype.Eldraine -> "Eldraine"
  Subtype.Eldrazi -> "Eldrazi"
  Subtype.Elemental -> "Elemental"
  Subtype.Elephant -> "Elephant"
  Subtype.Elf -> "Elf"
  Subtype.Elk -> "Elk"
  Subtype.Ellywick -> "Ellywick"
  Subtype.Elminster -> "Elminster"
  Subtype.Elspeth -> "Elspeth"
  Subtype.Employee -> "Employee"
  Subtype.Equilor -> "Equilor"
  Subtype.Equipment -> "Equipment"
  Subtype.Ergamon -> "Ergamon"
  Subtype.Estrid -> "Estrid"
  Subtype.Eternal -> "Eternal"
  Subtype.Eye -> "Eye"
  Subtype.Fabacin -> "Fabacin"
  Subtype.Faerie -> "Faerie"
  Subtype.Ferret -> "Ferret"
  Subtype.Fiora -> "Fiora"
  Subtype.Fish -> "Fish"
  Subtype.Flagbearer -> "Flagbearer"
  Subtype.Food -> "Food"
  Subtype.Forest -> "Forest"
  Subtype.Fortification -> "Fortification"
  Subtype.Fox -> "Fox"
  Subtype.Fractal -> "Fractal"
  Subtype.Freyalise -> "Freyalise"
  Subtype.Frog -> "Frog"
  Subtype.Fungus -> "Fungus"
  Subtype.Gallifrey -> "Gallifrey"
  Subtype.Gamer -> "Gamer"
  Subtype.Gamma -> "Gamma"
  Subtype.Gargantikar -> "Gargantikar"
  Subtype.Gargoyle -> "Gargoyle"
  Subtype.Garruk -> "Garruk"
  Subtype.Gate -> "Gate"
  Subtype.Germ -> "Germ"
  Subtype.Giant -> "Giant"
  Subtype.Gideon -> "Gideon"
  Subtype.Giraffe -> "Giraffe"
  Subtype.Gith -> "Gith"
  Subtype.Glimmer -> "Glimmer"
  Subtype.Gnoll -> "Gnoll"
  Subtype.Gnome -> "Gnome"
  Subtype.Goat -> "Goat"
  Subtype.Gobakhan -> "Gobakhan"
  Subtype.Goblin -> "Goblin"
  Subtype.God -> "God"
  Subtype.Gold -> "Gold"
  Subtype.Golem -> "Golem"
  Subtype.Gorgon -> "Gorgon"
  Subtype.Graveborn -> "Graveborn"
  Subtype.Gremlin -> "Gremlin"
  Subtype.Griffin -> "Griffin"
  Subtype.Grist -> "Grist"
  Subtype.Guest -> "Guest"
  Subtype.Guff -> "Guff"
  Subtype.Hag -> "Hag"
  Subtype.Halfling -> "Halfling"
  Subtype.Hamster -> "Hamster"
  Subtype.Harpy -> "Harpy"
  Subtype.Hedgehog -> "Hedgehog"
  Subtype.Hellion -> "Hellion"
  Subtype.Hero -> "Hero"
  Subtype.Hippo -> "Hippo"
  Subtype.Hippogriff -> "Hippogriff"
  Subtype.Homarid -> "Homarid"
  Subtype.Homunculus -> "Homunculus"
  Subtype.Horror -> "Horror"
  Subtype.Horse -> "Horse"
  Subtype.HorseheadNebula -> "HorseheadNebula"
  Subtype.Huatli -> "Huatli"
  Subtype.Human -> "Human"
  Subtype.Hydra -> "Hydra"
  Subtype.Hyena -> "Hyena"
  Subtype.Ikoria -> "Ikoria"
  Subtype.Illusion -> "Illusion"
  Subtype.Imp -> "Imp"
  Subtype.Incarnation -> "Incarnation"
  Subtype.Incubator -> "Incubator"
  Subtype.Infinity -> "Infinity"
  Subtype.Inhuman -> "Inhuman"
  Subtype.Inkling -> "Inkling"
  Subtype.Innistrad -> "Innistrad"
  Subtype.Inquisitor -> "Inquisitor"
  Subtype.Insect -> "Insect"
  Subtype.Iquatana -> "Iquatana"
  Subtype.Ir -> "Ir"
  Subtype.Island -> "Island"
  Subtype.Ixalan -> "Ixalan"
  Subtype.Jace -> "Jace"
  Subtype.Jackal -> "Jackal"
  Subtype.Jared -> "Jared"
  Subtype.Jaya -> "Jaya"
  Subtype.Jellyfish -> "Jellyfish"
  Subtype.Jeska -> "Jeska"
  Subtype.Juggernaut -> "Juggernaut"
  Subtype.Junk -> "Junk"
  Subtype.Kaito -> "Kaito"
  Subtype.Kaldheim -> "Kaldheim"
  Subtype.Kamigawa -> "Kamigawa"
  Subtype.Kandoka -> "Kandoka"
  Subtype.Kangaroo -> "Kangaroo"
  Subtype.Karn -> "Karn"
  Subtype.Karsus -> "Karsus"
  Subtype.Kasmina -> "Kasmina"
  Subtype.Kavu -> "Kavu"
  Subtype.Kaya -> "Kaya"
  Subtype.Kephalai -> "Kephalai"
  Subtype.Kinshala -> "Kinshala"
  Subtype.Kiora -> "Kiora"
  Subtype.Kirin -> "Kirin"
  Subtype.Kithkin -> "Kithkin"
  Subtype.Knight -> "Knight"
  Subtype.Kobold -> "Kobold"
  Subtype.Kolbahan -> "Kolbahan"
  Subtype.Kor -> "Kor"
  Subtype.Koth -> "Koth"
  Subtype.Kraken -> "Kraken"
  Subtype.Kree -> "Kree"
  Subtype.Kylem -> "Kylem"
  Subtype.Kyneth -> "Kyneth"
  Subtype.Lair -> "Lair"
  Subtype.Lamia -> "Lamia"
  Subtype.Lammasu -> "Lammasu"
  Subtype.Lander -> "Lander"
  Subtype.Leech -> "Leech"
  Subtype.Lemur -> "Lemur"
  Subtype.Lesson -> "Lesson"
  Subtype.Leviathan -> "Leviathan"
  Subtype.Lhurgoyf -> "Lhurgoyf"
  Subtype.Licid -> "Licid"
  Subtype.Liliana -> "Liliana"
  Subtype.Lizard -> "Lizard"
  Subtype.Llama -> "Llama"
  Subtype.Lobster -> "Lobster"
  Subtype.Locus -> "Locus"
  Subtype.Lolth -> "Lolth"
  Subtype.Lorwyn -> "Lorwyn"
  Subtype.Lukka -> "Lukka"
  Subtype.Luvion -> "Luvion"
  Subtype.Manticore -> "Manticore"
  Subtype.Map -> "Map"
  Subtype.Mars -> "Mars"
  Subtype.Masticore -> "Masticore"
  Subtype.Mercadia -> "Mercadia"
  Subtype.Mercenary -> "Mercenary"
  Subtype.Merfolk -> "Merfolk"
  Subtype.Metathran -> "Metathran"
  Subtype.Mine -> "Mine"
  Subtype.Minion -> "Minion"
  Subtype.Minotaur -> "Minotaur"
  Subtype.Minsc -> "Minsc"
  Subtype.Mirrodin -> "Mirrodin"
  Subtype.Mite -> "Mite"
  Subtype.Moag -> "Moag"
  Subtype.Mole -> "Mole"
  Subtype.Monger -> "Monger"
  Subtype.Mongoose -> "Mongoose"
  Subtype.Mongseng -> "Mongseng"
  Subtype.Monk -> "Monk"
  Subtype.Monkey -> "Monkey"
  Subtype.Moogle -> "Moogle"
  Subtype.Moon -> "Moon"
  Subtype.Moonfolk -> "Moonfolk"
  Subtype.Mordenkainen -> "Mordenkainen"
  Subtype.Mount -> "Mount"
  Subtype.Mountain -> "Mountain"
  Subtype.Mouse -> "Mouse"
  Subtype.Muraganda -> "Muraganda"
  Subtype.Mutagen -> "Mutagen"
  Subtype.Mutant -> "Mutant"
  Subtype.Myr -> "Myr"
  Subtype.Mystic -> "Mystic"
  Subtype.Nahiri -> "Nahiri"
  Subtype.Narset -> "Narset"
  Subtype.Nautilus -> "Nautilus"
  Subtype.Necron -> "Necron"
  Subtype.Necros -> "Necros"
  Subtype.Nephilim -> "Nephilim"
  Subtype.NewEarth -> "NewEarth"
  Subtype.NewPhyrexia -> "NewPhyrexia"
  Subtype.Nightmare -> "Nightmare"
  Subtype.Nightstalker -> "Nightstalker"
  Subtype.Niko -> "Niko"
  Subtype.Ninja -> "Ninja"
  Subtype.Nissa -> "Nissa"
  Subtype.Nixilis -> "Nixilis"
  Subtype.Noble -> "Noble"
  Subtype.Noggle -> "Noggle"
  Subtype.Nomad -> "Nomad"
  Subtype.Nymph -> "Nymph"
  Subtype.Octopus -> "Octopus"
  Subtype.Ogre -> "Ogre"
  Subtype.Oko -> "Oko"
  Subtype.Omen -> "Omen"
  Subtype.Ooze -> "Ooze"
  Subtype.Orb -> "Orb"
  Subtype.Orc -> "Orc"
  Subtype.Orgg -> "Orgg"
  Subtype.Otter -> "Otter"
  Subtype.Ouphe -> "Ouphe"
  Subtype.OutsideMuttersSpiral -> "OutsideMuttersSpiral"
  Subtype.Ox -> "Ox"
  Subtype.Oyster -> "Oyster"
  Subtype.Pangolin -> "Pangolin"
  Subtype.Peasant -> "Peasant"
  Subtype.Pegasus -> "Pegasus"
  Subtype.Pentavite -> "Pentavite"
  Subtype.Performer -> "Performer"
  Subtype.Pest -> "Pest"
  Subtype.Phelddagrif -> "Phelddagrif"
  Subtype.Phoenix -> "Phoenix"
  Subtype.Phyrexia -> "Phyrexia"
  Subtype.Phyrexian -> "Phyrexian"
  Subtype.Pilot -> "Pilot"
  Subtype.Pincher -> "Pincher"
  Subtype.Pirate -> "Pirate"
  Subtype.Plains -> "Plains"
  Subtype.Plan -> "Plan"
  Subtype.Planet -> "Planet"
  Subtype.Plant -> "Plant"
  Subtype.Platypus -> "Platypus"
  Subtype.Porcupine -> "Porcupine"
  Subtype.Possum -> "Possum"
  Subtype.PowerPlant -> "PowerPlant"
  Subtype.Powerstone -> "Powerstone"
  Subtype.Praetor -> "Praetor"
  Subtype.Primarch -> "Primarch"
  Subtype.Prism -> "Prism"
  Subtype.Processor -> "Processor"
  Subtype.Pyrulea -> "Pyrulea"
  Subtype.Qu -> "Qu"
  Subtype.Quintorius -> "Quintorius"
  Subtype.Rabbit -> "Rabbit"
  Subtype.Rabiah -> "Rabiah"
  Subtype.Raccoon -> "Raccoon"
  Subtype.Ral -> "Ral"
  Subtype.Ranger -> "Ranger"
  Subtype.Rat -> "Rat"
  Subtype.Rath -> "Rath"
  Subtype.Ravnica -> "Ravnica"
  Subtype.Rebel -> "Rebel"
  Subtype.Reflection -> "Reflection"
  Subtype.Regatha -> "Regatha"
  Subtype.Rhino -> "Rhino"
  Subtype.Rigger -> "Rigger"
  Subtype.Robot -> "Robot"
  Subtype.Rogue -> "Rogue"
  Subtype.Role -> "Role"
  Subtype.Room -> "Room"
  Subtype.Rowan -> "Rowan"
  Subtype.Rune -> "Rune"
  Subtype.Sable -> "Sable"
  Subtype.Saga -> "Saga"
  Subtype.Saheeli -> "Saheeli"
  Subtype.Salamander -> "Salamander"
  Subtype.Samurai -> "Samurai"
  Subtype.Samut -> "Samut"
  Subtype.Sand -> "Sand"
  Subtype.Saproling -> "Saproling"
  Subtype.Sarkhan -> "Sarkhan"
  Subtype.Satyr -> "Satyr"
  Subtype.Scarecrow -> "Scarecrow"
  Subtype.Scientist -> "Scientist"
  Subtype.Scion -> "Scion"
  Subtype.Scorpion -> "Scorpion"
  Subtype.Scout -> "Scout"
  Subtype.Sculpture -> "Sculpture"
  Subtype.Seal -> "Seal"
  Subtype.Segovia -> "Segovia"
  Subtype.Serf -> "Serf"
  Subtype.Serpent -> "Serpent"
  Subtype.Serra -> "Serra"
  Subtype.SerrasRealm -> "SerrasRealm"
  Subtype.Servo -> "Servo"
  Subtype.Shade -> "Shade"
  Subtype.Shadowmoor -> "Shadowmoor"
  Subtype.Shaman -> "Shaman"
  Subtype.Shandalar -> "Shandalar"
  Subtype.Shapeshifter -> "Shapeshifter"
  Subtype.Shard -> "Shard"
  Subtype.Shark -> "Shark"
  Subtype.Sheep -> "Sheep"
  Subtype.Shenmeng -> "Shenmeng"
  Subtype.Shiar -> "Shiar"
  Subtype.Shrine -> "Shrine"
  Subtype.Siege -> "Siege"
  Subtype.Siren -> "Siren"
  Subtype.Sivitri -> "Sivitri"
  Subtype.Skaro -> "Skaro"
  Subtype.Skeleton -> "Skeleton"
  Subtype.Skrull -> "Skrull"
  Subtype.Skunk -> "Skunk"
  Subtype.Slith -> "Slith"
  Subtype.Sliver -> "Sliver"
  Subtype.Sloth -> "Sloth"
  Subtype.Slug -> "Slug"
  Subtype.Snail -> "Snail"
  Subtype.Snake -> "Snake"
  Subtype.Soldier -> "Soldier"
  Subtype.Soltari -> "Soltari"
  Subtype.Sorcerer -> "Sorcerer"
  Subtype.Sorin -> "Sorin"
  Subtype.Spacecraft -> "Spacecraft"
  Subtype.Spawn -> "Spawn"
  Subtype.Specter -> "Specter"
  Subtype.Spellshaper -> "Spellshaper"
  Subtype.Sphere -> "Sphere"
  Subtype.Sphinx -> "Sphinx"
  Subtype.Spider -> "Spider"
  Subtype.Spike -> "Spike"
  Subtype.Spirit -> "Spirit"
  Subtype.Splinter -> "Splinter"
  Subtype.Sponge -> "Sponge"
  Subtype.Spy -> "Spy"
  Subtype.Squid -> "Squid"
  Subtype.Squirrel -> "Squirrel"
  Subtype.Starfish -> "Starfish"
  Subtype.Stone -> "Stone"
  Subtype.Surrakar -> "Surrakar"
  Subtype.Survivor -> "Survivor"
  Subtype.Swamp -> "Swamp"
  Subtype.Symbiote -> "Symbiote"
  Subtype.Synth -> "Synth"
  Subtype.Szat -> "Szat"
  Subtype.Tamiyo -> "Tamiyo"
  Subtype.Tarkir -> "Tarkir"
  Subtype.Tasha -> "Tasha"
  Subtype.Teferi -> "Teferi"
  Subtype.Tentacle -> "Tentacle"
  Subtype.Tetravite -> "Tetravite"
  Subtype.Teyo -> "Teyo"
  Subtype.Tezzeret -> "Tezzeret"
  Subtype.Thalakos -> "Thalakos"
  Subtype.TheAbyss -> "TheAbyss"
  Subtype.TheDalekAsylum -> "TheDalekAsylum"
  Subtype.TheLibrary -> "TheLibrary"
  Subtype.Theros -> "Theros"
  Subtype.Thopter -> "Thopter"
  Subtype.Thrull -> "Thrull"
  Subtype.Tibalt -> "Tibalt"
  Subtype.Tiefling -> "Tiefling"
  Subtype.Time -> "Time"
  Subtype.TimeLord -> "TimeLord"
  Subtype.Tower -> "Tower"
  Subtype.Town -> "Town"
  Subtype.Toy -> "Toy"
  Subtype.Trap -> "Trap"
  Subtype.Treasure -> "Treasure"
  Subtype.Treefolk -> "Treefolk"
  Subtype.Trenzalore -> "Trenzalore"
  Subtype.Trilobite -> "Trilobite"
  Subtype.Triskelavite -> "Triskelavite"
  Subtype.Troll -> "Troll"
  Subtype.Turtle -> "Turtle"
  Subtype.Tyranid -> "Tyranid"
  Subtype.Tyvar -> "Tyvar"
  Subtype.Ugin -> "Ugin"
  Subtype.Ulgrotha -> "Ulgrotha"
  Subtype.Undercity -> "Undercity"
  Subtype.Unicorn -> "Unicorn"
  Subtype.UnknownPlanet -> "UnknownPlanet"
  Subtype.Urza -> "Urza"
  Subtype.Urzas -> "Urzas"
  Subtype.Utrom -> "Utrom"
  Subtype.Valla -> "Valla"
  Subtype.Vampire -> "Vampire"
  Subtype.Varmint -> "Varmint"
  Subtype.Vedalken -> "Vedalken"
  Subtype.Vehicle -> "Vehicle"
  Subtype.Venser -> "Venser"
  Subtype.Vibranium -> "Vibranium"
  Subtype.Villain -> "Villain"
  Subtype.Vivien -> "Vivien"
  Subtype.Volver -> "Volver"
  Subtype.Vraska -> "Vraska"
  Subtype.Vronos -> "Vronos"
  Subtype.Vryn -> "Vryn"
  Subtype.Wall -> "Wall"
  Subtype.Walrus -> "Walrus"
  Subtype.Warlock -> "Warlock"
  Subtype.Warrior -> "Warrior"
  Subtype.Weasel -> "Weasel"
  Subtype.Weird -> "Weird"
  Subtype.Werewolf -> "Werewolf"
  Subtype.Whale -> "Whale"
  Subtype.Wildfire -> "Wildfire"
  Subtype.Will -> "Will"
  Subtype.Windgrace -> "Windgrace"
  Subtype.Wizard -> "Wizard"
  Subtype.Wolf -> "Wolf"
  Subtype.Wolverine -> "Wolverine"
  Subtype.Wombat -> "Wombat"
  Subtype.Worm -> "Worm"
  Subtype.Wraith -> "Wraith"
  Subtype.Wrenn -> "Wrenn"
  Subtype.Wurm -> "Wurm"
  Subtype.Xenagos -> "Xenagos"
  Subtype.Xerex -> "Xerex"
  Subtype.Yanggu -> "Yanggu"
  Subtype.Yanling -> "Yanling"
  Subtype.Yeti -> "Yeti"
  Subtype.Zariel -> "Zariel"
  Subtype.Zendikar -> "Zendikar"
  Subtype.Zhalfir -> "Zhalfir"
  Subtype.Zombie -> "Zombie"
  Subtype.Zubera -> "Zubera"

fromJson :: Value.Value -> Either Text.Text Subtype.Subtype
fromJson =
  Common.decodeNullary
    "Subtype"
    [ ("Adventure", Subtype.Adventure),
      ("Advisor", Subtype.Advisor),
      ("Aetherborn", Subtype.Aetherborn),
      ("Ajani", Subtype.Ajani),
      ("Alara", Subtype.Alara),
      ("AlfavaMetraxis", Subtype.AlfavaMetraxis),
      ("Alien", Subtype.Alien),
      ("Ally", Subtype.Ally),
      ("Aminatou", Subtype.Aminatou),
      ("Amonkhet", Subtype.Amonkhet),
      ("AndrozaniMinor", Subtype.AndrozaniMinor),
      ("Angel", Subtype.Angel),
      ("Angrath", Subtype.Angrath),
      ("Antausia", Subtype.Antausia),
      ("Antelope", Subtype.Antelope),
      ("Apalapucia", Subtype.Apalapucia),
      ("Ape", Subtype.Ape),
      ("Arcane", Subtype.Arcane),
      ("Arcavios", Subtype.Arcavios),
      ("Archer", Subtype.Archer),
      ("Archon", Subtype.Archon),
      ("Arkhos", Subtype.Arkhos),
      ("Arlinn", Subtype.Arlinn),
      ("Armadillo", Subtype.Armadillo),
      ("Army", Subtype.Army),
      ("Artificer", Subtype.Artificer),
      ("Ashiok", Subtype.Ashiok),
      ("Assassin", Subtype.Assassin),
      ("AssemblyWorker", Subtype.AssemblyWorker),
      ("Astartes", Subtype.Astartes),
      ("Atog", Subtype.Atog),
      ("Attraction", Subtype.Attraction),
      ("Aura", Subtype.Aura),
      ("Aurochs", Subtype.Aurochs),
      ("Avatar", Subtype.Avatar),
      ("Avishkar", Subtype.Avishkar),
      ("Azgol", Subtype.Azgol),
      ("Azra", Subtype.Azra),
      ("Background", Subtype.Background),
      ("Badger", Subtype.Badger),
      ("Bahamut", Subtype.Bahamut),
      ("Balloon", Subtype.Balloon),
      ("Barbarian", Subtype.Barbarian),
      ("Bard", Subtype.Bard),
      ("Basilisk", Subtype.Basilisk),
      ("Basri", Subtype.Basri),
      ("Bat", Subtype.Bat),
      ("Bear", Subtype.Bear),
      ("Beast", Subtype.Beast),
      ("Beaver", Subtype.Beaver),
      ("Beeble", Subtype.Beeble),
      ("Beholder", Subtype.Beholder),
      ("Belenon", Subtype.Belenon),
      ("Berserker", Subtype.Berserker),
      ("Bird", Subtype.Bird),
      ("Bison", Subtype.Bison),
      ("Blinkmoth", Subtype.Blinkmoth),
      ("Blood", Subtype.Blood),
      ("Boar", Subtype.Boar),
      ("Bobblehead", Subtype.Bobblehead),
      ("Bolas", Subtype.Bolas),
      ("BolassMeditationRealm", Subtype.BolassMeditationRealm),
      ("Book", Subtype.Book),
      ("Bringer", Subtype.Bringer),
      ("Brushwagg", Subtype.Brushwagg),
      ("Calix", Subtype.Calix),
      ("Camarid", Subtype.Camarid),
      ("Camel", Subtype.Camel),
      ("Capenna", Subtype.Capenna),
      ("Capybara", Subtype.Capybara),
      ("Caribou", Subtype.Caribou),
      ("Carrier", Subtype.Carrier),
      ("Cartouche", Subtype.Cartouche),
      ("Case", Subtype.Case),
      ("Cat", Subtype.Cat),
      ("Cave", Subtype.Cave),
      ("Centaur", Subtype.Centaur),
      ("Chandra", Subtype.Chandra),
      ("Child", Subtype.Child),
      ("Chimera", Subtype.Chimera),
      ("Citizen", Subtype.Citizen),
      ("Class", Subtype.Class),
      ("Cleric", Subtype.Cleric),
      ("Clown", Subtype.Clown),
      ("Clue", Subtype.Clue),
      ("Cockatrice", Subtype.Cockatrice),
      ("Comet", Subtype.Comet),
      ("Construct", Subtype.Construct),
      ("Contraption", Subtype.Contraption),
      ("Coward", Subtype.Coward),
      ("Coyote", Subtype.Coyote),
      ("Crab", Subtype.Crab),
      ("Cridhe", Subtype.Cridhe),
      ("Crocodile", Subtype.Crocodile),
      ("Ctan", Subtype.Ctan),
      ("Curse", Subtype.Curse),
      ("Custodes", Subtype.Custodes),
      ("Cyberman", Subtype.Cyberman),
      ("Cyclops", Subtype.Cyclops),
      ("Dack", Subtype.Dack),
      ("Dakkon", Subtype.Dakkon),
      ("Dalek", Subtype.Dalek),
      ("Daretti", Subtype.Daretti),
      ("Darillium", Subtype.Darillium),
      ("Dauthi", Subtype.Dauthi),
      ("Davriel", Subtype.Davriel),
      ("Dellian", Subtype.Dellian),
      ("Demigod", Subtype.Demigod),
      ("Demon", Subtype.Demon),
      ("Desert", Subtype.Desert),
      ("Deserter", Subtype.Deserter),
      ("Detective", Subtype.Detective),
      ("Devil", Subtype.Devil),
      ("Dihada", Subtype.Dihada),
      ("Dinosaur", Subtype.Dinosaur),
      ("Djinn", Subtype.Djinn),
      ("Doctor", Subtype.Doctor),
      ("Dog", Subtype.Dog),
      ("Dominaria", Subtype.Dominaria),
      ("Domri", Subtype.Domri),
      ("Dovin", Subtype.Dovin),
      ("Dragon", Subtype.Dragon),
      ("Drake", Subtype.Drake),
      ("Dreadnought", Subtype.Dreadnought),
      ("Drix", Subtype.Drix),
      ("Drone", Subtype.Drone),
      ("Druid", Subtype.Druid),
      ("Dryad", Subtype.Dryad),
      ("Dwarf", Subtype.Dwarf),
      ("Earth", Subtype.Earth),
      ("Echidna", Subtype.Echidna),
      ("Echoir", Subtype.Echoir),
      ("Efreet", Subtype.Efreet),
      ("Egg", Subtype.Egg),
      ("Elder", Subtype.Elder),
      ("Eldraine", Subtype.Eldraine),
      ("Eldrazi", Subtype.Eldrazi),
      ("Elemental", Subtype.Elemental),
      ("Elephant", Subtype.Elephant),
      ("Elf", Subtype.Elf),
      ("Elk", Subtype.Elk),
      ("Ellywick", Subtype.Ellywick),
      ("Elminster", Subtype.Elminster),
      ("Elspeth", Subtype.Elspeth),
      ("Employee", Subtype.Employee),
      ("Equilor", Subtype.Equilor),
      ("Equipment", Subtype.Equipment),
      ("Ergamon", Subtype.Ergamon),
      ("Estrid", Subtype.Estrid),
      ("Eternal", Subtype.Eternal),
      ("Eye", Subtype.Eye),
      ("Fabacin", Subtype.Fabacin),
      ("Faerie", Subtype.Faerie),
      ("Ferret", Subtype.Ferret),
      ("Fiora", Subtype.Fiora),
      ("Fish", Subtype.Fish),
      ("Flagbearer", Subtype.Flagbearer),
      ("Food", Subtype.Food),
      ("Forest", Subtype.Forest),
      ("Fortification", Subtype.Fortification),
      ("Fox", Subtype.Fox),
      ("Fractal", Subtype.Fractal),
      ("Freyalise", Subtype.Freyalise),
      ("Frog", Subtype.Frog),
      ("Fungus", Subtype.Fungus),
      ("Gallifrey", Subtype.Gallifrey),
      ("Gamer", Subtype.Gamer),
      ("Gamma", Subtype.Gamma),
      ("Gargantikar", Subtype.Gargantikar),
      ("Gargoyle", Subtype.Gargoyle),
      ("Garruk", Subtype.Garruk),
      ("Gate", Subtype.Gate),
      ("Germ", Subtype.Germ),
      ("Giant", Subtype.Giant),
      ("Gideon", Subtype.Gideon),
      ("Giraffe", Subtype.Giraffe),
      ("Gith", Subtype.Gith),
      ("Glimmer", Subtype.Glimmer),
      ("Gnoll", Subtype.Gnoll),
      ("Gnome", Subtype.Gnome),
      ("Goat", Subtype.Goat),
      ("Gobakhan", Subtype.Gobakhan),
      ("Goblin", Subtype.Goblin),
      ("God", Subtype.God),
      ("Gold", Subtype.Gold),
      ("Golem", Subtype.Golem),
      ("Gorgon", Subtype.Gorgon),
      ("Graveborn", Subtype.Graveborn),
      ("Gremlin", Subtype.Gremlin),
      ("Griffin", Subtype.Griffin),
      ("Grist", Subtype.Grist),
      ("Guest", Subtype.Guest),
      ("Guff", Subtype.Guff),
      ("Hag", Subtype.Hag),
      ("Halfling", Subtype.Halfling),
      ("Hamster", Subtype.Hamster),
      ("Harpy", Subtype.Harpy),
      ("Hedgehog", Subtype.Hedgehog),
      ("Hellion", Subtype.Hellion),
      ("Hero", Subtype.Hero),
      ("Hippo", Subtype.Hippo),
      ("Hippogriff", Subtype.Hippogriff),
      ("Homarid", Subtype.Homarid),
      ("Homunculus", Subtype.Homunculus),
      ("Horror", Subtype.Horror),
      ("Horse", Subtype.Horse),
      ("HorseheadNebula", Subtype.HorseheadNebula),
      ("Huatli", Subtype.Huatli),
      ("Human", Subtype.Human),
      ("Hydra", Subtype.Hydra),
      ("Hyena", Subtype.Hyena),
      ("Ikoria", Subtype.Ikoria),
      ("Illusion", Subtype.Illusion),
      ("Imp", Subtype.Imp),
      ("Incarnation", Subtype.Incarnation),
      ("Incubator", Subtype.Incubator),
      ("Infinity", Subtype.Infinity),
      ("Inhuman", Subtype.Inhuman),
      ("Inkling", Subtype.Inkling),
      ("Innistrad", Subtype.Innistrad),
      ("Inquisitor", Subtype.Inquisitor),
      ("Insect", Subtype.Insect),
      ("Iquatana", Subtype.Iquatana),
      ("Ir", Subtype.Ir),
      ("Island", Subtype.Island),
      ("Ixalan", Subtype.Ixalan),
      ("Jace", Subtype.Jace),
      ("Jackal", Subtype.Jackal),
      ("Jared", Subtype.Jared),
      ("Jaya", Subtype.Jaya),
      ("Jellyfish", Subtype.Jellyfish),
      ("Jeska", Subtype.Jeska),
      ("Juggernaut", Subtype.Juggernaut),
      ("Junk", Subtype.Junk),
      ("Kaito", Subtype.Kaito),
      ("Kaldheim", Subtype.Kaldheim),
      ("Kamigawa", Subtype.Kamigawa),
      ("Kandoka", Subtype.Kandoka),
      ("Kangaroo", Subtype.Kangaroo),
      ("Karn", Subtype.Karn),
      ("Karsus", Subtype.Karsus),
      ("Kasmina", Subtype.Kasmina),
      ("Kavu", Subtype.Kavu),
      ("Kaya", Subtype.Kaya),
      ("Kephalai", Subtype.Kephalai),
      ("Kinshala", Subtype.Kinshala),
      ("Kiora", Subtype.Kiora),
      ("Kirin", Subtype.Kirin),
      ("Kithkin", Subtype.Kithkin),
      ("Knight", Subtype.Knight),
      ("Kobold", Subtype.Kobold),
      ("Kolbahan", Subtype.Kolbahan),
      ("Kor", Subtype.Kor),
      ("Koth", Subtype.Koth),
      ("Kraken", Subtype.Kraken),
      ("Kree", Subtype.Kree),
      ("Kylem", Subtype.Kylem),
      ("Kyneth", Subtype.Kyneth),
      ("Lair", Subtype.Lair),
      ("Lamia", Subtype.Lamia),
      ("Lammasu", Subtype.Lammasu),
      ("Lander", Subtype.Lander),
      ("Leech", Subtype.Leech),
      ("Lemur", Subtype.Lemur),
      ("Lesson", Subtype.Lesson),
      ("Leviathan", Subtype.Leviathan),
      ("Lhurgoyf", Subtype.Lhurgoyf),
      ("Licid", Subtype.Licid),
      ("Liliana", Subtype.Liliana),
      ("Lizard", Subtype.Lizard),
      ("Llama", Subtype.Llama),
      ("Lobster", Subtype.Lobster),
      ("Locus", Subtype.Locus),
      ("Lolth", Subtype.Lolth),
      ("Lorwyn", Subtype.Lorwyn),
      ("Lukka", Subtype.Lukka),
      ("Luvion", Subtype.Luvion),
      ("Manticore", Subtype.Manticore),
      ("Map", Subtype.Map),
      ("Mars", Subtype.Mars),
      ("Masticore", Subtype.Masticore),
      ("Mercadia", Subtype.Mercadia),
      ("Mercenary", Subtype.Mercenary),
      ("Merfolk", Subtype.Merfolk),
      ("Metathran", Subtype.Metathran),
      ("Mine", Subtype.Mine),
      ("Minion", Subtype.Minion),
      ("Minotaur", Subtype.Minotaur),
      ("Minsc", Subtype.Minsc),
      ("Mirrodin", Subtype.Mirrodin),
      ("Mite", Subtype.Mite),
      ("Moag", Subtype.Moag),
      ("Mole", Subtype.Mole),
      ("Monger", Subtype.Monger),
      ("Mongoose", Subtype.Mongoose),
      ("Mongseng", Subtype.Mongseng),
      ("Monk", Subtype.Monk),
      ("Monkey", Subtype.Monkey),
      ("Moogle", Subtype.Moogle),
      ("Moon", Subtype.Moon),
      ("Moonfolk", Subtype.Moonfolk),
      ("Mordenkainen", Subtype.Mordenkainen),
      ("Mount", Subtype.Mount),
      ("Mountain", Subtype.Mountain),
      ("Mouse", Subtype.Mouse),
      ("Muraganda", Subtype.Muraganda),
      ("Mutagen", Subtype.Mutagen),
      ("Mutant", Subtype.Mutant),
      ("Myr", Subtype.Myr),
      ("Mystic", Subtype.Mystic),
      ("Nahiri", Subtype.Nahiri),
      ("Narset", Subtype.Narset),
      ("Nautilus", Subtype.Nautilus),
      ("Necron", Subtype.Necron),
      ("Necros", Subtype.Necros),
      ("Nephilim", Subtype.Nephilim),
      ("NewEarth", Subtype.NewEarth),
      ("NewPhyrexia", Subtype.NewPhyrexia),
      ("Nightmare", Subtype.Nightmare),
      ("Nightstalker", Subtype.Nightstalker),
      ("Niko", Subtype.Niko),
      ("Ninja", Subtype.Ninja),
      ("Nissa", Subtype.Nissa),
      ("Nixilis", Subtype.Nixilis),
      ("Noble", Subtype.Noble),
      ("Noggle", Subtype.Noggle),
      ("Nomad", Subtype.Nomad),
      ("Nymph", Subtype.Nymph),
      ("Octopus", Subtype.Octopus),
      ("Ogre", Subtype.Ogre),
      ("Oko", Subtype.Oko),
      ("Omen", Subtype.Omen),
      ("Ooze", Subtype.Ooze),
      ("Orb", Subtype.Orb),
      ("Orc", Subtype.Orc),
      ("Orgg", Subtype.Orgg),
      ("Otter", Subtype.Otter),
      ("Ouphe", Subtype.Ouphe),
      ("OutsideMuttersSpiral", Subtype.OutsideMuttersSpiral),
      ("Ox", Subtype.Ox),
      ("Oyster", Subtype.Oyster),
      ("Pangolin", Subtype.Pangolin),
      ("Peasant", Subtype.Peasant),
      ("Pegasus", Subtype.Pegasus),
      ("Pentavite", Subtype.Pentavite),
      ("Performer", Subtype.Performer),
      ("Pest", Subtype.Pest),
      ("Phelddagrif", Subtype.Phelddagrif),
      ("Phoenix", Subtype.Phoenix),
      ("Phyrexia", Subtype.Phyrexia),
      ("Phyrexian", Subtype.Phyrexian),
      ("Pilot", Subtype.Pilot),
      ("Pincher", Subtype.Pincher),
      ("Pirate", Subtype.Pirate),
      ("Plains", Subtype.Plains),
      ("Plan", Subtype.Plan),
      ("Planet", Subtype.Planet),
      ("Plant", Subtype.Plant),
      ("Platypus", Subtype.Platypus),
      ("Porcupine", Subtype.Porcupine),
      ("Possum", Subtype.Possum),
      ("PowerPlant", Subtype.PowerPlant),
      ("Powerstone", Subtype.Powerstone),
      ("Praetor", Subtype.Praetor),
      ("Primarch", Subtype.Primarch),
      ("Prism", Subtype.Prism),
      ("Processor", Subtype.Processor),
      ("Pyrulea", Subtype.Pyrulea),
      ("Qu", Subtype.Qu),
      ("Quintorius", Subtype.Quintorius),
      ("Rabbit", Subtype.Rabbit),
      ("Rabiah", Subtype.Rabiah),
      ("Raccoon", Subtype.Raccoon),
      ("Ral", Subtype.Ral),
      ("Ranger", Subtype.Ranger),
      ("Rat", Subtype.Rat),
      ("Rath", Subtype.Rath),
      ("Ravnica", Subtype.Ravnica),
      ("Rebel", Subtype.Rebel),
      ("Reflection", Subtype.Reflection),
      ("Regatha", Subtype.Regatha),
      ("Rhino", Subtype.Rhino),
      ("Rigger", Subtype.Rigger),
      ("Robot", Subtype.Robot),
      ("Rogue", Subtype.Rogue),
      ("Role", Subtype.Role),
      ("Room", Subtype.Room),
      ("Rowan", Subtype.Rowan),
      ("Rune", Subtype.Rune),
      ("Sable", Subtype.Sable),
      ("Saga", Subtype.Saga),
      ("Saheeli", Subtype.Saheeli),
      ("Salamander", Subtype.Salamander),
      ("Samurai", Subtype.Samurai),
      ("Samut", Subtype.Samut),
      ("Sand", Subtype.Sand),
      ("Saproling", Subtype.Saproling),
      ("Sarkhan", Subtype.Sarkhan),
      ("Satyr", Subtype.Satyr),
      ("Scarecrow", Subtype.Scarecrow),
      ("Scientist", Subtype.Scientist),
      ("Scion", Subtype.Scion),
      ("Scorpion", Subtype.Scorpion),
      ("Scout", Subtype.Scout),
      ("Sculpture", Subtype.Sculpture),
      ("Seal", Subtype.Seal),
      ("Segovia", Subtype.Segovia),
      ("Serf", Subtype.Serf),
      ("Serpent", Subtype.Serpent),
      ("Serra", Subtype.Serra),
      ("SerrasRealm", Subtype.SerrasRealm),
      ("Servo", Subtype.Servo),
      ("Shade", Subtype.Shade),
      ("Shadowmoor", Subtype.Shadowmoor),
      ("Shaman", Subtype.Shaman),
      ("Shandalar", Subtype.Shandalar),
      ("Shapeshifter", Subtype.Shapeshifter),
      ("Shard", Subtype.Shard),
      ("Shark", Subtype.Shark),
      ("Sheep", Subtype.Sheep),
      ("Shenmeng", Subtype.Shenmeng),
      ("Shiar", Subtype.Shiar),
      ("Shrine", Subtype.Shrine),
      ("Siege", Subtype.Siege),
      ("Siren", Subtype.Siren),
      ("Sivitri", Subtype.Sivitri),
      ("Skaro", Subtype.Skaro),
      ("Skeleton", Subtype.Skeleton),
      ("Skrull", Subtype.Skrull),
      ("Skunk", Subtype.Skunk),
      ("Slith", Subtype.Slith),
      ("Sliver", Subtype.Sliver),
      ("Sloth", Subtype.Sloth),
      ("Slug", Subtype.Slug),
      ("Snail", Subtype.Snail),
      ("Snake", Subtype.Snake),
      ("Soldier", Subtype.Soldier),
      ("Soltari", Subtype.Soltari),
      ("Sorcerer", Subtype.Sorcerer),
      ("Sorin", Subtype.Sorin),
      ("Spacecraft", Subtype.Spacecraft),
      ("Spawn", Subtype.Spawn),
      ("Specter", Subtype.Specter),
      ("Spellshaper", Subtype.Spellshaper),
      ("Sphere", Subtype.Sphere),
      ("Sphinx", Subtype.Sphinx),
      ("Spider", Subtype.Spider),
      ("Spike", Subtype.Spike),
      ("Spirit", Subtype.Spirit),
      ("Splinter", Subtype.Splinter),
      ("Sponge", Subtype.Sponge),
      ("Spy", Subtype.Spy),
      ("Squid", Subtype.Squid),
      ("Squirrel", Subtype.Squirrel),
      ("Starfish", Subtype.Starfish),
      ("Stone", Subtype.Stone),
      ("Surrakar", Subtype.Surrakar),
      ("Survivor", Subtype.Survivor),
      ("Swamp", Subtype.Swamp),
      ("Symbiote", Subtype.Symbiote),
      ("Synth", Subtype.Synth),
      ("Szat", Subtype.Szat),
      ("Tamiyo", Subtype.Tamiyo),
      ("Tarkir", Subtype.Tarkir),
      ("Tasha", Subtype.Tasha),
      ("Teferi", Subtype.Teferi),
      ("Tentacle", Subtype.Tentacle),
      ("Tetravite", Subtype.Tetravite),
      ("Teyo", Subtype.Teyo),
      ("Tezzeret", Subtype.Tezzeret),
      ("Thalakos", Subtype.Thalakos),
      ("TheAbyss", Subtype.TheAbyss),
      ("TheDalekAsylum", Subtype.TheDalekAsylum),
      ("TheLibrary", Subtype.TheLibrary),
      ("Theros", Subtype.Theros),
      ("Thopter", Subtype.Thopter),
      ("Thrull", Subtype.Thrull),
      ("Tibalt", Subtype.Tibalt),
      ("Tiefling", Subtype.Tiefling),
      ("Time", Subtype.Time),
      ("TimeLord", Subtype.TimeLord),
      ("Tower", Subtype.Tower),
      ("Town", Subtype.Town),
      ("Toy", Subtype.Toy),
      ("Trap", Subtype.Trap),
      ("Treasure", Subtype.Treasure),
      ("Treefolk", Subtype.Treefolk),
      ("Trenzalore", Subtype.Trenzalore),
      ("Trilobite", Subtype.Trilobite),
      ("Triskelavite", Subtype.Triskelavite),
      ("Troll", Subtype.Troll),
      ("Turtle", Subtype.Turtle),
      ("Tyranid", Subtype.Tyranid),
      ("Tyvar", Subtype.Tyvar),
      ("Ugin", Subtype.Ugin),
      ("Ulgrotha", Subtype.Ulgrotha),
      ("Undercity", Subtype.Undercity),
      ("Unicorn", Subtype.Unicorn),
      ("UnknownPlanet", Subtype.UnknownPlanet),
      ("Urza", Subtype.Urza),
      ("Urzas", Subtype.Urzas),
      ("Utrom", Subtype.Utrom),
      ("Valla", Subtype.Valla),
      ("Vampire", Subtype.Vampire),
      ("Varmint", Subtype.Varmint),
      ("Vedalken", Subtype.Vedalken),
      ("Vehicle", Subtype.Vehicle),
      ("Venser", Subtype.Venser),
      ("Vibranium", Subtype.Vibranium),
      ("Villain", Subtype.Villain),
      ("Vivien", Subtype.Vivien),
      ("Volver", Subtype.Volver),
      ("Vraska", Subtype.Vraska),
      ("Vronos", Subtype.Vronos),
      ("Vryn", Subtype.Vryn),
      ("Wall", Subtype.Wall),
      ("Walrus", Subtype.Walrus),
      ("Warlock", Subtype.Warlock),
      ("Warrior", Subtype.Warrior),
      ("Weasel", Subtype.Weasel),
      ("Weird", Subtype.Weird),
      ("Werewolf", Subtype.Werewolf),
      ("Whale", Subtype.Whale),
      ("Wildfire", Subtype.Wildfire),
      ("Will", Subtype.Will),
      ("Windgrace", Subtype.Windgrace),
      ("Wizard", Subtype.Wizard),
      ("Wolf", Subtype.Wolf),
      ("Wolverine", Subtype.Wolverine),
      ("Wombat", Subtype.Wombat),
      ("Worm", Subtype.Worm),
      ("Wraith", Subtype.Wraith),
      ("Wrenn", Subtype.Wrenn),
      ("Wurm", Subtype.Wurm),
      ("Xenagos", Subtype.Xenagos),
      ("Xerex", Subtype.Xerex),
      ("Yanggu", Subtype.Yanggu),
      ("Yanling", Subtype.Yanling),
      ("Yeti", Subtype.Yeti),
      ("Zariel", Subtype.Zariel),
      ("Zendikar", Subtype.Zendikar),
      ("Zhalfir", Subtype.Zhalfir),
      ("Zombie", Subtype.Zombie),
      ("Zubera", Subtype.Zubera)
    ]
