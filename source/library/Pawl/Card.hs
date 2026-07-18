module Pawl.Card where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.StaticAbility as StaticAbility
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TypeLine as TypeLine

-- The Mountain's red mana ability is granted from its subtype by CR 305.6, so it
-- is derived by the engine, not stored on the card.
mountainPrinting :: Printing.Printing
mountainPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Mountain",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Basic,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.singleton Subtype.Mountain
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- The Swamp's black mana ability is granted from its subtype by CR 305.6, so it
-- is derived by the engine, not stored on the card.
swampPrinting :: Printing.Printing
swampPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Swamp",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Basic,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.singleton Subtype.Swamp
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- The Forest's green mana ability is granted from its subtype by CR 305.6, so it
-- is derived by the engine, not stored on the card.
forestPrinting :: Printing.Printing
forestPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Forest",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Basic,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.singleton Subtype.Forest
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Goblin Piker: {1}{R}, Creature - Goblin Warrior, 2/1, no rules text.
-- Genuinely vanilla -- its entire behavior is its type line, cost, and P/T, so
-- it needs zero opcodes. Chosen over Grizzly Bears ({1}{G}) to reuse M0's
-- Mountain mana base; {1}{R} still exercises generic AND colored payment.
pikerPrinting :: Printing.Printing
pikerPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Goblin Piker",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 1,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.fromList [Subtype.Goblin, Subtype.Warrior]
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- The M2a keyword cards. Each is mono-red, castable from the 36-Mountain mana
-- base, and genuinely vanilla-plus-one-keyword -- its entire behavior is its type
-- line, cost, P/T and one rule 702 citation, so all of them need zero opcodes.
--
-- Every one verified against Scryfall (api.scryfall.com/cards/named?exact=...).
-- The dumps in docs/ are other projects' working data: fine for FINDING a
-- candidate, never for confirming one.

-- Bird Maiden: {2}{R}, Creature - Human Bird, 1/2, Flying.
-- The cheapest vanilla red flier that exists -- there is none at {1}{R}. Its 1/2
-- body is deliberate: distinguishable from a Piker's 2/1 by P/T alone, and it
-- trades with one.
birdMaidenPrinting :: Printing.Printing
birdMaidenPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Bird Maiden",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.fromList [Subtype.Human, Subtype.Bird]
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 1)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 2)),
            Card.keywords = Set.singleton Keyword.Flying,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Nimble Birdsticker: {2}{R}, Creature - Goblin, 2/3, Reach.
-- A Goblin with reach, which is faintly ridiculous and entirely real. It is the
-- FALSIFIER for flying: it blocks a flier without having flying, so any
-- implementation that asks "does the blocker have flying?" fails against it.
nimbleBirdstickerPrinting :: Printing.Printing
nimbleBirdstickerPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Nimble Birdsticker",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Goblin
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 3)),
            Card.keywords = Set.singleton Keyword.Reach,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Ogre Sentry: {1}{R}, Creature - Ogre Warrior, 3/3, Defender.
-- A 3/3 on purpose: a defender that died to everything would let "a creature with
-- defender may still block" pass vacuously.
ogreSentryPrinting :: Printing.Printing
ogreSentryPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Ogre Sentry",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 1,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.fromList [Subtype.Ogre, Subtype.Warrior]
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 3)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 3)),
            Card.keywords = Set.singleton Keyword.Defender,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Windseeker Centaur: {1}{R}{R}, Creature - Centaur, 2/2, Vigilance.
-- Chosen over Yotian Soldier ({3}, 1/4, also vigilance): the Soldier is an
-- ARTIFACT creature, which would drag the artifact card type and colorless
-- casting into the milestone that is supposed to be proving one thing.
windseekerCentaurPrinting :: Printing.Printing
windseekerCentaurPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Windseeker Centaur",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 1,
                      ManaSymbol.OfType (ManaType.Colored Color.Red),
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Centaur
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 2)),
            Card.keywords = Set.singleton Keyword.Vigilance,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Goblin Chariot: {2}{R}, Creature - Goblin Warrior, 2/2, Haste.
goblinChariotPrinting :: Printing.Printing
goblinChariotPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Goblin Chariot",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.fromList [Subtype.Goblin, Subtype.Warrior]
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 2)),
            Card.keywords = Set.singleton Keyword.Haste,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- The M2b keyword cards. Each is mono-red, a 2/1 (the same body as a Goblin
-- Piker, so the only thing the engine can see is the keyword), and genuinely
-- vanilla-plus-one-keyword. Verified against Scryfall
-- (api.scryfall.com/cards/named?exact=...), zero Gatherer rulings.

-- Sabretooth Tiger: {2}{R}, Creature - Cat, 2/1, First strike.
sabretoothTigerPrinting :: Printing.Printing
sabretoothTigerPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Sabretooth Tiger",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Cat
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
            Card.keywords = Set.singleton Keyword.FirstStrike,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Ridgetop Raptor: {3}{R}, Creature - Dinosaur Beast, 2/1, Double strike.
ridgetopRaptorPrinting :: Printing.Printing
ridgetopRaptorPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Ridgetop Raptor",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 3,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.fromList [Subtype.Dinosaur, Subtype.Beast]
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
            Card.keywords = Set.singleton Keyword.DoubleStrike,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Typhoid Rats: {B}, Creature - Rat, 1/1, Deathtouch (CR 702.2).
-- Black, not red: mono-red deathtouch does not exist (Scryfall keyword:deathtouch
-- c=r is empty). Never cast, only placed in combat fixtures, so color is cosmetic.
-- A 1/1 on purpose: one power isolates deathtouch, since 1 damage is lethal to a
-- 3/3 ONLY because of 702.2. See the M2c spec, section 6.
typhoidRatsPrinting :: Printing.Printing
typhoidRatsPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Typhoid Rats",
            Card.manaCost =
              Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Black)]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Rat
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 1)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
            Card.keywords = Set.singleton Keyword.Deathtouch,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- War Mammoth: {3}{G}, Creature - Elephant, 3/3, Trample (CR 702.19).
-- Green: clean vanilla-plus-trample lives in green. A 3/3 tramples cleanly over a
-- 2/1 (assign 1, spill 2) and survives a 2/1 blocker, so the overflow is visible.
warMammothPrinting :: Printing.Printing
warMammothPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "War Mammoth",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 3,
                      ManaSymbol.OfType (ManaType.Colored Color.Green)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Elephant
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 3)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 3)),
            Card.keywords = Set.singleton Keyword.Trample,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Lightning Bolt: {R}, Instant, "Lightning Bolt deals 3 damage to any target."
-- The first card whose rules text is DATA. Verified against Scryfall
-- (api.scryfall.com/cards/named?exact=Lightning+Bolt); it has no Gatherer
-- rulings at all, so there is no Q&A-shaped edge case for this pool to miss
-- (design.md section 4; the M3a spec, section 6).
lightningBoltPrinting :: Printing.Printing
lightningBoltPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Lightning Bolt",
            Card.manaCost =
              Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Red)]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Instant,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 3)],
            Card.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget
          }
    }

-- Giant Growth: {G}, Instant, "Target creature gets +3/+3 until end of turn."
-- Scryfall-verified. Layer 7c, until end of turn -- the first duration effect and
-- the first creature-only target.
giantGrowthPrinting :: Printing.Printing
giantGrowthPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Giant Growth",
            Card.manaCost = Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Instant,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) (SlotName.MkSlotName (Text.pack "target"))],
            Card.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.CreatureTarget
          }
    }

-- Humility: {2}{W}{W}, Enchantment, "All creatures lose all abilities and have
-- base power and toughness 1/1." Scryfall-verified. Two static abilities: layer 6
-- (lose all abilities, CR 604.1/604.2) and layer 7b (set base 1/1), each over
-- AllCreatures. No targets, no effects -- the projection gathers it live while it
-- is on the battlefield (CR 613.7a: each takes Humility's own timestamp). White,
-- so it is a deterministic fixture only (no white matchup). Checked Gatherer's 3
-- rulings (design.md section 4): all are Humility+Opalescence layer/timestamp
-- interactions or animated-land mana-ability notes -- Opalescence is M3c and
-- nothing here animates a land, so none is expressible in the M3b pool.
humilityPrinting :: Printing.Printing
humilityPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Humility",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.White),
                      ManaSymbol.OfType (ManaType.Colored Color.White)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Enchantment,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities =
              [ StaticAbility.MkStaticAbility Affected.AllCreatures Modification.LoseAllAbilities,
                StaticAbility.MkStaticAbility Affected.AllCreatures (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1))
              ],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Serpent's Gift: {2}{G}, Instant, "Target creature gains deathtouch until end of
-- turn." Scryfall-verified. Layer 6, until end of turn -- the same ModifyTarget
-- opcode as Giant Growth with a GainKeyword modification. Granting deathtouch to
-- War Mammoth rebuilds M2c's synthetic deathtouch+trample fixture from real cards.
serpentsGiftPrinting :: Printing.Printing
serpentsGiftPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Serpent's Gift",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Green)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Instant,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Deathtouch) (SlotName.MkSlotName (Text.pack "target"))],
            Card.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.CreatureTarget
          }
    }

-- Blood Moon: {2}{R}, Enchantment, "Nonbasic lands are Mountains."
-- Scryfall-verified. One layer-4 static ability that SETS every nonbasic land's
-- subtype to Mountain (CR 305.7), stripping their rules-text abilities. Red, so
-- it is a deterministic fixture only (no white/blue matchup churn here).
bloodMoonPrinting :: Printing.Printing
bloodMoonPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Blood Moon",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Enchantment,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities =
              [StaticAbility.MkStaticAbility Affected.AllNonbasicLands (Modification.SetLandSubtype Subtype.Mountain)],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Urborg, Tomb of Yawgmoth: Legendary Land, "Each land is a Swamp in addition to
-- its other land types." Scryfall-verified. One layer-4 static ability that ADDS
-- the Swamp subtype to every land (CR 305.7 add). Nonbasic (Legendary, no Basic),
-- so Blood Moon strips it -- the existence dependency. No mana cost (CR 202.1).
urborgPrinting :: Printing.Printing
urborgPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Urborg, Tomb of Yawgmoth",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Legendary,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities =
              [StaticAbility.MkStaticAbility Affected.AllLands (Modification.AddLandSubtype Subtype.Swamp)],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Opalescence: {2}{W}{W}, Enchantment, "Each other non-Aura enchantment is a
-- creature in addition to its other types and has base power and base toughness
-- each equal to its mana value." Scryfall-verified. Two static abilities over the
-- same set (each OTHER non-Aura enchantment): layer 4 adds the Creature type,
-- layer 7b sets base P/T to the mana value. White, so it is a deterministic
-- fixture only (no white matchup).
opalescencePrinting :: Printing.Printing
opalescencePrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Opalescence",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.White),
                      ManaSymbol.OfType (ManaType.Colored Color.White)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Enchantment,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities =
              [ StaticAbility.MkStaticAbility Affected.OtherNonAuraEnchantments (Modification.AddCardType CardType.Creature),
                StaticAbility.MkStaticAbility Affected.OtherNonAuraEnchantments (Modification.SetBasePowerToughness Quantity.ManaValue Quantity.ManaValue)
              ],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- The registry the dataflow lint and future golden tests iterate. A printing
-- not listed here escapes the hygiene net -- add every new printing.
allPrintings :: [Printing.Printing]
allPrintings =
  [ mountainPrinting,
    swampPrinting,
    forestPrinting,
    pikerPrinting,
    birdMaidenPrinting,
    nimbleBirdstickerPrinting,
    ogreSentryPrinting,
    windseekerCentaurPrinting,
    goblinChariotPrinting,
    sabretoothTigerPrinting,
    ridgetopRaptorPrinting,
    typhoidRatsPrinting,
    warMammothPrinting,
    lightningBoltPrinting,
    giantGrowthPrinting,
    humilityPrinting,
    serpentsGiftPrinting,
    bloodMoonPrinting,
    urborgPrinting,
    opalescencePrinting
  ]

isLand :: Card.Card -> Bool
isLand c = Set.member CardType.Land (TypeLine.types (Card.typeLine c))

isCreature :: Card.Card -> Bool
isCreature c = Set.member CardType.Creature (TypeLine.types (Card.typeLine c))

-- CR 304.1: an instant is castable whenever its controller has priority. The
-- timing classification, shaped like isPermanent.
isInstant :: Card.Card -> Bool
isInstant c = Set.member CardType.Instant (TypeLine.types (Card.typeLine c))

-- CR 110.1: the permanent card types. An enumeration -- closed half, finite.
isPermanentType :: CardType.CardType -> Bool
isPermanentType cardType = case cardType of
  CardType.Land -> True
  CardType.Creature -> True
  CardType.Instant -> False
  CardType.Enchantment -> True

-- The classification resolution dispatches on (CR 608.3). This is the whole
-- reason the engine never needs to know WHICH card is resolving.
isPermanent :: Card.Card -> Bool
isPermanent c = any isPermanentType (Set.toList (TypeLine.types (Card.typeLine c)))
