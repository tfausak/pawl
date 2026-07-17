module Pawl.Card where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
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
            Card.keywords = Set.empty
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
            Card.keywords = Set.empty
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
            Card.keywords = Set.singleton Keyword.Flying
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
            Card.keywords = Set.singleton Keyword.Reach
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
            Card.keywords = Set.singleton Keyword.Defender
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
            Card.keywords = Set.singleton Keyword.Vigilance
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
            Card.keywords = Set.singleton Keyword.Haste
          }
    }

isLand :: Card.Card -> Bool
isLand c = Set.member CardType.Land (TypeLine.types (Card.typeLine c))

isCreature :: Card.Card -> Bool
isCreature c = Set.member CardType.Creature (TypeLine.types (Card.typeLine c))

-- CR 110.1: the permanent card types. An enumeration -- closed half, finite.
isPermanentType :: CardType.CardType -> Bool
isPermanentType cardType = case cardType of
  CardType.Land -> True
  CardType.Creature -> True

-- The classification resolution dispatches on (CR 608.3). This is the whole
-- reason the engine never needs to know WHICH card is resolving.
isPermanent :: Card.Card -> Bool
isPermanent c = any isPermanentType (Set.toList (TypeLine.types (Card.typeLine c)))
