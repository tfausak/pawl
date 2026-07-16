module Pawl.Card where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
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
            Card.toughness = Nothing
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
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1))
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
