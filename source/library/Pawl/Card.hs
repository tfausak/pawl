module Pawl.Card where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TypeLine as TypeLine

-- The Mountain's red mana ability is granted from its subtype by CR 305.6, so it
-- is derived by the engine, not stored on the card.
mountainPrinting :: Printing.Printing
mountainPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Mountain",
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Basic,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.singleton Subtype.Mountain
                }
          }
    }

isLand :: Card.Card -> Bool
isLand c = Set.member CardType.Land (TypeLine.types (Card.typeLine c))
