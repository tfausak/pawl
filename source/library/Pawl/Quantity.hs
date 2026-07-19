module Pawl.Quantity where

import qualified Pawl.Game as Game
import qualified Pawl.Type.Card as Card
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Quantity (Quantity)
import qualified Pawl.Type.Quantity as Quantity

-- Nothing when the value cannot be determined.
evaluate :: GameState -> ObjectId -> Quantity -> Maybe Integer
evaluate gs oid quantity = case quantity of
  Quantity.Literal n -> Just n
  Quantity.ManaValue -> fmap manaValueOf (Game.cardOf oid gs)

-- CR 202.3: the mana value is the total amount of mana in the cost -- each
-- generic symbol contributes its number, each colored/typed symbol contributes
-- one. A land has no mana cost (CR 202.1), so its mana value is 0.
manaValueOf :: Card.Card -> Integer
manaValueOf card = case Card.manaCost card of
  Nothing -> 0
  Just (ManaCost.MkManaCost symbols) -> sum (map symbolValue symbols)

symbolValue :: ManaSymbol.ManaSymbol -> Integer
symbolValue symbol = case symbol of
  ManaSymbol.Generic n -> toInteger n
  ManaSymbol.OfType _ -> 1
  -- CR 202.3b: off the stack a variable's contribution to mana value is 0.
  ManaSymbol.Variable -> 0
