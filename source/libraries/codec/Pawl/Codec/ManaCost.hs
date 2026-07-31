-- | The @ManaCost ⇆ Json@ codec (#481).
module Pawl.Codec.ManaCost where

import Data.Text (Text)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ManaSymbol (jsonToManaSymbol, manaSymbolToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ManaCost as ManaCost

manaCostToJson :: ManaCost.ManaCost -> Value
manaCostToJson (ManaCost.MkManaCost xs) = Json.listTo manaSymbolToJson xs

jsonToManaCost :: Value -> Either Text ManaCost.ManaCost
jsonToManaCost value = ManaCost.MkManaCost <$> Json.listFrom jsonToManaSymbol value
