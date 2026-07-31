-- | The @Printing ⇆ Json@ codec (#481).
module Pawl.Codec.Printing where

import Data.Text (Text)
import Pawl.Codec.Card (cardToJson, jsonToCard)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Printing as Printing

printingToJson :: Printing.Printing -> Value
printingToJson (Printing.MkPrinting c) = cardToJson c

jsonToPrinting :: Value -> Either Text Printing.Printing
jsonToPrinting value = Printing.MkPrinting <$> jsonToCard value
