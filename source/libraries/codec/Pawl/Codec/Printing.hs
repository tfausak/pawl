-- | The @Printing ⇆ Json@ codec (#481).
module Pawl.Codec.Printing where

import Data.Text (Text)
import qualified Pawl.Codec.Card as Card
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Printing as Printing

printingToJson :: Printing.Printing -> Value
printingToJson (Printing.MkPrinting c) = Card.toJson c

jsonToPrinting :: Value -> Either Text Printing.Printing
jsonToPrinting value = Printing.MkPrinting <$> Card.fromJson value
