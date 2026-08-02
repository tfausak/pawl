module Pawl.Codec.Printing where

import qualified Data.Text as Text
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Printing as Printing

toJson :: Printing.Printing -> Value.Value
toJson = Card.toJson . Printing.card

fromJson :: Value.Value -> Either Text.Text Printing.Printing
fromJson = fmap Printing.MkPrinting . Card.fromJson
