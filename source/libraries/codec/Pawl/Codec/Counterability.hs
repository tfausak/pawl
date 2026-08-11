module Pawl.Codec.Counterability where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Counterability as Counterability

toJson :: Counterability.Counterability -> Value.Value
toJson c = Common.nullary $ case c of
  Counterability.Counterable -> "Counterable"
  Counterability.CantBeCountered -> "CantBeCountered"

fromJson :: Value.Value -> Either Text.Text Counterability.Counterability
fromJson =
  Common.decodeNullary
    "Counterability"
    [ ("Counterable", Counterability.Counterable),
      ("CantBeCountered", Counterability.CantBeCountered)
    ]
