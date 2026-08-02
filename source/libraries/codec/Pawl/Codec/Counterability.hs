module Pawl.Codec.Counterability where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Counterability as Counterability

toJson :: Counterability.Counterability -> Value.Value
toJson c = Common.nullary $ case c of
  Counterability.Counterable -> "Counterable"
  Counterability.CantBeCountered -> "CantBeCountered"

-- | Absent means Counterable (CR 113.6g is printed text: a card either says it
-- or does not), the shape 'Common.decodeBooleanDefault' gives the other
-- defaulted keys.
fromJsonDefault :: Value.Value -> Either Text.Text Counterability.Counterability
fromJsonDefault value = case value of
  Value.Null _ -> Right Counterability.Counterable
  _ -> fromJson value

fromJson :: Value.Value -> Either Text.Text Counterability.Counterability
fromJson =
  Common.decodeNullary
    "Counterability"
    [ ("Counterable", Counterability.Counterable),
      ("CantBeCountered", Counterability.CantBeCountered)
    ]
