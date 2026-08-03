module Pawl.Codec.Supertype where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Supertype as Supertype

toJson :: Supertype.Supertype -> Value.Value
toJson s = Common.nullary $ case s of
  Supertype.Basic -> "Basic"
  Supertype.Legendary -> "Legendary"
  Supertype.Ongoing -> "Ongoing"
  Supertype.Snow -> "Snow"
  Supertype.World -> "World"

fromJson :: Value.Value -> Either Text.Text Supertype.Supertype
fromJson =
  Common.decodeNullary
    "Supertype"
    [ ("Basic", Supertype.Basic),
      ("Legendary", Supertype.Legendary),
      ("Ongoing", Supertype.Ongoing),
      ("Snow", Supertype.Snow),
      ("World", Supertype.World)
    ]
