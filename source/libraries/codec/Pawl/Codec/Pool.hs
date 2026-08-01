module Pawl.Codec.Pool where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Pool as Pool

toJson :: Pool.Pool -> Value.Value
toJson p = Common.nullary $ case p of
  Pool.Creatures -> "Creatures"
  Pool.Players -> "Players"
  Pool.AnyTarget -> "AnyTarget"
  Pool.Permanents -> "Permanents"
  Pool.Spells -> "Spells"
  Pool.SpellsAndPermanents -> "SpellsAndPermanents"

fromJson :: Value.Value -> Either Text.Text Pool.Pool
fromJson =
  Common.decodeNullary
    "Pool"
    [ ("Creatures", Pool.Creatures),
      ("Players", Pool.Players),
      ("AnyTarget", Pool.AnyTarget),
      ("Permanents", Pool.Permanents),
      ("Spells", Pool.Spells),
      ("SpellsAndPermanents", Pool.SpellsAndPermanents)
    ]
