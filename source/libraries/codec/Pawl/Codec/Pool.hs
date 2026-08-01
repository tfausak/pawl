-- | The @Pool ⇆ Json@ codec (#481).
module Pawl.Codec.Pool where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Pool as Pool

poolToJson :: Pool.Pool -> Value
poolToJson p = Json.nullary . Text.pack $ case p of
  Pool.Creatures -> "Creatures"
  Pool.Players -> "Players"
  Pool.AnyTarget -> "AnyTarget"
  Pool.Permanents -> "Permanents"
  Pool.Spells -> "Spells"
  Pool.Abilities -> "Abilities"
  Pool.SpellsAndPermanents -> "SpellsAndPermanents"

jsonToPool :: Value -> Either Text Pool.Pool
jsonToPool =
  Json.decodeNullary
    (Text.pack "Pool")
    [ (Text.pack "Creatures", Pool.Creatures),
      (Text.pack "Players", Pool.Players),
      (Text.pack "AnyTarget", Pool.AnyTarget),
      (Text.pack "Permanents", Pool.Permanents),
      (Text.pack "Spells", Pool.Spells),
      (Text.pack "Abilities", Pool.Abilities),
      (Text.pack "SpellsAndPermanents", Pool.SpellsAndPermanents)
    ]
