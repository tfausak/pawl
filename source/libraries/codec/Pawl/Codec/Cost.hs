-- | The @Cost ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Cost where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.CostComponent (costComponentToJson, jsonToCostComponent)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ManaCost (jsonToManaCost, manaCostToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Cost as Cost

costToJson :: Cost.Cost -> Value
costToJson c =
  Json.jObject
    [ (Text.pack "mana", Json.maybeTo manaCostToJson (Cost.mana c)),
      (Text.pack "components", Json.listTo costComponentToJson (Cost.components c))
    ]

-- CR 118.6: an ABSENT mana field decodes to Nothing -- an unpayable cost -- and
-- never to {0}. Every ability-bearing card file states its mana part explicitly
-- (`[]` for {0}), so the absent case is only ever reached by a malformed file.
jsonToCost :: Value -> Either Text Cost.Cost
jsonToCost value = do
  ps <- Json.asObject value
  m <- Json.maybeFrom jsonToManaCost (Json.getOpt (Text.pack "mana") ps)
  cs <- Json.listFromDefault jsonToCostComponent (Json.getOpt (Text.pack "components") ps)
  pure Cost.MkCost {Cost.mana = m, Cost.components = cs}
