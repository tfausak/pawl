-- | The @ManaProduction ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.ManaProduction where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ManaType (jsonToManaType, manaTypeToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ManaProduction as ManaProduction

manaProductionToJson :: ManaProduction.ManaProduction -> Value
manaProductionToJson mp = case mp of
  ManaProduction.OfType mt -> Json.tagged (Text.pack "OfType") (Just (manaTypeToJson mt))
  ManaProduction.AnyColor -> Json.nullary (Text.pack "AnyColor")

jsonToManaProduction :: Value -> Either Text ManaProduction.ManaProduction
jsonToManaProduction value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("OfType", Just v) -> ManaProduction.OfType <$> jsonToManaType v
    ("AnyColor", _) -> Right ManaProduction.AnyColor
    _ -> Left (Text.pack "unknown ManaProduction: " <> t)
