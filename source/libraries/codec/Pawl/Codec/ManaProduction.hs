module Pawl.Codec.ManaProduction where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ManaProduction as ManaProduction

toJson :: ManaProduction.ManaProduction -> Value.Value
toJson mp = case mp of
  ManaProduction.OfType mt -> Common.tagged "OfType" . Just $ ManaType.toJson mt
  ManaProduction.AnyColor -> Common.nullary "AnyColor"
  ManaProduction.SnowSymbol -> Common.nullary "SnowSymbol"

fromJson :: Value.Value -> Either Text.Text ManaProduction.ManaProduction
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("OfType", Just v) -> ManaProduction.OfType <$> ManaType.fromJson v
    ("AnyColor", _) -> Right ManaProduction.AnyColor
    ("SnowSymbol", _) -> Right ManaProduction.SnowSymbol
    _ -> Left . Text.pack $ "unknown ManaProduction: " <> t
