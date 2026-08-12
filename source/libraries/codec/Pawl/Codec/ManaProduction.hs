module Pawl.Codec.ManaProduction where

import qualified Data.Text as Text
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaProduction as ManaProduction

toJson :: ManaProduction.ManaProduction -> Value.Value
toJson mp = case mp of
  ManaProduction.OfType mt -> Common.tagged "OfType" . Just $ Codec.encode ManaType.codec mt
  ManaProduction.AnyColor -> Common.nullary "AnyColor"
  ManaProduction.Chosen -> Common.nullary "Chosen"
  ManaProduction.SnowSymbol -> Common.nullary "SnowSymbol"

fromJson :: Value.Value -> Either Text.Text ManaProduction.ManaProduction
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("OfType", Just v) -> ManaProduction.OfType <$> Codec.decode ManaType.codec v
    ("AnyColor", _) -> Right ManaProduction.AnyColor
    ("Chosen", _) -> Right ManaProduction.Chosen
    ("SnowSymbol", _) -> Right ManaProduction.SnowSymbol
    _ -> Left . Text.pack $ "unknown ManaProduction: " <> t
