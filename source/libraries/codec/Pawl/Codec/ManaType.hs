module Pawl.Codec.ManaType where

import qualified Data.Text as Text
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaType as ManaType

toJson :: ManaType.ManaType -> Value.Value
toJson mt = case mt of
  ManaType.Colored c -> Common.tagged "Colored" . Just $ Color.toJson c
  ManaType.Colorless -> Common.nullary "Colorless"

fromJson :: Value.Value -> Either Text.Text ManaType.ManaType
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Colored", Just v) -> ManaType.Colored <$> Color.fromJson v
    ("Colorless", _) -> Right ManaType.Colorless
    _ -> Left . Text.pack $ "unknown ManaType: " <> t
