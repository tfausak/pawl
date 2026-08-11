module Pawl.Codec.ManaSymbol where

import qualified Data.Text as Text
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaSymbol as ManaSymbol

toJson :: ManaSymbol.ManaSymbol -> Value.Value
toJson ms = case ms of
  ManaSymbol.Generic n -> Common.tagged "Generic" . Just $ Common.encodeNatural n
  ManaSymbol.OfType mt -> Common.tagged "OfType" . Just $ ManaType.toJson mt
  ManaSymbol.Hybrid a b -> Common.tagged "Hybrid" . Just . Value.array $ [ManaType.toJson a, ManaType.toJson b]
  ManaSymbol.MonocoloredHybrid mt -> Common.tagged "MonocoloredHybrid" . Just $ ManaType.toJson mt
  -- A Color, not a ManaType: CR 107.4f's five Phyrexian symbols are all coloured.
  ManaSymbol.Phyrexian c -> Common.tagged "Phyrexian" . Just $ Codec.encode Color.codec c
  -- Nullary: CR 107.4h's {S} names no mana type and no colour, so there is
  -- nothing for it to carry.
  ManaSymbol.Snow -> Common.nullary "Snow"
  ManaSymbol.Variable -> Common.nullary "Variable"

fromJson :: Value.Value -> Either Text.Text ManaSymbol.ManaSymbol
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Generic", Just v) -> ManaSymbol.Generic <$> Common.decodeNatural v
    ("OfType", Just v) -> ManaSymbol.OfType <$> ManaType.fromJson v
    ("Hybrid", Just (Value.Array (Array.MkArray [av, bv]))) -> ManaSymbol.Hybrid <$> ManaType.fromJson av <*> ManaType.fromJson bv
    ("MonocoloredHybrid", Just v) -> ManaSymbol.MonocoloredHybrid <$> ManaType.fromJson v
    ("Phyrexian", Just v) -> ManaSymbol.Phyrexian <$> Codec.decode Color.codec v
    ("Snow", _) -> Right ManaSymbol.Snow
    ("Variable", _) -> Right ManaSymbol.Variable
    _ -> Left . Text.pack $ "unknown ManaSymbol: " <> t
