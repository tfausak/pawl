-- | The @ManaSymbol ⇆ Json@ codec (#481).
module Pawl.Codec.ManaSymbol where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.ManaType as ManaType
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.ManaSymbol as ManaSymbol

manaSymbolToJson :: ManaSymbol.ManaSymbol -> Value
manaSymbolToJson ms = case ms of
  ManaSymbol.Generic n -> Json.tagged (Text.pack "Generic") (Just (Json.natTo n))
  ManaSymbol.OfType mt -> Json.tagged (Text.pack "OfType") (Just (ManaType.toJson mt))
  ManaSymbol.Hybrid a b -> Json.tagged (Text.pack "Hybrid") (Just (Array (MkArray [ManaType.toJson a, ManaType.toJson b])))
  ManaSymbol.MonocoloredHybrid mt -> Json.tagged (Text.pack "MonocoloredHybrid") (Just (ManaType.toJson mt))
  -- A Color, not a ManaType: CR 107.4f's five Phyrexian symbols are all coloured.
  ManaSymbol.Phyrexian c -> Json.tagged (Text.pack "Phyrexian") (Just (Color.toJson c))
  -- Nullary: CR 107.4h's {S} names no mana type and no colour, so there is
  -- nothing for it to carry.
  ManaSymbol.Snow -> Json.nullary (Text.pack "Snow")
  ManaSymbol.Variable -> Json.nullary (Text.pack "Variable")

jsonToManaSymbol :: Value -> Either Text ManaSymbol.ManaSymbol
jsonToManaSymbol value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Generic", Just v) -> ManaSymbol.Generic <$> Json.natFrom v
    ("OfType", Just v) -> ManaSymbol.OfType <$> ManaType.fromJson v
    ("Hybrid", Just (Array (MkArray [av, bv]))) -> ManaSymbol.Hybrid <$> ManaType.fromJson av <*> ManaType.fromJson bv
    ("MonocoloredHybrid", Just v) -> ManaSymbol.MonocoloredHybrid <$> ManaType.fromJson v
    ("Phyrexian", Just v) -> ManaSymbol.Phyrexian <$> Color.fromJson v
    ("Snow", _) -> Right ManaSymbol.Snow
    ("Variable", _) -> Right ManaSymbol.Variable
    _ -> Left (Text.pack "unknown ManaSymbol: " <> t)
