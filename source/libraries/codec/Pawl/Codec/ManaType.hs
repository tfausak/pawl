-- | The @ManaType ⇆ Json@ codec (#481).
module Pawl.Codec.ManaType where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ManaType as ManaType

manaTypeToJson :: ManaType.ManaType -> Value
manaTypeToJson mt = case mt of
  ManaType.Colored c -> Json.tagged (Text.pack "Colored") (Just (Color.toJson c))
  ManaType.Colorless -> Json.nullary (Text.pack "Colorless")

jsonToManaType :: Value -> Either Text ManaType.ManaType
jsonToManaType value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Colored", Just v) -> ManaType.Colored <$> Color.fromJson v
    ("Colorless", _) -> Right ManaType.Colorless
    _ -> Left (Text.pack "unknown ManaType: " <> t)
