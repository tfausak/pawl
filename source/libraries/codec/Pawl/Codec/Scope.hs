module Pawl.Codec.Scope where

import qualified Data.Text as Text
import qualified Pawl.Codec.EventShape as EventShape
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Scope as Scope

toJson :: Scope.Scope -> Value.Value
toJson s = case s of
  Scope.InZone z r -> Common.tagged "InZone" . Just . Value.array $ [Codec.encode Zone.codec z, PlayerRef.toJson r]
  Scope.InHistory e -> Common.tagged "InHistory" . Just $ EventShape.toJson e
  Scope.OverPlayers r -> Common.tagged "OverPlayers" . Just $ PlayerRef.toJson r

fromJson :: Value.Value -> Either Text.Text Scope.Scope
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("InZone", Just (Value.Array (Array.MkArray [z, r]))) -> Scope.InZone <$> Codec.decode Zone.codec z <*> PlayerRef.fromJson r
    ("InHistory", Just v) -> Scope.InHistory <$> EventShape.fromJson v
    ("OverPlayers", Just v) -> Scope.OverPlayers <$> PlayerRef.fromJson v
    _ -> Left . Text.pack $ "unknown Scope: " <> t
