module Pawl.Codec.Scope where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EventShape as EventShape
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Scope as Scope

toJson :: Scope.Scope -> Value.Value
toJson s = case s of
  Scope.InZone z r -> Common.tagged "InZone" . Just . Common.array $ [Zone.toJson z, PlayerRef.toJson r]
  Scope.InHistory e -> Common.tagged "InHistory" . Just $ EventShape.toJson e

fromJson :: Value.Value -> Either Text.Text Scope.Scope
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("InZone", Just (Value.Array (Array.MkArray [z, r]))) -> Scope.InZone <$> Zone.fromJson z <*> PlayerRef.fromJson r
    ("InHistory", Just v) -> Scope.InHistory <$> EventShape.fromJson v
    _ -> Left . Text.pack $ "unknown Scope: " <> t
