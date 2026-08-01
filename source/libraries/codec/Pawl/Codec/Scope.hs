-- | The @Scope ⇆ Json@ codec (#481).
module Pawl.Codec.Scope where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.EventShape as EventShape
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.PlayerRef (jsonToPlayerRef, playerRefToJson)
import qualified Pawl.Codec.Zone as Zone
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Scope as Scope

scopeToJson :: Scope.Scope -> Value
scopeToJson s = case s of
  Scope.InZone z r -> Json.tagged (Text.pack "InZone") (Just (Array (MkArray [Zone.toJson z, playerRefToJson r])))
  Scope.InHistory e -> Json.tagged (Text.pack "InHistory") (Just (EventShape.toJson e))

jsonToScope :: Value -> Either Text Scope.Scope
jsonToScope value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("InZone", Just (Array (MkArray [z, r]))) -> Scope.InZone <$> Zone.fromJson z <*> jsonToPlayerRef r
    ("InHistory", Just v) -> Scope.InHistory <$> EventShape.fromJson v
    _ -> Left (Text.pack "unknown Scope: " <> t)
