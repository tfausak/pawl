-- | The @PlayerRef ⇆ Json@ codec (#481).
module Pawl.Codec.PlayerRef where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.PlayerRelation (jsonToPlayerRelation, playerRelationToJson)
import Pawl.Codec.SlotName (jsonToSlotName, slotNameToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.PlayerRef as PlayerRef

playerRefToJson :: PlayerRef.PlayerRef -> Value
playerRefToJson r = case r of
  PlayerRef.EachPlayer -> Json.nullary (Text.pack "EachPlayer")
  PlayerRef.Relative rel -> Json.tagged (Text.pack "Relative") (Just (playerRelationToJson rel))
  PlayerRef.InSlot n -> Json.tagged (Text.pack "InSlot") (Just (slotNameToJson n))

jsonToPlayerRef :: Value -> Either Text PlayerRef.PlayerRef
jsonToPlayerRef value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("EachPlayer", _) -> Right PlayerRef.EachPlayer
    ("Relative", Just v) -> PlayerRef.Relative <$> jsonToPlayerRelation v
    ("InSlot", Just v) -> PlayerRef.InSlot <$> jsonToSlotName v
    _ -> Left (Text.pack "unknown PlayerRef: " <> t)
