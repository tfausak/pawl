module Pawl.Codec.PlayerRef where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerRef as PlayerRef

toJson :: PlayerRef.PlayerRef -> Value.Value
toJson r = case r of
  PlayerRef.EachPlayer -> Common.nullary "EachPlayer"
  PlayerRef.Relative rel -> Common.tagged "Relative" . Just $ Codec.encode PlayerRelation.codec rel
  PlayerRef.InSlot n -> Common.tagged "InSlot" . Just $ SlotName.toJson n

fromJson :: Value.Value -> Either Text.Text PlayerRef.PlayerRef
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("EachPlayer", _) -> Right PlayerRef.EachPlayer
    ("Relative", Just v) -> PlayerRef.Relative <$> Codec.decode PlayerRelation.codec v
    ("InSlot", Just v) -> PlayerRef.InSlot <$> SlotName.fromJson v
    _ -> Left . Text.pack $ "unknown PlayerRef: " <> t
