module Pawl.Codec.MonarchTarget where

import qualified Data.Text as Text
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.MonarchTarget as MonarchTarget

toJson :: MonarchTarget.MonarchTarget -> Value.Value
toJson t = case t of
  MonarchTarget.TheController -> Common.nullary "TheController"
  MonarchTarget.ControllerOfSource -> Common.nullary "ControllerOfSource"
  MonarchTarget.InSlot n -> Common.tagged "InSlot" . Just $ SlotName.toJson n

fromJson :: Value.Value -> Either Text.Text MonarchTarget.MonarchTarget
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("TheController", _) -> Right MonarchTarget.TheController
    ("ControllerOfSource", _) -> Right MonarchTarget.ControllerOfSource
    ("InSlot", Just v) -> MonarchTarget.InSlot <$> SlotName.fromJson v
    _ -> Left . Text.pack $ "unknown MonarchTarget: " <> t
