module Pawl.Codec.ModeSelection where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ModeSelection as ModeSelection

toJson :: ModeSelection.ModeSelection -> Value.Value
toJson m = Common.tagged "ChooseExactly" . Just . Common.encodeNatural $ ModeSelection.unwrap m

fromJson :: Value.Value -> Either Text.Text ModeSelection.ModeSelection
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("ChooseExactly", Just n) -> ModeSelection.ChooseExactly <$> Common.decodeNatural n
    _ -> Left . Text.pack $ "unknown ModeSelection: " <> t
