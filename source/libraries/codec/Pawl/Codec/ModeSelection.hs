-- | The @ModeSelection ⇆ Json@ codec (#481).
module Pawl.Codec.ModeSelection where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ModeSelection as ModeSelection

modeSelectionToJson :: ModeSelection.ModeSelection -> Value
modeSelectionToJson (ModeSelection.ChooseExactly n) =
  Json.tagged (Text.pack "ChooseExactly") (Just (Json.natTo n))

jsonToModeSelection :: Value -> Either Text ModeSelection.ModeSelection
jsonToModeSelection value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ChooseExactly", Just n) -> ModeSelection.ChooseExactly <$> Json.natFrom n
    _ -> Left (Text.pack "unknown ModeSelection: " <> t)
