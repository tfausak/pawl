-- | The @ModeIndex ⇆ Json@ codec (#481).
module Pawl.Codec.ModeIndex where

import Data.Text (Text)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ModeIndex as ModeIndex

modeIndexToJson :: ModeIndex.ModeIndex -> Value
modeIndexToJson (ModeIndex.MkModeIndex n) = Json.natTo n

jsonToModeIndex :: Value -> Either Text ModeIndex.ModeIndex
jsonToModeIndex value = ModeIndex.MkModeIndex <$> Json.natFrom value
