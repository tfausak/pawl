-- | The @MonarchTarget ⇆ Json@ codec (#481).
module Pawl.Codec.MonarchTarget where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.MonarchTarget as MonarchTarget

monarchTargetToJson :: MonarchTarget.MonarchTarget -> Value
monarchTargetToJson t = Json.nullary . Text.pack $ case t of
  MonarchTarget.TheController -> "TheController"
  MonarchTarget.ControllerOfSource -> "ControllerOfSource"

jsonToMonarchTarget :: Value -> Either Text MonarchTarget.MonarchTarget
jsonToMonarchTarget =
  Json.decodeNullary
    (Text.pack "MonarchTarget")
    [ (Text.pack "TheController", MonarchTarget.TheController),
      (Text.pack "ControllerOfSource", MonarchTarget.ControllerOfSource)
    ]
