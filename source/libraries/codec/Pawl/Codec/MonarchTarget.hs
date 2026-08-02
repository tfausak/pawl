module Pawl.Codec.MonarchTarget where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.MonarchTarget as MonarchTarget

toJson :: MonarchTarget.MonarchTarget -> Value.Value
toJson t = Common.nullary $ case t of
  MonarchTarget.TheController -> "TheController"
  MonarchTarget.ControllerOfSource -> "ControllerOfSource"

fromJson :: Value.Value -> Either Text.Text MonarchTarget.MonarchTarget
fromJson =
  Common.decodeNullary
    "MonarchTarget"
    [ ("TheController", MonarchTarget.TheController),
      ("ControllerOfSource", MonarchTarget.ControllerOfSource)
    ]
