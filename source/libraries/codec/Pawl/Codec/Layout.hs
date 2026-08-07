module Pawl.Codec.Layout where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Layout as Layout

toJson :: Layout.Layout -> Value.Value
toJson l = Common.nullary $ case l of
  Layout.Normal -> "Normal"
  Layout.Split -> "Split"
  Layout.Room -> "Room"
  Layout.Adventure -> "Adventure"
  Layout.Transforming -> "Transforming"
  Layout.ModalDoubleFaced -> "ModalDoubleFaced"

fromJson :: Value.Value -> Either Text.Text Layout.Layout
fromJson =
  Common.decodeNullary
    "Layout"
    [ ("Normal", Layout.Normal),
      ("Split", Layout.Split),
      ("Room", Layout.Room),
      ("Adventure", Layout.Adventure),
      ("Transforming", Layout.Transforming),
      ("ModalDoubleFaced", Layout.ModalDoubleFaced)
    ]
