module Pawl.Codec.Zone where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Zone as Zone

toJson :: Zone.Zone -> Value.Value
toJson z = Common.nullary $ case z of
  Zone.Library -> "Library"
  Zone.Hand -> "Hand"
  Zone.Graveyard -> "Graveyard"
  Zone.Battlefield -> "Battlefield"
  Zone.Stack -> "Stack"
  Zone.Exile -> "Exile"
  Zone.Command -> "Command"

fromJson :: Value.Value -> Either Text.Text Zone.Zone
fromJson =
  Common.decodeNullary
    "Zone"
    [ ("Library", Zone.Library),
      ("Hand", Zone.Hand),
      ("Graveyard", Zone.Graveyard),
      ("Battlefield", Zone.Battlefield),
      ("Stack", Zone.Stack),
      ("Exile", Zone.Exile),
      ("Command", Zone.Command)
    ]
