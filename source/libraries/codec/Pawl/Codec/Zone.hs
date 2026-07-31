-- | The @Zone ⇆ Json@ codec (#481).
module Pawl.Codec.Zone where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Zone as Zone

zoneToJson :: Zone.Zone -> Value
zoneToJson z = Json.nullary . Text.pack $ case z of
  Zone.Library -> "Library"
  Zone.Hand -> "Hand"
  Zone.Graveyard -> "Graveyard"
  Zone.Battlefield -> "Battlefield"
  Zone.Stack -> "Stack"
  Zone.Exile -> "Exile"
  Zone.Command -> "Command"

jsonToZone :: Value -> Either Text Zone.Zone
jsonToZone =
  Json.decodeNullary
    (Text.pack "Zone")
    [ (Text.pack "Library", Zone.Library),
      (Text.pack "Hand", Zone.Hand),
      (Text.pack "Graveyard", Zone.Graveyard),
      (Text.pack "Battlefield", Zone.Battlefield),
      (Text.pack "Stack", Zone.Stack),
      (Text.pack "Exile", Zone.Exile),
      (Text.pack "Command", Zone.Command)
    ]
