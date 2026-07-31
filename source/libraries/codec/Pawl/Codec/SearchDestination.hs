-- | The @SearchDestination ⇆ Json@ codec (#481).
module Pawl.Codec.SearchDestination where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.SearchDestination as SearchDestination

searchDestinationToJson :: SearchDestination.SearchDestination -> Value
searchDestinationToJson d = Json.nullary . Text.pack $ case d of
  SearchDestination.BattlefieldTapped -> "BattlefieldTapped"
  SearchDestination.RevealThenHand -> "RevealThenHand"

jsonToSearchDestination :: Value -> Either Text SearchDestination.SearchDestination
jsonToSearchDestination =
  Json.decodeNullary
    (Text.pack "SearchDestination")
    [ (Text.pack "BattlefieldTapped", SearchDestination.BattlefieldTapped),
      (Text.pack "RevealThenHand", SearchDestination.RevealThenHand)
    ]
