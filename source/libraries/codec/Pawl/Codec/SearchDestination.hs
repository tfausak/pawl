module Pawl.Codec.SearchDestination where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SearchDestination as SearchDestination

toJson :: SearchDestination.SearchDestination -> Value.Value
toJson d = Common.nullary $ case d of
  SearchDestination.BattlefieldTapped -> "BattlefieldTapped"
  SearchDestination.RevealThenHand -> "RevealThenHand"

fromJson :: Value.Value -> Either Text.Text SearchDestination.SearchDestination
fromJson =
  Common.decodeNullary
    "SearchDestination"
    [ ("BattlefieldTapped", SearchDestination.BattlefieldTapped),
      ("RevealThenHand", SearchDestination.RevealThenHand)
    ]
