module Pawl.Codec.TurnScope where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.TurnScope as TurnScope

toJson :: TurnScope.TurnScope -> Value.Value
toJson s = Common.nullary $ case s of
  TurnScope.EachTurn -> "EachTurn"
  TurnScope.ControllersTurn -> "ControllersTurn"
  TurnScope.OpponentsTurn -> "OpponentsTurn"

fromJson :: Value.Value -> Either Text.Text TurnScope.TurnScope
fromJson =
  Common.decodeNullary
    "TurnScope"
    [ ("EachTurn", TurnScope.EachTurn),
      ("ControllersTurn", TurnScope.ControllersTurn),
      ("OpponentsTurn", TurnScope.OpponentsTurn)
    ]
