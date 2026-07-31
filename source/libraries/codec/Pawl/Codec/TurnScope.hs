-- | The @TurnScope ⇆ Json@ codec (#481).
module Pawl.Codec.TurnScope where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.TurnScope as TurnScope

turnScopeToJson :: TurnScope.TurnScope -> Value
turnScopeToJson s = Json.nullary . Text.pack $ case s of
  TurnScope.EachTurn -> "EachTurn"
  TurnScope.ControllersTurn -> "ControllersTurn"

jsonToTurnScope :: Value -> Either Text TurnScope.TurnScope
jsonToTurnScope =
  Json.decodeNullary
    (Text.pack "TurnScope")
    [ (Text.pack "EachTurn", TurnScope.EachTurn),
      (Text.pack "ControllersTurn", TurnScope.ControllersTurn)
    ]
