-- | The @Expiry ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Expiry where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Condition (conditionToJson, jsonToCondition)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.PlayerId (jsonToPlayerId, playerIdToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Expiry as Expiry

-- CR 611.2: the STORED duration, which unlike every other type in this module
-- never appears in card JSON -- a card carries a Duration and Pawl.Engine.Expiry.arm
-- turns it into this. The one thing that serialises an Expiry is a
-- DelayedTrigger, below, because CR 603.7b lets a delayed ability state one.
expiryToJson :: Expiry.Expiry -> Value
expiryToJson e = case e of
  Expiry.AtCleanup -> Json.nullary (Text.pack "AtCleanup")
  Expiry.Never -> Json.nullary (Text.pack "Never")
  Expiry.While p c -> Json.tagged (Text.pack "While") (Just (Array (MkArray [playerIdToJson p, conditionToJson c])))
  Expiry.AtTurnOf p -> Json.tagged (Text.pack "AtTurnOf") (Just (playerIdToJson p))

jsonToExpiry :: Value -> Either Text Expiry.Expiry
jsonToExpiry value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AtCleanup", _) -> Right Expiry.AtCleanup
    ("Never", _) -> Right Expiry.Never
    ("While", Just (Array (MkArray [p, c]))) -> Expiry.While <$> jsonToPlayerId p <*> jsonToCondition c
    ("AtTurnOf", Just v) -> Expiry.AtTurnOf <$> jsonToPlayerId v
    _ -> Left (Text.pack "unknown Expiry: " <> t)
