module Pawl.Codec.Expiry where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Expiry as Expiry

-- | CR 611.2: the STORED duration, which unlike every other type in this module
-- never appears in card JSON -- a card carries a Duration and Pawl.Engine.Expiry.arm
-- turns it into this. The one thing that serialises an Expiry is a
-- DelayedTrigger, below, because CR 603.7b lets a delayed ability state one.
toJson :: Expiry.Expiry -> Value.Value
toJson e = case e of
  Expiry.AtCleanup -> Common.nullary "AtCleanup"
  Expiry.Never -> Common.nullary "Never"
  Expiry.While p c -> Common.tagged "While" . Just . Common.array $ [PlayerId.toJson p, Condition.toJson c]
  Expiry.AtTurnOf p -> Common.tagged "AtTurnOf" . Just $ PlayerId.toJson p

fromJson :: Value.Value -> Either Text.Text Expiry.Expiry
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("AtCleanup", _) -> Right Expiry.AtCleanup
    ("Never", _) -> Right Expiry.Never
    ("While", Just (Value.Array (Array.MkArray [p, c]))) -> Expiry.While <$> PlayerId.fromJson p <*> Condition.fromJson c
    ("AtTurnOf", Just v) -> Expiry.AtTurnOf <$> PlayerId.fromJson v
    _ -> Left . Text.pack $ "unknown Expiry: " <> t
