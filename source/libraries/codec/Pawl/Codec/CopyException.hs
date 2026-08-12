module Pawl.Codec.CopyException where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.CopyException as CopyException

toJson :: CopyException.CopyException -> Value.Value
toJson e = case e of
  CopyException.SetPowerToughness p t -> Common.tagged "SetPowerToughness" . Just . Common.array $ [Common.integer p, Common.integer t]

fromJson :: Value.Value -> Either Text.Text CopyException.CopyException
fromJson value = do
  (tag, mv) <- Common.asTagged value
  case (tag, mv) of
    ("SetPowerToughness", Just (Value.Array (Array.MkArray [p, t]))) -> do
      power <- Common.asInteger p
      toughness <- Common.asInteger t
      pure (CopyException.SetPowerToughness power toughness)
    _ -> Left . Text.pack $ "unknown CopyException: " <> tag
