module Pawl.Codec.CastingRestriction where

import qualified Data.Text as Text
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CastingRestriction as CastingRestriction

toJson :: CastingRestriction.CastingRestriction -> Value.Value
toJson r = case r of
  CastingRestriction.DuringPhase p -> Common.tagged "DuringPhase" . Just $ Codec.encode Phase.codec p
  CastingRestriction.AttackedThisStep -> Common.nullary "AttackedThisStep"

fromJson :: Value.Value -> Either Text.Text CastingRestriction.CastingRestriction
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("DuringPhase", Just v) -> CastingRestriction.DuringPhase <$> Codec.decode Phase.codec v
    ("AttackedThisStep", _) -> Right CastingRestriction.AttackedThisStep
    _ -> Left . Text.pack $ "unknown CastingRestriction: " <> t
