-- | The @CastingRestriction ⇆ Json@ codec (#481).
module Pawl.Codec.CastingRestriction where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.Phase as Phase
import Pawl.Json.Value (Value)
import qualified Pawl.Types.CastingRestriction as CastingRestriction

castingRestrictionToJson :: CastingRestriction.CastingRestriction -> Value
castingRestrictionToJson r = case r of
  CastingRestriction.DuringPhase p -> Json.tagged (Text.pack "DuringPhase") (Just (Phase.toJson p))
  CastingRestriction.AttackedThisStep -> Json.nullary (Text.pack "AttackedThisStep")

jsonToCastingRestriction :: Value -> Either Text CastingRestriction.CastingRestriction
jsonToCastingRestriction value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("DuringPhase", Just v) -> CastingRestriction.DuringPhase <$> Phase.fromJson v
    ("AttackedThisStep", _) -> Right CastingRestriction.AttackedThisStep
    _ -> Left (Text.pack "unknown CastingRestriction: " <> t)

-- Newtypes -------------------------------------------------------------------
