module Pawl.Codec.Condition where

import qualified Pawl.Codec.Compares as Compares
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Condition as Condition

-- | Tagged like every other sum. The two shapes were previously told apart by
-- their KEYS -- a comparison by @measured@\/@comparison@\/@threshold@ and a
-- disjunction by a lone @any@ -- which read as well as a tag but could not be
-- stated as a schema a decoder guarantees (#1304).
--
-- A bundle since @Compares@ gained a record for 'Pawl.JsonCodec.Fields.object'
-- to name (#1305); the wire format is unchanged by either step, since the
-- record's field names are the keys the payload already carried.
--
-- @Any@ recurses on 'codec' itself, which terminates for
-- 'Pawl.Codec.Quantity''s reason: 'Pawl.JsonSchema.Define.define' registers the
-- name before running the body, so re-entry emits a @$ref@.
codec :: Codec.Codec Condition.Condition
codec =
  Arm.tagged
    [ Arm.payload "Compares" Compares.codec Condition.Compares (\x -> case x of Condition.Compares y -> Just y; _ -> Nothing),
      Arm.payload "Any" (Common.list codec) Condition.Any (\x -> case x of Condition.Any y -> Just y; _ -> Nothing)
    ]
