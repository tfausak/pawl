{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TargetSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Pool as Pool
import qualified Pawl.Codec.TargetCount as TargetCount
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetCount as TargetCount
import qualified Pawl.Types.TargetSpec as TargetSpec

-- | The filter key is omitted when Nothing, mirroring how optional fields are
-- encoded elsewhere. CR 601.2c's "another" is a Not IsSource conjunct inside
-- that filter, not a key of its own (#163).
--
-- The count key is omitted when the slot takes exactly one, so an ordinary
-- "target creature" renders as it always did and only CR 601.2c's variable
-- counts spend a key.
codec :: Codec.Codec TargetSpec.TargetSpec
codec = Fields.object $ do
  pool <- Fields.required "pool" Pool.codec TargetSpec.pool
  filter_ <- Fields.defaulted "filter" Nothing (Common.maybe (Filter.codec Keyword.codec)) TargetSpec.filter
  count <- Fields.defaulted "count" TargetCount.one TargetCount.codec TargetSpec.count
  pure
    TargetSpec.MkTargetSpec
      { TargetSpec.pool = pool,
        TargetSpec.filter = filter_,
        TargetSpec.count = count
      }

-- | A slot-keyed map as a JSON object keyed by the slot name (#1303).
codecMap :: Codec.Codec (Map.Map SlotName.SlotName TargetSpec.TargetSpec)
codecMap = Common.textMap SlotName.unwrap SlotName.MkSlotName codec
