{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManaUnit where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaRetention as ManaRetention
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.Codec.ProductionTag as ProductionTag
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManaUnit as ManaUnit

-- | All four axes, `restriction` included: CR 106.6's restriction is a predicate
-- over the spell being paid for, so it rides the unit rather than being
-- recoverable from the source that made it.
codec :: Codec.Codec ManaUnit.ManaUnit
codec = Fields.object $ do
  manaType <- Fields.required "manaType" ManaType.codec ManaUnit.manaType
  tags <- Fields.required "tags" (Common.set ProductionTag.codec) ManaUnit.tags
  retention <- Fields.required "retention" ManaRetention.codec ManaUnit.retention
  restriction <- Fields.required "restriction" (Common.maybe (Filter.codec Keyword.codec)) ManaUnit.restriction
  pure
    ManaUnit.MkManaUnit
      { ManaUnit.manaType = manaType,
        ManaUnit.tags = tags,
        ManaUnit.retention = retention,
        ManaUnit.restriction = restriction
      }
