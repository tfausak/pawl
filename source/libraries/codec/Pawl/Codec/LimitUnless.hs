{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LimitUnless where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LimitUnless as LimitUnless

-- | The bare object the enclosing tag already carried, now with a record behind
-- it to name. "limit" and not "affected", because a bound names no creature: the
-- key set is what tells a reader of the card file which shape it is looking at
-- without consulting the tag. The wire format is unchanged.
codec :: Codec.Codec LimitUnless.LimitUnless
codec = Fields.object $ do
  limit <- Fields.required "limit" Common.natural LimitUnless.limit
  unless <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) LimitUnless.unless
  pure
    LimitUnless.MkLimitUnless
      { LimitUnless.limit = limit,
        LimitUnless.unless = unless
      }
