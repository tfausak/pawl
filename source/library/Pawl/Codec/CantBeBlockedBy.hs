{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CantBeBlockedBy where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy

-- | The bare object the enclosing tag already carried, now with a record behind
-- it to name. The wire format is unchanged.
--
-- "blockers" and not a second "affected": the two creature-naming keys are not
-- interchangeable, and naming them is what stops a card file barring the
-- attackers from themselves.
codec :: Codec.Codec CantBeBlockedBy.CantBeBlockedBy
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec CantBeBlockedBy.affected
  blockers <- Fields.required "blockers" (Filter.codec Keyword.codec) CantBeBlockedBy.blockers
  unless <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) CantBeBlockedBy.unless
  pure
    CantBeBlockedBy.MkCantBeBlockedBy
      { CantBeBlockedBy.affected = affected,
        CantBeBlockedBy.blockers = blockers,
        CantBeBlockedBy.unless = unless
      }
