{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CounterPattern where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterPattern as CounterPattern

-- | CR 109.5 reads a controller relation against the effect's source; "anyone's"
-- is the unrestricted reading, so it is what a pattern that says nothing means.
defaultWhose :: ControllerRelation.ControllerRelation
defaultWhose = ControllerRelation.Anyones

codec :: Codec.Codec CounterPattern.CounterPattern
codec = Fields.object $ do
  whichKind <- Fields.defaulted "whichKind" Nothing (Common.maybe (CounterKind.codec Keyword.codec)) CounterPattern.whichKind
  byWhom <- Fields.defaulted "byWhom" Nothing (Common.maybe ControllerRelation.codec) CounterPattern.byWhom
  whose <- Fields.defaulted "whose" defaultWhose ControllerRelation.codec CounterPattern.whose
  onWhat <- Fields.required "onWhat" (Filter.codec Keyword.codec) CounterPattern.onWhat
  onWho <- Fields.defaulted "onWho" Nothing (Common.maybe ControllerRelation.codec) CounterPattern.onWho
  pure
    CounterPattern.MkCounterPattern
      { CounterPattern.whichKind = whichKind,
        CounterPattern.byWhom = byWhom,
        CounterPattern.whose = whose,
        CounterPattern.onWhat = onWhat,
        CounterPattern.onWho = onWho
      }
