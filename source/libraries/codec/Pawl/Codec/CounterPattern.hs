{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CounterPattern where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.CounterSubject as CounterSubject
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

-- | CR 614.1: `subject` is REQUIRED where its neighbours default, because the
-- three printed subjects reach different placements and none of them is what a
-- clause that says nothing means -- an omitted key would have to pick one, and
-- picking the narrow one is what left two passively worded cards narrower than
-- printed; see #1232. Every CounterR in data\/cards writes this key.
codec :: Codec.Codec CounterPattern.CounterPattern
codec = Fields.object $ do
  whichKind <- Fields.defaulted "whichKind" Nothing (Common.maybe (CounterKind.codec Keyword.codec)) CounterPattern.whichKind
  subject <- Fields.required "subject" CounterSubject.codec CounterPattern.subject
  whose <- Fields.defaulted "whose" defaultWhose ControllerRelation.codec CounterPattern.whose
  onWhat <- Fields.required "onWhat" (Filter.codec Keyword.codec) CounterPattern.onWhat
  onWho <- Fields.defaulted "onWho" Nothing (Common.maybe ControllerRelation.codec) CounterPattern.onWho
  pure
    CounterPattern.MkCounterPattern
      { CounterPattern.whichKind = whichKind,
        CounterPattern.subject = subject,
        CounterPattern.whose = whose,
        CounterPattern.onWhat = onWhat,
        CounterPattern.onWho = onWho
      }
