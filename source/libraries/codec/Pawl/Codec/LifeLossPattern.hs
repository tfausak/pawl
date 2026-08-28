{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LifeLossPattern where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.LifeLossCause as LifeLossCause
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.LifeLossPattern as LifeLossPattern

-- | CR 109.5 reads a controller relation against the effect's source; "anyone's"
-- is the unrestricted reading, so it is what a pattern that says nothing means.
defaultWhose :: ControllerRelation.ControllerRelation
defaultWhose = ControllerRelation.Anyones

codec :: Codec.Codec LifeLossPattern.LifeLossPattern
codec = Fields.object $ do
  whose <- Fields.defaulted "whose" defaultWhose ControllerRelation.codec LifeLossPattern.whose
  whichCause <- Fields.defaulted "whichCause" Nothing (Common.maybe LifeLossCause.codec) LifeLossPattern.whichCause
  pure
    LifeLossPattern.MkLifeLossPattern
      { LifeLossPattern.whose = whose,
        LifeLossPattern.whichCause = whichCause
      }
