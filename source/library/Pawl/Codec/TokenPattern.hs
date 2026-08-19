{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TokenPattern where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.TokenPattern as TokenPattern

-- | CR 109.5 reads a controller relation against the effect's source; "anyone's"
-- is the unrestricted reading, so it is what a pattern that says nothing means.
defaultWhose :: ControllerRelation.ControllerRelation
defaultWhose = ControllerRelation.Anyones

codec :: Codec.Codec TokenPattern.TokenPattern
codec = Fields.object $ do
  whose <- Fields.defaulted "whose" defaultWhose ControllerRelation.codec TokenPattern.whose
  pure
    TokenPattern.MkTokenPattern
      { TokenPattern.whose = whose
      }
