{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TokenPattern where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.TokenPattern as TokenPattern

-- | CR 109.5 reads a controller relation against the effect's source; "anyone's"
-- is the unrestricted reading, so it is what a pattern that says nothing means.
defaultWhose :: ControllerRelation.ControllerRelation
defaultWhose = ControllerRelation.Anyones

codec :: Codec.Codec TokenPattern.TokenPattern
codec = Fields.object $ do
  whose <- Fields.defaulted "whose" defaultWhose ControllerRelation.codec TokenPattern.whose
  -- CR 111.1: elided is ANY token -- Doubling Season's "one or more tokens".
  whatToken <- Fields.defaulted "whatToken" (Filter.And []) (Filter.codec Keyword.codec) TokenPattern.whatToken
  pure
    TokenPattern.MkTokenPattern
      { TokenPattern.whose = whose,
        TokenPattern.whatToken = whatToken
      }
