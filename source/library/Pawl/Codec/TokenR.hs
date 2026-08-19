{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TokenR where

import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.Codec.TokenPattern as TokenPattern
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TokenR as TokenR

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec TokenR.TokenR
codec = Fields.object $ do
  matching <- Fields.required "matching" TokenPattern.codec TokenR.matching
  scaling <- Fields.required "scaling" Scaling.codec TokenR.scaling
  pure
    TokenR.MkTokenR
      { TokenR.matching = matching,
        TokenR.scaling = scaling
      }
