{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TokenR where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.Codec.TokenPattern as TokenPattern
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TokenR as TokenR

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
--
-- The card codec is a PARAMETER for Pawl.Codec.Create's reason: `plus` is card
-- data nested inside card data.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (TokenR.TokenR card)
codec cardCodec = Fields.object $ do
  matching <- Fields.required "matching" TokenPattern.codec TokenR.matching
  scaling <- Fields.defaulted "scaling" Nothing (Common.maybe Scaling.codec) TokenR.scaling
  plus <- Fields.defaulted "plus" Nothing (Common.maybe cardCodec) TokenR.plus
  pure
    TokenR.MkTokenR
      { TokenR.matching = matching,
        TokenR.scaling = scaling,
        TokenR.plus = plus
      }
