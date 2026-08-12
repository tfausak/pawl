{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CastOffer where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CastOffer as CastOffer

codec :: Codec.Codec CastOffer.CastOffer
codec = Fields.object $ do
  transformed <- Fields.defaulted "transformed" False Common.boolean CastOffer.transformed
  withoutPayingManaCost <- Fields.defaulted "withoutPayingManaCost" False Common.boolean CastOffer.withoutPayingManaCost
  pure
    CastOffer.MkCastOffer
      { CastOffer.transformed = transformed,
        CastOffer.withoutPayingManaCost = withoutPayingManaCost
      }

-- | The value the codec elides entirely: an offer that transforms nothing and
-- applies no alternative cost is an ordinary cast of the card (CR 712.11's
-- default face, CR 601.2b's own candidates).
defaultValue :: CastOffer.CastOffer
defaultValue =
  CastOffer.MkCastOffer
    { CastOffer.transformed = False,
      CastOffer.withoutPayingManaCost = False
    }
