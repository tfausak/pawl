module Pawl.Codec.CastOffer where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.CastOffer as CastOffer

toJson :: CastOffer.CastOffer -> Value.Value
toJson o =
  Common.object
    ( Common.optionalPair "transformed" False Common.boolean (CastOffer.transformed o)
        <> Common.optionalPair "withoutPayingManaCost" False Common.boolean (CastOffer.withoutPayingManaCost o)
    )

fromJson :: Value.Value -> Either Text.Text CastOffer.CastOffer
fromJson value = do
  ps <- Common.asObject value
  t <- Common.defaultedField "transformed" False Common.asBoolean ps
  w <- Common.defaultedField "withoutPayingManaCost" False Common.asBoolean ps
  pure
    CastOffer.MkCastOffer
      { CastOffer.transformed = t,
        CastOffer.withoutPayingManaCost = w
      }

-- | The value 'toJson' elides entirely: an offer that transforms nothing and
-- applies no alternative cost is an ordinary cast of the card (CR 712.11's
-- default face, CR 601.2b's own candidates).
defaultValue :: CastOffer.CastOffer
defaultValue =
  CastOffer.MkCastOffer
    { CastOffer.transformed = False,
      CastOffer.withoutPayingManaCost = False
    }
