{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CastOffer where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaSpending as ManaSpending
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.ManaSpending as ManaSpending.Type

codec :: Codec.Codec CastOffer.CastOffer
codec = Fields.object $ do
  transformed <- Fields.defaulted "transformed" False Common.boolean CastOffer.transformed
  withoutPayingManaCost <- Fields.defaulted "withoutPayingManaCost" False Common.boolean CastOffer.withoutPayingManaCost
  payingInstead <- Fields.defaulted "payingInstead" Nothing (Common.maybe (Cost.codec Keyword.codec)) CastOffer.payingInstead
  spending <- Fields.defaulted "spending" ManaSpending.Type.AsProduced ManaSpending.codec CastOffer.spending
  restriction <- Fields.defaulted "restriction" Nothing (Common.maybe (Filter.codec Keyword.codec)) CastOffer.restriction
  offeredBy <- Fields.defaulted "offeredBy" Nothing (Common.maybe Keyword.codec) CastOffer.offeredBy
  pure
    CastOffer.MkCastOffer
      { CastOffer.transformed = transformed,
        CastOffer.withoutPayingManaCost = withoutPayingManaCost,
        CastOffer.payingInstead = payingInstead,
        CastOffer.spending = spending,
        CastOffer.restriction = restriction,
        CastOffer.offeredBy = offeredBy
      }

-- | The value the codec elides entirely: an offer that transforms nothing,
-- applies no alternative cost, widens no payment and narrows no half is an
-- ordinary cast of the card (CR 712.11's default face, CR 601.2b's own
-- candidates, CR 118.14 unsaid, CR 709.3a's halves all offered, and no keyword
-- ability behind the cost).
defaultValue :: CastOffer.CastOffer
defaultValue =
  CastOffer.MkCastOffer
    { CastOffer.transformed = False,
      CastOffer.withoutPayingManaCost = False,
      CastOffer.payingInstead = Nothing,
      CastOffer.spending = ManaSpending.Type.AsProduced,
      CastOffer.restriction = Nothing,
      CastOffer.offeredBy = Nothing
    }
