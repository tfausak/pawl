module Pawl.Codec.DuplicateCardSpec where

import qualified Pawl.Codec.DuplicateCard as DuplicateCard
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DuplicateCard as DuplicateCard
import qualified Pawl.Types.PrintingId as PrintingId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DuplicateCard" $ do
  -- CR 108.2 / 707.2: the card, and the copiable values it was conjured with.
  Spec.it s "a printing and its conjured values" $
    Common.assertCodec
      s
      DuplicateCard.codec
      DuplicateCard.MkDuplicateCard
        { DuplicateCard.printing = PrintingId.MkPrintingId 17,
          DuplicateCard.values = ProjectedCharacteristicsSpec.minimalCharacteristics
        }
      " {\"printing\":17,\"values\":{\"names\":[\"Mountain\"],\"cardTypes\":[{\"type\":\"Land\"}]}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s DuplicateCard.codec
