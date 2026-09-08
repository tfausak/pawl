module Pawl.Codec.MergeComponentSpec where

import qualified Pawl.Codec.MergeComponent as MergeComponent
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MergeComponent as MergeComponent
import qualified Pawl.Types.PrintingId as PrintingId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MergeComponent" $ do
  -- CR 108.2.
  Spec.it s "OfCard" $
    Common.assertCodec
      s
      MergeComponent.codec
      (MergeComponent.OfCard (PrintingId.MkPrintingId 11))
      " {\"type\":\"OfCard\",\"value\":11} "
  -- CR 111.3 / 730.2d: a DIFFERENT tag on the same payload shape, which is the
  -- whole of what rule 730.2d asks of a component.
  Spec.it s "OfToken" $
    Common.assertCodec
      s
      MergeComponent.codec
      (MergeComponent.OfToken (PrintingId.MkPrintingId 12))
      " {\"type\":\"OfToken\",\"value\":12} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s MergeComponent.codec
