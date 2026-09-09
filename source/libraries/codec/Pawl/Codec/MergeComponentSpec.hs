module Pawl.Codec.MergeComponentSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Codec.MergeComponent as MergeComponent
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MeldSource as MeldSource
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
  -- CR 701.42a / 712.8g: the arm whose payload is not a printing at all -- the
  -- combined back face beside the two cards representing the component.
  Spec.it s "OfMeld" $
    Common.assertCodec
      s
      MergeComponent.codec
      (MergeComponent.OfMeld MeldSource.MkMeldSource {MeldSource.result = PrintingId.MkPrintingId 13, MeldSource.components = PrintingId.MkPrintingId 14 NonEmpty.:| [PrintingId.MkPrintingId 15]})
      " {\"type\":\"OfMeld\",\"value\":{\"components\":[14,15],\"result\":13}} "
  -- CR 730.2's "or copy": the third tag over the printing payload, which is what
  -- separates it from the two above.
  Spec.it s "OfSpellCopy" $
    Common.assertCodec
      s
      MergeComponent.codec
      (MergeComponent.OfSpellCopy (PrintingId.MkPrintingId 16))
      " {\"type\":\"OfSpellCopy\",\"value\":16} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s MergeComponent.codec
