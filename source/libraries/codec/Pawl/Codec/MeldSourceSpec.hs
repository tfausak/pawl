module Pawl.Codec.MeldSourceSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Codec.MeldSource as MeldSource
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MeldSource as MeldSource
import qualified Pawl.Types.PrintingId as PrintingId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MeldSource" $ do
  -- CR 701.42a: the resulting permanent, and the two cards that represent it.
  -- Every id DISTINCT, and the result's is not one of the components', so an
  -- encoder writing either key into the other would be caught.
  Spec.it s "a result and the cards representing it" $
    Common.assertCodec
      s
      MeldSource.codec
      MeldSource.MkMeldSource
        { MeldSource.result = PrintingId.MkPrintingId 8,
          MeldSource.components = PrintingId.MkPrintingId 9 NonEmpty.:| [PrintingId.MkPrintingId 10]
        }
      " {\"result\":8,\"components\":[9,10]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s MeldSource.codec
