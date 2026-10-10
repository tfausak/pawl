module Pawl.Codec.EntryPriceSpec where

import qualified Pawl.Codec.EntryPrice as EntryPrice
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EntryPrice as EntryPrice
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryPrice" $ do
  Spec.it s "PayLife (Razorgrass Field)" $
    Common.assertCodec s EntryPrice.codec (EntryPrice.PayLife 3) " {\"type\":\"PayLife\",\"value\":3} "
  Spec.it s "Reveal (Rustic Clachan)" $
    Common.assertCodec
      s
      EntryPrice.codec
      (EntryPrice.Reveal (Filter.HasSubtype Subtype.Kithkin))
      " {\"type\":\"Reveal\",\"value\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Kithkin\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s EntryPrice.codec
