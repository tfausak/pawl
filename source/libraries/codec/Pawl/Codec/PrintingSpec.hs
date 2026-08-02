module Pawl.Codec.PrintingSpec where

import qualified Pawl.Codec.CardSpec as CardSpec
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Printing as Printing
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Printing as Printing

-- | R7's one case for MkPrinting's single constructor, which just delegates to
-- Card's own codec (Pawl.Codec.Card.toJson/fromJson) -- so 'CardSpec.baseCard'
-- is reused rather than a second synthetic Card being built here. The
-- registry-backed "honesty round-trip over allPrintings" proof, over every real
-- Printing in the pool, stays in Pawl.CodecIntegrationSpec: this sublibrary sits above
-- Pawl.Registry and cannot reach it.
spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.Printing" . Spec.it s "MkPrinting delegates to Card's own codec" $
    Common.assertJsonCodec s Printing.toJson Printing.fromJson (Printing.MkPrinting CardSpec.baseCard) CardSpec.baseCardJson
