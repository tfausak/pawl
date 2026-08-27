module Pawl.Codec.ReplacementProvenanceSpec where

import qualified Pawl.Codec.ReplacementProvenance as ReplacementProvenance
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ReplacementProvenance as ReplacementProvenance

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReplacementProvenance" $ do
  Spec.it s "Printed" $
    Common.assertCodec
      s
      ReplacementProvenance.codec
      ReplacementProvenance.Printed
      " {\"type\":\"Printed\"} "
  Spec.it s "Minted" $
    Common.assertCodec
      s
      ReplacementProvenance.codec
      ReplacementProvenance.Minted
      " {\"type\":\"Minted\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ReplacementProvenance.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ReplacementProvenance.codec
