module Pawl.Codec.ProtectionSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Protection as Protection
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Protection as Protection
import qualified Pawl.Types.Subtype as Subtype

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Protection.Protection Keyword.Keyword)
codec = Protection.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Protection" $ do
  -- CR 702.16a's quality alone, which is what all but one printing states.
  Spec.it s "MkProtection" $
    Common.assertCodec
      s
      codec
      Protection.MkProtection {Protection.quality = Filter.HasColor Color.Black, Protection.spares = Nothing}
      " {\"quality\":{\"type\":\"HasColor\",\"value\":{\"type\":\"Black\"}},\"spares\":null} "
  -- CR 702.16n: Spectra Ward's "this effect doesn't remove Auras".
  Spec.it s "MkProtection carries CR 702.16n's exception" $
    Common.assertCodec
      s
      codec
      Protection.MkProtection {Protection.quality = Filter.HasColor Color.Blue, Protection.spares = Just (Filter.HasSubtype Subtype.Aura)}
      " {\"quality\":{\"type\":\"HasColor\",\"value\":{\"type\":\"Blue\"}},\"spares\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Aura\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
