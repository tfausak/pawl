module Pawl.Codec.ManaFilterSpec where

import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaFilter" $ do
  -- Upwelling's "unspent mana", with no type named.
  Spec.it s "Any" $
    Common.assertCodec
      s
      ManaFilter.codec
      ManaFilter.Any
      " {\"type\":\"Any\"} "
  -- CR 106.1a / Omnath, Locus of Mana's "unspent green mana".
  Spec.it s "OfType, a colour" $
    Common.assertCodec
      s
      ManaFilter.codec
      (ManaFilter.OfType (ManaType.Colored Color.Green))
      " {\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}} "
  -- CR 106.1b's sixth type, which is not a colour.
  Spec.it s "OfType, colorless" $
    Common.assertCodec
      s
      ManaFilter.codec
      (ManaFilter.OfType ManaType.Colorless)
      " {\"type\":\"OfType\",\"value\":{\"type\":\"Colorless\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaFilter.codec
