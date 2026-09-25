module Pawl.Codec.ImpendingSpec where

import qualified Pawl.Codec.Impending as Impending
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Impending as Impending
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Impending.Impending Keyword.Keyword)
codec = Impending.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Impending" $ do
  -- CR 702.176a: Overlord of the Mistmoors' "Impending 4--{2}{W}{W}".
  Spec.it s "MkImpending" $
    Common.assertCodec
      s
      codec
      ( Impending.MkImpending
          { Impending.counters = 4,
            Impending.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, white, white]), Cost.components = []}
          }
      )
      " {\"counters\":4,\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":2},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}}},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}}}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

white :: ManaSymbol.ManaSymbol
white = ManaSymbol.OfType (ManaType.Colored Color.White)
