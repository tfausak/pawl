module Pawl.Codec.SuspendSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Suspend as Suspend
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Suspend as Suspend

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Suspend.Suspend Keyword.Keyword)
codec = Suspend.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Suspend" $ do
  -- CR 702.62a: Rift Bolt's "Suspend 1--{R}".
  Spec.it s "MkSuspend" $
    Common.assertCodec
      s
      codec
      ( Suspend.MkSuspend
          { Suspend.counters = 1,
            Suspend.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Red)]), Cost.components = []}
          }
      )
      " {\"counters\":1,\"cost\":{\"mana\":[{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}}}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
