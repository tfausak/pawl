module Pawl.Codec.VanguardSpec where

import qualified Pawl.Codec.Vanguard as Vanguard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Vanguard as Vanguard

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Vanguard" $ do
  -- CR 313.6 / 313.7: Gerrard's pair, whose life modifier is a printed zero.
  Spec.it s "a negative hand modifier and a zero life modifier" $
    Common.assertCodec
      s
      Vanguard.codec
      Vanguard.MkVanguard {Vanguard.handModifier = -4, Vanguard.lifeModifier = 0}
      " {\"handModifier\":-4,\"lifeModifier\":0} "
  -- CR 313.6 / 313.7 the other way round, so neither sign is the wire's only
  -- case: Ashnod's pair.
  Spec.it s "a positive hand modifier and a negative life modifier" $
    Common.assertCodec
      s
      Vanguard.codec
      Vanguard.MkVanguard {Vanguard.handModifier = 1, Vanguard.lifeModifier = -8}
      " {\"handModifier\":1,\"lifeModifier\":-8} "
