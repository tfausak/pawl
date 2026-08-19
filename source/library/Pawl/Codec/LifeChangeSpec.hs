module Pawl.Codec.LifeChangeSpec where

import qualified Pawl.Codec.LifeChange as LifeChange
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LifeChange" $ do
  -- CR 118.2 / 118.3. Shared by GameEvent's LifeLost and LifeGained, which is
  -- why the record is named for its shape rather than for either arm.
  Spec.it s "MkLifeChange, both keys" $
    Common.assertCodec
      s
      LifeChange.codec
      (LifeChange.MkLifeChange {LifeChange.player = PlayerId.MkPlayerId 0, LifeChange.amount = 3})
      " {\"player\":0,\"amount\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeChange.codec
