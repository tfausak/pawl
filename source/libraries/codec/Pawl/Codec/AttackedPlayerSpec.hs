module Pawl.Codec.AttackedPlayerSpec where

import qualified Pawl.Codec.AttackedPlayer as AttackedPlayer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackedPlayer as AttackedPlayer
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackedPlayer" $ do
  Spec.it s "Related" $
    Common.assertCodec s AttackedPlayer.codec (AttackedPlayer.Related PlayerRelation.You) " {\"type\":\"Related\",\"value\":{\"type\":\"You\"}} "
  Spec.it s "Enchanted" $
    Common.assertCodec s AttackedPlayer.codec AttackedPlayer.Enchanted " {\"type\":\"Enchanted\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackedPlayer.codec
