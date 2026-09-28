module Pawl.Codec.PlanarDieRolledSpec where

import qualified Pawl.Codec.PlanarDieRolled as PlanarDieRolled
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlanarDieFace as PlanarDieFace
import qualified Pawl.Types.PlanarDieRolled as PlanarDieRolled
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlanarDieRolled" $ do
  Spec.it s "keys the roller and the face" $
    Common.assertCodec
      s
      PlanarDieRolled.codec
      (PlanarDieRolled.MkPlanarDieRolled (PlayerId.MkPlayerId 1) PlanarDieFace.Planeswalker)
      " {\"roller\":1,\"face\":{\"type\":\"Planeswalker\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlanarDieRolled.codec
