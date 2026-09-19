module Pawl.Codec.ControlSidesSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ControlSides as ControlSides
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControlSides as ControlSides
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ControlSides" $ do
  Spec.it s "BetweenTargets" $
    Common.assertCodec
      s
      ControlSides.codec
      (ControlSides.BetweenTargets (SlotName.MkSlotName (Text.pack "creatures")))
      " {\"type\":\"BetweenTargets\",\"value\":\"creatures\"} "
  Spec.it s "WithSource" $
    Common.assertCodec
      s
      ControlSides.codec
      (ControlSides.WithSource (SlotName.MkSlotName (Text.pack "permanent")))
      " {\"type\":\"WithSource\",\"value\":\"permanent\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ControlSides.codec
