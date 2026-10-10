module Pawl.Codec.BoundMeasureSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.BoundMeasure as BoundMeasure
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BoundMeasure as BoundMeasure
import qualified Pawl.Types.Measure as Measure
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BoundMeasure" $ do
  Spec.it s "MkBoundMeasure" $
    Common.assertCodec
      s
      BoundMeasure.codec
      (BoundMeasure.MkBoundMeasure (SlotName.MkSlotName (Text.pack "thatExploitedCreature")) Measure.Toughness)
      " {\"slot\":\"thatExploitedCreature\",\"measure\":{\"type\":\"Toughness\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BoundMeasure.codec
