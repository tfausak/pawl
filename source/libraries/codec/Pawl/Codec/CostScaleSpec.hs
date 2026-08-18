module Pawl.Codec.CostScaleSpec where

import qualified Pawl.Codec.CostScale as CostScale
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CostScale as CostScale

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostScale" $ do
  Spec.it s "Once" $
    Common.assertCodec
      s
      CostScale.codec
      CostScale.Once
      " {\"type\":\"Once\"} "
  -- Drought's "for each black mana symbol". Black rather than a colour picked at
  -- random: it is the one a card in the pool writes.
  Spec.it s "PerColoredSymbol" $
    Common.assertCodec
      s
      CostScale.codec
      (CostScale.PerColoredSymbol Color.Black)
      " {\"type\":\"PerColoredSymbol\",\"value\":{\"type\":\"Black\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CostScale.codec
