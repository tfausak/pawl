module Pawl.Codec.CostBasisSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CostBasis as CostBasis
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CostBasis as CostBasis
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostBasis" $ do
  -- CR 118.6 / CR 118.7, as Flash's "unless you pay its mana cost reduced by
  -- {2}" states it: the slot its first clause bound, and the {2}.
  Spec.it s "MkCostBasis, Flash's reduced mana cost" $
    Common.assertCodec
      s
      CostBasis.codec
      ( CostBasis.MkCostBasis
          { CostBasis.slot = SlotName.MkSlotName (Text.pack "put"),
            CostBasis.reducedBy = ManaCost.MkManaCost [ManaSymbol.Generic 2]
          }
      )
      " {\"slot\":\"put\",\"reducedBy\":[{\"type\":\"Generic\",\"value\":2}]} "
  -- The unreduced reading, which elides the key: {0} off a mana cost is that
  -- mana cost (CR 118.5).
  Spec.it s "MkCostBasis, an unreduced mana cost" $
    Common.assertCodec
      s
      CostBasis.codec
      ( CostBasis.MkCostBasis
          { CostBasis.slot = SlotName.MkSlotName (Text.pack "put"),
            CostBasis.reducedBy = ManaCost.MkManaCost []
          }
      )
      " {\"slot\":\"put\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CostBasis.codec
