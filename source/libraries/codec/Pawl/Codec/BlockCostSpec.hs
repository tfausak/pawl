-- Covers Pawl.Codec.BlockCost. Pawl.Codec.PerCreature, the one codec it
-- dispatches to that Pawl.Codec.Affected does not, is round-tripped through
-- Pawl.Codec.AttackCostSpec as well: one share type serves both combat costs, so
-- what is proved here is this object's own two keys.
module Pawl.Codec.BlockCostSpec where

import qualified Pawl.Codec.BlockCost as BlockCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.BlockCost as BlockCost
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.PerCreature as PerCreature

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BlockCost" $ do
  -- Oppressive Rays: the subject is what the Aura is attached to, and the cost is
  -- ONE blocker's share (CR 509.1d totals them).
  Spec.it s "MkBlockCost" $
    Common.assertCodec
      s
      BlockCost.codec
      ( BlockCost.MkBlockCost
          Affected.Attached
          (PerCreature.Fixed (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) []))
      )
      " {\"perBlocker\":{\"type\":\"Fixed\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}},\"subject\":{\"type\":\"Attached\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BlockCost.codec
