module Pawl.Codec.PermissionCostSpec where

import qualified Pawl.Codec.PermissionCost as PermissionCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.PermissionCost as PermissionCost

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermissionCost" $ do
  -- Rule 701.65a's airbend: "by paying {2} rather than paying its mana cost".
  Spec.it s "InsteadOfManaCost" $
    Common.assertCodec
      s
      PermissionCost.codec
      (PermissionCost.InsteadOfManaCost (ManaCost.MkManaCost [ManaSymbol.Generic 2]))
      " {\"type\":\"InsteadOfManaCost\",\"value\":[{\"type\":\"Generic\",\"value\":2}]} "
  -- Hama, the Bloodbender.
  Spec.it s "WaterbendManaValue" $
    Common.assertCodec
      s
      PermissionCost.codec
      PermissionCost.WaterbendManaValue
      " {\"type\":\"WaterbendManaValue\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermissionCost.codec
