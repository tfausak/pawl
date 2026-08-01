module Pawl.Codec.CostSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Cost" $ do
  Spec.it s "MkCost, with a mana part and components" $
    Common.assertJsonCodec
      s
      Cost.toJson
      Cost.fromJson
      Cost.MkCost
        { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
          Cost.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
        }
      "{\"mana\":[{\"type\":\"Generic\",\"value\":4}],\"components\":[{\"type\":\"TapThis\"},{\"type\":\"SacrificeThis\"}]}"
  -- CR 118.5a: {0} is a real, payable cost, and ManaCost's empty list IS {0}.
  -- This is the shape every migrated ability now carries.
  Spec.it s "MkCost, {0} and no components" $
    Common.assertJsonCodec
      s
      Cost.toJson
      Cost.fromJson
      Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost []), Cost.components = []}
      "{\"mana\":[],\"components\":[]}"
  -- CR 118.6: an ABSENT mana field is an UNPAYABLE cost, not {0}. This is the
  -- footgun the corpus migration exists to avoid, pinned so a future card file
  -- cannot lose its mana field unnoticed.
  Spec.it s "an omitted mana field decodes to Nothing, not to {0}" $
    Common.assertFromJson
      s
      Cost.fromJson
      "{\"components\":[]}"
      Cost.MkCost {Cost.mana = Nothing, Cost.components = []}
