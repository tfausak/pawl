module Pawl.Codec.ForetellCostSpec where

import qualified Pawl.Codec.ForetellCost as ForetellCost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.ForetellCost as ForetellCost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (ForetellCost.ForetellCost Keyword.Keyword)
codec = ForetellCost.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ForetellCost" $ do
  Spec.it s "Stated" $
    Common.assertCodec
      s
      codec
      (ForetellCost.Stated (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []))
      " {\"type\":\"Stated\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
  Spec.it s "ManaCostReducedBy" $
    Common.assertCodec
      s
      codec
      (ForetellCost.ManaCostReducedBy (ManaCost.MkManaCost [ManaSymbol.Generic 2]))
      " {\"type\":\"ManaCostReducedBy\",\"value\":[{\"type\":\"Generic\",\"value\":2}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
