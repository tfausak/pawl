module Pawl.Codec.MadnessCostSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.MadnessCost as MadnessCost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.MadnessCost as MadnessCost
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (MadnessCost.MadnessCost Keyword.Keyword)
codec = MadnessCost.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MadnessCost" $ do
  Spec.it s "Stated" $
    Common.assertCodec
      s
      codec
      (MadnessCost.Stated (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []))
      " {\"type\":\"Stated\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
  Spec.it s "OwnManaCost" $
    Common.assertCodec
      s
      codec
      MadnessCost.OwnManaCost
      " {\"type\":\"OwnManaCost\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
