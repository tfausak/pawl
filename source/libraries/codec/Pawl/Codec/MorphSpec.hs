module Pawl.Codec.MorphSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Morph as Morph
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.MorphVariant as MorphVariant

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Morph.Morph Keyword.Keyword)
codec = Morph.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Morph" $ do
  -- CR 702.37b: megamorph is this arm with a variant, not a sibling.
  Spec.it s "MkMorph" $
    Common.assertCodec
      s
      codec
      ( Morph.MkMorph
          { Morph.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []},
            Morph.variant = MorphVariant.Mega
          }
      )
      " {\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]},\"variant\":{\"type\":\"Mega\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
