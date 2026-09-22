module Pawl.Codec.SpliceSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Splice as Splice
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Splice as Splice
import qualified Pawl.Types.Subtype as Subtype

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Splice.Splice Keyword.Keyword)
codec = Splice.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Splice" $ do
  -- CR 702.47a: Desperate Ritual's "Splice onto Arcane {1}{R}".
  Spec.it s "MkSplice" $
    Common.assertCodec
      s
      codec
      ( Splice.MkSplice
          { Splice.onto = Filter.HasSubtype Subtype.Arcane,
            Splice.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]), Cost.components = []}
          }
      )
      " {\"onto\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Arcane\"}},\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}}}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
