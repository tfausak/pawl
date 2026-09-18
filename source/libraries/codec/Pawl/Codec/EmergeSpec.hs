module Pawl.Codec.EmergeSpec where

import qualified Pawl.Codec.Emerge as Emerge
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Emerge as Emerge
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Emerge.Emerge Keyword.Keyword)
codec = Emerge.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Emerge" $ do
  -- CR 702.119a: plain emerge, so quality is Nothing.
  Spec.it s "MkEmerge" $
    Common.assertCodec
      s
      codec
      ( Emerge.MkEmerge
          { Emerge.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 7]), Cost.components = []},
            Emerge.quality = Nothing
          }
      )
      " {\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":7}]},\"quality\":null} "
  -- CR 702.119b: Crabomination's "Emerge from artifact {5}{B}{B}".
  Spec.it s "MkEmerge with a quality" $
    Common.assertCodec
      s
      codec
      ( Emerge.MkEmerge
          { Emerge.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 5]), Cost.components = []},
            Emerge.quality = Just (Filter.HasCardType CardType.Artifact)
          }
      )
      " {\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":5}]},\"quality\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
