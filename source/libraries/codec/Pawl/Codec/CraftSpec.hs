module Pawl.Codec.CraftSpec where

import qualified Pawl.Codec.Craft as Craft
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Craft as Craft
import qualified Pawl.Types.ExileMaterials as ExileMaterials
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Craft.Craft Keyword.Keyword)
codec = Craft.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Craft" $ do
  -- CR 702.167a: Tithing Blade's "Craft with creature {4}{B}".
  Spec.it s "MkCraft" $
    Common.assertCodec
      s
      codec
      ( Craft.MkCraft
          { Craft.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, ManaSymbol.OfType (ManaType.Colored Color.Black)]), Cost.components = []},
            Craft.materials = ExileMaterials.MkExileMaterials {ExileMaterials.count = 1, ExileMaterials.orMore = False, ExileMaterials.whichObjects = Filter.HasCardType CardType.Creature}
          }
      )
      " {\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":4},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Black\"}}}]},\"materials\":{\"count\":1,\"whichObjects\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
