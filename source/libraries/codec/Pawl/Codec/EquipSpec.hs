module Pawl.Codec.EquipSpec where

import qualified Pawl.Codec.Equip as Equip
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Equip as Equip
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Subtype as Subtype

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Equip.Equip Keyword.Keyword)
codec = Equip.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Equip" $ do
  -- CR 702.6a: plain equip, so quality is Nothing.
  Spec.it s "MkEquip" $
    Common.assertCodec
      s
      codec
      ( Equip.MkEquip
          { Equip.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []},
            Equip.quality = Nothing
          }
      )
      " {\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]},\"quality\":null} "
  -- CR 702.6c: Dúnedain Blade's "Equip Human {1}".
  Spec.it s "MkEquip with a quality" $
    Common.assertCodec
      s
      codec
      ( Equip.MkEquip
          { Equip.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]), Cost.components = []},
            Equip.quality = Just (Filter.HasSubtype Subtype.Human)
          }
      )
      " {\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"quality\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Human\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
