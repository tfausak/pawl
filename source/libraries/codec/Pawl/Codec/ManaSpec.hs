module Pawl.Codec.ManaSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Mana as Mana
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ProductionTag as ProductionTag

-- | The unit almost every source makes, spelled once so the cases below differ
-- only in what this spec is about.
plain :: ManaType.ManaType -> ManaUnit.ManaUnit
plain manaType =
  ManaUnit.MkManaUnit
    { ManaUnit.manaType = manaType,
      ManaUnit.tags = Set.empty,
      ManaUnit.retention = ManaRetention.Ordinary,
      ManaUnit.restriction = Nothing,
      ManaUnit.rider = Nothing
    }

plainJson :: String -> String
plainJson manaType =
  "{\"manaType\":" <> manaType <> ",\"tags\":[],\"retention\":{\"type\":\"Ordinary\"},\"restriction\":null,\"rider\":null}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Mana" $ do
  -- CR 106.4: an empty pool, which is what every player has at the end of every
  -- step and phase -- that rule's own "each player's mana pool empties".
  Spec.it s "an empty pool" $
    Common.assertCodec s Mana.codec (Mana.MkMana []) " [] "
  -- CR 106.4's unspent mana, spelled as a list because the units are not fungible:
  -- the two red units here are equal and BOTH must survive, which a set would
  -- collapse and a count-per-type could not tell from one red plus one snow red.
  Spec.it s "two indistinguishable red units beside a snow one" $
    Common.assertCodec
      s
      Mana.codec
      ( Mana.MkMana
          [ plain (ManaType.Colored Color.Red),
            plain (ManaType.Colored Color.Red),
            ManaUnit.MkManaUnit
              { ManaUnit.manaType = ManaType.Colored Color.Red,
                ManaUnit.tags = Set.singleton ProductionTag.Snow,
                ManaUnit.retention = ManaRetention.Ordinary,
                ManaUnit.restriction = Nothing,
                ManaUnit.rider = Nothing
              }
          ]
      )
      ( " ["
          <> plainJson "{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}}"
          <> ","
          <> plainJson "{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}}"
          <> ",{\"manaType\":{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}},\"tags\":[{\"type\":\"Snow\"}],\"retention\":{\"type\":\"Ordinary\"},\"restriction\":null,\"rider\":null}] "
      )
  Spec.it s "has a schema" $
    Common.assertHasSchema s Mana.codec
