module Pawl.Codec.ManaUnitSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.ManaUnit as ManaUnit
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaRestriction as ManaRestriction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaUnit" $ do
  -- The mana almost every source makes: no tag, CR 500.5's ordinary retention,
  -- and CR 106.6's restriction absent.
  Spec.it s "plain mana" $
    Common.assertCodec
      s
      ManaUnit.codec
      ManaUnit.MkManaUnit
        { ManaUnit.manaType = ManaType.Colorless,
          ManaUnit.tags = Set.empty,
          ManaUnit.retention = ManaRetention.Ordinary,
          ManaUnit.restriction = Nothing,
          ManaUnit.rider = Nothing,
          ManaUnit.sourceChosenSubtype = Nothing
        }
      " {\"manaType\":{\"type\":\"Colorless\"},\"tags\":[],\"retention\":{\"type\":\"Ordinary\"},\"restriction\":null,\"rider\":null,\"sourceChosenSubtype\":null} "
  -- All six axes at once, each away from its default: CR 107.4h's snow tag, CR
  -- 514.2's retention, Geosurge's "spend this mana only to cast" restriction, and
  -- CR 607.2d's subtype baked off the source (Pillar of Origins).
  Spec.it s "snow mana kept until end of turn, spendable only on creature spells" $
    Common.assertCodec
      s
      ManaUnit.codec
      ManaUnit.MkManaUnit
        { ManaUnit.manaType = ManaType.Colored Color.Red,
          ManaUnit.tags = Set.singleton ProductionTag.Snow,
          ManaUnit.retention = ManaRetention.UntilEndOfTurn,
          ManaUnit.restriction = Just (ManaRestriction.onlyCasts (Filter.HasCardType CardType.Creature)),
          ManaUnit.rider = Nothing,
          ManaUnit.sourceChosenSubtype = Just Subtype.Goblin
        }
      " {\"manaType\":{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}},\"tags\":[{\"type\":\"Snow\"}],\"retention\":{\"type\":\"UntilEndOfTurn\"},\"restriction\":{\"casts\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"rider\":null,\"sourceChosenSubtype\":{\"type\":\"Goblin\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaUnit.codec
