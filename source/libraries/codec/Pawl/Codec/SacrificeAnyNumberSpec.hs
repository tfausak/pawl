module Pawl.Codec.SacrificeAnyNumberSpec where

import qualified Pawl.Codec.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SacrificeAnyNumber" $ do
  Spec.it s "MkSacrificeAnyNumber, a counter kind stated" $
    Common.assertCodec
      s
      SacrificeAnyNumber.codec
      ( SacrificeAnyNumber.MkSacrificeAnyNumber
          { SacrificeAnyNumber.each = 1,
            SacrificeAnyNumber.filter = Filter.HasCardType CardType.Creature,
            SacrificeAnyNumber.kind = Just CounterKind.PlusOnePlusOne
          }
      )
      " {\"each\":1,\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"kind\":{\"type\":\"PlusOnePlusOne\"}} "
  -- The key is REQUIRED rather than elided: a null is a real answer here (the
  -- sacrifice places no counters), not an absence.
  Spec.it s "MkSacrificeAnyNumber, no counter kind" $
    Common.assertCodec
      s
      SacrificeAnyNumber.codec
      ( SacrificeAnyNumber.MkSacrificeAnyNumber
          { SacrificeAnyNumber.each = 1,
            SacrificeAnyNumber.filter = Filter.HasCardType CardType.Creature,
            SacrificeAnyNumber.kind = Nothing
          }
      )
      " {\"each\":1,\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"kind\":null} "
  -- CR 702.82a's devour 3: the multiplier is on the wire, so a row that scales
  -- and one that does not are different values.
  Spec.it s "MkSacrificeAnyNumber, a per-sacrifice multiplier" $
    Common.assertCodec
      s
      SacrificeAnyNumber.codec
      ( SacrificeAnyNumber.MkSacrificeAnyNumber
          { SacrificeAnyNumber.each = 3,
            SacrificeAnyNumber.filter = Filter.HasCardType CardType.Creature,
            SacrificeAnyNumber.kind = Just CounterKind.PlusOnePlusOne
          }
      )
      " {\"each\":3,\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"kind\":{\"type\":\"PlusOnePlusOne\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SacrificeAnyNumber.codec
