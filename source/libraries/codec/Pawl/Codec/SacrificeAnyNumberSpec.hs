{-# LANGUAGE MultilineStrings #-}

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
  -- CR 614.1c's devour shape: sacrifice creatures, take counters for each.
  Spec.it s "MkSacrificeAnyNumber, a counter kind stated" $
    Common.assertCodec
      s
      SacrificeAnyNumber.codec
      ( SacrificeAnyNumber.MkSacrificeAnyNumber
          { SacrificeAnyNumber.filter = Filter.HasCardType CardType.Creature,
            SacrificeAnyNumber.kind = Just CounterKind.PlusOnePlusOne
          }
      )
      """ {"filter":{"type":"HasCardType","value":{"type":"Creature"}},"kind":{"type":"PlusOnePlusOne"}} """
  -- The key is REQUIRED rather than elided: a null is a real answer here (the
  -- sacrifice places no counters), not an absence.
  Spec.it s "MkSacrificeAnyNumber, no counter kind" $
    Common.assertCodec
      s
      SacrificeAnyNumber.codec
      ( SacrificeAnyNumber.MkSacrificeAnyNumber
          { SacrificeAnyNumber.filter = Filter.HasCardType CardType.Creature,
            SacrificeAnyNumber.kind = Nothing
          }
      )
      """ {"filter":{"type":"HasCardType","value":{"type":"Creature"}},"kind":null} """
  Spec.it s "has a schema" $ Common.assertHasSchema s SacrificeAnyNumber.codec
