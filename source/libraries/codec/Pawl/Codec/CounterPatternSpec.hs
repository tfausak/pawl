{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CounterPatternSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.CounterPattern as CounterPattern
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterPattern" $ do
  -- Hardened Scales, per the type's own doc: a fixed kind and a real filter.
  Spec.it s "Hardened Scales (a fixed kind, a real filter)" $
    Common.assertJsonCodec
      s
      CounterPattern.toJson
      CounterPattern.fromJson
      CounterPattern.MkCounterPattern
        { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
          CounterPattern.whose = ControllerRelation.Yours,
          CounterPattern.onWhat = Filter.HasCardType CardType.Creature
        }
      """ {"whichKind":{"type":"PlusOnePlusOne"},"whose":{"type":"Yours"},"onWhat":{"type":"HasCardType","value":{"type":"Creature"}}} """
  -- Doubling Season: whichKind = Nothing means ANY kind, never "no kind" -- and
  -- the trivial filter matching every permanent. The omitted key is what an
  -- absent whichKind means (R1 of the omit-defaults design), same as an
  -- explicit JSON null would.
  Spec.it s "Doubling Season (any kind, the trivial filter)" $
    Common.assertJsonCodec
      s
      CounterPattern.toJson
      CounterPattern.fromJson
      CounterPattern.MkCounterPattern
        { CounterPattern.whichKind = Nothing,
          CounterPattern.whose = ControllerRelation.Yours,
          CounterPattern.onWhat = Filter.And []
        }
      """ {"whose":{"type":"Yours"},"onWhat":{"type":"And","value":[]}} """
  -- CR 109.5: whichKind's Nothing and whose's Anyones are both what a pattern
  -- that says nothing means, so only the required onWhat key survives.
  Spec.it s "an all-default value omits every optional key" $
    Common.assertJsonCodec
      s
      CounterPattern.toJson
      CounterPattern.fromJson
      CounterPattern.MkCounterPattern
        { CounterPattern.whichKind = Nothing,
          CounterPattern.whose = ControllerRelation.Anyones,
          CounterPattern.onWhat = Filter.And []
        }
      """ {"onWhat":{"type":"And","value":[]}} """
