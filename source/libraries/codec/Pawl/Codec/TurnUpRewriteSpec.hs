{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TurnUpRewriteSpec where

import qualified Pawl.Codec.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TurnUpRewrite" $ do
  Spec.it s "WithCounters (megamorph, CR 702.37b)" $
    Common.assertCodec
      s
      TurnUpRewrite.codec
      (TurnUpRewrite.WithCounters CounterKind.PlusOnePlusOne 1)
      """ {"type":"WithCounters","value":[{"type":"PlusOnePlusOne"},1]} """
  Spec.it s "MayAttachTo (Gift of Doom, CR 303.4k)" $
    Common.assertCodec
      s
      TurnUpRewrite.codec
      (TurnUpRewrite.MayAttachTo (Filter.HasCardType CardType.Creature))
      """ {"type":"MayAttachTo","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s TurnUpRewrite.codec
