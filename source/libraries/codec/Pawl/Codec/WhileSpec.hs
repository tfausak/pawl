{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.WhileSpec where

import qualified Pawl.Codec.While as While
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.While as While

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.While" $ do
  -- CR 611.2b's baked seat and the condition it is checked as. Runtime-only:
  -- the one thing that serialises this is a DelayedTrigger (CR 603.7b).
  Spec.it s "MkWhile, both keys" $
    Common.assertCodec
      s
      While.codec
      ( While.MkWhile
          { While.player = PlayerId.MkPlayerId 0,
            While.condition = Condition.Compares (Compares.MkCompares (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0))
          }
      )
      """ {"player":0,"condition":{"type":"Compares","value":{"measured":{"type":"Literal","value":0},"comparison":{"type":"Exactly"},"threshold":{"type":"Literal","value":0}}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s While.codec
