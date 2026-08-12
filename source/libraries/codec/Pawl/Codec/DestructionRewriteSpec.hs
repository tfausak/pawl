{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DestructionRewriteSpec where

import qualified Pawl.Codec.DestructionRewrite as DestructionRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DestructionRewrite" $ do
  Spec.it s "Regenerate" $
    Common.assertCodec
      s
      DestructionRewrite.codec
      DestructionRewrite.Regenerate
      """ {"type":"Regenerate"} """
  -- CR 122.1c's replacement half. Minted from a permanent's shield counters and
  -- never authored on a card, so this codec is the only place its wire form is
  -- pinned.
  Spec.it s "RemoveShieldCounter" $
    Common.assertCodec
      s
      DestructionRewrite.codec
      DestructionRewrite.RemoveShieldCounter
      """ {"type":"RemoveShieldCounter"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s DestructionRewrite.codec
