{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerRelationSpec where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerRelation" $ do
  Spec.it s "You" $
    Common.assertCodec
      s
      PlayerRelation.codec
      PlayerRelation.You
      """ {"type":"You"} """
  Spec.it s "Opponent" $
    Common.assertCodec
      s
      PlayerRelation.codec
      PlayerRelation.Opponent
      """ {"type":"Opponent"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PlayerRelation.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlayerRelation.codec
