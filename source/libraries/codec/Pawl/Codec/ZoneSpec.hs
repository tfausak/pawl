{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ZoneSpec where

import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Zone" $ do
  Spec.it s "Library" $
    Common.assertCodec
      s
      Zone.codec
      Zone.Library
      """ {"type":"Library"} """
  Spec.it s "Hand" $
    Common.assertCodec
      s
      Zone.codec
      Zone.Hand
      """ {"type":"Hand"} """
  Spec.it s "Graveyard" $
    Common.assertCodec
      s
      Zone.codec
      Zone.Graveyard
      """ {"type":"Graveyard"} """
  Spec.it s "Battlefield" $
    Common.assertCodec
      s
      Zone.codec
      Zone.Battlefield
      """ {"type":"Battlefield"} """
  Spec.it s "Stack" $
    Common.assertCodec
      s
      Zone.codec
      Zone.Stack
      """ {"type":"Stack"} """
  Spec.it s "Exile" $
    Common.assertCodec
      s
      Zone.codec
      Zone.Exile
      """ {"type":"Exile"} """
  Spec.it s "Command" $
    Common.assertCodec
      s
      Zone.codec
      Zone.Command
      """ {"type":"Command"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Zone.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Zone.codec
