{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.MonarchTargetSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.MonarchTarget as MonarchTarget
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MonarchTarget" $ do
  Spec.it s "TheController" $
    Common.assertCodec
      s
      MonarchTarget.codec
      MonarchTarget.TheController
      """ {"type":"TheController"} """
  Spec.it s "ControllerOfSource" $
    Common.assertCodec
      s
      MonarchTarget.codec
      MonarchTarget.ControllerOfSource
      """ {"type":"ControllerOfSource"} """
  Spec.it s "InSlot" $
    Common.assertCodec
      s
      MonarchTarget.codec
      (MonarchTarget.InSlot (SlotName.MkSlotName (Text.pack "player")))
      """ {"type":"InSlot","value":"player"} """
  Spec.it s "has a schema" $ Common.assertHasSchema s MonarchTarget.codec
