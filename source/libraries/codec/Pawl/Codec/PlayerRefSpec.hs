{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerRefSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerRef" $ do
  Spec.it s "EachPlayer" $
    Common.assertJsonCodec
      s
      PlayerRef.toJson
      PlayerRef.fromJson
      PlayerRef.EachPlayer
      """ {"type":"EachPlayer"} """
  Spec.it s "Relative" $
    Common.assertJsonCodec
      s
      PlayerRef.toJson
      PlayerRef.fromJson
      (PlayerRef.Relative PlayerRelation.You)
      """ {"type":"Relative","value":{"type":"You"}} """
  Spec.it s "InSlot" $
    Common.assertJsonCodec
      s
      PlayerRef.toJson
      PlayerRef.fromJson
      (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"InSlot","value":"target"} """
  Spec.it s "Specific" $
    Common.assertJsonCodec
      s
      PlayerRef.toJson
      PlayerRef.fromJson
      (PlayerRef.Specific (PlayerId.MkPlayerId 1))
      """ {"type":"Specific","value":1} """
