module Pawl.Codec.PlayerRefSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerRef" $ do
  Spec.it s "EachPlayer" $
    Common.assertCodec
      s
      PlayerRef.codec
      PlayerRef.EachPlayer
      " {\"type\":\"EachPlayer\"} "
  Spec.it s "Relative" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.Relative PlayerRelation.You)
      " {\"type\":\"Relative\",\"value\":{\"type\":\"You\"}} "
  Spec.it s "InSlot" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"InSlot\",\"value\":\"target\"} "
  Spec.it s "Specific" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.Specific (PlayerId.MkPlayerId 1))
      " {\"type\":\"Specific\",\"value\":1} "
  Spec.it s "Candidate" $
    Common.assertCodec
      s
      PlayerRef.codec
      PlayerRef.Candidate
      " {\"type\":\"Candidate\"} "
  Spec.it s "ControllerOfBound" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.ControllerOfBound (SlotName.MkSlotName (Text.pack "permanent")))
      " {\"type\":\"ControllerOfBound\",\"value\":\"permanent\"} "
  Spec.it s "an unknown tag is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"EachOpponent\"} ") >>= Codec.decode PlayerRef.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerRef.codec
