module Pawl.Codec.AttackTargetRefSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AttackTargetRef as AttackTargetRef
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackTargetRef as AttackTargetRef
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackTargetRef" $ do
  -- Alluring Siren's "attacks you".
  Spec.it s "Players" $
    Common.assertCodec
      s
      AttackTargetRef.codec
      (AttackTargetRef.Players (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"Players\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
  -- Gideon Jura's "attack Gideon Jura".
  Spec.it s "Permanents" $
    Common.assertCodec
      s
      AttackTargetRef.codec
      (AttackTargetRef.Permanents (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Permanents\",\"value\":{\"type\":\"InSlot\",\"value\":\"self\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackTargetRef.codec
