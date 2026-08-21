module Pawl.Codec.RequireAttackSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.RequireAttack as RequireAttack
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RequireAttack" $ do
  -- CR 508.1d, Alluring Siren's sentence.
  Spec.it s "MkRequireAttack, all three keys" $
    Common.assertCodec
      s
      RequireAttack.codec
      ( RequireAttack.MkRequireAttack
          { RequireAttack.duration = Duration.UntilEndOfTurn,
            RequireAttack.attacker = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            RequireAttack.defender = PlayerRef.Relative PlayerRelation.You
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"attacker\":{\"type\":\"InSlot\",\"value\":\"target\"},\"defender\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s RequireAttack.codec
