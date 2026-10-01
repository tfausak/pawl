module Pawl.Codec.AttackTargetRef where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.AttackTargetRef as AttackTargetRef

-- | Reached from card data through Pawl.Codec.RequireAttack's `defender` key.
codec :: Codec.Codec AttackTargetRef.AttackTargetRef
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Players" PlayerRef.codec AttackTargetRef.Players (\x -> case x of AttackTargetRef.Players y -> Just y; _ -> Nothing),
      Arm.payload "Permanents" ObjectRef.codec AttackTargetRef.Permanents (\x -> case x of AttackTargetRef.Permanents y -> Just y; _ -> Nothing)
    ]

tagOf :: AttackTargetRef.AttackTargetRef -> String
tagOf x = case x of
  AttackTargetRef.Players {} -> "Players"
  AttackTargetRef.Permanents {} -> "Permanents"
