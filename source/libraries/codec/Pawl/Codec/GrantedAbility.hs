module Pawl.Codec.GrantedAbility where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.GrantedAbility as GrantedAbility

-- | The quoted ability a CR 613.1f grant carries, tagged by CR 113.3's ability
-- kind. Nested inside the modification's own @value@ rather than spelled as two
-- modification tags, because Pawl.Codec.Modification is parametric in exactly
-- this type and cannot name either ability codec.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (GrantedAbility.GrantedAbility card)
codec cardCodec =
  Arm.tagged
    [ Arm.payload "Activated" (ActivatedAbility.codec cardCodec) GrantedAbility.Activated (\x -> case x of GrantedAbility.Activated y -> Just y; _ -> Nothing),
      Arm.payload "Triggered" (TriggeredAbility.codec cardCodec) GrantedAbility.Triggered (\x -> case x of GrantedAbility.Triggered y -> Just y; _ -> Nothing)
    ]
