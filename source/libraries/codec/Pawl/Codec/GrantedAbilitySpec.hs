module Pawl.Codec.GrantedAbilitySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.GrantedAbility as GrantedAbility
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | The `card` parameter is instantiated at 'Text.Text', reached only through the
-- Modal codec, the posture 'Pawl.Codec.ActivatedAbilitySpec' takes.
codec :: Codec.Codec (GrantedAbility.GrantedAbility Text.Text)
codec = GrantedAbility.codec Common.text

emptyModal :: Modal.Modal Text.Text
emptyModal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)

-- Both arms, since the whole point of the type is that CR 613.1f's grant reaches
-- two of CR 113.3's ability kinds and the wire has to say which.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GrantedAbility" $ do
  -- Presence of Gond's "{T}: ..." reduced to its cost.
  Spec.it s "Activated" $
    Common.assertCodec
      s
      codec
      ( GrantedAbility.Activated
          ( ActivatedAbility.MkActivatedAbility
              (Cost.MkCost Nothing [CostComponent.TapThis])
              emptyModal
              []
              Nothing
          )
      )
      " {\"type\":\"Activated\",\"value\":{\"cost\":{\"mana\":null,\"components\":[{\"type\":\"TapThis\"}]},\"modal\":{\"modes\":[{}]}}} "
  -- Sixth Sense's "Whenever this creature deals combat damage to a player, ..."
  -- reduced to its condition.
  Spec.it s "Triggered" $
    Common.assertCodec
      s
      codec
      ( GrantedAbility.Triggered
          ( TriggeredAbility.MkTriggeredAbility
              TriggerCondition.SelfDealsCombatDamageToPlayer
              emptyModal
              Nothing
              TriggerLimit.Unlimited
          )
      )
      " {\"type\":\"Triggered\",\"value\":{\"condition\":{\"type\":\"SelfDealsCombatDamageToPlayer\"},\"modal\":{\"modes\":[{}]}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
