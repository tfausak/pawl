module Pawl.Codec.TriggeredAbilitySourceSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Codec.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource

-- | What the ability itself encodes to is Pawl.Codec.TriggeredAbility's own
-- case; a bare enters trigger is enough to show the two keys this record adds
-- around it.
ability :: TriggeredAbility.TriggeredAbility Card.Card
ability =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfEnters,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode Seq.empty Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TriggeredAbilitySource" $ do
  -- CR 603.3: the source permanent's id travels with the ability, which is why
  -- the ability resolves after the source has left (CR 603.3d). The key is
  -- `source`, and Pawl.Codec.InherentTriggerSource's is `controller`: the two
  -- records carry the same ability under different first fields.
  Spec.it s "a triggered ability and the id of what it came from" $
    Common.assertCodec
      s
      TriggeredAbilitySource.codec
      TriggeredAbilitySource.MkTriggeredAbilitySource
        { TriggeredAbilitySource.source = ObjectId.MkObjectId 6,
          TriggeredAbilitySource.ability = ability
        }
      " {\"source\":6,\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s TriggeredAbilitySource.codec
