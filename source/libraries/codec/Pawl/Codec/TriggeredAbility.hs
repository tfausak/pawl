{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TriggeredAbility where

import qualified Data.Map.Strict as Map
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.TriggerCondition as TriggerCondition
import qualified Pawl.Codec.TriggerLimit as TriggerLimit
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (TriggeredAbility.TriggeredAbility card)
codec cardCodec = Fields.object $ do
  condition <- Fields.required "condition" TriggerCondition.codec TriggeredAbility.condition
  modal <- Fields.required "modal" (Modal.codec cardCodec) TriggeredAbility.modal
  intervening <- Fields.defaulted "intervening" Nothing (Common.maybe Condition.codec) TriggeredAbility.intervening
  limit <- Fields.defaulted "limit" TriggerLimit.Unlimited TriggerLimit.codec TriggeredAbility.limit
  pure
    TriggeredAbility.MkTriggeredAbility
      { TriggeredAbility.condition = condition,
        TriggeredAbility.modal = modal,
        TriggeredAbility.intervening = intervening,
        TriggeredAbility.limit = limit
      }

-- | A name-keyed map as a JSON OBJECT keyed by the ability name.
codecDelayed ::
  (Typeable.Typeable card, Eq card) =>
  Codec.Codec card ->
  Codec.Codec (Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility card))
codecDelayed cardCodec = Common.textMap AbilityName.unwrap AbilityName.MkAbilityName (codec cardCodec)
