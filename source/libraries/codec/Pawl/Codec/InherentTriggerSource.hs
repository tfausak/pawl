{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.InherentTriggerSource where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource

-- | The key is `controller` rather than `source`, which is the whole of what
-- separates this from Pawl.Codec.TriggeredAbilitySource: CR 725.2's inherent
-- trigger has no source object to name.
codec :: Codec.Codec InherentTriggerSource.InherentTriggerSource
codec = Fields.object $ do
  controller <- Fields.required "controller" PlayerId.codec InherentTriggerSource.controller
  ability <- Fields.required "ability" (TriggeredAbility.codec Card.codec) InherentTriggerSource.ability
  pure
    InherentTriggerSource.MkInherentTriggerSource
      { InherentTriggerSource.controller = controller,
        InherentTriggerSource.ability = ability
      }
