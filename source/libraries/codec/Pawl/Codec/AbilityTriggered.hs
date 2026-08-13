{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AbilityTriggered where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TriggerCondition as TriggerCondition
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered

-- | A bare object keyed by the record's field names, replacing the positional
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec AbilityTriggered.AbilityTriggered
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec AbilityTriggered.source
  controller <- Fields.required "controller" PlayerId.codec AbilityTriggered.controller
  condition <- Fields.required "condition" TriggerCondition.codec AbilityTriggered.condition
  pure
    AbilityTriggered.MkAbilityTriggered
      { AbilityTriggered.source = source,
        AbilityTriggered.controller = controller,
        AbilityTriggered.condition = condition
      }
