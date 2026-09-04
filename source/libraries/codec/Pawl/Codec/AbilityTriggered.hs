{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AbilityTriggered where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.GrantedAbility as GrantedAbility
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TriggerSource as TriggerSource
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered

-- | A bare object keyed by the record's field names, replacing the positional
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
--
-- The ABILITY under its own key rather than the trigger condition alone, which
-- is what the record used to carry: the condition does not say which of an
-- object's abilities triggered. Pawl.Codec.TriggeredAbilitySource is the same
-- ability beside the same kind of key, once the ability is on the stack.
codec :: Codec.Codec AbilityTriggered.AbilityTriggered
codec = Fields.object $ do
  source <- Fields.required "source" TriggerSource.codec AbilityTriggered.source
  controller <- Fields.required "controller" PlayerId.codec AbilityTriggered.controller
  ability <- Fields.required "ability" (TriggeredAbility.codec Card.codec (GrantedAbility.codec Card.codec)) AbilityTriggered.ability
  pure
    AbilityTriggered.MkAbilityTriggered
      { AbilityTriggered.source = source,
        AbilityTriggered.controller = controller,
        AbilityTriggered.ability = ability
      }
